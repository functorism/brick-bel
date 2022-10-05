{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module Bel where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (foldrM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Graphics.Vty as V
import Optics (Prism', preview)
import Optics.TH (makePrisms)

data Dur = Milliseconds Int | Seconds Int | Minutes Int | Hours Int deriving (Eq, Ord, Show)

durToMicro :: Dur -> Int
durToMicro (Milliseconds i) = i * 1000
durToMicro (Seconds i) = i * 1_000_000
durToMicro (Minutes i) = i * 60_000_000
durToMicro (Hours i) = i * 3_600_000_000

data Sub e
  = Every Dur e
  | FocusIn e
  | FocusOut e
  deriving (Eq, Ord, Show)

makePrisms ''Sub

type Subscriptions s e = s -> [Sub e]

type View s n = s -> Widget n

type Update s e = s -> e -> IO s

newtype Subscription e = Job ThreadId deriving (Eq, Ord, Show)

type SubMap e = Map (Sub e) (Subscription e)

data BelState s e n = BelState
  { subMap :: SubMap e,
    state :: s,
    chan :: BChan e
  }

data Bel s e n = Bel
  { belSubscriptions :: Subscriptions s e,
    belView :: View s n,
    belUpdate :: Update s e,
    belInit :: s,
    belAttrMap :: AttrMap
  }

type BelApp s e n = App (BelState s e n) e n

setState :: BelState s e n -> s -> BelState s e n
setState s s' = s {state = s'}

cleanupSubs :: Eq e => [Sub e] -> SubMap e -> IO (SubMap e)
cleanupSubs subs =
  Map.traverseMaybeWithKey
    (\sub v@(Job tid) -> if sub `elem` subs then pure (Just v) else killThread tid >> pure Nothing)

forkSub :: BChan e -> Sub e -> Maybe (IO (Subscription e))
forkSub chan (Every d e) = Just $ Job <$> forkIO (forever $ writeBChan chan e >> threadDelay (durToMicro d))
forkSub _ _ = Nothing

forkSubs :: Ord e => BChan e -> [Sub e] -> SubMap e -> IO (SubMap e)
forkSubs chan =
  flip
    ( foldrM
        ( \sub ->
            Map.alterF
              (\m -> if isJust m then pure m else sequence (forkSub chan sub))
              sub
        )
    )

manage ::
  Ord e =>
  Bel s e n ->
  BelState s e n ->
  IO (BelState s e n)
manage c cs@BelState {subMap, state, chan} = do
  let subs = belSubscriptions c state
  subMap' <- forkSubs chan subs =<< cleanupSubs subs subMap
  pure $ cs {subMap = subMap'}

updateAndManage ::
  Ord e =>
  Bel s e n ->
  BelState s e n ->
  e ->
  IO (BelState s e n)
updateAndManage c cs e = do
  s' <- belUpdate c (state cs) e
  manage c (cs {state = s'})

handleFocusEvent ::
  Ord e =>
  Bel s e n ->
  BelState s e n ->
  Prism' (Sub e) e ->
  EventM n (Next (BelState s e n))
handleFocusEvent c cs _Case =
  continue
    =<< liftIO
      ( foldrM
          (\sub cs' -> maybe (pure cs') (updateAndManage c cs') (preview _Case sub))
          cs
          (belSubscriptions c (state cs))
      )

belEventHandler ::
  Ord e =>
  Bel s e n ->
  BelState s e n ->
  BrickEvent n e ->
  EventM n (Next (BelState s e n))
belEventHandler c cs (AppEvent e) = continue =<< liftIO (updateAndManage c cs e)
belEventHandler _ cs (VtyEvent (EvKey KEsc _)) = halt cs
belEventHandler c cs (VtyEvent EvLostFocus) = handleFocusEvent c cs _FocusOut
belEventHandler c cs (VtyEvent EvGainedFocus) = handleFocusEvent c cs _FocusIn
belEventHandler _ s _ = continue s

bel ::
  Ord e =>
  Bel s e n ->
  BelState s e n ->
  BelApp s e n
bel c@Bel {belView, belAttrMap} initState =
  App
    { appDraw = \s -> [belView (state s)],
      appChooseCursor = \_ _ -> Nothing,
      appHandleEvent = belEventHandler c,
      appStartEvent = const $ pure initState,
      appAttrMap = const belAttrMap
    }

belMain :: Ord e => Ord n => Bel s e n -> IO s
belMain c = do
  config <- V.userConfig
  vty <- V.mkVty config
  V.setMode (V.outputIface vty) V.Focus True
  chan <- newBChan 100
  cs <- manage c (BelState mempty (belInit c) chan)
  state <$> customMain vty (pure vty) (Just chan) (bel c cs) cs
