# brick-bel

_bel (brick elm loop)_

Small wrapper to add subscriptions to Brick apps

``` haskell
module Main where

import Brick (App, str, attrMap)
import Bel (Update, Dur(..), Bel(..), Subscriptions, Sub(..), View, belMain)
import Graphics.Vty (defAttr)
import Control.Monad (void)

data State = State { count :: Int } deriving (Eq, Ord)
data Event = Tick deriving (Eq, Ord)
type Name = ()
type ExampleApp = App State Event Name

state :: State
state = State { count = 0 }

update :: Update State Event
update s Tick = pure $ State { count = 1 + count s }

subscriptions :: Subscriptions State Event
subscriptions _ = [Every (Seconds 1) Tick]

view :: View State Name
view s = str (show (count s))

app :: Bel State Event Name
app =
  Bel
    { belSubscriptions = subscriptions,
      belView = view,
      belUpdate = update,
      belInit = state,
      belAttrMap = attrMap defAttr []
    }

main :: IO ()
main = void $ belMain app
```
