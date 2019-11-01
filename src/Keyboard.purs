module Keyboard (KEYPRESS, KeyPressEvent, KeyPressF(..), waitForKey, endProcess, _keyPress) where

import Prelude

import Control.Monad.Free (Free)
import Run (FProxy, Run, SProxy(..))
import Run as Run

type KeyPressEvent =
  { sequence :: String
  , name :: String
  , ctrl :: Boolean
  , meta :: Boolean
  , shift :: Boolean
  }

data KeyPressF a
  = WaitForKey (KeyPressEvent -> a)
  | EndProcess a

derive instance functorKeyPressF :: Functor KeyPressF

type KeyPress = Free KeyPressF
type KEYPRESS = FProxy KeyPressF
_keyPress = SProxy :: SProxy "keyPress"

waitForKey :: forall r. Run (keyPress :: KEYPRESS | r) KeyPressEvent
waitForKey = Run.lift _keyPress (WaitForKey identity)

endProcess :: forall r. Run (keyPress :: KEYPRESS | r) Unit
endProcess = Run.lift _keyPress (EndProcess unit)

