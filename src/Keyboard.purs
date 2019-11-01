module Keyboard (KEYPRESS, KeyPressF, runRealKeyPress, waitForKey, endProcess) where

import Prelude

import Control.Monad.Free (Free)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Run (AFF, FProxy, Run, SProxy(..), liftAff, on)
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

realKeyPress :: forall r. KeyPressF ~> Run (aff :: AFF | r)
realKeyPress (WaitForKey reply) = do
  keyEvent <- liftAff $ fromEffectFnAff getNextKey
  pure $ (reply $ keyEvent)
realKeyPress (EndProcess next) = do
  liftAff $ fromEffectFnAff exitProcess
  pure next

runRealKeyPress :: forall r. Run (keyPress :: KEYPRESS, aff :: AFF | r) ~> Run(aff :: AFF | r)
runRealKeyPress = Run.interpretRec(on _keyPress realKeyPress Run.send)

foreign import getNextKey :: EffectFnAff KeyPressEvent
foreign import exitProcess :: EffectFnAff Unit
