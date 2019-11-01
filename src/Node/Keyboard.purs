module Node.Keyboard where

import Prelude

import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Keyboard (KEYPRESS, KeyPressEvent, KeyPressF(..), _keyPress)
import Run (AFF, Run, liftAff, on)
import Run as Run

realKeyPress :: forall r. KeyPressF ~> Run (aff :: AFF | r)
realKeyPress (WaitForKey reply) = do
  keyEvent <- liftAff $ fromEffectFnAff getNextKey
  pure $ (reply $ keyEvent)
realKeyPress (EndProcess next) = do
  liftAff $ fromEffectFnAff exitProcess
  pure next

nodeKeyPressRunner :: forall r. Run (keyPress :: KEYPRESS, aff :: AFF | r) ~> Run(aff :: AFF | r)
nodeKeyPressRunner = Run.interpretRec(on _keyPress realKeyPress Run.send)

foreign import getNextKey :: EffectFnAff KeyPressEvent
foreign import exitProcess :: EffectFnAff Unit
