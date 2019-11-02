module Node.Keyboard where

import Prelude

import Ansi.Codes (prefix)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), stripPrefix)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Keyboard (ControlKeyType(..), Direction(..), KEYPRESS, KeyPressEvent(..), KeyPressF(..), _keyPress, mkInterface)
import Run (AFF, Run, liftAff, on)
import Run as Run

type RawKeyPress =
  { sequence :: String
  , name :: String
  , ctrl :: Boolean
  , meta :: Boolean
  , shift :: Boolean
  }

toKeyEvent :: RawKeyPress -> KeyPressEvent
toKeyEvent raw = case stripPrefix (Pattern prefix) raw.sequence of
  Just c -> case c of
    "A" -> Arrow Up options
    "B" -> Arrow Down options
    "C" -> Arrow Right options
    "D" -> Arrow Left options
    _ -> UnsupportedKey
  Nothing -> case raw.name of
    "return" -> ControlKey Return options
    "enter" -> ControlKey Enter options
    "escape" -> ControlKey Esc options
    "space" -> ControlKey Space options
    _ -> Key raw.name options
  where
    options = ({ ctrl : raw.ctrl, meta : raw.meta, shift: raw.shift })

realKeyPress :: forall r. KeyPressF ~> Run (aff :: AFF | r)
realKeyPress (BeginCapture reply) = do
  liftAff $ fromEffectFnAff setNodeRawMode
  pure $ reply mkInterface
realKeyPress (WaitForKey _ reply) = do
  keyEvent <- liftAff $ fromEffectFnAff getNextKey
  pure $ (reply $ toKeyEvent keyEvent)
realKeyPress (EndProcess interface next) = do
  liftAff $ fromEffectFnAff exitProcess
  pure next

nodeKeyPressRunner :: forall r. Run (keyPress :: KEYPRESS, aff :: AFF | r) ~> Run(aff :: AFF | r)
nodeKeyPressRunner = Run.interpretRec(on _keyPress realKeyPress Run.send)

foreign import getNextKey :: EffectFnAff RawKeyPress
foreign import exitProcess :: EffectFnAff Unit
foreign import setNodeRawMode :: EffectFnAff Unit
