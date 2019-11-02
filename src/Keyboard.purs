module Keyboard (
  KEYPRESS,
  Direction(..),
  KeyOptions,
  KeyPressEvent(..),
  KeyPressF(..),
  waitForKey,
  beginCapture,
  stopCapture,
  _keyPress,
  KeyPressInterface,
  mkInterface,
  ControlKeyType(..)) where

import Prelude

import Control.Monad.Free (Free)
import Run (FProxy, Run, SProxy(..))
import Run as Run

data Direction = Up | Down | Left | Right
instance showDirection :: Show Direction
  where
    show Up = "Up"
    show Down = "Down"
    show Left = "Left"
    show Right = "Right"

newtype Character = Character String

type KeyOptions =
  { ctrl :: Boolean
  , meta :: Boolean
  , shift :: Boolean
  }

data ControlKeyType = Return | Enter | Esc | Space
instance showControlKeyType :: Show ControlKeyType
  where
    show Return = "<return>"
    show Enter = "<enter>"
    show Esc = "<esc>"
    show Space = "<space>"

data KeyPressEvent
  = Arrow Direction KeyOptions
  | Key String KeyOptions
  | ControlKey ControlKeyType KeyOptions
  | UnsupportedKey

instance showKeyPress :: Show KeyPressEvent
  where
    show (Arrow dir _) = "Arrow" <> show dir
    show (Key k _) = "Key press: " <> k
    show (ControlKey seq _) = "Control key: " <> show seq
    show _ = "Unknown key"

data KeyPressInterface = PrivateKeyPress

mkInterface :: KeyPressInterface
mkInterface = PrivateKeyPress

data KeyPressF a
  = BeginCapture (KeyPressInterface -> a)
  | WaitForKey KeyPressInterface (KeyPressEvent -> a)
  | EndProcess KeyPressInterface a

derive instance functorKeyPressF :: Functor KeyPressF

type KeyPress = Free KeyPressF
type KEYPRESS = FProxy KeyPressF
_keyPress = SProxy :: SProxy "keyPress"

waitForKey :: forall r. KeyPressInterface -> Run (keyPress :: KEYPRESS | r) KeyPressEvent
waitForKey interface = Run.lift _keyPress (WaitForKey interface identity)

stopCapture :: forall r. KeyPressInterface -> Run (keyPress :: KEYPRESS | r) Unit
stopCapture interface = Run.lift _keyPress (EndProcess interface unit)

beginCapture :: forall r. Run(keyPress :: KEYPRESS | r) KeyPressInterface
beginCapture = Run.lift _keyPress (BeginCapture identity)

