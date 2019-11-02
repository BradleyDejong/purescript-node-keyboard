module RunKeyboard.Console (class ConsoleShow, consoleShow, consoleShowSelected) where

import Prelude

class (Show a) <= ConsoleShow a where
  consoleShow :: a -> String
  consoleShowSelected :: a -> String
