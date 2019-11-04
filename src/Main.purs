module RunKeyboard.Main where

import Prelude
import RunKeyboard.Console

import Ansi.Codes (Color(..), EraseParam(..), EscapeCode(..), GraphicsParam(..), escapeCodeToString)
import Ansi.Output (background, foreground, withGraphics)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (List(..), concat, fromFoldable, intercalate, snoc, unsnoc, (:))
import Data.List.NonEmpty (NonEmptyList(..), fromList, head, singleton, tail)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), Error, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Keyboard (ControlKeyType(..), Direction, KEYPRESS, KeyPressEvent(..), KeyPressInterface, beginCapture, stopCapture, waitForKey)
import Keyboard as Keyboard
import Node.Keyboard (nodeKeyPressRunner)
import Node.ReadLine (Interface, close, createConsoleInterface, noCompletion, question)
import Run (AFF, Run, liftAff)
import Run as Run

type SafeList a =
  { before :: List a
  , ptr :: a
  , after :: List a
  }

initFromNonEmptyList :: forall a. NonEmptyList a -> SafeList a
initFromNonEmptyList l =
  { before: Nil
  , ptr: head l
  , after: tail l
  }

initFromList :: forall a. List a -> Maybe (SafeList a)
initFromList l = initFromNonEmptyList <$> fromList l

moveCursor :: forall a. SafeList a -> Direction -> SafeList a
moveCursor {before: b, ptr: p, after: Nil} Keyboard.Down = { before: b, ptr: p, after: Nil}
moveCursor {before: b, ptr: p, after: x : xs} Keyboard.Down = { before: snoc b p, ptr: x, after: xs}
moveCursor ui Keyboard.Up =
  case unsnoc ui.before of
    Just { init: i, last: l} -> { before: i, ptr: l, after: ui.ptr : ui.after }
    Nothing -> ui
moveCursor ui _ = ui

hardCodedStuff :: SafeList String
hardCodedStuff = {
  before: Nil,
  ptr: "a",
  after: "foo" : "bar" : "baz" : Nil
  }

renderSelectList :: forall a. ConsoleShow a => Ord a => SafeList a -> Set a -> Aff Unit
renderSelectList items selected = do
  log $ escapeCodeToString $ EraseData Entire
  log $ escapeCodeToString $ Position 0 0
  log $ (withGraphics ((foreground BrightBlue) <> (background Black)) "Select one or more work items\n\n")
  log $ intercalate "\n" $ concat $ (renderLines items.before) : (currentLine : Nil) : (renderLines items.after) : Nil
  log $ (withGraphics (singleton (PForeground BrightBlack)) "\nArrows or j/k to move. <space> to toggle selection. <return> to confirm.")
  where
    graphicsFor :: a -> String
    graphicsFor x = if Set.member x selected
                   then consoleShowSelected x
                   else consoleShow x
    renderLines lines = 
      ((\x -> "  " <> (graphicsFor x)) <$> lines)
    currentLine = "► " <> (graphicsFor items.ptr)

render :: forall a. Show a => SafeList a -> Aff Unit
render ui = do
  log $ escapeCodeToString $ EraseData Entire
  log $ intercalate "\n" $ concat $ (renderLines ui.before) : (currentLine : Nil) : (renderLines ui.after) : Nil
  where
    renderLines lines = ((\x -> "  " <> show x) <$> lines)
    currentLine = "► " <> show ui.ptr

multiSelectFromList :: forall r a. Ord a => ConsoleShow a => SafeList a -> Run (keyPress :: KEYPRESS, aff :: AFF | r) (List a)
multiSelectFromList options = do
  interface <- beginCapture
  result <- doMultiSelect interface options Set.empty
  pure $ fromFoldable result
  --stopCapture interface
  where
    doMultiSelect :: Keyboard.KeyPressInterface -> SafeList a -> Set a -> Run (keyPress :: KEYPRESS, aff :: AFF | r) (Set a)
    doMultiSelect interface opts selected = do
      liftAff $ renderSelectList opts selected
      key <- waitForKey interface
      case key of
        ControlKey Enter _ -> do
          pure $ selected
        ControlKey Return _ -> do
          pure $ selected
        ControlKey Space _ -> do
          doMultiSelect interface opts ((toggle opts.ptr selected))
        Arrow dir _ -> do
          doMultiSelect interface (moveCursor opts dir) selected
        Key "j" { ctrl: false, meta: false, shift: false} ->
          doMultiSelect interface (moveCursor opts Keyboard.Down) selected
        Key "k" { ctrl: false, meta: false, shift: false} ->
          doMultiSelect interface (moveCursor opts Keyboard.Up) selected
        _ -> doMultiSelect interface opts selected
    toggle :: forall c. Ord c => c -> Set c -> Set c
    toggle item items
      | Set.member item items = Set.delete item items
      | otherwise = Set.insert item items

pickFromList :: forall r a. Show a => SafeList a -> Run(keyPress :: KEYPRESS, aff :: AFF | r) Unit
pickFromList options = do
  interface <- beginCapture
  result <- testProgram_ interface options
  liftAff $ log $ "Final output: " <> show result
  --stopCapture interface
  where
    testProgram_ :: forall a. Show a => KeyPressInterface -> SafeList a -> Run(keyPress :: KEYPRESS, aff :: AFF | r) a
    testProgram_ interface items = do
      liftAff $ render items
      key <- waitForKey interface
      liftAff $ log $ show key
      case key of
        Key "q" { ctrl: true, meta: _, shift: _} -> do
          pure items.ptr
        Key k _ -> do
          testProgram_ interface items
        Arrow dir _ -> do
          testProgram_ interface (moveCursor items dir)
        _ -> do
          testProgram_ interface items

mainLoop :: Aff Unit
mainLoop = do
  interface <- liftEffect $ createConsoleInterface noCompletion
  res <- askUser interface "HEY"
  log res
  items <- Run.runBaseAff $ (multiSelectFromList hardCodedStuff) # nodeKeyPressRunner
  log $ "items " <> show items

  res2 <- askUser interface "HEY"
  log res2
  liftEffect $ close interface

main :: Effect Unit
main = do
  launchAff_ mainLoop

askUser :: Interface -> String -> Aff String
askUser interface message = do
  res <- makeAff $ go
  pure res
  where
    go :: ((Either Error String -> Effect Unit) -> Effect Canceler)
    go runAffFunction =
      nonCanceler <$ question message (runAffFunction <<< Right) interface
