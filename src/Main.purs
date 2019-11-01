module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)
import Run (AFF, Run, liftAff)
import Run as Run

import Keyboard (KEYPRESS, waitForKey, runRealKeyPress, endProcess)

testProgram :: forall r. Run(keyPress :: KEYPRESS, aff :: AFF | r) String
testProgram = testProgram_ ""
  where
    testProgram_ :: String -> Run(keyPress :: KEYPRESS, aff :: AFF | r) String
    testProgram_ word = do
      key <- waitForKey
      if (key.ctrl && (eq key.name "q"))
      then do
        liftAff $ log word
        endProcess
        pure word
      else do
        liftAff $ log $ show key
        testProgram_ (word <> key.sequence)

mainLoop :: Aff Unit
mainLoop = do
  str <- Run.runBaseAff interpreted
  log $ "Final word was: " <> str
  where
    interpreted :: forall r. Run(aff :: AFF | r) String
    interpreted = testProgram # runRealKeyPress

main :: Effect Unit
main = do
  launchAff_ mainLoop
