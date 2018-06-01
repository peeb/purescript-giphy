module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import Test.Giphy (giphyAPI)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  giphyAPI
