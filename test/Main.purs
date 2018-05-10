module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Giphy (apiUrl)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Giphy" do
    it "API URL is correct" do
      let expected = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=kittens"
      (apiUrl "kittens") `shouldEqual` expected
