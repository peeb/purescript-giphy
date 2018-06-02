module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import Components.GIF (apiURL)

-- | Tests for the Giphy API module
giphyAPI :: forall r. Spec r Unit
giphyAPI =
  describe "Giphy API" do
    it "URL is correct" do
      let searchTerm = "kittens"
          expected   = "https://api.giphy.com/v1/gifs/random" <>
                       "?api_key=dc6zaTOxFJmzC" <>
                       "&tag=kittens"
      (apiURL searchTerm) `shouldEqual` expected

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  giphyAPI
