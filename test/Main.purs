module Test.Main where

import Prelude

import Components.GIF (apiURL)
import Effect (Effect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)


-- | Giphy API tests
giphyAPI :: Spec Unit
giphyAPI =
  describe "Giphy API" do
    it "URL is correct" do
      let expected = "https://api.giphy.com/v1/gifs/random"
                     <> "?api_key=dc6zaTOxFJmzC"
                     <> "&tag=kittens"
      (apiURL "kittens") `shouldEqual` expected

main :: Effect Unit
main = run [consoleReporter] do
  giphyAPI
