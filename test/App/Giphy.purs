module Test.App.Giphy (giphyAPI) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import App.Giphy (apiURL)

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
