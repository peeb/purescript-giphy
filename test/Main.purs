module Test.Main where

import Prelude

import Components.GIF (apiURL)
import Data.Array (head, index)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, split)
import Effect (Effect)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)


giphyAPI :: Spec Unit
giphyAPI =
  describe "Giphy API" do
    it "is secure" do
      let parts = split (Pattern ":") (apiURL "kittens")
      (head parts) `shouldEqual` (Just "https")
    it "has correct version" do
      let parts = split (Pattern "/") (apiURL "kittens")
      (index parts 3) `shouldEqual` (Just "v1")
    it "contains tag" $
      quickCheck \tag -> do
        let pattern = Pattern $ "&tag=" <> tag
            url = apiURL tag
        contains pattern url === true

main :: Effect Unit
main = run [consoleReporter] giphyAPI
