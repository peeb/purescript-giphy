module Test.Main where

import Prelude

import Data.Array ((!!), head)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, split)
import Effect (Effect)
import Giphy.URL (buildURL)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

giphyAPI :: Spec Unit
giphyAPI =
  let
      testURL = buildURL "cute kittens"
  in
  describe "Giphy API URL" do
    it "is secure" do
      let parts = split (Pattern ":") testURL
      head parts `shouldEqual` (Just "https")
    it "has correct domain" do
      let parts = split (Pattern "/") testURL
      (parts !! 2) `shouldEqual` (Just "api.giphy.com")
    it "has correct API version" do
      let parts = split (Pattern "/") testURL
      (parts !! 3) `shouldEqual` (Just "v1")
    it "contains tag" $
      quickCheck \tag -> do
        let pattern = Pattern $ "tag=" <> tag
            url = buildURL tag
        contains pattern url === true

main :: Effect Unit
main = run [consoleReporter] giphyAPI
