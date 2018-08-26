module Giphy.URL (buildURL) where

import Prelude

buildURL :: String -> String
buildURL tag =
  let
    baseURL = "https://api.giphy.com/v1/gifs/random"
    apiKey  = "dc6zaTOxFJmzC"
  in
  baseURL <> "?api_key=" <> apiKey <> "&tag=" <> tag
