module Giphy
  ( GIF
  , SearchTerm
  , URL
  , getRandom
  , getURL
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foreign (Foreign, ForeignError)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import Simple.JSON as JSON

type DecodeErrors = NonEmptyList ForeignError

type GIF =
  { image_url :: URL
  , title :: String
  }

type GiphyResponse = { data :: GIF }

type SearchTerm = String

type URL = String

fetch :: M.Fetch
fetch = M.fetch windowFetch

decodeToGiphyResponse :: Foreign -> Either DecodeErrors GiphyResponse
decodeToGiphyResponse = JSON.read

getRandom :: SearchTerm -> Aff (Maybe GIF)
getRandom searchTerm = do
  let url = M.URL $ getURL searchTerm
  response <- M.json =<< fetch url M.defaultFetchOptions
  pure $ case decodeToGiphyResponse response of
    Right { data: gif } -> Just gif
    Left _ -> Nothing

getURL :: SearchTerm -> URL
getURL searchTerm =
  let
    baseURL = "https://api.giphy.com/v1/gifs/random"
    apiKey  = "dc6zaTOxFJmzC"
  in
  baseURL <> "?api_key=" <> apiKey <> "&tag=" <> searchTerm
