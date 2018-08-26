module Giphy (GIF, getRandom) where

import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foreign (Foreign, ForeignError)
import Giphy.URL (buildURL)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import Simple.JSON as JSON

type DecodeErrors = NonEmptyList ForeignError

type GIF =
  { image_url :: String
  , title :: String
  }

type GiphyResponse = { data :: GIF }

fetch :: M.Fetch
fetch = M.fetch windowFetch

decodeToGiphyResponse :: Foreign -> Either DecodeErrors GiphyResponse
decodeToGiphyResponse = JSON.read

getRandom :: String -> Aff (Maybe GIF)
getRandom searchTerm = do
  let url = M.URL $ buildURL searchTerm
  response <- M.json =<< fetch url M.defaultFetchOptions
  pure $ case decodeToGiphyResponse response of
    Right { data: gif } -> Just gif
    Left _ -> Nothing
