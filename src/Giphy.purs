module Giphy (GIF, SearchTerm, getRandom, getURL) where

import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foreign (ForeignError)
import Network.HTTP.Affjax (URL, get)
import Network.HTTP.Affjax.Response (string)
import Simple.JSON as JSON

type GIF =
  { image_url :: URL
  , title :: String
  }

type GiphyResponse = { data :: GIF }

type SearchTerm = String

decodeToGiphyResponse :: String -> Either (NonEmptyList ForeignError) GiphyResponse
decodeToGiphyResponse = JSON.readJSON

getRandom :: SearchTerm -> Aff (Maybe GIF)
getRandom s = do
  response <- get string $ getURL s
  pure $ case decodeToGiphyResponse response.response of
    Right { data: gif } -> Just gif
    Left _ -> Nothing

getURL :: SearchTerm -> URL
getURL searchTerm =
  let
    baseURL = "https://api.giphy.com/v1/gifs/random"
    apiKey  = "dc6zaTOxFJmzC"
  in
  baseURL <> "?api_key=" <> apiKey <> "&tag=" <> searchTerm
