module Giphy (GIF(..), SearchTerm, apiURL, getRandom) where

import Prelude

import Control.Monad.Aff (Aff)

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Network.HTTP.Affjax (AJAX, URL, get)

type APIKey     = String
type SearchTerm = String

newtype GIF =
  GIF { title :: String
      , url   :: URL
      }

instance decodeJsonGIF :: DecodeJson GIF where
  decodeJson json = do
    o <- decodeJson json
    url <- o .? "image_url"
    title <- o .? "title"
    pure $ GIF { title, url }

-- | Get a random `GIF` for the given search term
getRandom :: forall eff. SearchTerm -> Aff (ajax :: AJAX | eff) (Maybe GIF)
getRandom searchTerm = do
  response <- get $ apiURL searchTerm
  let result = do
        o <- decodeJson response.response
        d <- o .? "data"
        decodeJson d
  pure $ case result of
    Right (GIF gif) -> Just $ GIF gif
    Left _          -> Nothing

apiURL :: SearchTerm -> URL
apiURL searchTerm =
  let
    baseURL = "https://api.giphy.com/v1/gifs/random"
    apiKey  = "dc6zaTOxFJmzC"
  in
  baseURL <> "?api_key=" <> apiKey <> "&tag=" <> searchTerm
