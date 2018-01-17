module App.Giphy (Gif(..), SearchTerm, getRandomGif) where

import Prelude

import Control.Monad.Aff (Aff)

import Data.Argonaut (class DecodeJson, (.?), decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Network.HTTP.Affjax as AX

-- | Type alias for SearchTerm
type SearchTerm = String

-- | Type representing a Giphy API response
newtype GiphyResponse = GiphyResponse { gif :: Gif }

-- | `DecodeJson` instance for `GiphyResponse` type
instance decodeGiphyResponse :: DecodeJson GiphyResponse where
  decodeJson json = do
    obj <- decodeJson json
    gif <- obj .? "data"
    pure $ GiphyResponse { gif }

-- | Type representing a Giphy GIF
newtype Gif = Gif { url :: AX.URL }

-- | `DecodeJson` instance for `Gif` type
instance decodeGif :: DecodeJson Gif where
  decodeJson json = do
    obj <- decodeJson json
    url <- obj .? "image_url"
    pure $ Gif { url }

-- | Attempt to get a random `Gif` for the given search term.
-- | Return either `Just Gif` or `Nothing`.
getRandomGif :: forall eff. SearchTerm -> Aff (ajax :: AX.AJAX | eff) (Maybe Gif)
getRandomGif searchTerm = do
  result <- apiUrl searchTerm # AX.get
  pure $ case decodeJson result.response of
    Right (GiphyResponse { gif }) -> Just gif
    Left _ -> Nothing

-- | Build a Giphy API URL from the given search term
apiUrl :: SearchTerm -> AX.URL
apiUrl searchTerm =
  let
    apiKey  = "dc6zaTOxFJmzC"
    baseURL = "http://api.giphy.com/v1/gifs/random"
  in
    baseURL <> "?api_key=" <> apiKey <> "&tag=" <> searchTerm
