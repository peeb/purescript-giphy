module Data.Giphy (GIF(..), SearchTerm, apiUrl, getRandom) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)

import Data.Either (Either(..))
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

import Network.HTTP.Affjax as AX

type SearchTerm = String

decoderOptions :: Options
decoderOptions = defaultOptions { unwrapSingleConstructors = true }

newtype GiphyResponse = GiphyResponse { data :: GIF }

derive instance genericGiphyResponse :: Generic GiphyResponse _

instance decodeGiphyResponse :: Decode GiphyResponse where
  decode = genericDecode decoderOptions

instance showGiphyResponse :: Show GiphyResponse where
  show = genericShow

newtype GIF = GIF { image_url :: AX.URL
                  , title :: String
                  }

derive instance genericGIF :: Generic GIF _

instance decodeGIF :: Decode GIF where
  decode = genericDecode decoderOptions

instance showGIF :: Show GIF where
  show = genericShow

-- | Get a random GIF for the given search term
getRandom :: forall eff. SearchTerm -> Aff (ajax :: AX.AJAX | eff) (Maybe GIF)
getRandom searchTerm = do
  result <- AX.get $ apiUrl searchTerm
  let response = runExcept $ decode result.response
  pure $ case response of
    Right (GiphyResponse { data: gif }) -> Just gif
    Left _                              -> Nothing

apiUrl :: SearchTerm -> AX.URL
apiUrl searchTerm =
  let
    apiKey  = "dc6zaTOxFJmzC"
    baseURL = "https://api.giphy.com/v1/gifs/random"
  in
    baseURL <> "?api_key=" <> apiKey <> "&tag=" <> searchTerm
