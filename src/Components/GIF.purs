module Components.GIF where

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
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bulma as HB
import Network.HTTP.Affjax as AX

type APIKey = String
type SearchTerm = String

decoderOptions :: Options
decoderOptions = defaultOptions { unwrapSingleConstructors = true }

newtype GiphyResponse = GiphyResponse { data :: GIF }

derive instance genericGiphyResponse :: Generic GiphyResponse _

instance decodeGiphyResponse :: Decode GiphyResponse where
  decode = genericDecode decoderOptions

instance showGiphyResponse :: Show GiphyResponse where
  show = genericShow

newtype GIF =
  GIF { image_url :: AX.URL
      , title :: String
      }

derive instance genericGIF :: Generic GIF _

instance decodeGIF :: Decode GIF where
  decode = genericDecode decoderOptions

instance showGIF :: Show GIF where
  show = genericShow

-- | Get a random `GIF` for the given search term
getRandomGIF :: forall eff. SearchTerm -> Aff (ajax :: AX.AJAX | eff) (Maybe GIF)
getRandomGIF searchTerm = do
  response <- AX.get $ apiURL searchTerm
  let result = runExcept $ decode response.response
  pure $ case result of
    Right (GiphyResponse { data: gif }) -> Just gif
    Left _ -> Nothing

apiURL :: SearchTerm -> AX.URL
apiURL searchTerm =
  let
    baseURL = "https://api.giphy.com/v1/gifs/random"
    apiKey  = "dc6zaTOxFJmzC"
  in
  baseURL <> "?api_key=" <> apiKey <> "&tag=" <> searchTerm

type State =
  { isLoading :: Boolean
  , searchTerm :: SearchTerm
  , result :: Maybe GIF
  }

data Query a
  = SetSearchTerm SearchTerm a
  | MakeRequest a

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  initialState :: State
  initialState =
    { isLoading: false
    , searchTerm: ""
    , result: Nothing
    }

  render :: State -> H.ComponentHTML Query
  render { isLoading, result, searchTerm } =
    HH.section [ HP.class_ HB.container ] $
      [ HH.div [ HP.class_ HB.field ]
          [ HH.div [ HP.class_ HB.control ]
              [ HH.input
                  [ HP.class_ HB.input
                  , HP.placeholder "Enter search term"
                  , HE.onValueInput $ HE.input SetSearchTerm
                  , HP.value searchTerm
                  ]
              ]
          ]
      , HH.div [ HP.class_ HB.control ]
          [ HH.button
              [ HP.classes [ HB.button, HB.isPrimary ]
              , HP.disabled isLoading
              , HE.onClick $ HE.input_ MakeRequest
              ]
              [ HH.text "Go!" ]
          ]
      , HH.div [ HP.class_ HB.container ]
          case result of
            Nothing -> []
            Just (GIF { image_url, title }) ->
              [ HH.div_
                  [ HH.div [ HP.classes [ HB.notification, HB.isInfo ] ]
                      [ HH.button [ HP.class_ HB.delete ] []
                      , HH.text $ "I found this \"" <> searchTerm <> "\" GIF for you!"
                      ]
                  , HH.figure [ HP.classes [ HB.image, HB.is3By2 ] ]
                      [ HH.img
                          [ HP.alt title
                          , HP.src image_url
                          , HP.title title
                          ]
                      ]
                  ]
              ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX | eff))
  eval = case _ of
    SetSearchTerm searchTerm next -> do
      H.modify $ _ { searchTerm = searchTerm }
      pure next
    MakeRequest next -> do
      H.modify $ _ { isLoading = true }
      searchTerm <- H.gets _.searchTerm
      result <- H.liftAff $ getRandomGIF searchTerm
      H.modify $ _ { isLoading = false, result = result }
      pure next
