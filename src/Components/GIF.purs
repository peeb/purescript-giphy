module Components.GIF where

import Prelude

import Control.Monad.Aff (Aff)

import Data.Argonaut (class DecodeJson, (.?), decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events  as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bulma as HB

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
getRandomGIF :: forall eff. SearchTerm -> Aff (ajax :: AJAX | eff) (Maybe GIF)
getRandomGIF searchTerm = do
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

type State =
  { loading    :: Boolean
  , searchTerm :: SearchTerm
  , result     :: Maybe GIF
  }

data Query a
  = SetSearchTerm SearchTerm a
  | MakeRequest a

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AJAX | eff))
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
    { loading: false
    , searchTerm: ""
    , result: Nothing
    }

  render :: State -> H.ComponentHTML Query
  render { loading, result, searchTerm } =
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
              , HP.disabled loading
              , HE.onClick $ HE.input_ MakeRequest
              ]
              [ HH.text "Go!" ]
          ]
      , HH.div [ HP.class_ HB.container ]
          case result of
            Nothing -> []
            Just (GIF { title, url }) ->
              [ HH.figure
                  [ HP.classes [ HB.figure, HB.is3By2 ] ]
                  [ HH.img
                      [ HP.alt title
                      , HP.src url
                      , HP.title title
                      ]
                  ]
              ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AJAX | eff))
  eval = case _ of
    SetSearchTerm searchTerm next -> do
      H.modify $ _ { searchTerm = searchTerm }
      pure next
    MakeRequest next -> do
      H.modify $ _ { loading = true }
      searchTerm <- H.gets _.searchTerm
      result <- H.liftAff $ getRandomGIF searchTerm
      H.modify $ _ { loading = false, result = result }
      pure next
