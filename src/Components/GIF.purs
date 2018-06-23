module Components.GIF where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, (.?), decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bulma as HB
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXResponse

type APIKey = String
type SearchTerm = String

newtype GIF =
  GIF { title :: String
      , url :: AX.URL
      }

instance decodeJsonGIF :: DecodeJson GIF where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .? "title"
    url <- obj .? "image_url"
    pure $ GIF { title, url }

-- | Get a random `GIF` for the given search term
getRandomGIF :: SearchTerm -> Aff (Maybe GIF)
getRandomGIF searchTerm = do
  response <- AX.get AXResponse.json $ apiURL searchTerm
  let result = do
        obj <- decodeJson response.response
        gif <- obj .? "data"
        decodeJson gif
  pure $ case result of
    Right (GIF gif) -> Just $ GIF gif
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
  , result :: Maybe GIF
  , searchTerm :: SearchTerm
  }

data Query a
  = SetSearchTerm SearchTerm a
  | MakeRequest a

ui :: H.Component HH.HTML Query Unit Void Aff
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
              [ HP.classes [ HB.button, HB.isLarge, HB.isPrimary ]
              , HP.disabled isLoading
              , HE.onClick $ HE.input_ MakeRequest
              ]
              [ HH.text "Go!" ]
          ]
      , HH.div [ HP.class_ HB.container ]
          case result of
            Nothing -> []
            Just (GIF { title, url }) ->
              [ HH.div_
                  [ HH.div [ HP.classes [ HB.notification, HB.isInfo ] ]
                      [ HH.button [ HP.class_ HB.delete ] []
                      , HH.text $ "I found this " <> searchTerm <> " GIF for you!"
                      ]
                  , HH.figure [ HP.classes [ HB.image, HB.is3By2 ] ]
                      [ HH.img
                          [ HP.alt title
                          , HP.src url
                          , HP.title title
                          ]
                      ]
                  ]
              ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void Aff
  eval = case _ of
    SetSearchTerm searchTerm next -> do
      H.modify_ $ _ { searchTerm = searchTerm }
      pure next
    MakeRequest next -> do
      H.modify_ $ _ { isLoading = true }
      searchTerm <- H.gets _.searchTerm
      result <- H.liftAff $ getRandomGIF searchTerm
      H.modify_ $ _ { isLoading = false, result = result }
      pure next
