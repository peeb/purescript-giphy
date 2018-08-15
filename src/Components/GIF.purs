module Components.GIF where

import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foreign (Foreign, ForeignError)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bulma as HB
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import Simple.JSON as JSON
import Utils (apiURL)

-- Giphy API

type GiphyResponse = { data :: GIF }

type GIF =
  { image_url :: String
  , title :: String
  }

fetch :: M.Fetch
fetch = M.fetch windowFetch

decodeToGiphyResponse :: Foreign -> Either (NonEmptyList ForeignError) GiphyResponse
decodeToGiphyResponse = JSON.read

-- | Get a random `GIF` for the given search term
getRandomGIF :: String -> Aff (Maybe GIF)
getRandomGIF s = do
  response <- M.json =<< fetch (M.URL $ apiURL s) M.defaultFetchOptions
  pure $ case decodeToGiphyResponse response of
    Right { data: gif } -> Just gif
    Left _ -> Nothing

-- Halogen Component

type State =
  { isLoading :: Boolean
  , result :: Maybe GIF
  , searchTerm :: String
  }

initialState :: State
initialState =
  { isLoading: false
  , result: Nothing
  , searchTerm: ""
  }

data Query a
  = SetSearchTerm String a
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

  render :: State -> H.ComponentHTML Query
  render { isLoading, result, searchTerm } =
    HH.section [ HP.class_ HB.container ]
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
            Just { image_url, title } ->
              [ HH.div_
                  [ HH.br_
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

  eval :: Query ~> H.ComponentDSL State Query Void Aff
  eval =
    case _ of
      SetSearchTerm searchTerm next -> do
        H.modify_ $ _ { searchTerm = searchTerm }
        pure next
      MakeRequest next -> do
        H.modify_ $ _ { isLoading = true }
        searchTerm <- H.gets _.searchTerm
        result <- H.liftAff $ getRandomGIF searchTerm
        H.modify_ $ _ { isLoading = false, result = result }
        pure next
