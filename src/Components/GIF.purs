module Components.GIF where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Giphy (GIF, SearchTerm, getRandom)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bulma as HB

type State =
  { gif :: Maybe GIF
  , isLoading :: Boolean
  , searchTerm :: SearchTerm
  }

initialState :: State
initialState =
  { gif: Nothing
  , isLoading: false
  , searchTerm: ""
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

  render :: State -> H.ComponentHTML Query
  render { gif, isLoading, searchTerm } =
    HH.section [ HP.class_ HB.container ]
      [ HH.div [ HP.class_ HB.field ]
          [ HH.div [ HP.class_ HB.control ]
              [ HH.input
                  [ HP.class_ HB.input
                  , HP.disabled isLoading
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
          case gif of
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
        gif <- H.liftAff $ getRandom searchTerm
        H.modify_ $ _ { gif = gif, isLoading = false }
        pure next
