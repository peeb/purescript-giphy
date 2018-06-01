module GIF (Query(..), State, ui) where

import Prelude

import Control.Monad.Aff (Aff)

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events  as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB

import Network.HTTP.Affjax (AJAX)

import Giphy (GIF(..), SearchTerm, getRandom)

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
    HH.section_ $
      [ HH.form
          [ HP.class_ $ H.ClassName "container" ]
          [ HH.div
              [ HP.class_ HB.formGroup ]
              [ HH.input
                  [ HP.class_ HB.formControl
                  , HP.placeholder "Enter search term"
                  , HE.onValueInput $ HE.input SetSearchTerm
                  , HP.value searchTerm
                  ]
              ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnPrimary ]
              , HP.disabled loading
              , HE.onClick $ HE.input_ MakeRequest
              ]
              [ HH.text "Go!" ]
          , HH.div_
              [ HH.small
                  [ HP.class_ HB.textMuted ]
                  [ HH.text $ if loading then "Working..." else "" ]
              ]
          , HH.div_
              case result of
                Nothing -> []
                Just (GIF { title, url }) ->
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
      result <- H.liftAff $ getRandom searchTerm
      H.modify $ _ { loading = false, result = result }
      pure next
