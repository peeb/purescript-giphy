module App.Component (Query(..), State, ui) where

import Prelude

import Control.Monad.Aff (Aff)

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events  as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB

import Network.HTTP.Affjax as AX

import App.Giphy (GIF(..), SearchTerm, getRandom)

-- | Component state
type State =
  { loading :: Boolean
  , searchTerm :: SearchTerm
  , result :: Maybe GIF
  }

-- | Component query algebra
data Query a
  = SetSearchTerm SearchTerm a
  | MakeRequest a

-- | Component definition
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
    { loading: false
    , searchTerm: ""
    , result: Nothing
    }

  render :: State -> H.ComponentHTML Query
  render { loading, result, searchTerm } =
    HH.div_ $
      [ HH.h1
          [ HP.class_ HB.jumbotron ]
          [ HH.div
              [ HP.class_ HB.container ]
              [ HH.text "PureScript GIF-o-matic" ]
          ]
      , HH.form
          [ HP.class_ HB.container ]
          [ HH.label_
              [ HH.div_
                  [ HH.text "Enter a search term" ]
              , HH.input
                  [ HP.value searchTerm
                  , HE.onValueInput $ HE.input SetSearchTerm
                  ]
              ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnSuccess ]
              , HP.disabled loading
              , HE.onClick $ HE.input_ MakeRequest
              ]
              [ HH.text "Go!" ]
          , HH.p_ [ HH.text $ if loading then "Working..." else "" ]
          , HH.div_
              [ HH.img [ HP.src url ] ]
          ]
      ]
      where

        url :: AX.URL
        url =
          case result of
            Nothing -> "./default.gif"
            Just (GIF { url }) -> url


  eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX | eff))
  eval = case _ of
    SetSearchTerm searchTerm next  -> do
      H.modify $ _ { searchTerm = searchTerm }
      pure next
    MakeRequest next -> do
      H.modify $ _ { loading = true }
      searchTerm <- H.gets _.searchTerm
      gif <- getRandom searchTerm # H.liftAff
      H.modify $ _ { loading = false, result = gif }
      pure next
