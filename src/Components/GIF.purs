module Components.GIF (Query(..), State, ui) where

import Prelude

import Control.Monad.Aff (Aff)

import Data.Giphy (GIF(..), SearchTerm, getRandom)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events  as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB

import Network.HTTP.Affjax as AX

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
    HH.section_ $
      [ HH.form
          [ HP.class_ HB.container ]
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
          , HH.p_
              [ HH.small
                  [ HP.class_ HB.textMuted ]
                  [ HH.text $ if loading then "Working..." else "" ]
              ]
          , HH.div_
              [ HH.img [ HP.src url ] ]
          ]
      ]
      where

        url :: AX.URL
        url =
          case result of
            Nothing -> "./default.gif"
            Just (GIF gif) -> gif.url

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
