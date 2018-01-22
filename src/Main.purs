module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Network.HTTP.Affjax (AJAX)

import Components.GIF (ui)

-- | Run the app
main :: Eff (HA.HalogenEffects (ajax :: AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
