module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

import Network.HTTP.Affjax (AJAX)

import Components.GIF (ui)

main :: Eff (HalogenEffects (ajax :: AJAX)) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui unit body
