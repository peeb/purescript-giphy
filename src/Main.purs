module Main where

import Prelude

import Components.GIF (ui)
import Control.Monad.Eff (Eff)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)

main :: Eff (HalogenEffects (ajax :: AJAX)) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui unit body
