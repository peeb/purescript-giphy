module Main where

import Prelude

import Components.GIF (ui)
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui unit body
