module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Halogen.Aff as HA
import Halogen.VDom.Driver as Driver

import Network.HTTP.Affjax as AX

import Components.GIF as GIF

-- | Run the component
main :: Eff (HA.HalogenEffects (ajax :: AX.AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  Driver.runUI GIF.ui unit body
