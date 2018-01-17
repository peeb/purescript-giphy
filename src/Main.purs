module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Halogen.Aff as HA
import Halogen.VDom.Driver as VDom

import Network.HTTP.Affjax as AX

import App.Component as Component

-- | Run the app
main :: Eff (HA.HalogenEffects (ajax :: AX.AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  VDom.runUI Component.ui unit body
