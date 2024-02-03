module Main where

import Prelude

import App.Data.Sudoku.Board (Size(..))
import App.Utils.Device (isMobile)
import App.Widget.SudokuApp as App
import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (localStorage)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    w <- liftEffect $ window
    storage <- liftEffect $ localStorage w
    mobile <- liftEffect $ isMobile
    runUI App.component { size: Size 3, localStorage: storage, isMobile: mobile } body