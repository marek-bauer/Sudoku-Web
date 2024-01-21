module Main where

import Prelude

import App.Controller.PuzzleController as PuzzleController
import App.Data.Puzzle (savePuzzle)
import App.Wasm.Generator (generateSudoku)
import App.Widget.FreeGame as Game
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
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
    runUI PuzzleController.controller {localStorage: storage, size: 3} body