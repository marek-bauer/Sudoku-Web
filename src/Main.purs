module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import App.Widget.FreeGame as Game

main :: Effect Unit
main = do 
  -- board <- getFilledDiagonal 3
  HA.runHalogenAff do
    -- maybeSolution <- runMaybeT $ solveSudoku board
    -- let solved = case maybeSolution of
    --               Just s -> s
    --               Nothing -> board
    body <- HA.awaitBody
    runUI Game.component unit body