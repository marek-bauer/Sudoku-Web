module Main where

import Prelude

import App.Logic.Generator (getFilledDiagonal)
import App.Wasm.Solver (solveSudoku)
import App.Widget.Board (component)
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do 
  board <- getFilledDiagonal 3
  HA.runHalogenAff do
    maybeSolution <- runMaybeT $ solveSudoku board
    let solved = case maybeSolution of
                  Just s -> s
                  Nothing -> board
    body <- HA.awaitBody
    runUI component { board: solved } body