module Main where

import Prelude

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

main :: Effect Unit
main = do 
  -- board <- getFilledDiagonal 3
  launchAff_ $ do 
    _ <- runMaybeT do
      puzzle <- generateSudoku 3 60 100 1 100 
      liftEffect $ log $ fromMaybe "error" $ savePuzzle puzzle
      pure unit
    pure unit
  HA.runHalogenAff do
    -- maybeSolution <- runMaybeT $ solveSudoku board
    -- let solved = case maybeSolution of
    --               Just s -> s
    --               Nothing -> board
    body <- HA.awaitBody
    runUI Game.component unit body