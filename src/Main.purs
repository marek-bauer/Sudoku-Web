module Main where

import App.Utils.Test
import Prelude

import App.Logic.Generator (getFilledDiagonal)
import App.Logic.Solver (solveSudoku)
-- import App.Utils.Test (_new, _next)
import App.Widget.Board (component)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import App.Utils.Test(new, next)

main :: Effect Unit
main = do 
  board <- getFilledDiagonal 2
  runAff_ (\_ -> log "dasdas") (new unit)
  runAff_ (\x -> case x of
    Right t -> do
      log $ show $ next t
      log $ show $ next t
      log $ show $ next t
      log $ show $ next t
      log $ show $ next t
      log $ show $ next t
    Left _ -> log $ "Error"
  ) (new unit)
  let solved = case solveSudoku board of
                 Nothing -> board
                 Just x -> x
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component { board: solved } body