module Main where

import Prelude

import App.Controller.PuzzleController as PuzzleController
import App.Data.Puzzle (savePuzzle)
import App.Data.Sudoku.Board (Size(..))
import App.Data.Sudoku.Field (Value(..))
import App.Wasm.Generator (generateSudoku)
import App.Widget.FreeGame as Game
import App.Widget.Keyboard as Keyboard
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), attempt, delay, error, forkAff, invincible, joinFiber, killFiber, launchAff_, message, supervise, try)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (localStorage)

main :: Effect Unit
main = do
  launchAff_ $ supervise $ do 
    m <- forkAff $ do
      delay $ Milliseconds 5000.0
      pure "Marek"
    void $ forkAff $ do
      delay $ Milliseconds 3000.0
      killFiber (error "Kill") m
    res <- attempt $ joinFiber m
    case res of 
      Right r -> liftEffect $ log $ r
      Left err -> liftEffect $ log $ "C"

  HA.runHalogenAff do
    body <- HA.awaitBody
    w <- liftEffect $ window
    storage <- liftEffect $ localStorage w
    runUI Game.component (Size 3) body