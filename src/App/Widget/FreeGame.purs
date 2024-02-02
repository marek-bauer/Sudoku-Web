module App.Widget.FreeGame
  ( State'
  , Query(..)
  , component
  )
  where

import Prelude

import App.Data.Sudoku.Board (Board, Position, Size, getEmptyBoard, isComplete, setAt)
import App.Data.Sudoku.Error (GameState(..))
import App.Data.Sudoku.Field (valueToUserInput)
import App.Logic.Error (calcErrors)
import App.Wasm.Solver (solveSudoku, getHint)
import App.Widget.Board as BoardWidget
import App.Widget.Game (Action, MandatoryAction(..), Slots, _board, mkGameComponent)
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Partial.Unsafe (unsafePartial)

type State' = 
  { board       :: Board 
  , selectedPos :: Maybe Position
  , gameState   :: GameState
  , freeze      :: Boolean
  }

type Input = Size

data Query a = Restart a

component :: forall m o. MonadAff m => H.Component Query Input o m
component = mkGameComponent init handleMandatory (const $ pure unit) handleQuery
  where 
    init :: Input -> State'
    init size = { board: getEmptyBoard size, selectedPos: Nothing, gameState: Incomplite [], freeze: false }

    handleMandatory :: forall c. MandatoryAction Input -> H.HalogenM State' (Action Input c) Slots o m Unit
    handleMandatory = case _ of 
      BoardUpdated board -> do 
        let errors = calcErrors board
        case errors of 
          [] | isComplete board -> do 
            H.tell _board unit BoardWidget.ResetSelection
            H.modify_ $ \s -> s { gameState = Complite } 
          _ -> H.modify_ $ \s -> s { gameState = Incomplite errors } 
      Solve -> do
        { board } <- H.modify $ \s -> s { freeze = true }
        H.tell _board unit BoardWidget.ResetSelection
        mSolution <- liftAff $ runMaybeT $ solveSudoku board
        case mSolution of
          Nothing -> liftEffect $ log "No solutions"
          Just solution -> do 
            H.modify_ $ \s -> s { board = solution, selectedPos = Nothing }
            handleMandatory $ BoardUpdated board
        H.modify_ $ \s -> s { freeze = false }
      Hint -> do 
        { board } <- H.modify $ \s -> s { freeze = true }
        H.tell _board unit BoardWidget.ResetSelection
        mHint <- liftAff $ runMaybeT $ getHint board 2
        case mHint of
          Nothing -> liftEffect $ log "No hint"
          Just { position, digit, level } -> do
            liftEffect $ log $ "Hint level " <> show level
            newState <- H.modify $ \s -> s { board = unsafePartial $ setAt position (valueToUserInput digit) s.board }
            handleMandatory $ BoardUpdated newState.board
        H.modify_ $ \s -> s { freeze = false }
      Refresh size ->
        H.put $ init size
      Final -> pure unit

    handleQuery :: forall c a. Query a -> H.HalogenM State' (Action Input c) Slots o m (Maybe a)
    handleQuery = case _ of 
      Restart a -> do
        { board } <- H.get
        H.put $ init board.size
        pure $ Just a
        