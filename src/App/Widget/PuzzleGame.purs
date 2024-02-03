module App.Widget.PuzzleGame
  ( Output(..)
  , component
  )
  where

import Prelude

import App.Data.Puzzle (Puzzle)
import App.Data.Sudoku.Board (Board, Position, isComplete, setAt, freezeSudoku)
import App.Data.Sudoku.Error (GameState(..))
import App.Data.Sudoku.Field (Value(..), valueToUserInput)
import App.Logic.Difference (emptyDifferances, filledDifferances)
import App.Logic.Error (calcErrors)
import App.Utils.Aff (withTimeout)
import App.Utils.RandomMonad (randomEntry)
import App.Wasm.Solver (getHint)
import App.Widget.Board as BoardWidget
import App.Widget.Game (Action, MandatoryAction(..), Slots, _board, mkGameComponent)
import Control.Monad.Except (except, runExceptT)
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (error, log)
import Halogen as H
import Partial.Unsafe (unsafePartial)

hintTimeout :: Milliseconds
hintTimeout = Milliseconds $ 1000.0 * 5.0

type State' = 
  { board       :: Board 
  , puzzle      :: Puzzle
  , selectedPos :: Maybe Position
  , gameState   :: GameState
  , freeze      :: Boolean
  , isMobile    :: Boolean
  }

type Input = { puzzle :: Puzzle, isMobile :: Boolean }

data Output = Solved 


component :: forall q m. MonadAff m => H.Component q Input Output m
component = mkGameComponent init handleMandatory (const $ pure unit) (const $ pure $ Nothing)
  where
    init :: Input -> State'
    init i = { board: i.puzzle.puzzle
             , puzzle: i.puzzle
             , selectedPos: Nothing
             , gameState: Incomplite []
             , freeze: false
             , isMobile: i.isMobile 
             }
    
    handleMandatory :: forall c. MandatoryAction Input -> H.HalogenM State' (Action Input c) Slots Output m Unit
    handleMandatory = case _ of 
      BoardUpdated board -> do 
        let errors = calcErrors board
        case errors of 
          [] | isComplete board -> do 
            H.modify_ $ \s -> s { gameState = Complite, board = freezeSudoku s.board }
            H.tell _board unit BoardWidget.ResetSelection 
            H.raise Solved
          _ -> H.modify_ $ \s -> s { gameState = Incomplite errors } 
      Solve -> do
        { board } <- H.modify $ \s -> s { board = s.puzzle.solution, selectedPos = Nothing }
        H.tell _board unit BoardWidget.ResetSelection
        handleMandatory $ BoardUpdated $ board
      Hint -> do
        { board, puzzle } <- H.modify $ \s -> s { freeze = true }
        H.tell _board unit BoardWidget.ResetSelection
        let solution = puzzle.solution
        eHint <- runExceptT $ do 
          foundErrors <- except 
            $ note "filledDifferances error" 
            $ filledDifferances board solution
          case foundErrors of 
            [] -> do -- No mistakes
              mHint <- liftAff $ map join $ withTimeout hintTimeout $ runMaybeT $ getHint board 3
              case mHint of 
                Just hint -> pure hint
                Nothing -> do 
                  emptyPositionsWithSolutions <- except 
                    $ note "emptyDifferances error"
                    $ emptyDifferances board solution
                  Tuple position value <- randomEntry emptyPositionsWithSolutions
                  pure $ { position, digit: value, level: 4 }
            errors -> do 
              liftAff $ delay $ Milliseconds 600.0
              position <- randomEntry errors
              pure $ { position, digit: Value 0, level: -1 }
        case eHint of 
          Left e -> error $ e
          Right { position, digit, level } -> do
            log $ "Hint level " <> show level
            newState <- H.modify $ \s -> s { board = unsafePartial $ setAt position (valueToUserInput digit) s.board }
            handleMandatory $ BoardUpdated $ newState.board
        H.modify_ $ \s -> s { freeze = false }
      Refresh input -> do 
        H.modify_ $ const $ init input
        H.tell _board unit BoardWidget.ResetSelection
      Final -> pure unit

