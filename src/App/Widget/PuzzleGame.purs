module App.Widget.PuzzleGame
  ( Action(..)
  , Output(..)
  , State
  , component
  , handleAction
  )
  where

import Prelude

import App.Data.Puzzle (Puzzle)
import App.Data.Sudoku.Board (Board, Position, Size, isComplete, peekAt, setAt)
import App.Data.Sudoku.Error (Error, GameState(..))
import App.Data.Sudoku.Field (Value(..), fieldToValue, valueToUserInput)
import App.Logic.Difference (emptyDiffrances, filledDifferances)
import App.Logic.Error (calcErrors)
import App.Utils.Aff (withTimeout)
import App.Utils.Partial (runPartial)
import App.Utils.RandomMonad (class RandomMonad, randomEntry)
import App.Wasm.Solver (getHint)
import App.Widget.Board as BoardWidget
import App.Widget.Keyboard as KeyboardWidget
import Control.Monad.Except (except, runExceptT)
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (error, log)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

hintTimeout :: Milliseconds
hintTimeout = Milliseconds $ 1000.0 * 5.0

type State = 
  { board :: Board 
  , puzzle :: Puzzle
  , selectedPos :: Maybe Position
  , gameState :: GameState
  , freeze :: Boolean
  }

type Input = Puzzle

data Action 
  = HandleBoard BoardWidget.Output
  | HandleKeyboard KeyboardWidget.Output
  | CheckSolution
  | Solve 
  | Hint

type Slots = 
  ( board    :: H.Slot BoardWidget.Query BoardWidget.Output Unit
  , keyboard :: forall q. H.Slot q KeyboardWidget.Output Unit
  )

data Output = Solved 

_board = Proxy :: Proxy "board"
_keyboard = Proxy :: Proxy "keyboard"


component :: forall q m. RandomMonad m => MonadEffect m => MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \puzzle -> { board: puzzle.puzzle, puzzle, selectedPos: Nothing, gameState: Incomplite [], freeze: false }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  HH.div [ HP.class_ $ ClassName "game" ]
    [ HH.slot _board unit BoardWidget.component { board: state.board, errors: fst gameState, solved: snd gameState } HandleBoard
    , HH.div [ HP.class_ $ ClassName $ if isNothing state.selectedPos then "keyboard-hidden" else "keyboard-show" ]
        [ HH.slot _keyboard unit KeyboardWidget.component { selected: selectedValue, size } HandleKeyboard ]
    , HH.div [ HP.class_ $ ClassName "help-btns" ]
      [ HH.button 
          [ HE.onClick \_ -> Solve, HP.class_ $ ClassName "solve-btn" ] 
          [ HH.text "Solve this sudoku" ]
      , HH.button 
          [ HE.onClick \_ -> Hint, HP.class_ $ ClassName "solve-btn" ] 
          [ HH.text "Get hint" ]
      ]
    ]
  where
    gameState :: Tuple (Array Error) Boolean
    gameState = case state.gameState of 
      Incomplite err -> Tuple err false
      Complite -> Tuple [] true

    size :: Size
    size = state.board.size

    selectedValue :: Value 
    selectedValue = fromMaybe (Value 0) $ do
      pos <- state.selectedPos
      field <- runPartial (peekAt pos) state.board
      pure $ fieldToValue field

handleAction :: forall m. RandomMonad m => MonadEffect m => MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  HandleBoard msg -> case msg of 
    BoardWidget.ValueInserted pos val -> do
      { board } <- H.get
      let mNewBoard = runPartial (setAt pos $ valueToUserInput val) board
      case mNewBoard of 
        Just b -> do
          H.modify_ $ \s -> s { board = b }
          handleAction CheckSolution
        Nothing -> error "Could not updated board"
    BoardWidget.SelectionChanged pos -> 
      H.modify_ $ \s -> s { selectedPos = Just pos }
  HandleKeyboard (KeyboardWidget.Selected val) -> do
    { board, selectedPos } <- H.get
    let 
      mNewBoard = do 
        pos <- selectedPos
        runPartial (setAt pos $ valueToUserInput val) board
    case mNewBoard of 
      Just b -> do
        H.modify_ $ \s -> s { board = b }
        handleAction CheckSolution
      Nothing -> error "Could not updated board"
  CheckSolution -> do 
    { board } <- H.get
    let errors = calcErrors board
    case errors of 
      [] | isComplete board -> do 
        H.modify_ $ \s -> s { gameState = Complite } 
        H.raise Solved
      _ -> H.modify_ $ \s -> s { gameState = Incomplite errors } 
  Solve -> do
    H.modify_ $ \s -> s { board = s.puzzle.solution }
    H.tell _board unit BoardWidget.ResetSelection
  Hint -> do
    { board, puzzle } <- H.modify $ \s -> s { freeze = true }
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
                $ note "emptyDiffrances error"
                $ emptyDiffrances board solution
              Tuple position value <- randomEntry emptyPositionsWithSolutions
              pure $ { position, digit: value, level: 4 }
        errors -> do 
          liftAff $ delay $ Milliseconds 1000.0
          position <- randomEntry errors
          pure $ { position, digit: Value 0, level: -1 }
    case eHint of 
      Left e -> error $ e
      Right { position, digit, level } -> do
        log $ "Hint level " <> show level
        H.modify_ $ \s -> s { board = unsafePartial $ setAt position (valueToUserInput digit) s.board }
    H.modify_ $ \s -> s { freeze = false }
