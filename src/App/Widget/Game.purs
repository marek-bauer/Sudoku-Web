module App.Widget.Game
  ( MandatoryAction(..)
  , Action
  , State
  , Slots
  , _board
  , mkGameComponent
  )
  where

import Prelude

import App.Data.Sudoku.Board (Board, Position, Size, peekAt, setAt, toRowSize)
import App.Data.Sudoku.Error (Error, GameState(..))
import App.Data.Sudoku.Field (Value(..), fieldToValue, unValue, valueToUserInput)
import App.Utils.Partial (runPartial)
import App.Widget.Board as BoardWidget
import App.Widget.Keyboard as KeyboardWidget
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (error)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type State r = 
  { board       :: Board 
  , selectedPos :: Maybe Position
  , gameState   :: GameState
  , freeze      :: Boolean
  | r
  }

data Action i c
  = HandleBoard BoardWidget.Output
  | HandleKeyboard KeyboardWidget.Output
  | Mandatory (MandatoryAction i)
  | Custom c

data MandatoryAction i
  = BoardUpdated Board
  | Solve 
  | Hint
  | Refresh i
  | Final

type Slots = 
  ( board    :: H.Slot BoardWidget.Query BoardWidget.Output Unit
  , keyboard :: forall q. H.Slot q KeyboardWidget.Output Unit
  )

_board = Proxy :: Proxy "board"
_keyboard = Proxy :: Proxy "keyboard"


mkGameComponent :: forall q m i o r c. MonadEffect m 
  => (i -> State r)
  -> (MandatoryAction i -> H.HalogenM (State r) (Action i c) Slots o m Unit)
  -> (c -> H.HalogenM (State r) (Action i c) Slots o m Unit)
  -> (forall a. q a -> H.HalogenM (State r) (Action i c) Slots o m (Maybe a))
  -> H.Component q i o m
mkGameComponent prepareInput handleMandatory handleCustom handleQuery =
  H.mkComponent
    { initialState: prepareInput
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction handleMandatory handleCustom
                                   , receive = Just <<< Mandatory <<< Refresh
                                   , finalize = Just $ Mandatory $ Final
                                   , handleQuery = handleQuery 
                                   }
    }

render :: forall m i c r. State r -> H.ComponentHTML (Action i c) Slots m
render state =
  HH.div [ HP.class_ $ ClassName "game" ]
    [ HH.slot _board unit BoardWidget.component { board: state.board, errors, solved } HandleBoard
    , HH.div [ HP.class_ $ ClassName $ if isSelected then "keyboard-hidden" else "keyboard-show" ]
        [ HH.slot _keyboard unit KeyboardWidget.component { selected: selectedValue, size } HandleKeyboard ]
    , HH.div [ HP.class_ $ ClassName "help-btns" ]
      [ HH.button 
          [ HE.onClick \_ -> Mandatory $ Solve, HP.class_ $ ClassName "solve-btn", HP.disabled solved ] 
          [ HH.text "Solve this sudoku" ]
      , HH.button 
          [ HE.onClick \_ -> Mandatory $ Hint, HP.class_ $ ClassName "solve-btn", HP.disabled solved ] 
          [ HH.text "Get hint" ]
      ]
    ]
  where
    isSelected :: Boolean
    isSelected = isNothing state.selectedPos

    gameState :: Tuple (Array Error) Boolean
    gameState = case state.gameState of 
      Incomplite err -> Tuple err false
      Complite -> Tuple [] true

    errors = fst gameState
    solved = snd gameState

    size :: Size
    size = state.board.size

    selectedValue :: Value 
    selectedValue = fromMaybe (Value 0) $ do
      pos <- state.selectedPos
      field <- runPartial (peekAt pos) state.board
      pure $ fieldToValue field

handleAction :: forall c i r o m. MonadEffect m 
  => (MandatoryAction i -> H.HalogenM (State r) (Action i c) Slots o m Unit)
  -> (c -> H.HalogenM (State r) (Action i c) Slots o m Unit) 
  -> Action i c
  -> H.HalogenM (State r) (Action i c) Slots o m Unit
handleAction handleMandatory handleCustom = case _ of
  HandleBoard msg -> case msg of 
    BoardWidget.ValueInserted pos val -> do
      { board } <- H.get
      when (unValue val <= toRowSize board.size) $ do
        let mNewBoard = runPartial (setAt pos $ valueToUserInput val) board
        case mNewBoard of 
          Just b -> do
            H.modify_ $ \s -> s { board = b }
            handleAction handleMandatory handleCustom $ Mandatory $ BoardUpdated b
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
        handleAction handleMandatory handleCustom $ Mandatory $ BoardUpdated b
      Nothing -> error "Could not updated board"
  Mandatory action -> handleMandatory action
  Custom action -> handleCustom action