-- | Widget for displaying sudoku board 
module App.Widget.Board
  ( Input
  , InputRows
  , Output(..)
  , Query(..)
  , component
  )
  where

import Prelude

import App.Data.Sudoku.Board (Position, peekAt)
import App.Data.Sudoku.Board as Board
import App.Data.Sudoku.Error (Error)
import App.Data.Sudoku.Field (Field(..), Value(..), isEditable, stringToValue)
import App.Logic.Error (errorsToInvalidField)
import App.Utils.Array (withIndex)
import App.Utils.Event (eventKey)
import Data.Array (singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (IProp, prop)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)

type State = 
  { focused :: Maybe Position
  | InputRows
  }

type InputRows = 
  ( board    :: Board.Board
  , errors   :: Array Error
  , solved   :: Boolean
  , isMobile :: Boolean
  )

type Input = Record InputRows

data Output 
  = SelectionChanged (Maybe Board.Position)
  | ValueInserted Board.Position Value

data Query a = ResetSelection a

data Action
  = SelectionChange Board.Position
  | ValueInsert Board.Position String
  | Refresh Input

component :: forall m. H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \i -> { focused: Nothing
                          , board: i.board
                          , errors: i.errors
                          , solved: i.solved
                          , isMobile: i.isMobile  
                          }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction 
                                   , receive = \i -> Just $ Refresh i
                                   , handleQuery = handleQuery
                                   }
    }

data BorderType = NormalBorder | BoxBorder | NoBorder 

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state 
  = HH.table
      [HP.style "border-collapse: collapse;", HP.classes $ map ClassName ["board"]]
      (displayBoard sudokuBoard)
    where
      boxSize = Board.getBoxSize state.board
      boardSize = Board.getSize state.board
      sudokuBoard = Board.getSudokuByRows state.board

      redFields :: Set.Set Position
      redFields = errorsToInvalidField boxSize state.errors

      borderForIndex :: Int -> BorderType
      borderForIndex i 
        | i == boardSize - 1 = NoBorder
        | i `mod` boxSize == boxSize - 1 = BoxBorder
        | otherwise = NormalBorder

      displayBoard :: forall w. Array (Array Field) -> Array (HH.HTML w Action)
      displayBoard = map (\(Tuple y row) -> displayRow y row) <<< withIndex

      displayRow :: forall w. Int -> Array Field -> HH.HTML w Action
      displayRow y = HH.tr_ <<< map (\(Tuple x f) -> displayField { x, y } f) <<< withIndex

      displayField :: forall w. Position -> Field -> HH.HTML w Action
      displayField pos = HH.td [HP.classes classes, HE.onClick $ \_ -> SelectionChange pos] <<< singleton 
        <<< case _ of
        Empty -> HH.input 
          [ HE.onKeyDown \ev -> ValueInsert pos (eventKey ev)
          , HP.readOnly state.isMobile
          , maxLength 0
          , HP.type_ InputText
          ]
        UserInput i -> HH.input 
          [ HE.onKeyDown \ev -> ValueInsert pos (eventKey ev)
          , HP.value <<< show $ i
          , HP.readOnly state.isMobile
          , maxLength 1
          , HP.type_ InputText
          ]
        Given i -> HH.input 
          [ HP.class_ $ ClassName "given"
          , HP.readOnly true
          , maxLength 1
          , HP.value <<< show $ i
          , HP.type_ InputText
          ]
        where
          classes :: Array ClassName
          classes = map ClassName
            [ getBorderStyle "bottom" (borderForIndex pos.y) 
            , getBorderStyle "right" (borderForIndex pos.x)
            , if Just pos == state.focused then "field-focused"
              else if Set.member pos redFields then "field-error"
              else if state.solved then "field-success"
              else "field-normal"
            , "field"
            ]
      
      getBorderStyle :: String -> BorderType -> String
      getBorderStyle side borderType = "border-" <> side <> case borderType of
        NormalBorder -> "-normal"
        BoxBorder -> "-box"
        NoBorder -> "-no"

      maxLength :: forall r i. Int -> IProp (maxLength :: Int | r) i
      maxLength = prop (H.PropName "maxLength")

handleQuery :: forall cs m a. Query a -> H.HalogenM State Action cs Output m (Maybe a)
handleQuery = case _ of 
  ResetSelection a -> do
    H.modify_ $ \s -> s { focused = Nothing }
    H.raise $ SelectionChanged $ Nothing
    pure $ Just a 

handleAction :: forall cs m. Action -> H.HalogenM State Action cs Output m Unit
handleAction = case _ of
  SelectionChange pos -> do 
    { focused, board } <- H.get
    if (focused /= Just pos && (unsafePartial $ isEditable $ peekAt pos board)) then do 
      H.modify_ $ \s -> s { focused = Just pos }
      H.raise $ SelectionChanged $ Just pos
      
    else do
      H.modify_ $ \s -> s { focused = Nothing }
      H.raise $ SelectionChanged $ Nothing
  ValueInsert pos val -> H.raise $ ValueInserted pos $ fromMaybe (Value 0) $ stringToValue val
  Refresh input -> do
    H.modify_ $ \s -> s { board = input.board, errors = input.errors, solved = input.solved }
