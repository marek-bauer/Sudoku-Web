-- | Widget for displaying sudoku board 
module App.Widget.Board
  ( Input
  , Output(..)
  , State(..)
  , component
  )
  where

import Prelude

import App.Data.Sudoku.Board (Position)
import App.Data.Sudoku.Board as Board
import App.Data.Sudoku.Field (Field(..), Value(..), stringToValue)
import App.Utils.Array (withIndex)
import App.Utils.Event (eventKey)
import Data.Array (singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (IProp, prop)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP

type State = {
  board :: Board.Board
}

type Input = State

data Output 
  = SelectionChanged (Maybe Board.Position)
  | ValueInserted Board.Position Value

data Action
  = SelectionChange (Maybe Board.Position)
  | ValueInsert Board.Position String
  | Refresh Input

component :: forall q m. H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \x -> x
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction 
                                   , receive = \i -> Just $ Refresh i
                                   }
    }

data BorderType = NormalBorder | BoxBorder | NoBorder 

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.table
    [HP.style "border-collapse: collapse;", HP.class_ (ClassName "sudoku")]
    (displayBoard sudokuBoard)
    where

      boxSize = Board.getBoxSize state.board
      boardSize = Board.getSize state.board
      sudokuBoard = Board.getSudokuByRows state.board

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
      displayField pos = HH.td [HP.style borderString] <<< singleton <<< case _ of
        Empty -> HH.input [HE.onKeyDown \ev -> ValueInsert pos (eventKey ev), maxLength 0, HP.type_ InputText]
        UserInput i -> HH.input [HE.onKeyDown \ev -> ValueInsert pos (eventKey ev), HP.value <<< show $ i, maxLength 1, HP.type_ InputText]
        Given i -> HH.text <<< show $ i
        where
          borderString = getBorderStyle "bottom" (borderForIndex pos.y) 
            <> getBorderStyle "right" (borderForIndex pos.x)
      
      getBorderStyle :: String -> BorderType -> String
      getBorderStyle side borderType = "border-" <> side <> case borderType of
        NormalBorder -> ": solid 1px black;"
        BoxBorder -> ": solid 3px blue;"
        NoBorder -> ": 0px;"

      maxLength :: forall r i. Int -> IProp (maxLength :: Int | r) i
      maxLength = prop (H.PropName "maxLength")

handleAction :: forall cs m. Action -> H.HalogenM State Action cs Output m Unit
handleAction = case _ of
  SelectionChange pos -> H.raise $ SelectionChanged pos
  ValueInsert pos val -> H.raise $ ValueInserted pos $ fromMaybe (Value 0) $ stringToValue val
  Refresh input -> H.modify_ (const input)
