-- | Widget for displaying sudoku board 
module App.Widget.Board 
  ( component
  , Output
  , Input
  , State(..)
  )
where

import Prelude

import App.Data.Sudoku.Board as Board
import App.Utils.Array (withIndex)
import Data.Array (length, range, singleton, zip)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor.Strong (fanout)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen (ClassName(..), liftEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (map_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = {
  board :: Board.Board
}

type Input = State

data Output 
  = SelectionChanged (Maybe Board.Position)

data Action
  = SelectionChange (Maybe Board.Position)
  | ValueInserted Board.Position String
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

      displayBoard :: forall w. Array (Array Board.Field) -> Array (HH.HTML w Action)
      displayBoard = map (\(Tuple index row) -> displayRow (borderForIndex index) row) <<< withIndex

      displayRow :: forall w. BorderType -> Array Board.Field -> HH.HTML w Action
      displayRow rowBorder = HH.tr_ <<< map (\(Tuple index f) -> displayField rowBorder (borderForIndex index) f) <<< withIndex

      displayField :: forall w. BorderType -> BorderType -> Board.Field -> HH.HTML w Action
      displayField rowBorder columnBorder = HH.td [HP.style borderString] <<< singleton <<< case _ of
        Board.Empty -> HH.input [HP.value ""]
        Board.UserInput i -> HH.input [HP.value <<< show $ i]
        Board.Given i -> HH.text <<< show $ i
        where
          borderString = getBorderStyle "bottom" rowBorder <> getBorderStyle "right" columnBorder
      
      getBorderStyle :: String -> BorderType -> String
      getBorderStyle side borderType = "border-" <> side <> case borderType of
        NormalBorder -> ": solid 1px black;"
        BoxBorder -> ": solid 3px blue;"
        NoBorder -> ": 0px;"


handleAction :: forall cs m. Action -> H.HalogenM State Action cs Output m Unit
handleAction = case _ of
  _ -> pure unit

