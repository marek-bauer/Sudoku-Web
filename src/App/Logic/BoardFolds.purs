module App.Logic.BoardFolds
  ( foldBoard
  , foldBox
  , foldColumn
  , foldRow
  , foldSection
  )
  where

import Prelude

import App.Data.Sudoku.Board (Board, Position, getBoxSize, getSize, getSudokuByRows, peekAt)
import App.Data.Sudoku.Field (Field)
import App.Utils.Array (withIndex)
import Data.Array (range)
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))

foldBoard :: forall a. (a -> Position -> Field -> a) -> a -> Board -> a
foldBoard f init board 
  = foldl 
      (\prev (Tuple y row) -> foldl (\prev' (Tuple x field) -> f prev' { x, y } field) prev (withIndex row) ) 
      init $ withIndex (getSudokuByRows board)

foldSection :: forall a. Partial => (Int -> Position) -> (a -> Field -> a) -> a -> Board -> a
foldSection selector f init board = foldl (\prev index -> f prev $ peekAt (selector index) board) init (range 0 (size - 1))
  where
    size = getSize board

foldColumn :: forall a. Partial => Int -> (a -> Field -> a) -> a -> Board -> a
foldColumn col = foldSection (\y -> {x: col, y})

foldRow :: forall a. Partial => Int -> (a -> Field -> a) -> a -> Board -> a 
foldRow row = foldSection (\x -> {x, y: row})

foldBox :: forall a. Partial => Int -> Int -> (a -> Field -> a) -> a -> Board -> a 
foldBox xBox yBox f init board = foldSection (\i -> {x: xBox * boxSize + i `mod` boxSize, y: yBox * boxSize + i `div` boxSize}) f init board
  where
    boxSize = getBoxSize board