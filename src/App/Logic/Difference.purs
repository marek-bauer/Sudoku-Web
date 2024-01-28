module App.Logic.Difference
  ( emptyDifferances
  , filledDifferances
  )
  where

import Prelude

import App.Data.Sudoku.Board (Board, Position, peekAt)
import App.Data.Sudoku.Field (Field(..), Value, fieldToValue)
import App.Logic.BoardFolds (foldBoard)
import App.Utils.Partial (runPartial)
import Data.Array (cons)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

-- | Check for differances up to type of entry
--   Empty field matches everything 
filledDifferances' :: Partial => Board -> Board -> Array Position
filledDifferances' board board' = foldBoard checkDiff [] board
  where
    checkDiff :: Array Position -> Position -> Field -> Array Position
    checkDiff prev pos field = case (Tuple field (peekAt pos board')) of 
      Tuple Empty _ -> prev
      Tuple _ Empty -> prev
      Tuple x y | fieldToValue x == fieldToValue y -> prev
      _ -> cons pos prev

-- | Check for differances up to type of entry
--   Empty field matches everything 
filledDifferances :: Board -> Board -> Maybe (Array Position)
filledDifferances b b' = runPartial (filledDifferances' b) b'

-- | Findes all places that are empty, but solutuon have them filled 
emptyDifferances' :: Partial => Board -> Board -> Array (Tuple Position Value)
emptyDifferances' board solution = foldBoard checkEmpty [] board
  where
    checkEmpty :: Array (Tuple Position Value) -> Position -> Field -> Array (Tuple Position Value)
    checkEmpty prev pos field = case (Tuple field (peekAt pos solution)) of 
      Tuple Empty Empty -> prev
      Tuple Empty x -> cons (Tuple pos $ fieldToValue x) prev
      _ -> prev

-- | Findes all places that are empty, but solutuon have them filled 
emptyDifferances :: Board -> Board -> Maybe (Array (Tuple Position Value))
emptyDifferances board solution = runPartial (emptyDifferances' board) solution
