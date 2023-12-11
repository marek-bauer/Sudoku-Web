module App.Logic.Solver where

import Prelude

import App.Data.Sudoku.Board (Board, Position, getBoxSize, getSize, setAt)
import App.Data.Sudoku.Field (Field(..), Value(..))
import App.Logic.BoardFolds (foldBoard, foldBox, foldColumn, foldRow)
import Data.Array (range)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

solveSudoku :: Board -> Maybe Board
solveSudoku sudoku = if isFull then Just sudoku else do
    (Tuple pos options) <- posWithLeastOptions
    unsafePartial $ foldl (computeNext pos) Nothing options
  where
    size = getSize sudoku
    boxSize = getBoxSize sudoku

    computeNext :: Partial => Position -> Maybe Board -> Value -> Maybe Board
    computeNext _ (Just res) _ = Just res
    computeNext pos Nothing val = solveSudoku (setAt pos (Given val) sudoku)

    isFull :: Boolean
    isFull = foldBoard (\p _ f -> p && (not $ isEmpty f)) true sudoku
      where
        isEmpty Empty = true
        isEmpty _ = false

    optionSet :: Set.Set Value
    optionSet = Set.fromFoldable <<< map Value $ range 1 size

    posWithLeastOptions :: Maybe (Tuple Position (Set.Set Value))
    posWithLeastOptions = unsafePartial $ foldBoard go Nothing sudoku
      where
        go :: Partial => Maybe (Tuple Position (Set.Set Value)) -> Position -> Field -> Maybe (Tuple Position (Set.Set Value))
        go prev pos Empty = do
          let optionsHere = optionsAtPos pos
          case prev of
            Nothing -> Just (Tuple pos optionsHere)
            Just (Tuple _ prevOptions) | Set.size prevOptions > Set.size optionsHere -> Just (Tuple pos optionsHere)
            _ -> prev
        go prev _ _ = prev

    optionsAtPos :: Partial => Position -> Set.Set Value
    optionsAtPos pos = removeDueToBox <<< removeDueToRow <<< removeDueToColumn $ optionSet
      where
        removeDueToColumn :: Set.Set Value -> Set.Set Value
        removeDueToColumn set = foldColumn pos.x remove set sudoku

        removeDueToRow :: Set.Set Value -> Set.Set Value
        removeDueToRow set = foldRow pos.y remove set sudoku

        removeDueToBox :: Set.Set Value -> Set.Set Value
        removeDueToBox set = foldBox (pos.x `div` boxSize) (pos.y `div` boxSize) remove set sudoku

        remove :: Set.Set Value -> Field -> Set.Set Value
        remove set = case _ of
          Empty -> set
          UserInput v -> Set.delete v set
          Given v -> Set.delete v set