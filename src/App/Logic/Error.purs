module App.Logic.Error
  ( calcErrors
  , errorsToInvalidField
  )
  where
  
import Prelude

import App.Data.Sudoku.Board (Board, Position, getBoxSize, getSize)
import App.Data.Sudoku.Error (Error(..))
import App.Data.Sudoku.Field (Field, fieldToValue, Value(..))
import App.Logic.BoardFolds (foldBox, foldColumn, foldRow)
import Data.Array (catMaybes, range)
import Data.List.Internal (Set, emptySet, insertAndLookupBy)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

calcErrors :: Board -> Array Error
calcErrors board 
  =  catMaybes (map checkRow boardRange)
  <> catMaybes (map checkColumn boardRange)
  <> catMaybes (map checkBox boxesRange)
  where
    boardSize = getSize board
    boxSize = getBoxSize board

    boardRange :: Array Int
    boardRange = range 0 (boardSize - 1)

    boxesRange :: Array (Tuple Int Int)
    boxesRange = do 
      x <- range 0 (boxSize - 1)
      y <- range 0 (boxSize - 1)
      pure $ Tuple x y

    checkRow :: Int -> Maybe Error
    checkRow index = case unsafePartial $ foldRow index (flip checkDuplicates) (Just emptySet) board of
      Nothing -> Just $ RowError index
      Just _ -> Nothing

    checkColumn :: Int -> Maybe Error
    checkColumn index = case unsafePartial $ foldColumn index (flip checkDuplicates) (Just emptySet) board of
      Nothing -> Just $ ColumnError index
      Just _ -> Nothing

    checkBox :: Tuple Int Int -> Maybe Error
    checkBox (Tuple x y) = case unsafePartial $ foldBox x y (flip checkDuplicates) (Just emptySet) board of
      Nothing -> Just $ BoxError x y
      Just _ -> Nothing

    -- Nothing means there was duplicate
    checkDuplicates :: Field -> Maybe (Set Int) -> Maybe (Set Int)
    checkDuplicates field s = case fieldToValue field of
      Nothing -> s
      Just (Value v) -> case s of
        Nothing -> Nothing
        Just set -> do 
          let { found, result } = insertAndLookupBy compare v set 
          if found 
            then Nothing
            else pure result

errorsToInvalidField :: Int -> Array Error -> Set.Set Position
errorsToInvalidField boxSize errors = Set.fromFoldable $ do
  error <- errors
  case error of 
    RowError y -> map (\x -> { x, y }) boardRange
    ColumnError x -> map (\y -> { x, y }) boardRange
    BoxError xBox yBox -> map (\(Tuple x y) -> { x: (xBox * boxSize + x), y: (yBox * boxSize + y) }) boxesRange
  where
    boardSize = boxSize * boxSize

    boardRange :: Array Int
    boardRange = range 0 (boardSize - 1)

    boxesRange :: Array (Tuple Int Int)
    boxesRange = do 
      x <- range 0 (boxSize - 1)
      y <- range 0 (boxSize - 1)
      pure $ Tuple x y