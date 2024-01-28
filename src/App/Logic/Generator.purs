module App.Logic.Generator where

import Prelude

import App.Data.Sudoku.Board (Board, Size, getEmptyBoard, getSize, setAt, unSize)
import App.Data.Sudoku.Field (Field(..), Value(..))
import App.Utils.RandomMonad (class RandomMonad, randomPermutationRange)
import Data.Array (foldl, foldM, zip, range)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

getFilledDiagonal :: forall m. RandomMonad m => Size -> m Board
getFilledDiagonal s = unsafePartial $ foldM fillRandDiagBox emptyBoard (range 0 (boxSize - 1))
  where
    boxSize = unSize s
    emptyBoard = getEmptyBoard s
    size = getSize emptyBoard
    sizeRange = range 0 (size - 1)

    fillRandDiagBox :: Partial => Board -> Int -> m Board
    fillRandDiagBox prev boxToFill = do 
      perm <- map (\x -> Value $ x + 1) <$> randomPermutationRange size
      pure $ foldl (fillDiagBoxField boxToFill) prev (zip sizeRange perm)

    fillDiagBoxField :: Partial => Int -> Board -> Tuple Int Value -> Board
    fillDiagBoxField boxToFill board (Tuple pos val) =
      setAt {x: boxToFill * boxSize + (pos `mod` boxSize), y: boxToFill * boxSize + (pos `div` boxSize)} 
        (Given val) board


    