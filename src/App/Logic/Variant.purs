module App.Logic.Variant
  ( randomPuzzleVariant
  )
  where

import Prelude

import App.Data.Puzzle (Puzzle)
import App.Data.Sudoku.Board (Board)
import App.Utils.RandomMonad (class RandomMonad, randomInt)
import Data.Array (reverse, transpose)

randomPuzzleVariant :: forall m. RandomMonad m => Puzzle -> m Puzzle
randomPuzzleVariant p = do 
  variant <- randomInt 0 7
  let f = boardVariant variant
  pure $ p { puzzle = f p.puzzle, solution = f p.solution }
  where
    boardMirror :: Board -> Board
    boardMirror b = b { sudoku = reverse b.sudoku }

    boardTranspose :: Board -> Board
    boardTranspose b = b { sudoku = transpose b.sudoku }

    turn90 :: Board -> Board
    turn90 = boardTranspose <<< boardMirror

    boardVariant :: Int -> Board -> Board
    boardVariant n = times (n `mod` 2) boardMirror <<< times (n `div` 2) turn90

    times :: forall a. Int -> (a -> a) -> a -> a
    times 0 _ x = x
    times n f x = f (times (n - 1) f x)