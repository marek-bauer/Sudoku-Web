-- | Errors in sudoku game
module App.Data.Sudoku.Error
  ( Error(..)
  , GameState(..)
  )
  where

data GameState = Incomplite (Array Error) | Complite

data Error = RowError Int | ColumnError Int | BoxError Int Int
