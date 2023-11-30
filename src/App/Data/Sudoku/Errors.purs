-- | Errors in sudoku game
module App.Data.Sudoku.Error 
  ( Error(..)
  )
where

data Error = RowError Int | ColumnError Int | BoxError Int Int
