module App.Data.Sudoku.Hint 
  (Hint)
where

import App.Data.Sudoku.Board (Position)
import App.Data.Sudoku.Field (Value)
  
type Hint =
  { position :: Position
  , digit    :: Value
  , level    :: Int
  }