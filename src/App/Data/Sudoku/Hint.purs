module App.Data.Sudoku.Hint 
  (Hint)
where

import App.Data.Sudoku.Board (Position)
  
type Hint =
  { position :: Position
  , digit    :: Int
  , level    :: Int
  }