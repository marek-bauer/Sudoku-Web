-- | Sudoku board 
module App.Data.Sudoku.Board
  ( Board
  , Field(..)
  , Position
  , Value(..)
  , getBoxSize
  , getEmptyBoard
  , getSize
  , getSudokuByRows
  , peekAt
  , setAt
  )
  where

import Prelude

import App.Utils.Array (get)
import Data.Array (range)
import Data.Array.Extra.Unsafe (unsafeModifyAt, unsafeUpdateAt)

newtype Value = Value Int

derive instance Eq Value
derive instance Ord Value

instance Show Value where
  show (Value i) = show i 

data Field = Empty | UserInput Value | Given Value

type Position = { x :: Int, y :: Int }

data Board  =  
  Board
  { size :: Int
  , sudoku :: Array (Array Field)
  }

getSize :: Board -> Int
getSize (Board b) = b.size * b.size

getBoxSize :: Board -> Int
getBoxSize (Board b) = b.size

getSudokuByRows :: Board -> Array (Array Field)
getSudokuByRows (Board b) = b.sudoku

getEmptyBoard :: Int -> Board
getEmptyBoard size = 
  Board { size, sudoku: map (\_ -> map (const Empty) boardRange) boardRange}
  where
    boardSize = size * size
    boardRange = range 0 (boardSize - 1)

updateSudoku :: (Array (Array Field) -> Array (Array Field)) -> Board -> Board
updateSudoku f (Board board) = Board board {sudoku = f board.sudoku}

setAt :: Partial => Position -> Field -> Board -> Board
setAt pos field = 
  updateSudoku $ unsafeModifyAt pos.y (\r -> unsafeUpdateAt pos.x field r)

peekAt :: Partial => Position -> Board -> Field
peekAt pos (Board board) = get pos.x $ get pos.y board.sudoku