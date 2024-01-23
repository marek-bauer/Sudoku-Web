-- | Sudoku board 
module App.Data.Sudoku.Board
  ( Board
  , Position
  , Size(..)
  , getBoxSize
  , getEmptyBoard
  , getSize
  , getSudokuByRows
  , loadBoard
  , peekAt
  , saveBoard
  , setAt
  , toRowSize
  )
  where

import Prelude

import App.Data.Sudoku.Field (charToValue, valueToGiven, fieldToChar, Field(..))
import App.Utils.Array (chunks, get)
import Data.Array (concat, range)
import Data.Array.Extra.Unsafe (unsafeModifyAt, unsafeUpdateAt)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe)
import Data.Number (sqrt)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)

type Position = { x :: Int, y :: Int }
newtype Size = Size Int

data Board  =  
  Board
  { size :: Int
  , sudoku :: Array (Array Field)
  }

toRowSize :: Size -> Int 
toRowSize (Size s) = s * s

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

saveBoard :: Board -> Maybe String
saveBoard (Board b) 
  = map fromCharArray 
  <<< traverse fieldToChar
  $ concat b.sudoku    

loadBoard :: String -> Maybe Board
loadBoard str = do 
  size <- fromNumber <<< sqrt <<< sqrt <<< toNumber <<< length $ str
  sudoku_data <- traverse (map valueToGiven <<< charToValue)  $ toCharArray str
  let sudoku = chunks (size * size) sudoku_data
  pure $ Board { size, sudoku }