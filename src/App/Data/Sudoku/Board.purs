-- | Sudoku board 
module App.Data.Sudoku.Board
  ( Board
  , Position
  , Size(..)
  , freezeSudoku
  , getBoxSize
  , getEmptyBoard
  , getSize
  , getSudokuByRows
  , isComplete
  , loadBoard
  , peekAt
  , saveBoard
  , setAt
  , toRowSize
  , unSize
  )
  where

import Prelude

import App.Data.Sudoku.Field (charToValue, valueToGiven, fieldToChar, toGiven, Field(..))
import App.Utils.Array (chunks, get)
import Data.Array (all, concat, range)
import Data.Array.Extra.Unsafe (unsafeModifyAt, unsafeUpdateAt)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe)
import Data.Number (sqrt)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)

type Position = { x :: Int, y :: Int }
newtype Size = Size Int

unSize :: Size -> Int 
unSize (Size s) = s

instance sizeShow :: Show Size where
  show (Size s) = show s

type Board = 
  { size   :: Size
  , sudoku :: Array (Array Field)
  }

toRowSize :: Size -> Int 
toRowSize (Size s) = s * s

getSize :: Board -> Int
getSize b = toRowSize b.size

getBoxSize :: Board -> Int
getBoxSize b = unSize b.size

getSudokuByRows :: Board -> Array (Array Field)
getSudokuByRows b = b.sudoku

getEmptyBoard :: Size -> Board
getEmptyBoard size = 
  { size, sudoku: map (\_ -> map (const Empty) boardRange) boardRange}
  where
    boardSize = toRowSize size
    boardRange = range 0 (boardSize - 1)

updateSudoku :: (Array (Array Field) -> Array (Array Field)) -> Board -> Board
updateSudoku f board = board {sudoku = f board.sudoku}

setAt :: Partial => Position -> Field -> Board -> Board
setAt pos field = 
  updateSudoku $ unsafeModifyAt pos.y (\r -> unsafeUpdateAt pos.x field r)

peekAt :: Partial => Position -> Board -> Field
peekAt pos board = get pos.x $ get pos.y board.sudoku

isComplete :: Board -> Boolean
isComplete board = all (\row -> all (_ /= Empty) row) board.sudoku

saveBoard :: Board -> Maybe String
saveBoard b 
  = map fromCharArray 
  <<< traverse fieldToChar
  $ concat b.sudoku    

loadBoard :: String -> Maybe Board
loadBoard str = do 
  size <- fromNumber <<< sqrt <<< sqrt <<< toNumber <<< length $ str
  sudoku_data <- traverse (map valueToGiven <<< charToValue)  $ toCharArray str
  let sudoku = chunks (size * size) sudoku_data
  pure $ { size: Size size, sudoku }

freezeSudoku :: Board -> Board
freezeSudoku b = b { sudoku = map (map toGiven) b.sudoku }