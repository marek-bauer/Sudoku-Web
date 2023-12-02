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
  , loadBoard
  , peekAt
  , saveBoard
  , setAt
  )
  where

import Prelude

import App.Utils.Array (chunks, get)
import Data.Array (concat, range)
import Data.Array.Extra.Unsafe (unsafeModifyAt, unsafeUpdateAt)
import Data.Char (fromCharCode, toCharCode)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (sqrt)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)

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

saveBoard :: Board -> Maybe String
saveBoard (Board b) = map fromCharArray <<< traverse convert $ concat b.sudoku
  where
    convert :: Field -> Maybe Char
    convert (Empty) = Just ' '
    convert (Given (Value i)) = encode i
    convert (UserInput (Value i)) = encode i

    encode :: Int -> Maybe Char
    encode = case _ of
      n | n < 10 -> fromCharCode $ toCharCode '0' + n
      n -> fromCharCode $ toCharCode 'A' - 10 + n

loadBoard :: String -> Maybe Board
loadBoard str = do 
  size <- fromNumber <<< sqrt <<< sqrt <<< toNumber <<< length $ str
  sudoku_data <- traverse convert $ toCharArray str
  let sudoku = chunks (size * size) sudoku_data
  pure $ Board { size, sudoku }
  where
    convert :: Char -> Maybe Field
    convert c = case toCharCode c of
      32 -> Just $ Empty -- code of space
      n | toCharCode '0' < n && n <= toCharCode '9' 
          -> Just <<<  Given <<< Value $ n - toCharCode '0'
      n | toCharCode 'A' < n && n <= toCharCode 'Z' 
          -> Just <<<  Given <<< Value $ n - toCharCode 'A' + 10
      _ -> Nothing