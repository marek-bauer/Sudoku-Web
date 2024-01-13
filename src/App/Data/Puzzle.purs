module App.Data.Puzzle
  ( Puzzle
  , loadPuzzle
  , savePuzzle
  )
  where

import Prelude

import App.Data.Sudoku.Board (Board, loadBoard, saveBoard)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)

type Puzzle =
  { puzzle     :: Board
  , solution   :: Board
  , difficulty :: Int
  }

savePuzzle :: Puzzle -> Maybe String
savePuzzle p = do 
  puzzleStr <- saveBoard p.puzzle 
  solutionStr <- saveBoard p.solution
  pure $ show p.difficulty <> ";" <> puzzleStr <> ";" <> solutionStr

loadPuzzle :: String -> Maybe Puzzle
loadPuzzle str = do
  case split (Pattern ";") str of 
    [diffStr, puzzleStr, solutionStr] -> do
      difficulty <- fromString diffStr
      puzzle <- loadBoard puzzleStr
      solution <- loadBoard solutionStr
      pure $ {difficulty, puzzle, solution}
    _ -> Nothing