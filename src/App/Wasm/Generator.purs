module App.Wasm.Generator
  ( generateSudoku
  )
  where

import Prelude

import App.Data.Puzzle (Puzzle)
import App.Data.Sudoku.Board (loadBoard)
import App.Utils.Hoist (hoistMaybe)
import App.Utils.Undefinedtable (Undefinedtable, toMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

foreign import data PuzzleWasm :: Type
foreign import generate_sudoku :: Int -> Int -> Int -> Int -> Int-> EffectFnAff (Undefinedtable PuzzleWasm)
foreign import convert_puzzle :: forall a. (String -> String -> Int -> Int -> a) -> PuzzleWasm -> a


generateSudoku :: Int -> Int -> Int -> Int -> Int -> MaybeT Aff Puzzle
generateSudoku size minDifficulty maxDifficulty maxHintLevel fuel = do 
  wasm_puzzle <- MaybeT 
    <<< map toMaybe
    <<< fromEffectFnAff 
    $ generate_sudoku size minDifficulty maxDifficulty maxHintLevel fuel
  hoistMaybe $ convert_puzzle converse wasm_puzzle
  where
    converse :: String -> String -> Int -> Int -> Maybe Puzzle
    converse puzzleStr solutionStr _ difficulty = do
      puzzle <- loadBoard puzzleStr
      solution <- loadBoard solutionStr
      pure $ {puzzle, solution, difficulty}