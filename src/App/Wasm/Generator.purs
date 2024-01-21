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


generateSudoku :: {size :: Int, minDifficulty :: Int, maxDifficulty :: Int, maxHintLevel :: Int, fuel :: Int} -> MaybeT Aff Puzzle
generateSudoku r = do 
  wasm_puzzle <- MaybeT 
    <<< map toMaybe
    <<< fromEffectFnAff 
    $ generate_sudoku r.size r.minDifficulty r.maxDifficulty r.maxHintLevel r.fuel
  hoistMaybe $ convert_puzzle converse wasm_puzzle
  where
    converse :: String -> String -> Int -> Int -> Maybe Puzzle
    converse puzzleStr solutionStr _ difficulty = do
      puzzle <- loadBoard puzzleStr
      solution <- loadBoard solutionStr
      pure $ {puzzle, solution, difficulty}