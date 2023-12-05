module App.Wasm.Solver
  ( getAllSolutions
  , getHint
  , solveSudoku
  )
  where

import Prelude

import App.Data.Sudoku.Board (Board, getBoxSize, loadBoard, saveBoard)
import App.Data.Sudoku.Field (Value(..))
import App.Data.Sudoku.Hint (Hint)
import App.Utils.Hoist (hoistMaybe)
import App.Utils.Undefinedtable (Undefinedtable, toMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
  
foreign import data WasmHint :: Type
foreign import convert_hint :: forall a. (Int -> Int -> Int -> Int -> a) -> WasmHint -> a

foreign import wasm_solve_sudoku :: String -> Int -> EffectFnAff (Undefinedtable String)
foreign import wasm_get_hint :: String -> Int -> Int -> EffectFnAff (Undefinedtable WasmHint)
foreign import wasm_get_all_solutions :: String -> Int -> EffectFnAff (Undefinedtable (Array String))
    
solveSudoku :: Board -> MaybeT Aff Board
solveSudoku board = do 
  let size = getBoxSize board
  input_str <- hoistMaybe $ saveBoard board
  solution_str <- MaybeT <<< map toMaybe <<< fromEffectFnAff $ wasm_solve_sudoku input_str size
  hoistMaybe $ loadBoard solution_str

getHint :: Board -> Int -> MaybeT Aff Hint
getHint board maxLevel = do
  let size = getBoxSize board
  input_str <- hoistMaybe $ saveBoard board
  hint <- MaybeT <<< map toMaybe <<< fromEffectFnAff $ wasm_get_hint input_str size maxLevel
  pure $ convert_hint (\x y d level -> { position: { x, y }, digit: Value d, level }) hint

getAllSolutions :: Board -> MaybeT Aff (Array Board)
getAllSolutions board = do
  let size = getBoxSize board
  input_str <- hoistMaybe $ saveBoard board
  solutions <- MaybeT <<< map toMaybe <<< fromEffectFnAff $ wasm_get_all_solutions input_str size
  traverse (\s -> hoistMaybe $ loadBoard s) solutions