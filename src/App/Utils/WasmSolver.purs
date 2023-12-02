module App.Utils.WasmSolver
  ( solveSudoku
  , wasm_solve_sudoku
  )
  where

import Prelude

import App.Data.Sudoku.Board (Board, getBoxSize, loadBoard, saveBoard)
import App.Utils.Undefinedtable (Undefinedtable, toMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
  
foreign import data WasmHint :: Type
foreign import convert_hint :: forall a. (Int -> Int -> Int -> Int -> a) -> WasmHint -> a

foreign import wasm_solve_sudoku :: String -> Int -> EffectFnAff (Undefinedtable String)
foreign import wasm_get_hint :: String -> Int -> Int -> EffectFnAff (Undefinedtable WasmHint)
foreign import wasm_get_all_solutions :: String -> Int -> EffectFnAff (Array String)
    
solveSudoku :: Board -> MaybeT Aff Board
solveSudoku board = do 
  let size = getBoxSize board
  input_str <- hoistMaybe $ saveBoard board
  solution_str <- MaybeT <<< map toMaybe <<< fromEffectFnAff $ wasm_solve_sudoku input_str size
  hoistMaybe $ loadBoard solution_str

hoistMaybe :: forall m. Monad m => Maybe ~> MaybeT m
hoistMaybe = MaybeT <<< pure