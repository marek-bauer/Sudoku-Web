import __wbg_init, * as Wasm from "wasm_sudoku_solver";

export const convert_puzzle = f => puzzle_wasm => {
  let puzzle = puzzle_wasm.get_puzzle();
  let solution = puzzle_wasm.get_solution();
  let size = puzzle_wasm.get_size();
  let difficulty = puzzle_wasm.get_difficulty();

  return f(puzzle)(solution)(size)(difficulty);
}

export const generate_sudoku = size => min_difficulty => max_difficult => hint_level => fuel => {
  return function (onError, onSuccess) {
    __wbg_init().then(w => {
      let puzzle = Wasm.generate_random_sudoku(size, min_difficulty, max_difficult, hint_level, fuel);
      onSuccess(puzzle);
    })
    return function (cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess(); // we do not need canceling generation
    } 
  } 
}