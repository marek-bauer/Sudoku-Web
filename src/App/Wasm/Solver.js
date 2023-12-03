import __wbg_init, * as Wasm from "wasm_sudoku_solver";

export const convert_hint = f => hint => {
  let x = hint.get_x();
  let y = hint.get_y();
  let digit = hint.get_digit();
  let level = hint.get_level();

  return f(x)(y)(digit)(level);
}

const async_with_lock = function (wasm_function) {
  return function (onError, onSuccess) {
    let lock = undefined;
    let canceler = undefined;
    __wbg_init().then(w => {
      if (lock === undefined) {
        lock = Wasm.AbortLock.prepare();
        let res = wasm_function(lock);
        if (canceler === undefined) {
          onSuccess(res);
        } else {
          canceler();
        }
      }
    })
    return function (cancelError, onCancelerError, onCancelerSuccess) {
      canceler = onCancelerSuccess;
    } 
  } 
}

export const wasm_solve_sudoku = sudoku_str => sudoku_size => 
  async_with_lock(lock => Wasm.solve_sudoku(sudoku_str, sudoku_size, lock));

export const wasm_get_hint = sudoku_str => sudoku_size => max_level => 
  async_with_lock(lock => Wasm.get_hint(sudoku_str, sudoku_size, max_level, lock));

export const wasm_get_all_solutions = sudoku_str => sudoku_size => 
  async_with_lock(lock => Wasm.get_all_solutions(sudoku_str, sudoku_size, lock));