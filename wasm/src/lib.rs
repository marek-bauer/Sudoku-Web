mod utils;
pub mod sudoku;
pub mod solver;
pub mod flags;
pub mod matching;
pub mod abort_lock;
pub mod generator;

use generator::Puzzle;
use solver::*;
use sudoku::Sudoku;
use wasm_bindgen::prelude::*;
use abort_lock::AbortLock;
use generator::*;

#[wasm_bindgen]
extern "C" {
  fn alert(msg: &str);
}

#[wasm_bindgen]
pub fn solve_sudoku(sudoku_str: &str, size: u8, lock: &AbortLock) -> Option<String> {
  let sudoku = Sudoku::load(sudoku_str, size);

  if lock.is_locked() {
    solution(sudoku, &lock).map(|s| s.save())
  } else {
    None
  }
}

#[wasm_bindgen]
pub fn get_hint(sudoku_str: &str, size: u8, max_level: u8, lock: &AbortLock) -> Option<Hint> {
  let sudoku = Sudoku::load(sudoku_str, size);

  if lock.is_locked() {
    match hint(sudoku, max_level, &lock) {
      None => None,
      Some((digit, (x, y), level)) => 
        Some (Hint { x: x as u8, y: y as u8, digit, level })
    }
  } else {
    None
  }
}

#[wasm_bindgen]
pub fn get_all_solutions(sudoku_str: &str, size: u8, lock: &AbortLock) -> Option<Vec<String>> {
  let sudoku = Sudoku::load(sudoku_str, size);

  let mut solutions = vec![];
  if lock.is_locked() {
    let iter = solution_iter(sudoku, &lock);
    for s in iter {
      solutions.push(s.save())
    }
  }

  if lock.is_locked() {
    Some(solutions)
  } else {
    None
  }
}

#[wasm_bindgen]
pub fn generate_random_sudoku(size: u8, min_difficulty: u32, max_difficult: u32, hint_level: u8, fuel: u32) -> Option<Puzzle> {
  generate_sudoku(size, min_difficulty, max_difficult, hint_level, fuel)
}

#[derive(Clone)]
#[wasm_bindgen]
pub struct Hint {
  x: u8,
  y: u8,
  digit: u8,
  level: u8
}

#[wasm_bindgen]
impl Hint {
  pub fn get_x(&self) -> u8 {
    self.x
  }

  pub fn get_y(&self) -> u8 {
    self.y
  }

  pub fn get_digit(&self) -> u8 {
    self.digit
  }

  pub fn get_level(&self) -> u8 {
    self.level
  }
}
