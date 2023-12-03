mod utils;
pub mod sudoku;
pub mod solver;
pub mod flags;
pub mod matching;

use std::cell::RefCell;

use solver::*;
use sudoku::Sudoku;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
  fn alert(msg: &str);
}

#[wasm_bindgen]
pub struct AbortLock(RefCell<bool>);

#[wasm_bindgen]
impl AbortLock {
  pub fn prepare() -> AbortLock {
    AbortLock( RefCell::new(false) )
  }

  pub fn abort(&mut self) {
    *self.0.borrow_mut() = true;
  }

  pub fn is_locked(&self) -> bool {
    !*(self.0.borrow())
  }
}

#[wasm_bindgen]
pub fn solve_sudoku(sudoku_str: &str, size: u8, lock: &AbortLock) -> Option<String> {
  let sudoku = Sudoku::load(sudoku_str, size);
  
  if lock.is_locked() {
    solution(sudoku, &(lock.0)).map(|s| s.save())
  } else {
    None
  }
}

#[wasm_bindgen]
pub fn get_hint(sudoku_str: &str, size: u8, max_level: u8, lock: &AbortLock) -> Option<Hint> {
  let sudoku = Sudoku::load(sudoku_str, size);

  if lock.is_locked() {
    match hint(sudoku, max_level, &(lock.0)) {
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
    let iter = solution_iter(sudoku, &(lock.0));
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
