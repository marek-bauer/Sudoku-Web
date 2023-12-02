mod utils;
pub mod sudoku;
pub mod solver;
pub mod flags;
pub mod matching;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
struct SudokuState {
    iter: Box<dyn Iterator<Item = i32>>
}



#[wasm_bindgen]
impl SudokuState {
  pub fn new(data: String, size: u8) -> SudokuState {
    SudokuState{
      iter: Box::new(0 .. 4)
    }
  }

  pub fn new_with_known_solution(current: String, solution: String, size: u8) -> SudokuState {
    SudokuState{
      iter: Box::new(0 .. 4)
    }
  }

  pub fn compute_hint(&mut self) {
  }

  pub fn get_hint(&mut self) -> Option<Hint>{
    None
  }

  pub fn abort(&mut self) {
  }

  pub fn has_unique_solution(&mut self) -> bool {
    true
  }

  pub fn get_solution(&self) -> Option<String> {
    None
  }

  pub fn compute_all_solutions(&mut self) {
  }

  pub fn get_all_solutions(&self) -> Vec<String> {
    vec![]
  }
}

#[wasm_bindgen]
struct Hint {
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