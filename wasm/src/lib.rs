mod utils;
pub mod sudoku;
pub mod solver;
pub mod flags;
pub mod matching;

use std::cell::RefCell;

use solver::solution_iter;
use sudoku::Sudoku;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
struct SudokuState {
  input: Sudoku,
  solutions: Vec<Sudoku>,
  has_unique_solution: Option<bool>,
  found_hint: Option<Hint>,
  abort_solution_computations: RefCell<bool>,
  solution_computations_in_progress: bool,
  abort_hint_computations: RefCell<bool>,
  hint_computations_in_progress: bool
}

#[wasm_bindgen]
impl SudokuState {
  fn new(input: Sudoku, solutions: Vec<Sudoku>, unique: Option<bool>) -> SudokuState {
    let solutions_abort = RefCell::new(true);
    SudokuState{
      input: input.clone(),
      solutions: solutions,
      has_unique_solution: unique,
      found_hint: None,
      abort_solution_computations: solutions_abort,
      abort_hint_computations: RefCell::new(false),
    }
  }

  pub fn new_empty(data: String, size: u8) -> SudokuState {
    let input_sudoku = Sudoku::load(data.as_str(), size);
    SudokuState::new(input_sudoku, vec![], None)
  }

  pub fn new_with_known_solution(current: String, solution: String, size: u8) -> SudokuState {
    let input_sudoku = Sudoku::load(current.as_str(), size);
    let solution_sudoku = Sudoku::load(solution.as_str(), size);
    SudokuState::new(input_sudoku, vec![solution_sudoku], None)
  }

  pub fn new_with_known_unique_solution(current: String, solution: String, size: u8) -> SudokuState {
    let input_sudoku = Sudoku::load(current.as_str(), size);
    let solution_sudoku = Sudoku::load(solution.as_str(), size);
    SudokuState::new(input_sudoku, vec![solution_sudoku], Some(true))
  }

  pub fn compute_hint(&mut self) {
  }

  pub fn get_hint(&mut self) -> Option<Hint>{
    None
  }

  pub fn abort_hints_computations(&mut self) {
    *self.abort_hint_computations.borrow_mut() = true;
  }

  pub fn abort(&mut self) {
    self.abort_hints_computations();
    *self.abort_solution_computations.borrow_mut() = true;
  }

  pub fn has_unique_solution(&mut self) -> bool {
    true
  }

  pub fn get_solution(&self) -> Option<String> {
    None
  }

  fn add_solution(&mut self, sudoku: Sudoku) {
    if !self.solutions.contains(&sudoku) {
      self.solutions.push(sudoku)
    }
  }

  pub fn compute_all_solutions(&mut self) {
    if self.solution_computations_in_progress {
      return
    }
    if self.has_unique_solution.is_some() {
      return
    }
    self.solution_computations_in_progress = true;
    let iter = solution_iter(self.input.clone(), &self.abort_solution_computations);
    for sol in iter {
      self.add_solution(sol)
    }
    if self.solutions.len() == 1 {
      self.has_unique_solution = Some(true);
    } else {
      self.has_unique_solution = Some(false);
    }
    self.solution_computations_in_progress = false;
  }

  pub fn get_all_solutions(&mut self) -> Vec<String> {
    self.solutions.iter().map(|s| s.save()).collect()
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

#[cfg(test)]
mod test {
    use std::cell::RefCell;

  #[test]
  fn t() {
    unsafe {
      let mut t: RefCell<bool> = RefCell::new(true);
      let y = &t;

      *t.borrow_mut() = false;
      println!("{} {}", t.borrow(), y.borrow())    
    }
  }
}