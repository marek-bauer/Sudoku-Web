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
    SudokuState{
      input: input.clone(),
      solutions: solutions,
      has_unique_solution: unique,
      found_hint: None,
      abort_solution_computations: RefCell::new(true),
      solution_computations_in_progress: false,
      abort_hint_computations: RefCell::new(false),
      hint_computations_in_progress: false,
    }
  }

  pub fn new_empty(data: String, size: u8) -> SudokuState {
    let input_sudoku = Sudoku::load(data.as_str(), size);
    match solution(input_sudoku.clone(), &RefCell::new(false)) {
      None => SudokuState::new(input_sudoku, vec![], Some(false)),
      Some(s) => SudokuState::new(input_sudoku, vec![s], None),
    }
    
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

  pub fn compute_hint(&mut self, max_level: u8) {
    self.hint_computations_in_progress = true;
    let hint = hint(self.input.clone(), max_level, &self.abort_hint_computations);
    self.found_hint = match hint {
      None => None,
      Some((digit, (x, y), level)) => 
        Some (Hint { x: x as u8, y: y as u8, digit, level })
    };
    self.hint_computations_in_progress = false;
  }

  pub fn get_hint(&self) -> Option<Hint>{
    match &self.found_hint {
      Some(h) => Some(h.clone()),
      None if self.solutions.len() > 0 => {
        let solution = &self.solutions[0]; 
        (0 .. self.input.board_size())
          .flat_map(|x| (0 .. self.input.board_size()).map(move |y| (x, y)))
          .filter(|pos| self.input.at(*pos) == 0)
          .map(|(x, y)| Hint { x: x as u8, y: y as u8, digit: solution.at((x, y)), level: 100})
          .next()
      }
      _ => None
    }
  }

  pub fn abort_hints_computations(&mut self) {
    *self.abort_hint_computations.borrow_mut() = true;
  }

  pub fn abort(&mut self) {
    self.abort_hints_computations();
    *self.abort_solution_computations.borrow_mut() = true;
  }

  pub fn has_unique_solution(&mut self) -> Option<bool> {
    self.has_unique_solution
  }

  pub fn get_solution(&self) -> Option<String> {
    if self.solutions.len() > 0 {
      Some(self.solutions[0].save())
    } else if self.has_unique_solution.is_none() {
      *self.abort_solution_computations.borrow_mut() = false;
      solution(self.input.clone(), &self.abort_solution_computations)
        .map(|s| s.save())
    } else {
      None
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
      if !self.solutions.contains(&sol) {
        self.solutions.push(sol)
      }
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

#[derive(Clone)]
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
