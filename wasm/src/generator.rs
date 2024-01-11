const DIFFICULTY_EXPONENT: u32 = 8;
const DIFFICULTY_MAX: u32 = 8_u32.pow(10);

extern crate rand;

use std::cmp::min;
use crate::solver::*;
use crate::abort_lock::*;
use crate::sudoku::Sudoku;
use rand::*;

fn has_unique_solution(sudoku: Sudoku) -> bool {
  solution_iter(sudoku, &AbortLock::prepare())
    .skip(1)
    .next()
    .is_none()
}

/// Return difficulty of sudoku if it is has unique solutions 
/// max_level = 3 means usage of brute force 
fn solvable_by_hints(mut sudoku: Sudoku, max_level: u8) -> Option<u32> {
  let hint_level = min(2, max_level);
  let mut difficulty = 0;
  while let Some((d, pos, l)) = hint(sudoku.clone(), hint_level, &AbortLock::prepare()) {
    sudoku = sudoku.set(pos, d); 
    difficulty += DIFFICULTY_EXPONENT.pow(l as u32);
  }

  if sudoku.is_solved() {
    Some(difficulty)
  } else if max_level == 3 && has_unique_solution(sudoku) {
    Some(DIFFICULTY_MAX)
  } else {
    None
  }
}

fn prepare_sudoku_game(sudoku: Sudoku, min_difficulty: u32, max_difficult: u32, hint_level: u8, fuel: i32) -> Result<(Sudoku, u32, i32), i32> {
  let difficulty =  solvable_by_hints(sudoku.clone(), hint_level);
  if difficulty.is_some_and(|d| min_difficulty <= d && d <= max_difficult) {
    return Ok((sudoku, difficulty.unwrap(), fuel));
  } else if difficulty.is_none() {
    return Err(1);
  } else if fuel <= 0 {
    return Err(0);
  }
  let filled = sudoku.filled_spaces();
  let mut rng = thread_rng();
  let attempts = rng.gen_range(2 .. 6);
  let mut used_fuel = 0;
  for _ in 0 .. attempts {
    let random_field = sudoku.get_nth_filled_space(rng.gen_range(2 .. filled)).unwrap();

    match prepare_sudoku_game(sudoku.clone().set(random_field, 0), min_difficulty, max_difficult, hint_level, fuel - used_fuel - 1) {
      Ok(result) => return Ok(result),
      Err(fuel_lost) => used_fuel += fuel_lost
    }
  }
  Err(used_fuel)
}

fn random_permutation(size: u8) -> Vec<u8> {
  let mut result: Vec<u8> = (0 .. size).into_iter().collect();
  let mut rng = thread_rng();
  for i in 0 .. (size as usize - 2) {
    let swap_pos = rng.gen_range(i .. size as usize - 1);
    result.swap(i, swap_pos);
  }
  result
}

fn random_sudoku_filled_diagonal(size: u8) -> Sudoku {
  let mut sudoku = Sudoku::empty(size);
  for i in 0 .. size {
    let mut random_values = random_permutation(size * size).into_iter();
    for pos in (i * size .. (i + 1) * size)
      .flat_map(move |x| (i * size .. (i + 1) * size).map(move |y| (x as usize, y as usize))) {
        sudoku = sudoku.set(pos, random_values.next().unwrap());
    }
  }
  sudoku
}

pub fn generate_sudoku(size: u8, min_difficulty: u32, max_difficult: u32, hint_level: u8, fuel: u32) -> Option<(Sudoku, u32, i32)> {
  let diagonal_filled_sudoku = random_sudoku_filled_diagonal(size);
  let solved_sudoku = solution(diagonal_filled_sudoku, &AbortLock::prepare())?;
  match prepare_sudoku_game(solved_sudoku, min_difficulty, max_difficult, hint_level, fuel as i32) {
    Ok((game, difficulty, remaining_fuel)) => Some((game, difficulty, fuel as i32 - remaining_fuel)),
    Err(_) => None
  }
}

#[cfg(test)]
mod test {
  use crate::generator::generate_sudoku;

  #[test]
  fn test1() {
    let gen = generate_sudoku(3, 150,  200, 2, 200);
    let (s, difficulty, used_fuel) = gen.unwrap();
    println!("Difficulty {}", difficulty);
    println!("Used fuel {}", used_fuel);
    println!("{}", s)
  }
}

