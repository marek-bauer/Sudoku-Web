extern crate itertools;

use crate::sudoku::*;
use crate::flags::*;
use crate::matching::has_perfect_matching;
use std::cell::RefCell;
use std::cmp::Ordering::Equal;
use itertools::Itertools;

pub fn solution(sudoku: Sudoku, aborted: &RefCell<bool>) -> Option<Sudoku> {
  match get_best_options(&sudoku) {
    None => Some(sudoku),
    Some((flags, (x, y))) => {
      flags.to_vec().into_iter().fold(None, |prev, digit| match prev {
        None => {
          let updated_sudoku = sudoku.clone().set((x, y), digit);
          solution(updated_sudoku, aborted)
        }
        Some (s) => Some(s)
      })
    }
  }
}

fn get_best_options(sudoku: &Sudoku) -> Option<(Flags, (usize, usize))> {
  let options: Vec<_> = sudoku.iter()
    .filter(|(d, _)| *d == 0)
    .map(|(_, pos)| (sudoku.available(pos), pos))
    .collect();

  let min_option_pos = options.iter()
    .min_by(|(f1, _), (f2, _)| f1.partial_cmp(f2).unwrap_or(Equal));

  if min_option_pos.is_some_and(|(f, _)| f.size() > 1) {
    options.iter()
      .map(|(flags, pos)| {
        let real_possibilities = flags.to_vec().into_iter()
          .filter(|digit| {
            let updated_sudoku = sudoku.clone().set(*pos, *digit);
            !is_unsolvable(&updated_sudoku)
          })
          .collect();
        (Flags::from_vec(real_possibilities), *pos)
      }).min_by(|(f1, _), (f2, _)| f1.partial_cmp(f2).unwrap_or(Equal))
  } else {
    min_option_pos.map(|x| *x)
  }
}

pub fn solution_iter<'r> (sudoku: Sudoku, aborted: &'r RefCell<bool>) 
    -> Box<dyn Iterator<Item=Sudoku> + 'r> {
  match get_best_options(&sudoku) {
    None => Box::new(vec![sudoku].into_iter()),
    Some((flags, pos)) => {
      return Box::new(
        flags.to_vec().into_iter()
          .flat_map(move |digit| {
            let updated_sudoku = sudoku.clone().set(pos, digit);
            solution_iter(updated_sudoku, aborted)
          })
        )
    }
  }
}

/// Checks for direct problems in this sudoku
pub fn is_unsolvable(sudoku: &Sudoku) -> bool {
  let is_field_out_of_options = sudoku.iter()
    .filter(|(d, _)| *d == 0)
    .map(|(_, (x, y))| sudoku.used((x, y)).inverse(sudoku.board_size() as u8))
    .min_by(|f1, f2| f1.partial_cmp(f2).unwrap_or(Equal))
    .is_some_and(|f| f.size() < 1);

  let row_without_solution = (0 .. sudoku.board_size())
    .any(|y| !has_perfect_matching(sudoku.iter_row_avail(y).collect()));

  let column_without_solution = (0 .. sudoku.board_size())
    .any(|x| !has_perfect_matching(sudoku.iter_column_avail(x).collect()));

  let box_without_solution = (0 .. sudoku.box_size())
    .flat_map(|x| (0 .. sudoku.box_size()).map(move |y| (x, y)))
    .any(|pos| !has_perfect_matching(sudoku.iter_box_avail(pos).collect()));

  is_field_out_of_options || row_without_solution || column_without_solution || box_without_solution
}

pub fn contradiction(sudoku: &Sudoku, level: u8, aborted: &RefCell<bool>) -> bool {
  match level {
    0 => false,
    1 => is_unsolvable(&sudoku),
    _ => {
      let options: Vec<(Flags, (usize, usize))> = sudoku.iter()
        .filter(|(d, _)| *d == 0)
        .map(|(_, (x, y))| (sudoku.used((x, y)).inverse(sudoku.board_size() as u8), (x, y)))
        .sorted_by(|(f1, _), (f2, _)| f1.partial_cmp(f2).unwrap_or(Equal))
        .collect();
      
      options.iter().any(|(flags, (x, y))| {
        flags.to_vec().iter().all(|digit| {
          let updated_sudoku = sudoku.clone().set((*x, *y), *digit);
          is_unsolvable(&updated_sudoku) || contradiction(&updated_sudoku, level - 1, aborted)
        })
      })
    }
  }
}

pub fn hint(sudoku: Sudoku, max_level: u8, aborted: &RefCell<bool>) 
  -> Option<(u8, (usize, usize), u8)> {
  let mut options: Vec<(Vec<u8>, (usize, usize))> = sudoku.iter()
    .filter(|(d, _)| *d == 0)
    .map(|(_, pos)| (sudoku.available(pos).to_vec(), pos))
    .sorted_by(|(f1, _), (f2, _)| f1.len().cmp(&f2.len()))
    .collect();

  for level in 0 ..= max_level {
    options = options.into_iter().map(|(possibilities, (x, y))| {
      let real_possibilities: Vec<u8> = possibilities.into_iter()
        .filter(|digit| {
          let updated_sudoku = sudoku.clone().set((x, y), *digit);
          !contradiction(&updated_sudoku, level, aborted)
        }).collect();
      (real_possibilities, (x, y))
    }).collect();

    let minimal_options = options.iter()
      .min_by(|(f1, _), (f2, _)| f1.len().cmp(&f2.len()));
    match minimal_options {
      None => { return None; }
      Some((moves, pos)) => {
        match moves.len() {
          0 => { return None; }
          1 => { 
            println!("{}", level);
            return Some((moves[0], *pos, level)); 
          }
          _ => {}
        }
      }
    }
  } 
  None
}

macro_rules! time {
  ($x:expr) => {{
  let start = std::time::Instant::now();
  let result = $x;
  let duration = start.elapsed();
  println!("Time taken: {:?}", duration);
  result
  }};
}

#[cfg(test)]
mod test {
  use crate::solver::*;
  
  #[test]
  fn load_save_test() {
    let data 
      = "   3".to_string()
      + "3  2"
      + "2  1"
      + "1   ";
    let sudoku = Sudoku::load(data.as_str(), 2);
    let all = (*solution_iter(sudoku, &RefCell::new(false))).next();
    println!("{:?}", all);
  }

  #[test]
  fn load_save_test6() {
    let sudoku = Sudoku::empty(3)
      .set((7, 0), 1)
      .set((5, 1), 2)
      .set((8, 1), 3)
      .set((3, 2), 4)
      .set((6, 3), 5)
      .set((0, 4), 4)
      .set((2, 4), 1)
      .set((3, 4), 6)
      .set((2, 5), 7)
      .set((3, 5), 1)
      .set((1, 6), 5)
      .set((6, 6), 2)
      .set((4, 7), 8)
      .set((7, 7), 4)
      .set((1, 8), 3)
      .set((3, 8), 9)
      .set((4, 8), 1);
    let sol = time!(solution_iter(sudoku, &RefCell::new(false)).next().unwrap());
    println!("{:?}", sol);
  }

  fn solve_by_hints(mut sudoku: Sudoku) -> Sudoku {
    while let Some((d, pos, _)) = hint(sudoku.clone(), 3, &RefCell::new(false)) {
      sudoku = sudoku.set(pos, d)     
    }
    sudoku
  }

  #[test]
  fn load_save_test1() {
    let sudoku = Sudoku::empty(3)
      .set((7, 0), 1)
      .set((5, 1), 2)
      .set((8, 1), 3)
      .set((3, 2), 4)
      .set((6, 3), 5)
      .set((0, 4), 4)
      .set((2, 4), 1)
      .set((3, 4), 6)
      .set((2, 5), 7)
      .set((3, 5), 1)
      .set((1, 6), 5)
      .set((6, 6), 2)
      .set((4, 7), 8)
      .set((7, 7), 4)
      .set((1, 8), 3)
      .set((3, 8), 9)
      .set((4, 8), 1);
    let sol = time!(solve_by_hints(sudoku));
    println!("{}", sol)
  }

  #[test]
  fn load_save_test2() {
    let data 
      = "".to_string()
      + "2  " + "6 7" + "5  "
      + "   " + "   " + " 96"
      + "6 7" + "  1" + "3  "

      + " 5 " + "732" + "   "
      + " 7 " + "   " + " 2 "
      + "   " + "189" + " 7 "

      + "  3" + "5  " + "6 4"
      + "84 " + "   " + "   "
      + "  5" + "246" + "  8";

    let sudoku = Sudoku::load(data.as_str(), 3);
    println!("{:?}", hint(sudoku, 3, &RefCell::new(false)));
  }

  #[test]
  fn load_save_test8() {
    let data 
      = "".to_string()
      + "   " + "   " + "   "
      + "   " + "   " + "   "
      + "   " + "   " + "3  "

      + "   " + " 3 " + "   "
      + "   " + "   " + "   "
      + "   " + "   " + "   "

      + "  3" + "   " + "   "
      + "   " + "   " + "   "
      + "   " + "246" + "  8";

    let sudoku = Sudoku::load(data.as_str(), 3);
    println!("{:?}", hint(sudoku, 3, &RefCell::new(false)));
  }

  #[test]
  fn load_save_test121() {
    let data 
      = "".to_string()
      + "8  " + "   " + "   "
      + "  3" + "6  " + "   "
      + " 7 " + "  9" + "2  "

      + " 5 " + "  7" + "   "
      + "   " + " 45" + "7  "
      + "   " + "1  " + " 3 "

      + "  1" + "   " + " 68"
      + "  8" + "5  " + " 1 "
      + " 9 " + "   " + "4  ";

    let sudoku = Sudoku::load(data.as_str(), 3);
    let sol = time!(solution(sudoku, &RefCell::new(false)).unwrap());
    println!("{}", sol)
  }

  #[test]
  fn load_save_test3() {
    let data 
      = "".to_string()
      + "3  " + " 2 " + "   "
      + "  7" + "6  " + "  3"
      + "6  " + "9  " + "  4"

      + "2  " + "1  " + "6  "
      + "9  " + "8 3" + " 5 "
      + "  5" + "4  " + "7  "

      + " 8 " + "  9" + " 1 "
      + "   " + " 7 " + "2  "
      + " 4 " + "  6" + "3  ";

    let sudoku = Sudoku::load(data.as_str(), 3);
    let sol = time!(solve_by_hints(sudoku));
    println!("{}", sol)
  }

  #[test]
  fn load_save_test4() {
    let data 
      = "".to_string()
      + "  8" + "  3" + "461"
      + "2 6" + " 84" + "   "
      + "3  " + "  7" + " 9 "

      + " 3 " + "75 " + "68 "
      + " 87" + " 1 " + "   "
      + " 5 " + " 4 " + "13 "

      + "  9" + "27 " + "31 "
      + "   " + "   " + "   "
      + "763" + "4 1" + "8 2";

    let sudoku = Sudoku::load(data.as_str(), 3);
    let sol = time!(solve_by_hints(sudoku));
    println!("{}", sol)
  }
}