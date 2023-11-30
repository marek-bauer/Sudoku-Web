use std::fmt::Debug;

use itertools::Itertools;
use std::fmt;

use crate::flags::*;

pub type Position = (usize, usize);

#[derive(Clone)]
pub struct Sudoku {
  size: u8,
  board: Vec<Vec<u8>>,
}

impl Sudoku {
  pub fn empty(box_size: u8) -> Sudoku {
    let board_size = (box_size * box_size) as usize;
    Sudoku {
      size: box_size,
      board: vec![vec![0; board_size]; board_size]
    }
  }

  fn digit_to_char(d: u8) -> char {
    match d {
      0 => ' ',
      1 ..= 9 => ('0' as u8 + d) as char,
      10 ..= 25 => ('A' as u8 - 10 + d) as char,
      _ => '#'
    }
  }

  fn char_to_digit(c: char) -> u8 {
    match c {
      ' ' => 0,
      '1' ..= '9' => c as u8 - '0' as u8,
      'A' ..= 'Z' => c as u8 - 'A' as u8 + 10,
      _ => 26
    }
  }

  pub fn load(data_str: &str, box_size: u8) -> Sudoku {
    let board_size = (box_size * box_size) as usize;
    let data: Vec<u8> = data_str.chars()
      .map(|x| Sudoku::char_to_digit(x))
      .collect();

    let board = (0 .. board_size)
      .fold(vec![], |mut acc, y| {
        acc.push(data[y * board_size .. (y + 1) * board_size].to_vec());
        acc
      });

      Sudoku { size: box_size, board }
  }

  pub fn save(&self) -> String {
    self.board.iter()
      .flat_map(|v| v)
      .map(|x| Sudoku::digit_to_char(*x))
      .join("")
  }

  pub fn pretty_print(&self) -> String {
    let separator_line = (0 .. self.board_size()).map(|x| {
      if x % self.box_size() == self.box_size() - 1 && x != self.board_size()-1 {
        "-+"
      } else {
        "-"
      }
    }).join("");

    (0 .. self.board_size())
      .map(|y| {
        let line = (0 .. self.board_size())
          .map(|x| {
            if x % self.box_size() == self.box_size() - 1 && x != self.board_size() - 1{
              format!("{}|", Sudoku::digit_to_char(self.at((x, y))))
            } else {
              format!("{}", Sudoku::digit_to_char(self.at((x, y))))
            }
          })
          .join("");
        if y % self.box_size() == self.box_size() - 1 && y != self.board_size() - 1 {
          format!("{}\n{}\n", line, separator_line)
        } else {
          format!("{}\n", line)
        }
      }).join("")
  }

  pub fn board_size(&self) -> usize {
    (self.size * self.size) as usize
  }

  pub fn box_size(&self) -> usize {
    self.size as usize
  }

  pub fn at(&self, (x, y): Position) -> u8 {
    self.board[y][x]
  }

  pub fn set(mut self, (x, y): Position, new_val: u8) -> Sudoku {
    self.board[y][x] = new_val;
    self
  }

  pub fn is_solved(&self) -> bool {
    let all_rows_correct = (0 .. self.board_size())
      .all(|y| self.used_in_row(y).size() == self.board_size() as u8);
    let all_columns_correct = (0 .. self.board_size())
      .all(|x| self.used_in_column(y).size() == self.board_size() as u8);
    let all_boxes_correct = (0 .. self.box_size())
      .flat_map(|x| (0 .. self.box_size()).map(|y| (x, y)))
      .all(|pos| self.used_in_box(pos).size() == self.board_size() as u8);

      all_rows_correct && all_columns_correct && all_boxes_correct
  }

  pub fn used_in_row(&self, y: usize) -> Flags {
    let mut used = Flags::empty();
    for x in 0 .. self.board_size() {
      used = used.set(self.at((x, y)));
    }
    used
  }

  pub fn used_in_column(&self, x: usize) -> Flags {
    let mut used = Flags::empty();
    for y in 0 .. self.board_size() {
      used = used.set(self.at((x, y)));
    }
    used
  }

  pub fn used_in_box(&self, (pos_x, pos_y): Position) -> Flags {
    let box_offset_x = (pos_x / self.size as usize) * self.size as usize;
    let box_offset_y = (pos_y / self.size as usize) * self.size as usize;
    let mut used = Flags::empty();
    for i in 0 .. self.board_size() {
      let x = box_offset_x + (i / self.size as usize);
      let y = box_offset_y + (i % self.size as usize);
      used = used.set(self.at((x, y)));
    }
    used
  }

  pub fn used(&self, (pos_x, pos_y): Position) -> Flags {
    let used_in_row = self.used_in_row(pos_y);
    let used_in_column = self.used_in_column(pos_x);
    let used_in_box = self.used_in_box((pos_x, pos_y));
    used_in_row.or(used_in_column).or(used_in_box)
  }

  pub fn available(&self, pos: Position) -> Flags {
    let at_pos = self.at(pos);
    if at_pos == 0 {
      self.used(pos).inverse(self.board_size() as u8)
    } else {
      Flags::empty().set(at_pos)
    }
  }

  pub fn iter<'t>(&'t self) -> impl Iterator<Item=(u8, (usize, usize))> + 't {
    (0 .. self.board_size())
      .flat_map(move |x| (0 .. self.board_size()).map(move |y| (x, y)))
      .map(move |pos| (self.at(pos), pos))
  }

  pub fn iter_row_avail<'t>(&'t self, y: usize) -> impl Iterator<Item=Flags> + 't {
    (0 .. self.board_size())
      .map(move |x| (x, y))
      .map(move |pos| self.available(pos))
  }

  pub fn iter_column_avail<'t>(&'t self, x: usize) -> impl Iterator<Item=Flags> + 't {
    (0 .. self.board_size())
      .map(move |y| (x, y))
      .map(move |pos| self.available(pos))
  }

  pub fn iter_box_avail<'t>(&'t self, (x, y): (usize, usize)) -> impl Iterator<Item=Flags> + 't {
    (x * self.box_size() .. (x + 1) * self.box_size())
      .flat_map(move |x| (y * self.box_size() .. (y + 1) * self.box_size()).map(move |y| (x, y)))
      .map(move |pos| self.available(pos))
  }
}

impl Debug for Sudoku {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.pretty_print())
  }
}

impl fmt::Display for Sudoku {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.pretty_print())
  }
}

#[cfg(test)]
mod test {
  use crate::sudoku::*;

  #[test]
  fn simple_extend() {
    let s = Sudoku::empty(3).set((3,1), 2);
    println!("{}", s.pretty_print())
  }

  #[test]
  fn load_save_test() {
    let data 
      = "12 3".to_string()
      + "3412"
      + "2  1"
      + "412 ";
    println!("{}", Sudoku::load(data.as_str(), 2).pretty_print());
    assert_eq!(data, Sudoku::load(data.as_str(), 2).save())
  }
}