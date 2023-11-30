use crate::flags::Flags;

pub fn has_perfect_matching(positions: Vec<Flags>) -> bool {
  let graph: Vec<Vec<usize>> = vec![vec![0]].into_iter()
    .chain(positions.into_iter().map(|flags| flags.to_usize_vec()))
    .collect();
  get_perfect_matching(&graph).is_some()
}

fn set_in_vec<A>(mut vec: Vec<A>, pos: usize, val: A) -> Vec<A>{
  vec[pos] = val;
  vec
}

pub fn get_perfect_matching(connections: &Vec<Vec<usize>>) -> Option<Vec<Option<usize>>> {
  let empty_matching = vec![None; connections.len()];
  let visits = vec![false; connections.len()];
  (0 .. connections.len()).fold(Some(empty_matching), |matching, pos| {
    match matching {
      None => None,
      Some(m) => extend(pos, connections, m, &mut visits.clone())
    }
  })
}

pub fn extend( to_extend: usize, connections: &Vec<Vec<usize>>, matching: Vec<Option<usize>>, visited: &mut Vec<bool>) 
              -> Option<Vec<Option<usize>>> {
  if visited[to_extend] {
    return None;
  } else {
    visited[to_extend] = true;
  }

  let simple_conn = connections[to_extend].iter()
    .filter(|c| matching[**c].is_none())
    .map(|x| *x)
    .next();

  match simple_conn {
    Some(c) => Some(set_in_vec(matching, c, Some(to_extend))),
    None => {
      return connections[to_extend].iter()
        .map(|&c| {
          let broken = matching[c]?;
          let updated_matching 
            = set_in_vec(matching.clone(), c, Some(to_extend));
          extend(broken, connections, updated_matching, visited)
        }).filter(|o| o.is_some())
        .next()?;
    }
  }
}

#[cfg(test)]
mod test {
  use crate::matching::*;

  #[test]
  fn simple_extend() {
    let test_conn = vec![vec![1], vec![0]];
    let matching = vec![None, None];
    let mut visit = vec![false, false];
    assert_eq!(Some(vec![None, Some(0)]), extend(0, &test_conn, matching, &mut visit))
  }

  #[test]
  fn extend_with_switch() {
    let test_conn = vec![vec![1], vec![0, 1]];
    let matching = vec![None, Some(1)];
    let mut visit = vec![false, false];
    assert_eq!(Some(vec![Some(1), Some(0)]), extend(0, &test_conn, matching, &mut visit))
  }

  #[test]
  fn impossible_extend() {
    let test_conn = vec![vec![1], vec![1]];
    let matching = vec![None, Some(1)];
    let mut visit = vec![false, false];
    assert_eq!(None, extend(0, &test_conn, matching, &mut visit))
  }

  #[test]
  fn extend_with_switches() {
    let test_conn = vec![vec![0, 1], vec![1, 2], vec![2, 3], vec![3, 4], vec![4]];
    let matching = vec![None, Some(0), Some(1), Some(2), Some(3)];
    let mut visit = vec![false, false, false, false, false];
    assert_eq!(Some(vec![Some(0), Some(1), Some(2), Some(3), Some(4)]), extend(4, &test_conn, matching, &mut visit))
  }

  #[test]
  fn simple_perfect_matching() {
    let test_conn = vec![vec![0, 1], vec![1, 2], vec![2, 3], vec![3, 4], vec![4]];
    assert_eq!(Some(vec![Some(0), Some(1), Some(2), Some(3), Some(4)]), get_perfect_matching(&test_conn))
  } 

  #[test]
  fn annoying_perfect_matching() {
    let test_conn = vec![vec![0, 1, 2, 3, 4], vec![0, 1, 2, 3], vec![0, 1, 2], vec![0, 1], vec![0]];
    assert_eq!(Some(vec![Some(4), Some(3), Some(2), Some(1), Some(0)]), get_perfect_matching(&test_conn))
  } 

  #[test]
  fn no_perfect_matching() {
    let test_conn = vec![vec![0, 1], vec![0], vec![1], vec![0, 1, 2, 3, 4], vec![2, 3, 4]];
    assert_eq!(None, get_perfect_matching(&test_conn))
  } 
}