use std::cmp::Ordering;

/// Stores flags from 1 to 30
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Flags {
  flag: u32
}

impl Flags {
  pub fn empty() -> Self {
    Flags { flag: 1 }
  }

  pub fn set(self, pos: u8) -> Self {
    Flags { flag: self.flag | 1 << pos }
  }

  pub fn or(self, other: Self) -> Self {
    Flags { flag: self.flag | other.flag }
  }

  pub fn size(mut self) -> u8 {
    let mut count = 0;
    while  self.flag != 0 {
      self.flag = self.flag & (self.flag - 1);
      count += 1;
    }
    count - 1
  }

  pub fn to_vec(mut self) -> Vec<u8> {
    let mut res = vec![];
    let mut i = 0;
    while self.flag != 0 {
      self.flag = self.flag >> 1;
      i += 1;
      if self.flag % 2 == 1 {
        res.push(i);
      }
    }
    res
  }

  pub fn from_vec(v: Vec<u8>) -> Self {
    v.iter().fold(Flags::empty(), |flags, val| flags.set(*val))
  }

  pub fn to_usize_vec(mut self) -> Vec<usize> {
    let mut res = vec![];
    let mut i = 0;
    while self.flag != 0 {
      self.flag = self.flag >> 1;
      i += 1;
      if self.flag % 2 == 1 {
        res.push(i);
      }
    }
    res
  }

  pub fn inverse(self, size: u8) -> Self {
    let mask = (2_u32 << size as u32) - 2;
    Flags { flag: self.flag ^ mask }
  }
}

impl PartialOrd for Flags {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    if self.flag == other.flag{
      Some(Ordering::Equal)
    }  else if self.size() == other.size() {
      None
    } else {
      self.size().partial_cmp(&other.size())
    }
  }
}

#[cfg(test)]
mod test {
    extern crate quickcheck;
    use quickcheck::Arbitrary;
    use quickcheck::Gen;
    use quickcheck::quickcheck;
    use crate::flags::Flags;
    use std::iter::FromIterator;
    use std::collections::BTreeSet;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    struct Flag(u8);

    impl Arbitrary for Flag {
      fn arbitrary(g: &mut Gen) -> Self {
        let range = Vec::from_iter(1..31);
        let num = g.choose(range.as_slice());
        match num {
          Some(&v) => Flag(v),
          None => Flag(0),
        }
      }
    }

    impl Arbitrary for Flags {
      fn arbitrary(g: &mut Gen) -> Self {
        let flags:Vec<Flag> = Arbitrary::arbitrary(g);
        flags.iter()
          .fold(Flags::empty(), |acc, f| acc.set(f.0))
      }
    }

    quickcheck! {
      fn prop_inverse_inverse_is_the_same(flags: Flags, len: Flag) -> bool {
        flags == flags.inverse(len.0).inverse(len.0)
      }
    }

    quickcheck! {
      fn prop_flags_keep_status(flags: Vec<Flag>) -> bool {
        let by_flags = flags.clone().iter()
          .fold(Flags::empty(), |acc, f| acc.set(f.0));
        let by_set = BTreeSet::from_iter(flags.into_iter());
        BTreeSet::from_iter(by_flags.to_vec().into_iter().map(|f| Flag(f))) == by_set
      }
    }

    quickcheck! {
      fn prop_flags_keep_size(flags: Vec<Flag>) -> bool {
        let by_flags = flags.clone().iter()
          .fold(Flags::empty(), |acc, f| acc.set(f.0));
        let by_set = BTreeSet::from_iter(flags.into_iter());
        by_flags.size() == by_set.len() as u8
      }
    }
}