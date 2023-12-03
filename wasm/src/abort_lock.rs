use std::sync::RwLock;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct AbortLock(RwLock<bool>);

#[wasm_bindgen]
impl AbortLock {
  pub fn prepare() -> AbortLock {
    AbortLock( RwLock::new(false) )
  }

  pub fn abort(&self) {
    *(self.0.write().unwrap()) = true;
  }

  pub fn is_locked(&self) -> bool {
    !*(self.0.read().unwrap())
  }

  pub fn is_aborted(&self) -> bool {
    *(self.0.read().unwrap())
  }
}

#[cfg(test)]
mod test {

  use std::{thread, time::{Duration, Instant}};
  use super::AbortLock;

  fn delay_thread<'r>(delay: u64, lock: &'r AbortLock) -> impl Fn() + 'r {
    move || {
      for _ in 1 .. delay {
        if lock.is_aborted() {
          return;
        }
        thread::sleep(Duration::from_millis(1));
      }
    }
  }

  #[test]
  fn abort_works() {
    let lock = Box::leak(Box::new(AbortLock::prepare()));
    let start = Instant::now();

    let thread = thread::spawn(delay_thread(1000, lock));

    lock.abort();
    let res = thread.join();
    assert!(res.is_ok());
    assert!(start.elapsed() <= Duration::from_millis(100));
  }

  #[test]
  fn do_not_abort_by_itself() {
    let lock = Box::leak(Box::new(AbortLock::prepare()));
    let start = Instant::now();

    let thread = thread::spawn(delay_thread(1000, lock));

    let res = thread.join();
    assert!(res.is_ok());
    assert!(start.elapsed() >= Duration::from_secs(1));
  }
}