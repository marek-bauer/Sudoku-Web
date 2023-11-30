mod utils;
pub mod sudoku;
pub mod solver;
pub mod flags;
pub mod matching;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
struct Marek {
    iter: Box<dyn Iterator<Item = i32>>
}

#[wasm_bindgen]
impl Marek {
    pub fn new() -> Marek {
        Marek{
            iter: Box::new(0 .. 4)
        }
    }

    pub fn next(&mut self) -> Option<i32> {
        self.iter.as_mut().next()
    }
}

#[wasm_bindgen]
pub fn greet() {
    alert("Hello, wasm!");
}
