fn make_adder(x: Int) -> (Int) -> Int {
  fn adder(y) -> Int {
    x + y
  }
  adder
}

fn main {
  let adder = make_adder(1234)
  println(adder(5678))
}