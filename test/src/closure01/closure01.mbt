// Tests mutable variables captured by a closure.

fn x() -> () -> Int {
  let mut counter = 12;
  fn y() {
    counter += 2;
    counter
  }
  y
}

fn main {
  let w = x();
  let c1 = w();
  let c2 = w();
  println(c1);
  println(c2);
}