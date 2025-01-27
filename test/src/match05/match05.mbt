enum IntOption {
  None
  Some(Int)
} derive (Show)

fn map(x: IntOption, f: (Int) -> Int) -> IntOption {
  match x {
    Some(z) => Some(f(z))
    _ => None
  }
}

fn filter(x: IntOption, f: (Int) -> Bool) -> IntOption {
  match x {
    Some(z) => if f(z) { x } else { None }
    _ => None
  }
}

fn main {
  fn make_adder(x) {
    fn adder(y) { x + y }
    adder
  }

  let option = Some(88);
  println(option);
  
  let add = make_adder(37);
  println(map(option, add));
  
  let another = make_adder(9);
  let mapped = map(option, another);
  println(filter(mapped, fn (x) { x > 100 }));
  println(filter(mapped, fn (x) { x > 90 }));
}