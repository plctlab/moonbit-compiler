fn main {
  fn adder3(x : Int) -> (Int) -> (Int) -> Int {
    fn adder2(y : Int) -> (Int) -> Int {
      fn adder1(z : Int) -> Int {
        x + y + z
      }
      adder1
    }
    adder2
  }

  println(adder3(1)(2)(3))
}
