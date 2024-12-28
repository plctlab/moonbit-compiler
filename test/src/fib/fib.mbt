fn fib(x: Int) -> Int {
  if x <= 1 {
    1
  } else {
    fib(x - 1) + fib(x - 2)
  }
}

fn main {
  println(fib(7))
}