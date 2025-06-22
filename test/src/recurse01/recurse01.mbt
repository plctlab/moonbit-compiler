fn fact_aux(x : Int, acc : Int) -> Int {
  if x == 0 {
    1
  } else {
    fact_aux(x - 1, acc * x)
  }
}

fn fact(x : Int) -> Int {
  fact_aux(x, 1)
}

fn main {
  println(fact(5)) 
}
