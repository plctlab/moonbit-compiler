enum Expr {
  Add(Expr, Expr)
  Sub(Expr, Expr)
  Mul(Expr, Expr)
  Div(Expr, Expr)
  Literal(Int)
}

fn eval(expr: Expr) -> Int {
  match expr {
    Add(a, b) => eval(a) + eval(b)
    Sub(a, b) => eval(a) - eval(b)
    Mul(a, b) => eval(a) * eval(b)
    Div(a, b) => eval(a) / eval(b)
    Literal(x) => x
  }
}

fn main {
  // (9-5) * ((3+4) / 2) = 12
  let expr = Mul(Sub(Literal(9), Literal(5)), Div(Add(Literal(3), Literal(4)), Literal(2)));
  println(eval(expr))
}