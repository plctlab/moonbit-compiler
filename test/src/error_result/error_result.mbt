// Simulate error handling without language-level raises so the compiler
// verifies Result-style control flow using plain data structures.

struct IntResult {
  success : Bool
  value : Int
}

fn ok(value : Int) -> IntResult {
  { success: true, value: value }
}

fn err() -> IntResult {
  { success: false, value: 0 }
}

fn divide(x : Int, y : Int) -> IntResult {
  if y == 0 {
    err()
  } else {
    ok(x / y)
  }
}

fn validate_positive(x : Int) -> IntResult {
  if x < 0 {
    err()
  } else {
    ok(x)
  }
}

fn print_result(result : IntResult) -> Unit {
  if result.success {
    println(result.value)
  } else {
    println(-1)
  }
}

fn main {
  print_result(divide(10, 2))
  print_result(divide(10, 0))
  print_result(validate_positive(42))
  print_result(validate_positive(-5))
}
