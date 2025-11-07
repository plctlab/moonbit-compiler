// Test case for pure function call elimination optimization
// Pure functions whose results are not used should be eliminated
//
// This test validates that:
// 1. Pure functions with unused results are optimized away
// 2. Pure functions with used results are preserved
// 3. Non-pure functions (with side effects) are always kept
// 4. Recursive pure functions are handled correctly
//
// TODO: Add more comprehensive tests:
// - Pure functions that call other pure functions
// - Pure functions with complex control flow
// - Pure functions with array/struct allocations
// - Edge cases: empty functions, identity functions
// - Performance benchmarks showing optimization impact

// A pure function that adds two numbers
fn pure_add(x : Int, y : Int) -> Int {
  x + y
}

// A pure function that multiplies two numbers
fn pure_multiply(x : Int, y : Int) -> Int {
  x * y
}

// A pure function that computes factorial (recursive pure function)
fn pure_factorial(n : Int) -> Int {
  if n <= 1 {
    1
  } else {
    n * pure_factorial(n - 1)
  }
}

// A non-pure function with side effects (prints)
fn impure_print(x : Int) -> Int {
  println(x)
  x
}

fn main {
  // Test 1: Pure function call with unused result - should be eliminated
  let _ = pure_add(1, 2)

  // Test 2: Pure function call with used result - should NOT be eliminated
  let result1 = pure_add(3, 4)
  println(result1)

  // Test 3: Multiple pure function calls, some used, some unused
  let _ = pure_multiply(5, 6)  // unused, should be eliminated
  let result2 = pure_multiply(7, 8)
  println(result2)

  // Test 4: Recursive pure function call with unused result
  let _ = pure_factorial(5)  // unused, should be eliminated

  // Test 5: Recursive pure function call with used result
  let result3 = pure_factorial(6)
  println(result3)

  // Test 6: Non-pure function calls - should NEVER be eliminated
  let _ = impure_print(100)  // has side effect, must be kept

  // Test 7: Mix of pure and impure
  let _ = pure_add(10, 20)  // unused, should be eliminated
  let _ = impure_print(200)  // has side effect, must be kept
  let result4 = pure_add(30, 40)
  println(result4)
}
