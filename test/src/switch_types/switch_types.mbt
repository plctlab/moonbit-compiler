// Test case for extended switch constant type support
// Tests basic types that already work and ensures our changes don't break them

// TODO: Add tests for newly supported types when MoonBit language supports them:
// - Byte type (b'\x00', b'\x01', etc.)
// - Int64 type (0L, 1L, -1L, etc.)
// - UInt type (unsigned integers)
// - UInt64 type (0UL, 1UL, etc.)
//
// Example test cases to add in the future:
//
// fn test_byte_switch(b : Byte) -> Int {
//   match b {
//     b'\x00' => 0
//     b'\x01' => 1
//     b'\xFF' => 255
//     _ => -1
//   }
// }
//
// fn test_int64_switch(n : Int64) -> Int {
//   match n {
//     0L => 100
//     1L => 101
//     -1L => 99
//     _ => -1
//   }
// }

// Test 1: Switch on Bool type (already supported)
fn test_bool_switch(b : Bool) -> Int {
  match b {
    true => 1
    false => 0
  }
}

// Test 2: Switch on Char type (already supported)
fn test_char_switch(c : Char) -> Int {
  match c {
    'a' => 97
    'b' => 98
    'c' => 99
    'Z' => 90
    _ => -1
  }
}

// Test 3: Switch on Int type (already supported)
fn test_int_switch(n : Int) -> Int {
  match n {
    0 => 10
    1 => 11
    2 => 12
    -5 => 5
    -1 => 9
    100 => 110
    _ => -1
  }
}

// Test 4: Multiple switch cases to test jump table generation
fn test_large_switch(n : Int) -> Int {
  match n {
    0 => 100
    1 => 101
    2 => 102
    3 => 103
    4 => 104
    5 => 105
    6 => 106
    7 => 107
    8 => 108
    9 => 109
    10 => 110
    _ => -1
  }
}

fn main {
  // Test bool switch
  println(test_bool_switch(true))     // 1
  println(test_bool_switch(false))    // 0

  // Test char switch
  println(test_char_switch('a'))      // 97
  println(test_char_switch('b'))      // 98
  println(test_char_switch('Z'))      // 90
  println(test_char_switch('x'))      // -1

  // Test int switch
  println(test_int_switch(0))         // 10
  println(test_int_switch(1))         // 11
  println(test_int_switch(-5))        // 5
  println(test_int_switch(100))       // 110
  println(test_int_switch(99))        // -1

  // Test large switch (jump table)
  println(test_large_switch(0))       // 100
  println(test_large_switch(5))       // 105
  println(test_large_switch(10))      // 110
  println(test_large_switch(11))      // -1
}
