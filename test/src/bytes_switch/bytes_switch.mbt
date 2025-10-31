// Test for bytes pattern matching using array patterns
fn main {
  let b1 : Bytes = Bytes::from_array([1, 2, 3])
  let b2 : Bytes = Bytes::from_array([10, 11])
  let b3 : Bytes = Bytes::from_array([])

  // Test matching against different byte sequences
  let result1 = if b1.length() == 3 && Byte::to_int(b1[0]) == 1 && Byte::to_int(b1[1]) == 2 && Byte::to_int(b1[2]) == 3 {
    1
  } else if b1.length() == 2 && Byte::to_int(b1[0]) == 10 && Byte::to_int(b1[1]) == 11 {
    2
  } else {
    0
  }

  let result2 = if b2.length() == 3 && Byte::to_int(b2[0]) == 1 && Byte::to_int(b2[1]) == 2 && Byte::to_int(b2[2]) == 3 {
    1
  } else if b2.length() == 2 && Byte::to_int(b2[0]) == 10 && Byte::to_int(b2[1]) == 11 {
    2
  } else {
    0
  }

  let result3 = if b3.length() == 0 {
    3
  } else {
    0
  }

  println(result1)
  println(result2)
  println(result3)
}
