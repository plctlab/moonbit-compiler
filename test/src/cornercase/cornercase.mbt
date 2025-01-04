// Identity function, to avoid inlining
fn id(x: Int) -> Int {
  if (x > 0) {
    x
  } else {
    -x.abs()
  }
}

fn umul128(a : UInt64, b : UInt64) -> (UInt64, UInt64) {
  let aLo = a & 0xffffffff
  let aHi = a >> 32
  let bLo = b & 0xffffffff
  let bHi = b >> 32
  let x = aLo * bLo
  let y = aHi * bLo + (x >> 32)
  let z = aLo * bHi + (y & 0xffffffff)
  let w = aHi * bHi + (y >> 32) + (z >> 32)
  return (w, a * b)
}

fn main {
  let a = 2147483647;   // 2^31 - 1
  println(a);
  println(a + id(2));

  let b = 4294967295U;  // 2^32 - 1
  println(b);
  println(b + Int::reinterpret_as_uint(2));

  let c = 18446744073709551615UL;   // 2^64 - 1
  println(c);
  println(UInt64::to_int(c));

  let (high, low) = umul128(0x123456789ABCDEF0, 0xFEDCBA9876543210);
  println(high);
  println(low);
}