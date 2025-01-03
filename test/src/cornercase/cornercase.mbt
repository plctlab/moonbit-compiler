// Identity function, to avoid inlining
fn id(x: Int) -> Int {
  if (x > 0) {
    x
  } else {
    -x.abs()
  }
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
}