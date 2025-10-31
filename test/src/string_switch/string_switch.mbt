// Minimal test for string switch
fn main {
  let s = "a"
  let result = match s {
    "a" => 1
    "b" => 2
    _ => 0
  }
  println(result)
}
