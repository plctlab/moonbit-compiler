fn main {
  for i = 0; i < 6; i = i + 1 {
    match i {
      1 | 2 => println("one or two");
      3 => println("three");
      4 => println("four");
      n => println("other: " + n.to_string());
    }
  }
}
