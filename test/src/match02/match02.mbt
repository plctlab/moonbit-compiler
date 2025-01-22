fn main {
  for i =0 ; i< 6; i=i+1{
    let a = match i {
      1 | 2 => {
        println("one or two");
        "one or two"
      }
      3 => {
        println("three");
        "three"
      }
      4 => {
        println("four");
        "four"
      }
      _ => {
        println("other");
        "other"
      }
    }
    println(a)
  }
}