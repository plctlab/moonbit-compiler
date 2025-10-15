enum List[T] {
  Nil
  Cons(T, List[T])
}

fn List::length[T](l : List[T]) -> Int {
  match l {
    Nil => 0
    Cons(_, tail) => 1 + tail.length()
  }
}

fn List::get[T](l : List[T], index : Int) -> T? {
  match l {
    Nil => None
    Cons(head, tail) =>
      if index == 0 {
        Some(head)
      } else {
        tail.get(index - 1)
      }
  }
}

struct Point {
  x : Int
  y : Int 
} derive(Show)

fn main {
  let l = Cons({ x: 1, y: 2 }, Cons({ x: 3, y: 4 }, Nil))
  println(l.length())
  println(l.get(1)) 
}
