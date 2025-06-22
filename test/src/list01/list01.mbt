enum List[T] {
  Nil
  Cons(T, List[T])
}

fn List::length[T](l : List[T]) -> Int {
  match l {
    Nil => 0
    Cons(_, tail) => 1 + length(tail)
  }
}

fn List::get[T](l : List[T], index : Int) -> T? {
  match l {
    Nil => None
    Cons(head, tail) => 
      if index == 0 {
        Some(head)
      } else {
        get(tail, index - 1)
      }
  }
}

fn main {
  let l = Cons(1, Cons(2, Cons(3, Nil)))
  println(l.length())
  println(l.get(1)) 
}
