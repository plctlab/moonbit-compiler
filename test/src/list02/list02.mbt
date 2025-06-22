enum IntList {
  Nil
  Cons(Int, IntList)
}

fn IntList::length(l : IntList) -> Int {
  match l {
    Nil => 0
    Cons(_, tail) => 1 + length(tail)
  }
}

fn IntList::get(l : IntList, index : Int) -> Int? {
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

fn IntList::head(l : IntList) -> Int? {
  match l {
    Nil => None
    Cons(head, _) => Some(head)
  }
}

fn main {
  let l = Cons(1, Cons(2, Cons(3, Nil)))
  println(l.length())
  println(l.get(1)) 
  println(l.head())
}
