fn main {
  let mut x : Double = 0
  let mut t_geq : Int = 0
  let mut t_ge : Int = 0
  let mut t_leq : Int = 0
  let mut t_le : Int = 0
  let mut t_eq : Int = 0
  let mut t_neq : Int = 0

  for i = 0; i < 10; i = i + 1 {
    x += 0.1
    if (x <= 0.4) {
      t_leq += 1
    }
    if (x < 0.4) {
      t_le += 1
    }
    if (x >= 0.4) {
      t_geq += 1
    }
    if (x > 0.4) {
      t_ge += 1
    }
    if (x == 0.4) {
      t_eq += 1
    }
    if (x != 0.4) {
      t_neq += 1
    }
  }
  println(t_leq)
  println(t_le)
  println(t_geq)
  println(t_ge)
  println(t_eq)
  println(t_neq)
}