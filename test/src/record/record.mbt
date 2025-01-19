struct User {
  mut id: Int
  name: String
  mut email: String
}

fn main {
  let u = { id: 233, name: "John Doe", email: "john@doe.com" }
  u.email = "john@doe.name"
  u.id = 0
  println(u.id)
  println(u.name)
  println(u.email)
}