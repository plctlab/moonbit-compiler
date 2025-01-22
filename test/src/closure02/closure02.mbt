fn f(x: Int) -> Int {
  x - 6
}

fn create(n: Int) -> (Int) -> (Int) -> (Int) -> Unit {
  let a = f(7);

  fn layer1(x) {
    let b = f(11);
    
    fn layer2(y) {
      let c = f(8);
      
      fn layer3(z) {
        let v = if z <= 0 { x + y + n } else { layer1(z - 1)(z - 1)(z - 1); c }
        println(v)
      };

      if y <= 0 { layer3((b - a) / 4 - 1) } else { layer2(y - 1)(y - 1) }
      layer3
    };
    
    if x <= 0 { layer2(0)(0) } else { layer1(x - 1)(x - 1)(x - 1) };
    layer2
  };

  layer1
};

fn main {
    println(create(0)(0)(0)(0));
};
