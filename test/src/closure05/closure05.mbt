fn create(n: Int) -> (Int) -> (Int) -> (Int) -> Unit {

  fn layer1(x: Int) {
    
    fn layer2(y: Int) {
      
      fn layer3(z: Int) {
        let _ = if z <= 0 { 
          println(x + y + n) 
        } else {
          layer3(z - 1)
        };
      };

      layer3
    };
    
    layer2
  };

  layer1
};

fn main {
  println(create(10)(20)(30)(1));
};