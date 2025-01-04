// Fills the array with random numbers.
fn fill(x: Array[Int]) -> Unit {
  for i = 0; i < x.length(); i = i + 1 {
    let j = i + 12;
    x[i] = (j * j * j * j) % 5039;
  }
}

// Copies an array `src` to `dst`.
fn arrcpy(dst: Array[Int], doff: Int, src: Array[Int], soff: Int, len: Int) -> Unit {
  for i = 0; i < len; i = i + 1 {
    dst[doff + i] = src[soff + i];
  }
}

fn mergesort(x: Array[Int]) -> Unit {
  fn aux(x: Array[Int], begin, end) {
    if (end - begin <= 1) {
      return;
    }

    let mid = (begin + end) / 2;

    aux(x, begin, mid);
    aux(x, mid, end);

    let new = Array::make(end - begin, 0);
    let mut top = 0;
    let mut i = begin;
    let mut j = mid;
    while i != mid && j != end {
      if (x[i] < x[j]) {
        new[top] = x[i];
        i += 1;
      } else {
        new[top] = x[j];
        j += 1;
      }
      top += 1;
    }
    
    arrcpy(new, top, x, i, mid - i);
    arrcpy(new, top, x, j, end - j);
    arrcpy(x, begin, new, 0, end - begin);
  }

  aux(x, 0, x.length())
}

fn main {
  let arr = Array::make(20, 0);
  fill(arr);
  mergesort(arr);
  for i = 0; i < arr.length(); i = i + 1 {
    println(arr[i]);
  }
}