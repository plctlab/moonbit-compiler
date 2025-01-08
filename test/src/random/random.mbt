// Adapted from the core module `random`.

pub(all) struct ChaCha8 {
  state : State
}

pub fn ChaCha8::new(seed : Bytes) -> ChaCha8 {
  let c = { state: State::new() }
  c.state.init(seed)
  c
}

pub(all) struct State {
  mut buffer : FixedArray[UInt64] // size=[32]
  mut seed : FixedArray[UInt64] // size=[4]
  mut i : UInt
  mut n : UInt
  mut c : UInt
}

fn State::new() -> State {
  {
    buffer: FixedArray::make(32, 0UL),
    seed: FixedArray::make(4, 0UL),
    i: 0U,
    n: 0U,
    c: 0U,
  }
}

let ctrInc = 4U

let ctrMax = 16U

let chunk = 32U

let chachaReseed = 4

pub fn State::next(self : State) -> (UInt64, Bool) {
  let i = self.i
  if i >= self.n {
    return (0, false)
  }
  self.i = i + 1
  (self.buffer[i.reinterpret_as_int() & 31], true)
}

fn bytesToUInt64(b : Bytes) -> UInt64 {
  let mut result = 0UL
  for i = 0; i < 8; i = i + 1 {
    result = result | (b[i].to_int64().reinterpret_as_uint64() << (i * 8))
  }
  result
}

fn State::init(self : State, seed : Bytes) -> Unit {
  let seed0 = Bytes::new(8)
  let seed1 = Bytes::new(8)
  let seed2 = Bytes::new(8)
  let seed3 = Bytes::new(8)
  for i = 0; i < 8; i = i + 1 {
    seed0[i] = seed[i]
    seed1[i] = seed[8 + i]
    seed2[i] = seed[16 + i]
    seed3[i] = seed[24 + i]
  }
  let arr = FixedArray::make(4, 0UL)
  arr[0] = bytesToUInt64(seed0)
  arr[1] = bytesToUInt64(seed1)
  arr[2] = bytesToUInt64(seed2)
  arr[3] = bytesToUInt64(seed3)
  self.init64(arr)
}


fn State::init64(self : State, seed : FixedArray[UInt64]) -> Unit {
  self.seed = seed
  // convert FixedArray[UInt64] size=[32] to FixedArray[FixedArray[UInt]] size=[16][4]
  let buffer = FixedArray::makei(16, fn(_i) { FixedArray::make(4, 0U) })
  // split u64 to 2 u32 and store in buffer to meet the function signature
  for i = 0; i < 16; i = i + 1 {
    let hi = self.buffer[i * 2]
    let lo = self.buffer[i * 2 + 1]
    buffer[i][0] = (hi >> 32).to_uint()
    buffer[i][1] = hi.to_uint()
    buffer[i][2] = (lo >> 32).to_uint()
    buffer[i][3] = lo.to_uint()
  }
  // [buffer] is changed in chacha_block
  chacha_block(self.seed, buffer, 0)
  // concat 2 u32 to u64 and store in self.buffer
  for i = 0; i < 16; i = i + 1 {
    for j = 0; j < 4; j = j + 2 {
      let lo = buffer[i][j]
      let hi = buffer[i][j + 1]
      let u64 : UInt64 = (hi.to_uint64() << 32) | lo.to_uint64()
      self.buffer[i * 2 + j / 2] = u64
    }
  }
  self.c = 0
  self.i = 0
  self.n = chunk
}

pub fn State::refill(self : State) -> Unit {
  self.c = self.c + ctrInc
  if self.c == ctrMax {
    // reseed
    let len = self.buffer.length()
    self.seed[0] = self.buffer[len - chachaReseed + 0]
    self.seed[1] = self.buffer[len - chachaReseed + 1]
    self.seed[2] = self.buffer[len - chachaReseed + 2]
    self.seed[3] = self.buffer[len - chachaReseed + 3]
    self.c = 0
  }
  // convert FixedArray[UInt64] size=[32] to FixedArray[FixedArray[UInt]] size=[16][4]
  let buffer = FixedArray::makei(16, fn(_i) { FixedArray::make(4, 0U) })
  // split u64 to 2 u32 and store in buffer to meet the function signature
  for i = 0; i < 16; i = i + 1 {
    let hi = self.buffer[i * 2]
    let lo = self.buffer[i * 2 + 1]
    buffer[i][0] = (hi >> 32).to_uint()
    buffer[i][1] = hi.to_uint()
    buffer[i][2] = (lo >> 32).to_uint()
    buffer[i][3] = lo.to_uint()
  }
  chacha_block(self.seed, buffer, self.c)
  // concat 2 u32 to u64 and store in self.buffer
  for i = 0; i < 16; i = i + 1 {
    for j = 0; j < 4; j = j + 2 {
      let lo = buffer[i][j]
      let hi = buffer[i][j + 1]
      let u64 : UInt64 = (hi.to_uint64() << 32) | lo.to_uint64()
      self.buffer[i * 2 + j / 2] = u64
    }
  }
  self.i = 0
  self.n = self.buffer.length().reinterpret_as_uint()
  if self.c == ctrMax - ctrInc {
    self.n = (self.buffer.length() - chachaReseed).reinterpret_as_uint()
  }
}

fn State::reseed(self : State) -> Unit {
  let seed = FixedArray::make(4, 0UL)
  for i = 0; i < 4; i = i + 1 {
    for {
      let x = self.next()
      if x.1 {
        seed[i] = x.0
        break
      }
      self.refill()
    }
  }
  self.init64(seed)
}

fn chacha_block(
  seed : FixedArray[UInt64],
  buf : FixedArray[FixedArray[UInt]],
  counter : UInt
) -> Unit {
  fn qr(t : (UInt, UInt, UInt, UInt)) -> (UInt, UInt, UInt, UInt) {
    let a = t.0
    let b = t.1
    let c = t.2
    let d = t.3
    let a = a + b
    let d = d ^ a
    let d = (d << 16) | (d >> 16)
    let c = c + d
    let b = b ^ c
    let b = (b << 12) | (b >> 20)
    let a = a + b
    let d = d ^ a
    let d = (d << 8) | (d >> 24)
    let c = c + d
    let b = b ^ c
    let b = (b << 7) | (b >> 25)
    return (a, b, c, d)
  }

  setup(seed, buf, counter)
  for i = 0; i < buf[0].length(); i = i + 1 {
    let mut b0 = buf[0][i]
    let mut b1 = buf[1][i]
    let mut b2 = buf[2][i]
    let mut b3 = buf[3][i]
    let mut b4 = buf[4][i]
    let mut b5 = buf[5][i]
    let mut b6 = buf[6][i]
    let mut b7 = buf[7][i]
    let mut b8 = buf[8][i]
    let mut b9 = buf[9][i]
    let mut b10 = buf[10][i]
    let mut b11 = buf[11][i]
    let mut b12 = buf[12][i]
    let mut b13 = buf[13][i]
    let mut b14 = buf[14][i]
    let mut b15 = buf[15][i]
    for round = 0; round < 4; round = round + 1 {
      let tb1 = qr((b0, b4, b8, b12))
      b0 = tb1.0
      b4 = tb1.1
      b8 = tb1.2
      b12 = tb1.3
      let tb2 = qr((b1, b5, b9, b13))
      b1 = tb2.0
      b5 = tb2.1
      b9 = tb2.2
      b13 = tb2.3
      let tb3 = qr((b2, b6, b10, b14))
      b2 = tb3.0
      b6 = tb3.1
      b10 = tb3.2
      b14 = tb3.3
      let tb4 = qr((b3, b7, b11, b15))
      b3 = tb4.0
      b7 = tb4.1
      b11 = tb4.2
      b15 = tb4.3
      let tb5 = qr((b0, b5, b10, b15))
      b0 = tb5.0
      b5 = tb5.1
      b10 = tb5.2
      b15 = tb5.3
      let tb6 = qr((b1, b6, b11, b12))
      b1 = tb6.0
      b6 = tb6.1
      b11 = tb6.2
      b12 = tb6.3
      let tb7 = qr((b2, b7, b8, b13))
      b2 = tb7.0
      b7 = tb7.1
      b8 = tb7.2
      b13 = tb7.3
      let tb8 = qr((b3, b4, b9, b14))
      b3 = tb8.0
      b4 = tb8.1
      b9 = tb8.2
      b14 = tb8.3
    }
    buf[0][i] = b0
    buf[1][i] = b1
    buf[2][i] = b2
    buf[3][i] = b3
    buf[4][i] = buf[4][i] + b4
    buf[5][i] = buf[5][i] + b5
    buf[6][i] = buf[6][i] + b6
    buf[7][i] = buf[7][i] + b7
    buf[8][i] = buf[8][i] + b8
    buf[9][i] = buf[9][i] + b9
    buf[10][i] = buf[10][i] + b10
    buf[11][i] = buf[11][i] + b11
    buf[12][i] = b12
    buf[13][i] = b13
    buf[14][i] = b14
    buf[15][i] = b15
  }
}

fn setup(
  seed : FixedArray[UInt64],
  b32 : FixedArray[FixedArray[UInt]],
  counter : UInt
) -> Unit {
  b32[0][0] = 0x61707865
  b32[0][1] = 0x61707865
  b32[0][2] = 0x61707865
  b32[0][3] = 0x61707865
  b32[1][0] = 0x3320646e
  b32[1][1] = 0x3320646e
  b32[1][2] = 0x3320646e
  b32[1][3] = 0x3320646e
  b32[2][0] = 0x79622d32
  b32[2][1] = 0x79622d32
  b32[2][2] = 0x79622d32
  b32[2][3] = 0x79622d32
  b32[3][0] = 0x6b206574
  b32[3][1] = 0x6b206574
  b32[3][2] = 0x6b206574
  b32[3][3] = 0x6b206574
  b32[4][0] = seed[0].to_uint()
  b32[4][1] = seed[0].to_uint()
  b32[4][2] = seed[0].to_uint()
  b32[4][3] = seed[0].to_uint()
  b32[5][0] = (seed[0] >> 32).to_uint()
  b32[5][1] = (seed[0] >> 32).to_uint()
  b32[5][2] = (seed[0] >> 32).to_uint()
  b32[5][3] = (seed[0] >> 32).to_uint()
  b32[6][0] = seed[1].to_uint()
  b32[6][1] = seed[1].to_uint()
  b32[6][2] = seed[1].to_uint()
  b32[6][3] = seed[1].to_uint()
  b32[7][0] = (seed[1] >> 32).to_uint()
  b32[7][1] = (seed[1] >> 32).to_uint()
  b32[7][2] = (seed[1] >> 32).to_uint()
  b32[7][3] = (seed[1] >> 32).to_uint()
  b32[8][0] = seed[2].to_uint()
  b32[8][1] = seed[2].to_uint()
  b32[8][2] = seed[2].to_uint()
  b32[8][3] = seed[2].to_uint()
  b32[9][0] = (seed[2] >> 32).to_uint()
  b32[9][1] = (seed[2] >> 32).to_uint()
  b32[9][2] = (seed[2] >> 32).to_uint()
  b32[9][3] = (seed[2] >> 32).to_uint()
  b32[10][0] = seed[3].to_uint()
  b32[10][1] = seed[3].to_uint()
  b32[10][2] = seed[3].to_uint()
  b32[10][3] = seed[3].to_uint()
  b32[11][0] = (seed[3] >> 32).to_uint()
  b32[11][1] = (seed[3] >> 32).to_uint()
  b32[11][2] = (seed[3] >> 32).to_uint()
  b32[11][3] = (seed[3] >> 32).to_uint()
  b32[12][0] = counter + 0
  b32[12][1] = counter + 1
  b32[12][2] = counter + 2
  b32[12][3] = counter + 3
  b32[13][0] = 0
  b32[13][1] = 0
  b32[13][2] = 0
  b32[13][3] = 0
  b32[14][0] = 0
  b32[14][1] = 0
  b32[14][2] = 0
  b32[14][3] = 0
  b32[15][0] = 0
  b32[15][1] = 0
  b32[15][2] = 0
  b32[15][3] = 0
}

fn main {
  let s = State::new()
  let seed = b"ABCDEFGHIJKLMNOPQRSTUVWXYZ123456"
  s.init(seed)

  fn uint64(s : State) -> UInt64 {
    for {
      let x = s.next()
      if x.1 {
        return x.0
      }
      s.refill()
    }
  }

  for i = 0; i < 32; i = i + 1 {
    let x = uint64(s)
    println(x)
  }
}
