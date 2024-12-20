(* This type consists of all 32 integer registers and 32 FP registers of RISC-V. *)
(* It also contains 2 special marks about spilt registers. *)
type t = 
| Zero
| Ra
| Sp
| Gp
| Tp
| T0
| T1
| T2
| Fp
| S1
| A0
| A1
| A2
| A3
| A4
| A5
| A6
| A7
| S2
| S3
| S4
| S5
| S6
| S7
| S8
| S9
| S10
| S11
| T3
| T4
| T5
| T6
| Ft0
| Ft1
| Ft2
| Ft3
| Ft4
| Ft5
| Ft6
| Ft7
| Fs0
| Fs1
| Fa0
| Fa1
| Fa2
| Fa3
| Fa4
| Fa5
| Fa6
| Fa7
| Fs2
| Fs3
| Fs4
| Fs5
| Fs6
| Fs7
| Fs8
| Fs9
| Fs10
| Fs11
| Ft8
| Ft9
| Ft10
| Ft11
| Spilt of int    (* This integer is offset from stack pointer. *)
| SpiltFP of int

let to_string (t: t) =
  match t with
  | Zero -> "zero"
  | Ra -> "ra"
  | Sp -> "sp"
  | Gp -> "gp"
  | Tp -> "tp"
  | T0 -> "t0"
  | T1 -> "t1"
  | T2 -> "t2"
  | Fp -> "fp"
  | S1 -> "s1"
  | A0 -> "a0"
  | A1 -> "a1"
  | A2 -> "a2"
  | A3 -> "a3"
  | A4 -> "a4"
  | A5 -> "a5"
  | A6 -> "a6"
  | A7 -> "a7"
  | S2 -> "s2"
  | S3 -> "s3"
  | S4 -> "s4"
  | S5 -> "s5"
  | S6 -> "s6"
  | S7 -> "s7"
  | S8 -> "s8"
  | S9 -> "s9"
  | S10 -> "s10"
  | S11 -> "s11"
  | T3 -> "t3"
  | T4 -> "t4"
  | T5 -> "t5"
  | T6 -> "t6"
  | Ft0 -> "ft0"
  | Ft1 -> "ft1"
  | Ft2 -> "ft2"
  | Ft3 -> "ft3"
  | Ft4 -> "ft4"
  | Ft5 -> "ft5"
  | Ft6 -> "ft6"
  | Ft7 -> "ft7"
  | Fs0 -> "fs0"
  | Fs1 -> "fs1"
  | Fa0 -> "fa0"
  | Fa1 -> "fa1"
  | Fa2 -> "fa2"
  | Fa3 -> "fa3"
  | Fa4 -> "fa4"
  | Fa5 -> "fa5"
  | Fa6 -> "fa6"
  | Fa7 -> "fa7"
  | Fs2 -> "fs2"
  | Fs3 -> "fs3"
  | Fs4 -> "fs4"
  | Fs5 -> "fs5"
  | Fs6 -> "fs6"
  | Fs7 -> "fs7"
  | Fs8 -> "fs8"
  | Fs9 -> "fs9"
  | Fs10 -> "fs10"
  | Fs11 -> "fs11"
  | Ft8 -> "ft8"
  | Ft9 -> "ft9"
  | Ft10 -> "ft10"
  | Ft11 -> "ft11"
  | Spilt x -> "spilt." ^ Int.to_string x
  | SpiltFP x -> "spilt.d." ^ Int.to_string x