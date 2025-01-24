(** Registers for RV64GC **)

(* Module for physical registers (reg_t) *)
module Reg = struct
  type t =
    | Zero (* zero register *)
    | Ra (* caller return address *)
    | Sp (* caller (S0) stack pointer *)
    | Gp (* global pointer *)
    | Tp (* thread pointer *)
    | T0 (* caller temporary register *)
    | T1
    | T2
    | Fp (* callee stack bottom register *)
    | S1 (* callee saved register *)
    | A0 (* caller argument register *)
    | A1
    | A2
    | A3
    | A4
    | A5
    | A6
    | A7
    | S2 (* callee saved register *)
    | S3
    | S4
    | S5
    | S6
    | S7
    | S8
    | S9
    | S10
    | S11
    | T3 (* caller temporary register *)
    | T4
    | T5
    | T6 (* caller swap register *)

  (* Convert physical register to string *)
  let to_string r =
    match r with
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
  ;;
end

(* Module for floating-point registers (freg_t) *)
module FReg = struct
  type t =
    | Ft0 (* caller floating-point temporary register *)
    | Ft1
    | Ft2
    | Ft3
    | Ft4
    | Ft5
    | Ft6
    | Ft7
    | Fs0 (* callee floating-point saved register *)
    | Fs1
    | Fa0 (* caller floating-point argument register *)
    | Fa1
    | Fa2
    | Fa3
    | Fa4
    | Fa5
    | Fa6
    | Fa7
    | Fs2 (* callee floating-point saved register *)
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
    | Ft11 (* caller swap floating-point register *)

  (* Convert floating-point register to string *)
  let to_string fr =
    match fr with
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
  ;;
end


(**
Defines an immediate value type.

Immediate values can either be an integer (`IntImm`) or a floating-point number (`FloatImm`).
*)
module Imm = struct
  type t =
    | IntImm of int (* Integer immediate value *)
    | FloatImm of float (* Floating-point immediate value *)

  let to_string imm =
    match imm with
    | IntImm i -> string_of_int i
    | FloatImm f -> string_of_float f
  ;;
end

(**
Defines a slot type for both virtual and physical registers.

This type encapsulates different kinds of registers, including general-purpose registers, floating-point registers,
and specific slots representing values like `Unit` (no return value).
*)
module Slot = struct
  (* Slot type, which can include different kinds of registers *)
  type t =
    | Unit (* No return value, used in function calls or returns *)
    | Slot of int (* Integer register slot *)
    | FSlot of int (* Floating-point register slot *)
    | Reg of Reg.t (* Physical register *)
    | FReg of FReg.t (* Floating-point physical register *)

  include struct 
    let _ = fun (_ : t) -> ()
    let compare = compare
    let equal = (=)
    let hash = Hashtbl.hash
  end

  (* Convert t to string representation *)
  let to_string (s : t) : string =
    match s with
    | Slot i -> Printf.sprintf "%%%d" i
    | FSlot i -> Printf.sprintf "%%f%d" i
    | Reg r -> Reg.to_string r
    | FReg fr -> FReg.to_string fr
    | Unit -> "_"
  ;;

  (* Counter for integer slots *)
  let slot_cnt = ref 0

  (* Counter for floating-point slots *)
  let fslot_cnt = ref 0

  (* Create a new integer slot *)
  let new_slot () =
    let i = !slot_cnt in
    slot_cnt := i + 1;
    Slot i
  ;;

  (* Create a new floating-point slot *)
  let new_fslot () =
    let i = !fslot_cnt in
    fslot_cnt := i + 1;
    FSlot i
  ;;
end

(* Key t*)
module SlotSet = Basic_hashsetf.Make (Slot)
