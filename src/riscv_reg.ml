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

  (* Reg寄存器最大可分配数量*)
  let k = 11

  (* 用于调用者保存寄存器*)
  let caller_saved_regs =
    [ Ra; T0; T1; T2; A0; A1; A2; A3; A4; A5; A6; A7; T3; T4; T5; T6 ]
  ;;
  let callee_saved_regs =
    [ S1; S2; S3; S4; S5; S6; S7; S8; S9; S10; S11 ]
  ;;

  let spill_reg = T0
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

  let k = 32

  let caller_saved_fregs =
    [ Ft0; Ft1; Ft2; Ft3; Fa0; Fa1; Fa2; Fa3; Fa4; Fa5; Fa6; Fa7; Ft8; Ft9; Ft10; Ft11 ]
  ;;
end

(**
Defines an immediate value type.

Immediate values can either be an integer (`IntImm`) or a floating-point number (`FloatImm`).
*)
module Imm = struct
  type t =
    | IntImm of int (* Integer immediate value *)
    | Int64Imm of int64
    | FloatImm of float (* Floating-point immediate value *)

  let to_string imm =
    match imm with
    | IntImm i -> string_of_int i
    | Int64Imm i -> Int64.to_string i
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

  (* Convert t to string representation *)
  let to_string (s : t) : string =
    match s with
    | Slot i -> Printf.sprintf "%%%d" i
    | FSlot i -> Printf.sprintf "%%f%d" i
    | Reg r -> Reg.to_string r
    | FReg fr -> FReg.to_string fr
    | Unit -> "_"
  ;;

  include struct
    let _ = fun (_ : t) -> ()
    let compare = compare
    let equal = ( = )
    let hash = Hashtbl.hash

    (* Map function*)
    let sexp_of_t (x : t) : S.t = Atom (to_string x)
  end

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

  let is_int = function
    | Slot _ -> true
    | Reg _ -> true
    | _ -> false
  ;;

  let is_float = function
    | FSlot _ -> true
    | FReg _ -> true
    | _ -> false
  ;;
end

module SlotSet = struct
  include Basic_setf.Make (Slot)

  (* Clone function to create a deep copy of the SlotSet *)
  let clone s = of_list (to_list s)

  (* A comprehensive and efficient equal function *)
  let equal s1 s2 =
    (* 1. Physical reference check: same object, directly equal *)
    if s1 == s2
    then true
    else if
      (* 2. If number of elements differs, not equal *)
      cardinal s1 <> cardinal s2
    then false
    else
      (* 3. Check if each element in s1 exists in s2 *)
      for_all s1 (fun x -> mem s2 x)
  ;;

  (* Function to split the SlotSet into two sets based on Slot and FSlot *)
  let split_vars (vars : t) : t * t =
    let slot_set = ref empty in
    let fslot_set = ref empty in
    iter vars (fun s ->
      match s with
      | Slot _ | Reg _ -> slot_set := add !slot_set s
      | FSlot _ | FReg _ -> fslot_set := add !fslot_set s
      | _ -> ());
    !slot_set, !fslot_set
  ;;
end

module SlotMap = struct
  include Basic_mapf.Make (Slot)

  (* Clone function to create a deep copy of the SlotMap *)
  let clone m = of_array (to_sorted_array m)

  (*
     [equal eqv m1 m2] 
     returns true if and only if:
       1) [m1] and [m2] contain exactly the same set of keys, and
       2) for every key k, values in m1 and m2 are considered equal by [eqv].
  *)
  let equal (eqv : 'a -> 'a -> bool) (m1 : 'a t) (m2 : 'a t) : bool =
    (* If number of elements differs, directly return false *)
    if cardinal m1 <> cardinal m2
    then false
    else (
      (* Otherwise iterate through m1 to check each key-value pair *)
      try
        iter m1 (fun k v1 ->
          match find_opt m2 k with
          | None ->
            (* If key not found in m2, determine unequal and break *)
            raise Exit
          | Some v2 ->
            (* If custom eqv function determines unequal values, break *)
            if not (eqv v1 v2) then raise Exit);
        (* If no mismatches found, return true *)
        true
      with
      | Exit -> false)
  ;;

  (* Function to split the SlotMap into two maps based on Reg and FReg *)
  let split_vars (vars : 'a t) : 'a t * 'a t =
    let reg_map = ref empty in
    let freg_map = ref empty in
    iter vars (fun k v ->
      match k with
      | Slot _ | Reg _ -> reg_map := add !reg_map k v
      | FSlot _ | FReg _ -> freg_map := add !freg_map k v
      | _ -> ());
    !reg_map, !freg_map
  ;;
end
