(* RISC-V assembly commands. *)

let registers = [|
  (* Int registers *)
  "zero"; "ra"; "sp"; "gp"; "tp";
  "t0"; "t1"; "t2"; "fp"; "s1";
  "a0"; "a1"; "a2"; "a3"; "a4";
  "a5"; "a6"; "a7"; "s2"; "s3";
  "s4"; "s5"; "s6"; "s7"; "s8";
  "s9"; "s10"; "s11"; "t3"; "t4";
  "t5"; "t6";

  (* FP registers *)
  "ft0"; "ft1"; "ft2"; "ft3"; "ft4";
  "ft5"; "ft6"; "ft7"; "fs0"; "fs1";
  "fa0"; "fa1"; "fa2"; "fa3"; "fa4";
  "fa5"; "fa6"; "fa7"; "fs2"; "fs3";
  "fs4"; "fs5"; "fs6"; "fs7"; "fs8";
  "fs9"; "fs10"; "fs11"; "ft8"; "ft9";
  "ft10"; "ft11";
|]

type t =
| Add of int * int * int
| Sub of int * int * int
| Mul of int * int * int
| Div of int * int * int
| Call of string
| Label of string

let to_string asm =
  let convert_3reg ty rd rs1 rs2 =
    let rd_str = registers.(rd) in
    let rs1_str = registers.(rs1) in 
    let rs2_str = registers.(rs2) in 
    Printf.sprintf "%s %s, %s, %s" ty rd_str rs1_str rs2_str
  in
  
  match asm with
  | Add (rd, rs1, rs2) -> convert_3reg "add" rd rs1 rs2
  | Sub (rd, rs1, rs2) -> convert_3reg "sub" rd rs1 rs2
  | Mul (rd, rs1, rs2) -> convert_3reg "mul" rd rs1 rs2
  | Div (rd, rs1, rs2) -> convert_3reg "div" rd rs1 rs2
  | Call label -> Printf.sprintf "call %s" label
  | Label label -> Printf.sprintf "%s:" label

(**
Used when emitting assembly.

We expect every non-label command to be indented by 4 spaces.
*)
let to_asm_string asm = 
  match asm with
  | Label _ -> to_string asm
  | _ -> "    " ^ to_string asm
  
let generate _ssa = []