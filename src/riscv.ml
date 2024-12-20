(* RISC-V assembly commands. *)

module Reg = Riscv_reg

type label = string

type t =
| Add of Reg.t * Reg.t * Reg.t
| Sub of Reg.t * Reg.t * Reg.t
| Mul of Reg.t * Reg.t * Reg.t
| Div of Reg.t * Reg.t * Reg.t
| Call of label
| Label of label

let to_string asm =
  let convert_3reg ty rd rs1 rs2 =
    let rd_str = Reg.to_string rd in
    let rs1_str = Reg.to_string rs1 in 
    let rs2_str = Reg.to_string rs2 in 
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
  
let regalloc ssa = []