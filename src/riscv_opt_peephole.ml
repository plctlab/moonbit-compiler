open Riscv_ssa
open Riscv_opt

(** Variables that hold constant values. *)
let consts = Hashtbl.create 64

let value_of = Hashtbl.find consts
let is_const = Hashtbl.mem consts

let const_analysis fn =
  let propagate_r ({ rd; rs1; rs2 }) op =
    if is_const rs1 && is_const rs2 then
      Hashtbl.add consts rd (op (value_of rs1) (value_of rs2)) 
  in

  let propagate_i ({ rd; rs; imm }) op =
    if is_const rs then
      Hashtbl.add consts rd (op (value_of rs) imm)
  in

  let look_for_const name =
    let inst = block_of name in
    Basic_vec.iter inst.body (fun x -> match x with
    | AssignInt { rd; imm } -> Hashtbl.add consts rd imm
    | Add r -> propagate_r r (+)
    | Sub r -> propagate_r r (-)
    | Mul r -> propagate_r r ( * )
    | Div r -> propagate_r r (/)
    | And r -> propagate_r r Int.logand
    | Or r -> propagate_r r Int.logor
    | Xor r -> propagate_r r Int.logxor
    | Shr r -> propagate_r r Int.shift_left
    | Shl r -> propagate_r r Int.shift_right

    | Addi i -> propagate_i i (+)
    | Andi i -> propagate_i i Int.logand
    | Ori i -> propagate_i i Int.logor
    | Xori i -> propagate_i i Int.logxor
    | Slli i -> propagate_i i Int.shift_left
    | Srli i -> propagate_i i Int.shift_right
    | Phi { rd; rs } -> ()
    | _ -> ())
  in

  let blocks = get_blocks fn in
  List.iter look_for_const blocks

(**
For instructions like `add %1, %2, %3`,
if one of %2 and %3 is a constant in the correct range,
we can reconstruct it into `addi %1, %2, imm`.

For I-type instructions, the range is -2048 ~ 2047,
as the immediate is 12 bits long.
*)
let to_itype fn =
  let good var =
    if is_const var then
      let x = value_of var in
      x <= 2047 && x >= -2048
    else false
  in

  let blocks = get_blocks fn in

  let convert block = 
    let body = (block_of block).body |> Basic_vec.to_list in
    List.map (fun x -> match x with
    | Add { rd; rs1; rs2 } ->
        if good rs1 then
          Addi { rd; rs = rs2; imm = value_of rs1 }
        else if good rs2 then
          Addi { rd; rs = rs1; imm = value_of rs2 }
        else x

    | Sub { rd; rs1; rs2 } ->
        if good rs2 && value_of rs2 != -2048 then
          Addi { rd; rs = rs1; imm = -value_of rs2 }
        else x

    | And { rd; rs1; rs2 } ->
        if good rs1 then
          Andi { rd; rs = rs2; imm = value_of rs1 }
        else if good rs2 then
          Andi { rd; rs = rs1; imm = value_of rs2 }
        else x

    | Or { rd; rs1; rs2 } ->
        if good rs1 then
          Ori { rd; rs = rs2; imm = value_of rs1 }
        else if good rs2 then
          Ori { rd; rs = rs1; imm = value_of rs2 }
        else x

    | Xor { rd; rs1; rs2 } ->
        if good rs1 then
          Xori { rd; rs = rs2; imm = value_of rs1 }
        else if good rs2 then
          Xori { rd; rs = rs1; imm = value_of rs2 }
        else x

    | Shl { rd; rs1; rs2 } ->
        if good rs2 then
          Slli { rd; rs = rs1; imm = value_of rs2 }
        else x
    
    | Shr { rd; rs1; rs2 } ->
        if good rs2 then
          Srli { rd; rs = rs1; imm = value_of rs2 }
        else x

    | x -> x) body
    |> Basic_vec.of_list
  in
  List.iter (fun block -> (block_of block).body <- convert block) blocks

let peephole ssa = 
  iter_fn const_analysis ssa;
  iter_fn to_itype ssa