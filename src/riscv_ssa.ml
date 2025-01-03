(** Convert common IR into form of static single assignment (SSA). *)

module Ident = Basic_core_ident

module Stringset = Set.Make(String)

let (+=) refset mem =
  refset := Stringset.add mem !refset

type var = {
  name: string;
  ty: Mtype.t;
}

(*
We store all discarded values (e.g. unit) into variable of this name.
*)
let discard = "_"
let unit = { name = discard; ty = Mtype.T_unit }

(** Similar to R-type instructions in RISC-V. *)
type r_type = {
  rd: var;
  rs1: var;
  rs2: var;
}

(** R-type, but only one operand. *)
type r2_type = {
  rd: var;
  rs1: var;
}

(** I-type, with one destination register, one source and one immediate. *)
type i_type = {
  rd: var;
  rs: var;
  imm: int;
}

(** Calls function named `fn` with arguments `args`, and store the result in `rd`. *)
type call_data = {
  rd: var;
  fn: string;
  args: var list;
}

(** Call function pointer with address `rs` and arguments `args`, and returns in `rd` *)
type call_indirect = {
  rd: var;
  rs: var;
  args: var list;
}

(**
Assigns (un-)signed integer `imm` to `rd`.

We needn't care about immediate sizes.
Those should be handled in arithmetic operations.
*)
type assign_int = {
  rd: var;
  imm: int;
}

type assign_int64 = {
  rd: var;
  imm: int64;
}

(** Assigns floating point number `imm` to `rd`. *)
type assign_fp = {
  rd: var;
  imm: float;
}

(**
Assigns string `imm` to `rd`.

We don't care about how string should be represented;
that's the job for riscv.ml.
*)
type assign_str = {
  rd: var;
  imm: string;
}

(**
Assigns `rs` to `rd`.

These are both variables, as opposed to other assign_* types.
*)
type assign = {
  rd: var;
  rs: var;
}

(**
Similar to `ld` and `st` in RISC-V.

`rd` and `rs` have different meanings in loads and stores:
We load `byte` bytes from `rs` into `rd`,
and store `byte` bytes from `rd` into `rs`.
*)
type mem_access = {
  rd: var;
  rs: var;
  offset: int;
  byte: int;
}

type phi = {
  rd: var;
  rs: (var * string) list;
}

type extern_array = {
  label: string;
  values: string list;
}

type malloc = {
  rd: var;
  size: int;
}

type branch = {
  cond: var;
  ifso: string;
  ifnot: string;
}

type fn = {
  fn: string;
  args: var list;
  body: t list;
}

(** Instructions available in 3-address code and SSA. *)
and t =
(* 64-bit Arithmetic operations *)
| Add of r_type
| Sub of r_type
| Mul of r_type
| Div of r_type
| Mod of r_type
| Less of r_type
| Leq of r_type
| Great of r_type
| Geq of r_type
| Eq of r_type
| Neq of r_type
| Neg of r2_type

(* Bitwise operations *)
| And of r_type
| Or of r_type
| Sll of r_type
| Srl of r_type
| Sra of r_type
| Xor of r_type
| Not of r2_type

(* 64-bit I-type operations *)
| Addi of i_type
| Andi of i_type
| Ori of i_type
| Xori of i_type
| Slli of i_type      (* Left shift *)
| Srli of i_type      (* Right shift (unsigned) *)
| Srai of i_type      (* Right shift (signed) *)
| Slti of i_type      (* Set less than *)

(* Floating point operations *)
| FAdd of r_type
| FSub of r_type
| FMul of r_type
| FDiv of r_type
| FLess of r_type
| FLeq of r_type
| FGreat of r_type
| FGeq of r_type
| FEq of r_type
| FNeq of r_type
| FNeg of r2_type

(* Others *)
| Call of call_data
| AssignInt of assign_int
| AssignInt64 of assign_int64
| AssignFP of assign_fp
| AssignLabel of assign_str
| Assign of assign
| Load of mem_access
| Store of mem_access
| Jump of string
| Branch of branch
| Label of string
| Phi of phi
| FnDecl of fn
| GlobalVarDecl of var          (* See notes below *)
| ExtArray of extern_array      (* An array in `.data` section *)
| CallExtern of call_data       (* Call a C function *)
| CallIndirect of call_indirect (* Call a function pointer *)
| Malloc of malloc
| Return of var

(* Note: *)
(* GlobalVarDecl is only a declaration. *)
(* We will insert a function `_start` to initialize them. *)
(* Global vars should be traited as a label (an address). *)
(* We compile accesses to them in load/store in this stage, *)
(* So no special treatment needed in RISC-V generation stage. *)


(** Emits SSA form. We choose a less human-readable form to facilitate verifier. *)
let to_string t =
  let rtype op ({ rd; rs1; rs2 }: r_type) =
    Printf.sprintf "%s %s %s %s" op rd.name rs1.name rs2.name
  in

  let r2type op ({ rd; rs1; }: r2_type) =
    Printf.sprintf "%s %s %s" op rd.name rs1.name
  in

  let itype op ({ rd; rs; imm } : i_type) =
    Printf.sprintf "%s %s %s %d" op rd.name rs.name imm
  in

  (* Deals with signedness: signed or unsigned *)
  let rtypeu op ({ rd; rs1; rs2 }: r_type) =
    let width = (match rd.ty with
    | T_uint | T_uint64 -> "u"
    | _ -> "") in
    Printf.sprintf "%s%s %s %s %s" op width rd.name rs1.name rs2.name
  in

  (* Deals with width: 32 or 64 bit *)
  let rtypew op ({ rd; rs1; rs2 }: r_type) =
    let width = (match rd.ty with
    | T_int | T_uint -> "w"
    | _ -> "") in
    Printf.sprintf "%s%s %s %s %s" op width rd.name rs1.name rs2.name
  in

  (* Deals with both width and signedness *)
  let rtypeuw op ({ rd; rs1; rs2 }: r_type) =
    let width = (match rd.ty with
    | T_int -> "w"
    | T_uint -> "uw"
    | T_uint64 -> "u"
    | _ -> "") in
    Printf.sprintf "%s%s %s %s %s" op width rd.name rs1.name rs2.name
  in

  let itypew op ({ rd; rs; imm }: i_type) =
    let width = (match rd.ty with
    | T_int | T_uint -> "w"
    | _ -> "") in
    Printf.sprintf "%s%s %s %s %d" op width rd.name rs.name imm
  in

  let die x =
    failwith (Printf.sprintf "riscv_ssa.ml: invalid byte count (%d) in load/store" x)
  in

  (** Deal with indentation inside functions. *)
  let rec str t depth =
    String.make (depth * 2) ' ' ^
    match t with
    (* Note: add, sub and mul actually aren't affected by `u`; *)
    (* nevertheless, the `u`s are included for debug purposes *)
    | Add r -> rtypeuw "add" r
    | Sub r -> rtypeuw "sub" r
    | Mul r -> rtypeuw "mul" r
    | Div r -> rtypeuw "div" r
    | Mod r -> rtypeuw "mod" r
    | Less r -> rtypeu "le" r
    | Leq r -> rtypeu "leq" r
    | Great r -> rtypeu "ge" r
    | Geq r -> rtypeu "geq" r
    | Eq r -> rtype "eq" r
    | Neq r -> rtype "ne" r
    | Neg r -> r2type "neg" r

    | And r -> rtype "and" r
    | Or r -> rtype "or" r
    | Sll r -> rtypew "sll" r
    | Srl r -> rtypew "srl" r
    | Sra r -> rtypew "sra" r
    | Xor r -> rtype "xor" r
    | Not r -> r2type "not" r

    | Addi i -> itypew "addi" i
    | Andi i -> itype "andi" i
    | Ori i -> itype "ori" i
    | Xori i -> itype "xori" i
    | Slli i -> itypew "slli" i
    | Srli i -> itypew "srli" i
    | Srai i -> itypew "srai" i
    | Slti i -> itypew "slti" i

    | FAdd r -> rtype "fadd" r
    | FSub r -> rtype "fsub" r
    | FMul r -> rtype "fmul" r
    | FDiv r -> rtype "fdiv" r
    | FLess r -> rtype "fle" r
    | FLeq r -> rtype "fleq" r
    | FGreat r -> rtype "fge" r
    | FGeq r -> rtype "fgeq" r
    | FEq r -> rtype "feq" r
    | FNeq r -> rtype "fneq" r
    | FNeg r -> r2type "fneg" r

    | Call { rd; fn; args } ->
        let args_list = String.concat " " (List.map (fun x -> x.name) args) in
        Printf.sprintf "call %s %s %s" rd.name fn args_list

    | CallExtern { rd; fn; args } ->
        let args_list = String.concat " " (List.map (fun x -> x.name) args) in
        Printf.sprintf "call_libc %s %s %s" rd.name fn args_list

    | CallIndirect { rd; rs; args } ->
        let args_list = String.concat " " (List.map (fun x -> x.name) args) in
        Printf.sprintf "call_indirect %s %s %s" rd.name rs.name args_list

    | AssignInt { rd; imm; } ->
        Printf.sprintf "li %s %d" rd.name imm

    | AssignInt64 { rd; imm; } ->
        Printf.sprintf "li %s %s" rd.name (Int64.to_string imm)

    | AssignFP { rd; imm; } ->
        Printf.sprintf "fli %s = %f" rd.name imm
    
    | AssignLabel { rd; imm; } ->
        Printf.sprintf "la %s %s" rd.name imm

    | Assign { rd; rs; } ->
        Printf.sprintf "mv %s %s" rd.name rs.name

    (* byte can only be one of: 1, 4 and 8. *)
    | Load { rd; rs; offset; byte } ->
        let op = (match byte with
        | 1 -> "lb"
        | 2 -> "lh"
        | 4 -> "lw"
        | 8 -> "ld"
        | x -> die x) in
        Printf.sprintf "%s %s %s %d" op rd.name rs.name offset

    | Store { rd; rs; offset; byte } ->
        let op = (match byte with
        | 1 -> "sb"
        | 2 -> "sh"
        | 4 -> "sw"
        | 8 -> "sd"
        | x -> die x) in
        Printf.sprintf "%s %s %s %d" op rd.name rs.name offset

    | Jump target ->
        Printf.sprintf "j %s" target

    | Branch { cond; ifso; ifnot } ->
        Printf.sprintf "br %s %s %s" cond.name ifso ifnot

    | Label label ->
        Printf.sprintf "\n%s%s:" (String.make (depth * 2 - 2) ' ') label
    
    | Phi { rd; rs } ->
        let rs_str = List.map (fun (r, label) -> Printf.sprintf "%s %s" r.name label) rs in
        Printf.sprintf "phi %s %s" rd.name (String.concat " " rs_str)

    | Malloc { rd; size } ->
        Printf.sprintf "malloc %s %d" rd.name size
    
    | FnDecl { fn; args; body; } ->
        let args_str = String.concat ", " (List.map (fun x -> x.name) args) in
        let body_str = String.concat "\n" (List.map (fun t -> str t (depth + 1)) body) in
        
        Printf.sprintf "fn %s (%s) {\n%s\n}\n" fn args_str body_str

    | GlobalVarDecl var ->
        Printf.sprintf "global %s: %s\n" var.name (Mtype.to_string var.ty)

    | ExtArray { label; values } ->
        Printf.sprintf "global array %s:\n  %s\n" label (String.concat ", " values)

    | Return var ->
        Printf.sprintf "return %s" var.name
  
  in str t 0

(** Maps all result registers with `fd` and all operands with `fs`. *)
let rec reg_map fd fs t = match t with
| Add { rd; rs1; rs2; } -> Add { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Sub { rd; rs1; rs2; } -> Sub { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Mul { rd; rs1; rs2; } -> Mul { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Div { rd; rs1; rs2; } -> Div { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Mod { rd; rs1; rs2; } -> Mod { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Less { rd; rs1; rs2; } -> Less { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Leq { rd; rs1; rs2; } -> Leq { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Great { rd; rs1; rs2; } -> Great { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Geq { rd; rs1; rs2; } -> Geq { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Eq { rd; rs1; rs2; } -> Eq { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Neq { rd; rs1; rs2; } -> Neq { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Neg { rd; rs1 } -> Neg { rd = fd rd; rs1 = fs rs1 }
| And { rd; rs1; rs2; } -> And { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Or { rd; rs1; rs2; } -> Or { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Srl { rd; rs1; rs2; } -> Srl { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Sll { rd; rs1; rs2; } -> Sll { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Sra { rd; rs1; rs2; } -> Sra { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Xor { rd; rs1; rs2; } -> Xor { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| Not { rd; rs1 } -> Not { rd = fd rd; rs1 = fs rs1 }
| Addi { rd; rs; imm } -> Addi { rd = fd rd; rs = fs rs; imm }
| Andi { rd; rs; imm } -> Andi { rd = fd rd; rs = fs rs; imm }
| Ori { rd; rs; imm } -> Ori { rd = fd rd; rs = fs rs; imm }
| Xori { rd; rs; imm } -> Xori { rd = fd rd; rs = fs rs; imm }
| Slli { rd; rs; imm } -> Slli { rd = fd rd; rs = fs rs; imm }
| Srli { rd; rs; imm } -> Srli { rd = fd rd; rs = fs rs; imm }
| Srai { rd; rs; imm } -> Srai { rd = fd rd; rs = fs rs; imm }
| Slti { rd; rs; imm } -> Slti { rd = fd rd; rs = fs rs; imm }
| FAdd { rd; rs1; rs2; } -> FAdd { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| FSub { rd; rs1; rs2; } -> FSub { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| FMul { rd; rs1; rs2; } -> FMul { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| FDiv { rd; rs1; rs2; } -> FDiv { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| FLess { rd; rs1; rs2; } -> FLess { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| FLeq { rd; rs1; rs2; } -> FLeq { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| FGreat { rd; rs1; rs2; } -> FGreat { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| FGeq { rd; rs1; rs2; } -> FGeq { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| FEq { rd; rs1; rs2; } -> FEq { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| FNeq { rd; rs1; rs2; } -> FNeq { rd = fd rd; rs1 = fs rs1; rs2 = fs rs2 }
| FNeg { rd; rs1 } -> FNeg { rd = fd rd; rs1 = fs rs1 }
| Call { rd; fn; args } -> Call { rd = fd rd; fn; args = List.map fs args }
| CallExtern { rd; fn; args } -> CallExtern { rd = fd rd; fn; args = List.map fs args }
| CallIndirect { rd; rs; args } -> CallIndirect { rd = fd rd; rs = fs rs; args = List.map fs args }
| AssignInt { rd; imm; } -> AssignInt { rd = fd rd; imm; }
| AssignInt64 { rd; imm; } -> AssignInt64 { rd = fd rd; imm; }
| AssignFP { rd; imm; } -> AssignFP { rd = fd rd; imm; }
| AssignLabel { rd; imm } -> AssignLabel { rd = fd rd; imm; }
| Assign { rd; rs } -> Assign { rd = fd rd; rs = fs rs }
| Load { rd; rs; offset; byte } -> Load { rd = fd rd; rs = fs rs; offset; byte }
| Store { rd; rs; offset; byte } -> Store { rd = fs rd; rs = fs rs; offset; byte }
| Jump label -> Jump label
| Branch { cond; ifso; ifnot } -> Branch { cond = fs cond; ifso; ifnot }
| Label label -> Label label
| Phi { rd; rs } -> Phi { rd = fd rd; rs = List.map (fun (x, name) -> (fs x, name)) rs }
| FnDecl { fn; args; body } -> FnDecl { fn; args; body = List.map (fun x -> reg_map fd fs x) body } 
| GlobalVarDecl var -> GlobalVarDecl var
| ExtArray arr -> ExtArray arr
| Malloc { rd; size } -> Malloc { rd = fd rd; size }
| Return var -> Return (fs var)

let reg_iter fd fs t =
  reg_map (fun x -> fd x; x) (fun x -> fs x; x) t |> ignore

(** Iterate on `rd` only. *)
let reg_iterd fd t =
  reg_iter fd (fun x -> x) t

(** Iterate on `rs` only. *)
let reg_iters fs t =
  reg_iter (fun x -> x) fs t

(** Counter of temporaries. *)
let slot = ref 0

(** Construct a new temporary variable. *)
let new_temp ty =
  let name = "%" ^ Int.to_string !slot in
  slot := !slot + 1;
  { name; ty }


(** Construct a new label. *)
let new_label prefix =
  let name = prefix ^ Int.to_string !slot in
  slot := !slot + 1;
  name