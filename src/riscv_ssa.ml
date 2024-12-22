(** Convert common IR into form of static single assignment (SSA). *)

module Ident = Basic_core_ident

type var = {
  name: string;
  ty: Mtype.t;
}

(*
We store all discarded values (e.g. unit) into variable of this name.
*)
let discard = "_"

let to_string (r: var) = 
  Printf.sprintf "%s: %s" r.name (Mtype.to_string r.ty)

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

(**
Calls function named `fn` with args `args`,
and store the result in `rd`.
*)
type call_data = {
  rd: var;
  fn: string;
  args: var list;
}

(**
Lengths of immediates.
Other lengths (like 16-bit short) are not supported currently.
*)
type imm_type = Bit32 | Bit64 | Bit8

(**
Assigns (un-)signed integer `imm` to `rd`.
*)
type assign_int = {
  rd: var;
  imm: int64;
  size: imm_type;
  signed: bool;
}

(**
Assigns floating point number `imm` to `rd`.
*)
type assign_fp = {
  rd: var;
  imm: float;
  size: imm_type;
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
We load things from `rs` into `rd`,
and store things from `rd` into `rs`.
*)
type mem_access = {
  rd: var;
  rs: var;
  offset: int;
}

type phi = {
  rd: var;
  rs: (var * string) list;
}

type malloc = {
  rd: var;
  size: int;
}

(**
`entry` is the first basic block we'll encounter in this function.
*)
and fn = {
  fn: string;
  args: var list;
  body: t list;
}

and branch = {
  cond: var;
  ifso: string;
  ifnot: string;
}

(**
Instructions available in 3-address code and SSA.
*)
and t =
(* Arithmetic operations *)
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
| AssignFP of assign_fp
| AssignStr of assign_str
| Assign of assign
| Load of mem_access
| Store of mem_access
| Jump of string
| Branch of branch
| Label of string
| Phi of phi
| FnDecl of fn
| Malloc of malloc
| Return of var
| Nop

let to_string t =
  let rtype op ({ rd; rs1; rs2 }: r_type) =
    Printf.sprintf "%s = %s %s %s" (to_string rd) rs1.name op rs2.name
  in

  (** Deal with indentation inside functions. *)
  let rec str t depth =
    String.make (depth * 2) ' ' ^
    match t with
    | Add r -> rtype "+" r
    | Sub r -> rtype "-" r
    | Mul r -> rtype "*" r
    | Div r -> rtype "/" r
    | Mod r -> rtype "mod" r
    | Less r -> rtype "<" r
    | Leq r -> rtype "<=" r
    | Great r -> rtype ">" r
    | Geq r -> rtype ">=" r
    | Eq r -> rtype "==" r
    | Neq r -> rtype "!=" r
    | Neg { rd; rs1 } -> Printf.sprintf "%s = -%s" (to_string rd) rs1.name

    | FAdd r -> rtype "+." r
    | FSub r -> rtype "-." r
    | FMul r -> rtype "*." r
    | FDiv r -> rtype "/." r
    | FLess r -> rtype "<." r
    | FLeq r -> rtype "<=." r
    | FGreat r -> rtype ">." r
    | FGeq r -> rtype ">=." r
    | FEq r -> rtype "==." r
    | FNeq r -> rtype "!=." r
    | FNeg { rd; rs1 } -> Printf.sprintf "%s = -%s" (to_string rd) rs1.name

    | Call { rd; fn; args } ->
        let args_list = String.concat ", " (List.map (fun x -> x.name) args) in
        Printf.sprintf "%s = call %s (%s)" (to_string rd) fn args_list

    | AssignInt { rd; imm; size; signed } ->
        (* We follow C convention of representing literals. *)
        let suffix = (match (size, signed) with
          | (Bit32, true) -> ""
          | (Bit64, true) -> "ll"
          | (Bit32, false) -> "u"
          | (Bit64, false) -> "ull"
          | (Bit8, true) -> "b"
          | (Bit8, false) -> "ub")
        in 
        Printf.sprintf "%s = %s%s" (to_string rd) (Int64.to_string imm) suffix

    | AssignFP { rd; imm; size; } ->
        let suffix = (match size with
          | Bit32 -> "f"
          | Bit64 -> ""
          | _ -> failwith "riscv_ssa.ml: bad floating-point length")
        in
        Printf.sprintf "%s = %f%s" (to_string rd) imm suffix
    
    | AssignStr { rd; imm; } ->
        Printf.sprintf "%s = \"%s\"" (to_string rd) imm

    | Assign { rd; rs; } ->
        Printf.sprintf "%s = %s" (to_string rd) rs.name

    | Load { rd; rs; offset } ->
        Printf.sprintf "%s = %s[offset = %d]" (to_string rd) rs.name offset

    | Store { rd; rs; offset } ->
        Printf.sprintf "%s[offset = %d] = %s" rs.name offset rd.name

    | Jump target ->
        Printf.sprintf "jump %s" target

    | Branch { cond; ifso; ifnot } ->
        Printf.sprintf "br %s true:%s false:%s" cond.name ifso ifnot

    | Label label ->
        Printf.sprintf "\n%s%s:" (String.make (depth * 2 - 2) ' ') label
    
    | Phi { rd; rs } ->
        let rs_str = List.map (fun (r, label) -> Printf.sprintf "%s[%s]" r.name label) rs in
        Printf.sprintf "%s = φ %s" (to_string rd) (String.concat " " rs_str)

    | Malloc { rd; size } ->
        Printf.sprintf "%s = malloc %d" rd.name size
    
    | FnDecl { fn; args; body; } ->
        let args_str = String.concat ", " (List.map to_string args) in
        let body_str = String.concat "\n" (List.map (fun t -> str t (depth + 1)) body) in
        
        Printf.sprintf "fn %s (%s) {\n%s\n}\n" fn args_str body_str

    | Return var ->
        Printf.sprintf "return %s" var.name
    
    | Nop -> "nop"
  
  in str t 0

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

(**
Currently I don't know what does `prim` ever mean in some places,
so I ignore them in total.

Try update this when I know more.
*)
let warn prim = match prim with
| None -> ()
| Some _ -> prerr_endline "warning: prim is not null"


let offset_table = Hashtbl.create 64
let size_table = Hashtbl.create 64

let offsetof name pos = Hashtbl.find offset_table (name, pos)


(** This assumes RISCV64. Perhaps support 32 as well in future? *)
let rec sizeof ty =
  let pointer_size = 8 in

  match ty with
  | Mtype.T_bool -> 1
  | Mtype.T_byte -> 1
  | Mtype.T_bytes -> pointer_size
  | Mtype.T_char -> 1
  | Mtype.T_double -> 8
  | Mtype.T_float -> 4
  | Mtype.T_func _ -> pointer_size
  | Mtype.T_int -> 4
  | Mtype.T_int64 -> 8
  | Mtype.T_string -> pointer_size
  | Mtype.T_uint -> 4
  | Mtype.T_uint64 -> 8
  | Mtype.T_unit -> 0
  | Mtype.T_tuple { tys } -> List.fold_left (fun total x -> total + sizeof x) 0 tys
  | Mtype.T_constr id -> Hashtbl.find size_table id
  | _ -> failwith "riscv_ssa.ml: cannot calculate size"


(** The variable defined in the instruction. *)
let def t = match t with
| Add { rd; _ } -> [rd]
| Sub { rd; _ } -> [rd]
| Mul { rd; _ } -> [rd]
| Div { rd; _ } -> [rd]
| Mod { rd; _ } -> [rd]
| Less { rd; _ } -> [rd]
| Leq { rd; _ } -> [rd]
| Great { rd; _ } -> [rd]
| Geq { rd; _ } -> [rd]
| Eq { rd; _ } -> [rd]
| Neq { rd; _ } -> [rd]
| Neg { rd; _ } -> [rd]
| FAdd { rd; _ } -> [rd]
| FSub { rd; _ } -> [rd]
| FMul { rd; _ } -> [rd]
| FDiv { rd; _ } -> [rd]
| FLess { rd; _ } -> [rd]
| FLeq { rd; _ } -> [rd]
| FGreat { rd; _ } -> [rd]
| FGeq { rd; _ } -> [rd]
| FEq { rd; _ } -> [rd]
| FNeq { rd; _ } -> [rd]
| FNeg { rd; _ } -> [rd]
| Call { rd; _ } -> [rd]
| AssignInt { rd; _ } -> [rd]
| AssignFP { rd; _ } -> [rd]
| AssignStr { rd; _ } -> [rd]
| Assign { rd; _ } -> [rd]
| Load { rd; _ } -> [rd]
| Store _ -> []
| Jump _ -> []
| Branch _ -> []
| Label _ -> []
| Phi { rd; _ } -> [rd]
| FnDecl _ -> []
| Malloc { rd; _ } -> [rd]
| Return _ -> []
| Nop -> []

(** Variables that has been accessed in this instruction. *)
let use t = match t with
| Add { rs1; rs2; _ } -> [rs1; rs2]
| Sub { rs1; rs2; _ } -> [rs1; rs2]
| Mul { rs1; rs2; _ } -> [rs1; rs2]
| Div { rs1; rs2; _ } -> [rs1; rs2]
| Mod { rs1; rs2; _ } -> [rs1; rs2]
| Less { rs1; rs2; _ } -> [rs1; rs2]
| Leq { rs1; rs2; _ } -> [rs1; rs2]
| Great { rs1; rs2; _ } -> [rs1; rs2]
| Geq { rs1; rs2; _ } -> [rs1; rs2]
| Eq { rs1; rs2; _ } -> [rs1; rs2]
| Neq { rs1; rs2; _ } -> [rs1; rs2]
| Neg { rs1; _ } -> [rs1]
| FAdd { rs1; rs2; _ } -> [rs1; rs2]
| FSub { rs1; rs2; _ } -> [rs1; rs2]
| FMul { rs1; rs2; _ } -> [rs1; rs2]
| FDiv { rs1; rs2; _ } -> [rs1; rs2]
| FLess { rs1; rs2; _ } -> [rs1; rs2]
| FLeq { rs1; rs2; _ } -> [rs1; rs2]
| FGreat { rs1; rs2; _ } -> [rs1; rs2]
| FGeq { rs1; rs2; _ } -> [rs1; rs2]
| FEq { rs1; rs2; _ } -> [rs1; rs2]
| FNeq { rs1; rs2; _ } -> [rs1; rs2]
| FNeg { rs1; _ } -> [rs1]
| Call { args; _ } -> args
| AssignInt _ -> []
| AssignFP _ -> []
| AssignStr _ -> []
| Assign { rs; _ } -> [rs]
| Load { rs; _ } -> [rs]
| Store { rd; rs; _ } -> [rd; rs]
| Jump _ -> []
| Branch { cond; } -> [cond]
| Label _ -> []
| Phi { rs } -> List.map (fun (var, label) -> var) rs
| FnDecl _ -> []
| Malloc _ -> []
| Return var -> [var]
| Nop -> []


(** Push the correct sequence of instruction based on primitives. *)
let deal_with_prim ssa rd (prim: Primitive.prim) args =
  let die () =
    failwith "riscv_ssa.ml: bad primitive format"
  in

  match prim with
  | Pcomparison { operand_type; operator } ->
      let is_fp = (operand_type = F32 || operand_type = F64) in
      let op = (match is_fp, operator, args with
      | false, Lt, [rs1; rs2] -> (Less { rd; rs1; rs2 })
      | true, Lt, [rs1; rs2] -> (FLess { rd; rs1; rs2 })
      | false, Gt, [rs1; rs2] -> (Great { rd; rs1; rs2 })
      | true, Gt, [rs1; rs2] -> (FGreat { rd; rs1; rs2 })
      | false, Ne, [rs1; rs2] -> (Neq { rd; rs1; rs2 })
      | true, Ne, [rs1; rs2] -> (FNeq { rd; rs1; rs2 })
      | false, Eq, [rs1; rs2] -> (Eq { rd; rs1; rs2 })
      | true, Eq, [rs1; rs2] -> (FEq { rd; rs1; rs2 })
      | false, Le, [rs1; rs2] -> (Leq { rd; rs1; rs2 })
      | true, Le, [rs1; rs2] -> (FLeq { rd; rs1; rs2 })
      | false, Ge, [rs1; rs2] -> (Geq { rd; rs1; rs2 })
      | true, Ge, [rs1; rs2] -> (FGeq { rd; rs1; rs2 })
      | _ -> die ()) in
      Basic_vec.push ssa op
  
  | Parith { operand_type; operator } ->
      let is_fp = (operand_type = F32 || operand_type = F64) in
      let op = (match is_fp, operator, args with
      | false, Add, [rs1; rs2] -> (Add { rd; rs1; rs2 })
      | true, Add, [rs1; rs2] -> (FAdd { rd; rs1; rs2 })
      | false, Sub, [rs1; rs2] -> (Sub { rd; rs1; rs2 })
      | true, Sub, [rs1; rs2] -> (FSub { rd; rs1; rs2 })
      | false, Mul, [rs1; rs2] -> (Mul { rd; rs1; rs2 })
      | true, Mul, [rs1; rs2] -> (FMul { rd; rs1; rs2 })
      | false, Div, [rs1; rs2] -> (Div { rd; rs1; rs2 })
      | true, Div, [rs1; rs2] -> (FDiv { rd; rs1; rs2 })
      | false, Mod, [rs1; rs2] -> (Mod { rd; rs1; rs2 })
      | false, Neg, [rs1] -> (Neg { rd; rs1 })
      | true, Neg, [rs1] -> (FNeg { rd; rs1 })
      | _ -> die ()) in
      Basic_vec.push ssa op
  
  | _ -> Basic_vec.push ssa (Call { rd; fn = (Primitive.sexp_of_prim prim |> S.to_string); args })


(** Calculate offset of fields in record types. *)
let update_types ({ defs; _ }: Mtype.defs) =
  let types = Mtype.Id_hash.to_list defs in

  let visit (name, info) =
    match info with
    | Mtype.Placeholder -> ()
    | Mtype.Externref -> ()
    | Mtype.Trait _ -> ()

    | Mtype.Record { fields } -> 
        let extract (x: Mtype.field_info) = x.field_type in
        let field_types = List.map extract fields in
        let field_sizes = List.map sizeof field_types in
        let offset = ref 0 in
        let offsets = List.map (fun x -> let y = !offset in offset := x + !offset; y) field_sizes in
        List.iteri (fun i x -> Hashtbl.add offset_table (name, i) x) offsets;
        Hashtbl.add size_table name !offset
    
    | _ -> failwith "TODO: riscv_ssa.ml: cannot deal with this type"
  in
  List.iter visit types

(**
This is reserved for `continue`s.
See the match case for `Cexpr_loop` in `do_convert` for more details.

It represents a list of continue clauses, each with a list of arguments and a label,
marking where the argument comes from.
*)
let conts: (var * string) list list ref = ref []

(**
This function stores the SSA generated in the given argument `ssa`.

It returns the variable in which the result of the last instruction pushed is stored.
*)
let rec do_convert ssa (expr: Mcore.expr) =
  match expr with
  | Cexpr_unit _ ->
      { name = discard; ty = Mtype.T_unit }
  
  | Cexpr_var { id; ty; prim; _ } ->
      warn prim;

      let variable = { name = Ident.to_string id; ty } in

      (* We treat mutables as pointers. *)
      (match id with
      | Pmutable_ident _ ->
          let rd = new_temp ty in
          Basic_vec.push ssa (Load { rd; rs = variable; offset = 0 });
          rd
      
      | _ -> variable);
  
      
  (* Not quite sure about this; is it simply a variable access? *)
  | Cexpr_object { self; } ->
      do_convert ssa self
  
  (* Primitives are intrinsic functions. *)
  (* We tidy some of these up, and compile others into functions. *)
  | Cexpr_prim { prim; args; ty; _ } ->
      let rd = new_temp ty in
      let args = List.map (fun expr -> do_convert ssa expr) args in
      (* TODO: take special care with Psequand and Psequor. *)
      (* They are short-circuited and should be compiled into if-else. *)
      deal_with_prim ssa rd prim args;
      rd

  | Cexpr_let { name; rhs; body; _ } ->
      let rs = do_convert ssa rhs in
      (match name with
      | Pmutable_ident _ ->
          (* We use `bytes` to represent arbitrary pointers. *)
          let space = new_temp Mtype.T_bytes in
          let rd = { name = Ident.to_string name; ty = Mtype.T_bytes } in
          Basic_vec.push ssa (Malloc { rd = space; size = sizeof rd.ty });
          Basic_vec.push ssa (Assign { rd; rs = space });
          Basic_vec.push ssa (Store { rd; rs; offset = 0 });
      
      | _ ->
          let rd = { name = Ident.to_string name; ty = rs.ty } in
          Basic_vec.push ssa (Assign { rd; rs }));
      do_convert ssa body

  | Cexpr_apply { func; args; ty; prim; _ } ->
      warn prim;
      let rd = new_temp ty in
      let fn = Ident.to_string func in
      let args = List.map (fun expr -> do_convert ssa expr) args in
      Basic_vec.push ssa (Call { rd; fn; args });
      rd

  | Cexpr_sequence { expr1; expr2; _ } ->
      do_convert ssa expr1 |> ignore;
      do_convert ssa expr2

  (* Meaning: access the `pos`-th field of `record` *)
  | Cexpr_field { record; accessor; pos; ty; _ } ->
      let rd = new_temp ty in
      let rs = do_convert ssa record in
      
      let name =
        (match rs.ty with
        | T_constr id -> id
        | _ -> failwith "TODO: riscv_ssa.ml: currently unsupported record type")
      in

      (match accessor with
      | Label _ -> ()
      | _ -> failwith "TODO: riscv_ssa.ml: currently unsupported accessor");
      
      let offset = offsetof name pos in
      Basic_vec.push ssa (Load { rd; rs; offset; });
      rd
    
  (* Meaning: set the `pos`-th field of `record` to `field` *)
  | Cexpr_mutate { record; pos; field } ->
    let rs = do_convert ssa record in
    let rd = do_convert ssa field in
    
    let name =
      (match rs.ty with
      | T_constr id -> id
      | _ -> failwith "riscv_ssa.ml: can only mutate record types")
    in
    
    let offset = offsetof name pos in
    Basic_vec.push ssa (Store { rd; rs; offset; });
    { name = discard; ty = Mtype.T_unit }

  | Cexpr_if { cond; ifso; ifnot; ty; _ } ->
      let rd = new_temp ty in

      let cond = do_convert ssa cond in

      let ifso_ssa = Basic_vec.make ~dummy:Nop 20 in 
      let ifso_result = do_convert ifso_ssa ifso in

      let ifnot_ssa = Basic_vec.make ~dummy:Nop 20 in
      let ifnot_result =
        (match ifnot with
        | None -> { name = discard; ty = Mtype.T_unit }
        | Some x -> do_convert ifnot_ssa x
        )
      in

      let ifso_label = new_label "ifso_" in
      let ifnot_label = new_label "ifnot_" in
      let ifexit_label = new_label "ifexit_" in

      (*
        Compiling into:
      
          br %cond true:%ifso false:%ifnot
        
        ifso:
          ...
          jump ifexit
        
        ifnot:
          ...
          jump ifexit
        
        ifexit:
          %rd = φ %ifso_result[ifso] %ifnot_result[ifnot]
      *)

      Basic_vec.push ssa (Branch { cond; ifso = ifso_label; ifnot = ifnot_label });

      Basic_vec.push ssa (Label ifso_label);
      Basic_vec.append ssa ifso_ssa;
      Basic_vec.push ssa (Jump ifexit_label);

      Basic_vec.push ssa (Label ifnot_label);
      Basic_vec.append ssa ifnot_ssa;
      Basic_vec.push ssa (Jump ifexit_label);

      Basic_vec.push ssa (Label ifexit_label);
      Basic_vec.push ssa (Phi
        { rd; rs = [(ifso_result, ifso_label); (ifnot_result, ifnot_label)] });
      
      rd

  (*
    In MoonBit core IR, loops are by default not looping.
    They only jump to beginning when they meet `Cexpr_continue`,
    in which case their `args` will be substituted by the `args` provided there,
    and the loop entry condition will be tested again.

    Therefore the loop is compiled as follows:

      before:
        # evaluate args
        jump head

      loop:
        %arg = φ %arg[before] %arg1[cont1] ... %argn[contn]
        ...
        jump exit

      exit:

    A good thing is that loops don't return a value. We don't need to insert
    φ after the label `exit`.
  *)
  | Cexpr_loop { params; body; args; label; ty } ->
      (* We need to use the global variable `conts`. *)
      (* In case there's an outer loop, we might have tampered it; *)
      (* So we must store the contents somewhere. *)
      let old_conts = !conts in

      (* Get the labels *)
      let loop = Printf.sprintf "%s_%d" label.name label.stamp in
      let before = Printf.sprintf "before_%s" loop in
      let exit = Printf.sprintf "exit_%s" loop in

      (* Generate body. `conts` will be filled by Cexpr_continue. *)
      let body_ssa = Basic_vec.make ~dummy:Nop 32 in
      let _ = do_convert body_ssa body in

      (* Start generating according to the template described above. *)
      
      (* Generate `before`. *)

      Basic_vec.push ssa (Jump before);
      Basic_vec.push ssa (Label before);
      let results = List.map (do_convert ssa) args in
      let cont = List.map (fun x -> (x, before)) results in
      conts := cont :: !conts;

      (* Calculate the φ-call. *)

      Basic_vec.push ssa (Jump loop);
      Basic_vec.push ssa (Label loop);

      let rec transpose lst =
        match lst with
        | [] -> []
        | [] :: _ -> []
        | _ -> List.map List.hd lst :: transpose (List.map List.tl lst)
      in

      let grouped = transpose !conts in
      let gen_phi (par: Mcore.param) rs =
        Phi { rd = { name = Ident.to_string par.binder; ty = par.ty }; rs }
      in
      
      let phis = List.map2 gen_phi params grouped in
      List.iter (fun x -> Basic_vec.push ssa x) phis;

      (* Generate rest parts. *)

      Basic_vec.append ssa body_ssa;
      Basic_vec.push ssa (Jump exit);
      Basic_vec.push ssa (Label exit);

      (* Store `conts` back; let outer loop go on normally. *)
      conts := old_conts;

      { name = discard; ty = Mtype.T_unit }

  (* See the explanation for Cexpr_loop. *)
  | Cexpr_continue { args; label } ->
      (* Generate a label, and let the previous block jump to this block. *)
      let cont = new_label "continue_" in
      Basic_vec.push ssa (Jump cont);
      Basic_vec.push ssa (Label cont);

      (* Evaluate arguments and update `conts`. *)
      let results = List.map (do_convert ssa) args in
      let new_cont = List.map (fun x -> (x, cont)) results in
      conts := new_cont :: !conts;

      (* Jump back to the beginning of the loop. *)
      let loop_name = Printf.sprintf "%s_%d" label.name label.stamp in 
      Basic_vec.push ssa (Jump loop_name);

      { name = discard; ty = Mtype.T_unit }

  (* Assigns mutable variables. *)
  | Cexpr_assign { var; expr; ty } ->
      let rd = do_convert ssa expr in
      let rs = { name = Ident.to_string var; ty = Mtype.T_bytes} in
      Basic_vec.push ssa (Store { rd; rs; offset = 0 });
      { name = discard; ty = Mtype.T_unit }

  (* Builds a record type. *)
  | Cexpr_record { fields; ty; } ->
      (* Allocate space for the record *)
      let rd = new_temp Mtype.T_bytes in
      Basic_vec.push ssa (Malloc { rd; size = sizeof ty });

      let name =
        (match ty with
        | Mtype.T_constr id -> id
        (* This must be a record *)
        | _ -> assert false)
      in

      (* Construct all its fields *)
      let visit ({ pos; expr; _ }: Mcore.field_def) =
        let result = do_convert ssa expr in
        let offset = offsetof name pos in 
        Basic_vec.push ssa (Store { rd = result; rs = rd; offset })
      in

      List.iter visit fields;
      rd

  | Cexpr_break _ ->
      prerr_endline "break";
      { name = discard; ty = Mtype.T_unit }

  | Cexpr_return _ ->
      prerr_endline "return";
      { name = discard; ty = Mtype.T_unit }

  | Cexpr_letfn _ ->
      prerr_endline "letfn";
      { name = discard; ty = Mtype.T_unit }

  | Cexpr_function _ ->
      prerr_endline "function";
      { name = discard; ty = Mtype.T_unit }

  | Cexpr_constr _ ->
      prerr_endline "constr";
      { name = discard; ty = Mtype.T_unit }

  | Cexpr_letrec _ ->
      prerr_endline "letrec";
      { name = discard; ty = Mtype.T_unit }

  | Cexpr_tuple _ ->
      prerr_endline "tuple";
      { name = discard; ty = Mtype.T_unit }

  | Cexpr_record_update _ ->
      prerr_endline "record_update";
      { name = discard; ty = Mtype.T_unit }

  | Cexpr_switch_constr _ ->
      prerr_endline "switch constr";
      { name = discard; ty = Mtype.T_unit }

  | Cexpr_switch_constant _ ->
      prerr_endline "switch constant";
      { name = discard; ty = Mtype.T_unit }

  | Cexpr_handle_error _ ->
      prerr_endline "handle error";
      { name = discard; ty = Mtype.T_unit }

  | Cexpr_array _ ->
      prerr_endline "array";
      { name = discard; ty = Mtype.T_unit }

  | Cexpr_const { c; ty; _ } ->
      let rd = new_temp ty in
      let instruction = (match c with
      | C_string imm ->
          AssignStr { rd; imm; }
      | C_bool imm ->
          AssignInt { rd; imm = Int64.of_int (if imm then 1 else 0); size = Bit8; signed = true; }
      | C_char imm ->
          AssignInt { rd; imm = Int64.of_int (Uchar.to_int imm); size = Bit8; signed = false; }
      | C_int { v; _ } ->
          AssignInt { rd; imm = Int64.of_int32 v; size = Bit32; signed = true; }
      | C_int64 { v; _ } ->
          AssignInt { rd; imm = v; size = Bit64; signed = true; }
      | C_uint { v; _ } ->
          AssignInt { rd; imm = Int64.of_int32 v; size = Bit32; signed = false; }
      | C_uint64 { v; _ } ->
          AssignInt { rd; imm = v; size = Bit64; signed = false; }
      | C_float { v; _ } ->
          AssignFP { rd; imm = v; size = Bit32; }
      | C_double { v; _ } ->
          AssignFP { rd; imm = v; size = Bit64; }
      | C_bytes { v; _ } ->
          AssignStr { rd; imm = v }
      (* BigInt; currently not supported *)
      | _ -> failwith "TODO: riscv_ssa.ml: bigint not supported"
      ) in
      Basic_vec.push ssa instruction;
      rd

(**
Converts given `expr` into a list of SSA instructions,
along with the variable in which the result of this expression is stored.
*)
let convert_expr (expr: Mcore.expr) =
  let ssa = Basic_vec.make ~dummy:Nop 20 in
  let return = do_convert ssa expr in
  Basic_vec.push ssa (Return return);
  Basic_vec.map_into_list ssa (fun x -> x)

(** We will only do this with *)
let convert_toplevel (top: Mcore.top_item) =
  match top with
  | Ctop_fn { binder; func; export_info_; _ } ->
    let var_of_param ({ binder; ty; _ } : Mcore.param) =
      { name = Ident.to_string binder; ty }
    in
    let fn = Ident.to_string binder in
    let args = List.map var_of_param func.params in
    let body = convert_expr func.body in
    if export_info_ != None then
      prerr_endline "warning: export info is non-empty";
    [
      FnDecl { fn; args; body }
    ]

  (*
  No need to deal with stubs.
  They are just declarations of builtin functions, which we don't care -
  since they don't carry anything about implementation.
  *)
  | Ctop_stub _ -> []
  
  | _ -> failwith "TODO: riscv_ssa.ml: don't know this toplevel"

let ssa_of_mcore (core: Mcore.t) =
  Basic_io.write_s "core.ir" (Mcore.sexp_of_t core);
  (* Look through types, and calculate their field offsets *)
  update_types core.types;

  (* Deal with other functions *)
  let body = List.map convert_toplevel core.body |> List.flatten in

  (* Deal with main *)
  let with_main = match core.main with
    | Some (main_expr, _) ->
        let main_body = convert_expr main_expr in
        let main_decl = FnDecl { fn = "main"; args = []; body = main_body } in
        main_decl :: body
      
    | None -> body
  in
  Basic_io.write "core.ssa" (String.concat "\n" (List.map to_string with_main));
  with_main
