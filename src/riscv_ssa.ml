(** Convert common IR into form of static single assignment (SSA). *)

module Ident = Basic_core_ident

type var = {
  name: string;
  ty: Mtype.t;
}

let to_string (r: var) = 
  Printf.sprintf "%s: %s" r.name (Mtype.to_string r.ty)


(**
Similar to R-type instructions in RISC-V, hence the name.
It consists of `rd` (destination) and two sources `rs1` and `rs2`.
*)
type r_type = {
  rd: var;
  rs1: var;
  rs2: var;
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
type imm_type = Bit32 | Bit64

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

type fn = {
  fn: string;
  args: var list;
  body: t list;
  return: var;
}

(** Instructions available in SSA.  *)
and t = 
| Add of r_type
| Sub of r_type
| Mul of r_type
| Div of r_type
| Call of call_data
| AssignInt of assign_int
| AssignFP of assign_fp
| AssignStr of assign_str
| FnDecl of fn
| Nop

let to_string t =
  let convert_rtype ty ({rd; rs1; rs2}: r_type) =
    let rd_str = to_string rd in 
    let rs1_str = to_string rs1 in 
    let rs2_str = to_string rs2 in 
    Printf.sprintf "%s = %s %s, %s" rd_str ty rs1_str rs2_str
  in

  (** Deal with indentation inside functions. *)
  let rec to_str_with_depth t depth =
    String.make (depth * 2) ' ' ^
    match t with
    | Add r -> convert_rtype "add" r
    | Sub r -> convert_rtype "sub" r
    | Mul r -> convert_rtype "mul" r
    | Div r -> convert_rtype "div" r

    | Call { rd; fn; args } ->
        let args_list = String.concat ", " (List.map to_string args) in
        Printf.sprintf "%s = call %s (%s)" (to_string rd) fn args_list

    | AssignInt { rd; imm; size; signed } ->
        (* We follow C convention of representing literals. *)
        let suffix = (match (size, signed) with
          | (Bit32, true) -> ""
          | (Bit64, true) -> "ll"
          | (Bit32, false) -> "u"
          | (Bit64, false) -> "ull")
        in 
        Printf.sprintf "%s = %s%s" (to_string rd) (Int64.to_string imm) suffix

    | AssignFP { rd; imm; size; } ->
        let suffix = (match size with
          | Bit32 -> "f"
          | Bit64 -> "")
        in
        Printf.sprintf "%s = %s%s" (to_string rd) (Float.to_string imm) suffix
    
    | AssignStr { rd; imm; } ->
        Printf.sprintf "%s = \"%s\"" (to_string rd) imm
    
    | FnDecl { fn; args; body; return; } ->
        let args_str = String.concat ", " (List.map to_string args) in
        let body_str = String.concat "\n" (List.map (fun t -> to_str_with_depth t (depth + 1)) body) in
        let return_str = String.make (depth * 2 + 2) ' ' ^ "return " ^ (to_string return) in
        Printf.sprintf "fn %s (%s) {\n%s\n%s\n}" fn args_str body_str return_str
    
    | Nop -> "nop"
  
  in to_str_with_depth t 0

(** Counter of temporaries. *)
let slot = ref 0

(** Construct a new temporary variable. *)
let new_temp ty =
  let name = "%" ^ Int.to_string !slot in
  slot := !slot + 1;
  { name; ty }

(**
Currently I don't know what does `prim` ever mean in some places,
so I ignore them in total.

Try update this when I know more.
*)
let warn prim = match prim with
| None -> ()
| Some _ -> prerr_endline "warning: prim is not null"

(**
This function stores the SSA generated in the given argument `ssa`.

It returns the variable in which the result of the last instruction pushed is stored.
*)
let rec do_convert ssa (expr: Mcore.expr) =
  match expr with
  | Cexpr_var { id; ty; prim; _ } ->
    warn prim;
    { name = Ident.to_string id; ty }
  
  (*
  We treat primitives like special functions.
  
  Since all primitives start with "%",
  there is no risk that one of them coincide with user-defined functions.
  *)
  | Cexpr_prim { prim; args; ty; _ } ->
      let rd = new_temp ty in
      let name = Primitive.find_name prim in
      let args = List.map (fun expr -> do_convert ssa expr) args in
      let fn = match name with
        | None -> failwith "riscv_ssa.ml: unrecognized intrinsics"
        | Some x -> x
      in
      Basic_vec.push ssa (Call { rd; fn; args });
      rd

  | Cexpr_apply { func; args; ty; prim; _ } ->
      warn prim;
      let rd = new_temp ty in
      let fn = Ident.to_string func in
      let args = List.map (fun expr -> do_convert ssa expr) args in
      Basic_vec.push ssa (Call { rd; fn; args });
      rd

  | Cexpr_const { c; ty; _ } ->
      let rd = new_temp ty in
      let instruction = (match c with
      | C_string imm ->
          AssignStr { rd; imm; }
      | C_bool imm ->
          AssignInt { rd; imm = Int64.of_int (if imm then 1 else 0); size = Bit32; signed = true; }
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
      | _ -> failwith "TODO: riscv_ssa.ml: unsupported constant type"
      ) in
      Basic_vec.push ssa instruction;
      rd 

  | _ -> failwith "TODO: riscv_ssa.ml: cannot deal with this expression"

(**
Converts given `expr` into a list of SSA instructions,
along with the variable in which the result of this expression is stored.
*)
let convert_expr (expr: Mcore.expr) =
  let ssa = Basic_vec.make ~dummy:Nop 20 in
  let return = do_convert ssa expr in
  (Basic_vec.map_into_list ssa (fun x -> x), return)

let convert_toplevel (top: Mcore.top_item) =
  match top with
  | Ctop_expr _ -> []

  | Ctop_fn { binder; func; export_info_; loc_ } ->
    let var_of_param ({ binder; ty; _ } : Mcore.param) =
      { name = Ident.to_string binder; ty }
    in
    let fn = Ident.to_string binder in
    let args = List.map var_of_param func.params in
    let (body, return) = convert_expr func.body in
    if export_info_ != None then
      prerr_endline "warning: export info is non-empty";
    [
      FnDecl { fn; args; body; return }
    ]
  
  | _ -> []

let ssa_of_mcore (core: Mcore.t) =
  Basic_io.write_s "core.ir" (Mcore.sexp_of_t core);
  (* Deal with other functions *)
  let body = List.map convert_toplevel core.body |> List.flatten in

  (* Deal with main *)
  let with_main = match core.main with
    | Some (main_expr, _) ->
        let (main_body, return) = convert_expr main_expr in
        let main_decl = FnDecl { fn = "main"; args = []; body = main_body; return } in
        main_decl :: body
      
    | None -> body
  in
  Basic_io.write "core.ssa" (String.concat "\n" (List.map to_string with_main));
  with_main
