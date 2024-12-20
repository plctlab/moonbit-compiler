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
    | FnDecl { fn; args; body; return; } ->
        let args_str = String.concat ", " (List.map to_string args) in
        let body_str = String.concat "\n" (List.map (fun t -> to_str_with_depth t (depth + 1)) body) in
        let return_str = String.make (depth * 2 + 2) ' ' ^ (to_string return) in
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
We need to use a mutable structure to generate SSA.

However, this structure cannot be carried as return value,
since we must return the register in which the result of an SSA instruction is stored.

Therefore the assembly generated is placed in a global variable.
*)
let ssa = Basic_vec.make ~dummy:Nop 20

(*
Currently I don't know what does `prim` ever mean in some places,
so I ignore them in total.

Try update this when I know more.
*)
let warn prim = match prim with
| None -> ()
| Some _ -> prerr_endline "warning: prim is not null"

(**
As noted above, this function stores the SSA generated in global variable `ssa`.

It returns the variable in which 
*)
let do_convert (expr: Mcore.expr) =
  match expr with
  | Cexpr_var { id; ty; prim; _ } ->
    warn prim;
    { name = Ident.to_string id; ty }
  
  (*
  We treat primitives like special functions.
  It's just that we prefix them with `__prim_`,
  in order to distinguish from user-defined functions.
  *)
  | Cexpr_prim { prim; args; ty; _ } ->
    let rd = new_temp ty in
    rd

  | Cexpr_apply { func; args; ty; prim; _ } ->
    warn prim;
    let rd = new_temp ty in
    rd

  | _ -> failwith "TODO: cannot deal with this"

let convert_expr (expr: Mcore.expr) =
  let return = do_convert expr in
  (Basic_vec.map_into_list ssa (fun x -> x), return)

let convert_toplevel (top: Mcore.top_item) =
  match top with
  | Ctop_expr _ -> []
  | Ctop_fn { binder; func; export_info_; loc_ } ->
    let var_of_param ({ binder; ty; _ } : Mcore.param) =
      { name = Basic_core_ident.to_string binder; ty }
    in
    let fn = Basic_core_ident.to_string binder in
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
  let body = List.map convert_toplevel core.body |> List.flatten in
  Basic_io.write "core.ssa" (String.concat "\n" (List.map to_string body));
  body

