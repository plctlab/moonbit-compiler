(** Convert common IR into form of static single assignment (SSA). *)

type var = {
  name: string;
  typ: Mtype.t;
}

let to_string (r: var) = 
  Printf.sprintf "%s: %s" r.name (Mtype.to_string r.typ)


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
  is_export: bool;
}

(** Instructions available in SSA.  *)
and t = 
| Add of r_type
| Sub of r_type
| Mul of r_type
| Div of r_type
| Call of call_data
| FnDecl of fn

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
    | FnDecl { fn; args; body; is_export } ->
        let args_str = String.concat ", " (List.map to_string args) in
        let body_str = String.concat "\n" (List.map (fun t -> to_str_with_depth t (depth + 1)) body) in
        let fn_prefix = if is_export then "export fn" else "fn" in
        Printf.sprintf "%s %s (%s) {\n%s\n}" fn_prefix fn args_str body_str
  in to_str_with_depth t 0

(** Current counter of temporaries. *)
let slot = ref 0

(** Construct a new temporary name. *)
let new_temp () =
  let name = "%" ^ Int.to_string !slot in
  slot := !slot + 1;
  name

let convert_toplevel (top: Mcore.top_item) =
  match top with
  | Ctop_expr _ -> []
  | Ctop_fn { binder; func; export_info_; loc_ } ->
    let var_of_param ({ binder; ty; loc_ } : Mcore.param) =
      {
        name = Basic_core_ident.to_string binder;
        typ = ty
      }
    in
    let fn = Basic_core_ident.to_string binder in
    let args = List.map var_of_param func.params in
    [
      FnDecl { fn; args; body = []; is_export = false}
    ]
  | _ -> []

let ssa_of_mcore (core: Mcore.t) =
  Basic_io.write_s "core.ir" (Mcore.sexp_of_t core);
  let body = List.map convert_toplevel core.body |> List.flatten in
  Basic_io.write "core.ssa" (String.concat "\n" (List.map to_string body));
  body

