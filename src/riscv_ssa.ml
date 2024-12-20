(* Convert common IR into form of static single assignment (SSA). *)

type var = string
type func = string
type label = string

type t = 
| Add of var * var * var
| Sub of var * var * var
| Mul of var * var * var
| Div of var * var * var
| Call of var * func * var list

let to_string t =
  let convert_3reg ty rd rs1 rs2 =
    Printf.sprintf "%s = %s %s, %s" rd ty rs1 rs2
  in
  match t with
  | Add (rd, rs1, rs2) -> convert_3reg "add" rd rs1 rs2
  | Sub (rd, rs1, rs2) -> convert_3reg "sub" rd rs1 rs2
  | Mul (rd, rs1, rs2) -> convert_3reg "mul" rd rs1 rs2
  | Div (rd, rs1, rs2) -> convert_3reg "div" rd rs1 rs2
  | Call (rd, fn, args) -> Printf.sprintf "%s = call %s (%s)" rd fn (String.concat ", " args)

(* Current counter of temporaries *)
let slot = ref 0

(* Construct a new temporary name *)
let new_temp () =
  let name = "%" ^ Int.to_string !slot in
  slot := !slot + 1;
  name

let convert_toplevel (top: Mcore.top_item) =
  match top with
  | Ctop_expr _ -> ()
  | _ -> ()

let ssa_of_mcore (core: Mcore.t) =
  List.iter convert_toplevel core.body

