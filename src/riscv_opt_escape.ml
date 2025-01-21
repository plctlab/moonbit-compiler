(**
Does escape analysis, and put heap allocations to stack allocation / registers
based on the result.
*)
open Riscv_ssa
open Riscv_opt

type escape_state = 
| NoEscape      (* Does not escape the function *)
| LocalEscape   (* Escapes by getting captured by some closure *)
| GlobalEscape  (* Escapes by storing into some place *)

let join s1 s2 = match (s1, s2) with
| GlobalEscape, _ | _, GlobalEscape -> GlobalEscape
| LocalEscape, _ | _, LocalEscape -> LocalEscape
| _ -> NoEscape

let print_escape =
  Hashtbl.iter (fun var state -> Printf.printf "%s: %s\n" var (match state with
  | NoEscape -> "no escape"
  | LocalEscape -> "local escape"
  | GlobalEscape -> "global escape"))

let get_escape table (var: string) =
  if not (Hashtbl.mem table var) then
    Hashtbl.add table var NoEscape;
  Hashtbl.find table var


(**
Does escape analysis.
This does not yet support analysis of LocalEscape;
every variable is categorized into either No- or GlobalEscape.
*)
let escape_analysis fn =
  (* Do escape analysis in the data-flow way. *)
  (* It's quite similar to liveness analysis in riscv_opt.ml. *)
  let escape_in = Hashtbl.create 1024 in
  let escape_out = Hashtbl.create 1024 in

  let blocks = get_blocks fn in
  List.iter (fun name ->
    Hashtbl.add escape_in name (Hashtbl.create 64);
    Hashtbl.add escape_out name (Hashtbl.create 64);
  ) blocks;

  let worklist = Basic_vec.of_list blocks in
  while Basic_vec.length worklist != 0 do
    let name = Basic_vec.pop worklist in
    let block = block_of name in

    (* Escape_in should be the union of all escape_out *)
    Basic_vec.iter (fun pred ->
      let pred_out = Hashtbl.find escape_out pred in
      let block_in = Hashtbl.find escape_in name in
      Hashtbl.iter (fun var state ->
        let existing = get_escape block_in var in
        Hashtbl.replace block_in var (join existing state)
      ) pred_out
    ) block.pred;

    (* Now calculate escape_out based on it *)
    let old_out = Hashtbl.find escape_out name in
    let last_out = ref old_out in
    let new_out = Hashtbl.copy old_out in
    let changed = ref true in

    let replace var state =
      Hashtbl.replace new_out var.name state
    in

    while !changed do
      changed := false;
      Basic_vec.iter (fun x -> match x with
      | Assign { rd; rs } ->
          replace rd (get_escape new_out rs.name)

      | AssignLabel { rd; _ } -> replace rd GlobalEscape
      | Return x -> replace x GlobalEscape

      | Call { rd; args } 
      | CallExtern { rd; args } ->
          List.iter (fun arg -> 
            replace arg GlobalEscape
          ) args;
          replace rd GlobalEscape

      | Store { rd; rs }
      | Addi { rd; rs } ->
          let ed = get_escape new_out rd.name in
          let es = get_escape new_out rs.name in
          let state = join ed es in

          replace rd state;
          replace rs state

      | Add { rd; rs1; rs2 }
      | Sub { rd; rs1; rs2 } ->
          let ed = get_escape new_out rd.name in
          let es1 = get_escape new_out rs1.name in 
          let es2 = get_escape new_out rs2.name in
          let state = (join ed (join es1 es2)) in

          replace rd state;
          replace rs1 state;
          replace rs2 state

      | Phi { rd; rs } ->
          let state =
            List.fold_left (fun acc (var, _) ->
              join acc (get_escape new_out var.name)
            ) NoEscape rs
          in
          replace rd state;
          List.iter (fun (var, _) -> replace var state) rs

      | _ -> ()) block.body;
      
      Hashtbl.iter (fun var state ->
        if state != get_escape !last_out var then
          changed := true
      ) new_out;
      last_out := new_out;
    done;

    (* If anything changes, put it back to queue *)
    let changed = ref false in
    Hashtbl.iter (fun var state ->
      if state != get_escape old_out var then
        changed := true
    ) new_out;

    (* Note this `!` does not mean not *)
    if !changed then (
      Hashtbl.replace escape_out name new_out;
      Basic_vec.iter (fun x -> Basic_vec.push worklist x) block.succ
    )
  done;

  escape_out

(** Reforms `malloc` on heap to `alloca` on stack when possible. *)
let malloc_to_alloca fn =
  let blocks = get_blocks fn in
  let escape_data = escape_analysis fn in
  List.iter (fun name ->
    let block = block_of name in
    let body = block.body |> Basic_vec.to_list in
    let escaped = Hashtbl.find escape_data name in
    let changed = List.map (fun x -> match x with
    | Malloc { rd; size } ->
        if get_escape escaped rd.name = NoEscape then
          Alloca { rd; size }
        else
          Malloc { rd; size }
    | w -> w) body in
    block.body <- changed |> Basic_vec.of_list
  ) blocks

let lower_malloc ssa =
  iter_fn malloc_to_alloca ssa