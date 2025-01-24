(**
Does escape analysis, and put heap allocations to stack allocation / registers
based on the result.
*)
open Riscv_ssa
open Riscv_opt

let print_escape out_escape =
  Hashtbl.iter (fun x y ->
    Printf.printf "%s:\n" x;
    Stringset.iter (fun x -> Printf.printf "%s " x) y;
    Printf.printf "\n";
  ) out_escape;
  Printf.printf "\n"

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
    Hashtbl.add escape_in name Stringset.empty;
    Hashtbl.add escape_out name Stringset.empty
  ) blocks;

  let worklist = Basic_vec.of_list blocks in
  while Basic_vec.length worklist != 0 do
    let name = Basic_vec.pop worklist in
    let block = block_of name in

    (* Escape_out should be the union of all successor's escape_in *)
    let new_out = Vec.fold_left ~f:(fun acc succ ->
      Stringset.union acc (Hashtbl.find escape_in succ)
    ) Stringset.empty block.succ in
    Hashtbl.replace escape_out name new_out;

    (* Now calculate escape_out based on it *)
    let old_in = Hashtbl.find escape_in name in
    let new_in = ref (Stringset.union new_out old_in) in

    (* Adds all variables if any one of them escapes *)
    let add vars = 
      let escaped = List.exists (fun x -> Stringset.mem x.name !new_in) vars in
      if escaped then
        new_in := Stringset.union !new_in (Stringset.of_list (List.map (fun x -> x.name) vars))
    in

    Vec.rev_iter (fun x -> match x with
    | AssignLabel { rd; _ } -> new_in += rd.name
    | Return x -> new_in += x.name

    | Call { rd; args } 
    | CallExtern { rd; args } ->
        List.iter (fun arg -> new_in += arg.name) args;
        new_in += rd.name

    | Store { rd; rs }
    | Addi { rd; rs }
    | Assign { rd; rs } ->
        add [ rd; rs ]

    | Add { rd; rs1; rs2 }
    | Sub { rd; rs1; rs2 } ->
        add [ rd; rs1; rs2 ]

    | Phi { rd; rs } ->
        let vars = List.map (fun (x, y) -> x) rs in
        add (rd :: vars)

    | _ -> ()) block.body;

    (* If anything changes, put relevant blocks back to queue *)
    if Stringset.equal old_in !new_in |> not then (
      Hashtbl.replace escape_in name !new_in;
      Vec.iter (fun x -> Vec.push worklist x) block.succ;
    )
  done;

  escape_in

(** Reforms `malloc` on heap to `alloca` on stack when possible. *)
let malloc_to_alloca fn =
  let blocks = get_blocks fn in
  let escape_data = escape_analysis fn in
  List.iter (fun name ->
    let block = block_of name in
    let body = block.body |> Vec.to_list in
    let escaped = Hashtbl.find escape_data name in
    let changed = List.map (fun x -> match x with
    | Malloc { rd; size } ->
        if Stringset.mem rd.name escaped then
          Malloc { rd; size }
        else
          Alloca { rd; size }
    | w -> w) body in
    block.body <- changed |> Vec.of_list
  ) blocks

let lower_malloc ssa =
  iter_fn malloc_to_alloca ssa