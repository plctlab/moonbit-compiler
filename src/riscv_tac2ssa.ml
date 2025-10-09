(** Converts 3-address code (TAC) into SSA. *)

open Riscv_ssa
open Riscv_opt
module Vec = Basic_vec

(** Get places where a variable is defined *)
let get_defs fn =
  let blocks = get_blocks fn in
  let defs = Hashtbl.create 64 in

  List.iter (fun x ->
    List.iter (fun inst ->
      reg_iterd (fun var ->
        if not (Hashtbl.mem defs var.name) then
          Hashtbl.add defs var.name Stringset.empty;
        
        Hashtbl.replace defs var.name (Stringset.union (Stringset.singleton x) (Hashtbl.find defs var.name))
      ) inst
    ) (body_of x)
  ) blocks;

  let args = unit :: Hashtbl.find params fn in
  List.iter (fun x -> Hashtbl.add defs x.name (Stringset.singleton fn)) args;

  defs

(** Get all variables defined inside a function *)
let get_vars fn = 
  let blocks = get_blocks fn in
  let defs = Hashtbl.create 64 in

  List.iter (fun x ->
    List.iter (fun inst ->
      reg_iterd (fun var ->
        if not (Hashtbl.mem defs var.name) then
          Hashtbl.add defs var.name var.ty;
      ) inst
    ) (body_of x)
  ) blocks;

  let args = unit :: Hashtbl.find params fn in
  List.iter (fun x -> Hashtbl.add defs x.name x.ty) args;

  defs

(** Debug function *)
let output_dom doms =
  print_endline "Dominators:";
  Hashtbl.iter (fun x y ->
    Printf.printf "%s: \n" x;
    Stringset.iter (fun z -> Printf.printf "%s " z) y;
    Printf.printf "\n\n"
  ) doms

let output_idom idom =
  Hashtbl.iter (fun x y ->
    Printf.printf "idom(%s) = %s\n" x y;
  ) idom

let output_frontier frontier =
  print_endline "Frontiers:";
  Hashtbl.iter (fun x y ->
    Printf.printf "%s: \n" x;
    Stringset.iter (fun z -> Printf.printf "%s " z) y;
    Printf.printf "\n\n"
  ) frontier

(**
Calculate dominator.
Uses the classic data-flow approach, rather than the compilcated Lengauer-Tarjan algorithm.

See https://en.wikipedia.org/wiki/Dominator_(graph_theory).
*)
let dominator_analysis fn =
  let blocks = get_blocks fn in
  let non_start = List.filter (fun x -> x <> fn) blocks in
  let doms = Hashtbl.create 64 in

  (* Initialize dominators *)
  List.iter (fun x ->
    Hashtbl.add doms x (Stringset.of_list blocks)
  ) non_start;

  Hashtbl.add doms fn (Stringset.singleton fn);
  
  (* Iterate until stable *)
  let changed = ref true in
  while !changed do
    changed := false;

    List.iter (fun x ->
      let old_dom = Hashtbl.find doms x in
      let intersect =
        Vec.fold_left ~f:(fun acc pred ->
          Stringset.inter acc (Hashtbl.find doms pred)
        ) (Stringset.of_list blocks) (block_of x).pred
      in
      let dom = Stringset.union (Stringset.singleton x) intersect in
      Hashtbl.replace doms x dom;

      if dom <> old_dom then
        changed := true;
    ) non_start
  done;

  doms

(** Cauculate immediate dominator of each basic block *)
let idoms fn =
  let doms = dominator_analysis fn in

  (* Immediate dominator *)
  let idom = Hashtbl.create 64 in
  
  Hashtbl.iter (fun block dom ->
    if block <> fn then (
      let candidate = Stringset.filter (fun x -> x <> block) dom |> Stringset.to_seq |> List.of_seq in
      let immediate =
        (* Find the dominator that is dominated by all other dominators *)
        List.filter (fun x ->
          let d = Hashtbl.find doms x in
          List.for_all (fun y -> Stringset.mem y d) candidate
        ) candidate |> List.hd
      in

      Hashtbl.add idom block immediate
    )
  ) doms;
  
  idom

(**
Uses the algorithm described on Wikipedia:
https://en.wikipedia.org/wiki/Static_single-assignment_form#Computing_minimal_SSA_using_dominance_frontiers
*)
let dominance_frontier fn =
  let frontier = Hashtbl.create 64 in
  let idoms = idoms fn in
  let blocks = get_blocks fn in

  List.iter (fun x -> Hashtbl.add frontier x Stringset.empty) blocks;

  List.iter (fun x ->
    let preds = (block_of x).pred in
    if Vec.length preds >= 2 then (
      Vec.iter (fun y ->
        let runner = ref y in
        let idom = Hashtbl.find idoms x in
        while !runner <> idom do
          Hashtbl.replace frontier !runner
            (Stringset.add x (Hashtbl.find frontier !runner));
          
          runner := Hashtbl.find idoms !runner
        done
      ) preds
    )
  ) blocks;

  frontier

(** Inserts phi functions. Reference: The SSA Book, Section 3.1 *)
let insert_phi fn = 
  let df = dominance_frontier fn in
  let blocks = get_blocks fn in

  (* Get all variable names in the function *)
  let vnames = Hashtbl.create 64 in
  let add var =
    if not (Hashtbl.mem vnames var.name) then
      Hashtbl.add vnames var.name var.ty
  in

  List.iter (fun x ->
    List.iter (fun inst ->
      reg_iter add add inst
    ) (body_of x)
  ) blocks;

  let defs_data = get_defs fn in
  let defs v =
    match Hashtbl.find_opt defs_data v with
    | None -> failwith (Printf.sprintf "undefined variable %s" v)
    | Some x -> x
  in

  Hashtbl.iter (fun v ty ->
    let var = { name = v; ty } in

    (* Visited basic blocks *)
    let f = ref Stringset.empty in

    (* Basic blocks that contain definition of v *)
    let w = ref (defs v) in

    while not (Stringset.is_empty !w) do
      let hd = Stringset.min_elt !w in
      w := Stringset.remove hd !w;

      let frontier = Hashtbl.find df hd in
      Stringset.iter (fun x ->
        (* Add a phi node at the entry of x *)
        let block = block_of x in
        let body = Vec.empty () in
        Vec.push body (Phi { rd = var; rs = List.map (fun x ->
          ((if Stringset.mem x (defs var.name) then var else unit), x)
        ) (Vec.to_list block.pred) });


        Vec.append body block.body;
        block.body <- body;

        (* Mark block as visited *)
        f += x;

        (* Now the variable is also defined at x, so put it into defs *)
        if not (Stringset.mem x (defs v)) then (
          w += x;
          Hashtbl.replace defs_data v (Stringset.union (defs v) (Stringset.singleton x))
        )
      ) (Stringset.filter (fun x -> not (Stringset.mem x !f)) frontier)
    done
  ) vnames

(** Renames variables to conform SSA. Reference: The SSA Book, Section 3.1 *)
let rename fn =
  let blocks = get_blocks fn in

  let definition = Hashtbl.create 64 in
  let vars = get_vars fn in
  Hashtbl.iter (fun name ty ->
    Hashtbl.add definition name (Stack.create ())
  ) vars;

  (* Parameters are special; they are explicitly defined *)
  let params = unit :: Hashtbl.find params fn in
  List.iter (fun x ->
    Stack.push x (Hashtbl.find definition x.name)
  ) params;

  let rdef v =
    match Hashtbl.find_opt definition v with
    | None -> failwith (Printf.sprintf "unknown variable %s" v)
    | Some x -> match Stack.top_opt x with
      | None -> failwith (Printf.sprintf "variable %s with empty stack" v)
      | Some y -> y
  in

  let push_def var def =
    let stack = Hashtbl.find_opt definition var in
    match stack with
    | None ->
        failwith (Printf.sprintf "%s not found in push_def" var)

    | Some stack ->
        Stack.push def stack
  in

  let pop_def var =
    let stack = Hashtbl.find_opt definition var in
    match stack with
    | None ->
      failwith (Printf.sprintf "%s not found in pop_def" var)

    | Some stack ->
        Stack.pop stack |> ignore;
  in

  (* DFS on dominator tree, which has edges between each pair of idoms *)
  let domtree = Hashtbl.create 64 in
  let idoms = idoms fn in

  List.iter (fun x ->
    Hashtbl.add domtree x []
  ) blocks;

  Hashtbl.iter (fun x y ->
    Hashtbl.replace domtree y (x :: Hashtbl.find domtree y)
  ) idoms;

  let visited = ref Stringset.empty in

  let rec dfs name =
    if not (Stringset.mem name !visited) then (

    visited += name;
    let block = block_of name in
    let body = body_of name in

    (* Rename instructions inside this block *)
    let after = List.map (fun inst ->
      let rs_renamed =
        (match inst with
        | Phi _ -> inst
        | _ -> reg_map (fun x -> x) (fun rs -> rdef rs.name) inst)
      in
      
      let rd_renamed =
        reg_map (fun rd ->
          let fresh = new_temp rd.ty in
          push_def rd.name fresh;
          fresh
        ) (fun x -> x) rs_renamed
      in

      rd_renamed
    ) body in

    let old_body = block.body in
    block.body <- after |> Vec.of_list;

    (* Rename phi nodes for successors *)
    Vec.iter (fun x ->
      let block = block_of x in
      let body = body_of x in
      let after = List.map (fun inst ->  
        match inst with
        | Phi { rd; rs } ->
            Phi { rd; rs = 
              List.map (fun (v, label) ->
                if label = name then (rdef v.name, label) else (v, label)
              ) rs
            }

        | _ -> inst
      ) body in
      block.body <- after |> Vec.of_list
    ) block.succ;

    List.iter dfs (Hashtbl.find domtree name);

    Vec.iter (fun inst -> reg_iter (fun rd -> pop_def rd.name) (fun _rs -> ()) inst) old_body;
    ) in
  dfs fn

let ssa_of_tac tac =
  iter_fn insert_phi tac;
  iter_fn rename tac;
  map_fn ssa_of_cfg tac