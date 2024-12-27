(** Does all sorts of optimizations. *)

(** Instruction in SSA form; feel free to change it to anything you'd like *)
type instruction = Riscv_ssa.t

(** Note: `body` does not include the label before instructions. *)
type basic_block = {
  body: instruction Basic_vec.t;
  succ: string Basic_vec.t;
  pred: string Basic_vec.t;
}

let make () = 
  {
    body = Basic_vec.empty ();
    succ = Basic_vec.empty ();
    pred = Basic_vec.empty ();
  }


(** We use the name of a basic block to refer to it. *)
let basic_blocks = Hashtbl.create 1024

(** The exit block(s) for each function `fn`, i.e. whose final instruction is `return`. *)
let exit_fn = Hashtbl.create 256

(** Get the basic block with label `name`. *)
let block_of name = Hashtbl.find basic_blocks name

(**
Builds control flow graph.

Does not return anything;
stores all information in `basic_block`.
*)
let build_cfg fn body =
  (* Identify all basic blocks *)

  (* The first basic block in each function is unnamed, *)
  (* so we take the function name as its name. *)
  let name = ref fn in
  let vec = ref (Basic_vec.make ~dummy:Riscv_ssa.Nop 16) in

  (* There might be multiple jumps at end of each basic block. *)
  (* Clean them up. *)
  let tidy (vec: instruction Basic_vec.t) = 
    let rec iter () =
      let len = Basic_vec.length vec in
      if len <= 1 then ()

      (* Check penultimate instruction, and pop the last according to it *)
      else let x = Basic_vec.get vec (len - 2) in
      match x with
      | Jump _ -> Basic_vec.pop vec |> ignore; iter ()
      | Branch _ -> Basic_vec.pop vec |> ignore; iter ()
      | Return _ -> Basic_vec.pop vec |> ignore; iter ()
      | _ -> ()
    in
    iter ();
    vec
  in

  let separate_basic_block (inst: instruction) = 
    (match inst with
    | Label label ->
        Hashtbl.add basic_blocks !name (make ());
        Basic_vec.append (block_of !name).body (tidy !vec);

        (* Clear the instructions; Basic_vec does not offer clear() or something alike *)
        vec := Basic_vec.make ~dummy:Riscv_ssa.Nop 16;
        name := label
    
    | x -> Basic_vec.push !vec x)
  in
  List.iter separate_basic_block body;

  (* The last basic block is missed by `separate_basic_block` *)
  (* Manually add it *)
  Hashtbl.add basic_blocks !name (make ());
  Basic_vec.append (block_of !name).body (!vec);

  Hashtbl.add exit_fn fn (Basic_vec.empty ());

  (* Find successors of each block. *)

  (* From the generation of SSA, *)
  (* it is guaranteed that the structure of basic block is preserved; *)
  (* i.e. only the last instruction can be jump/branch/return. *)
  (* So we just look at them. *)
  let rec find_succ name =
    let block = block_of name in 
    if Basic_vec.is_empty block.succ then
      let successors =
        (match Basic_vec.last block.body with
        | Jump target -> [target]
        | Branch { ifso; ifnot } -> [ifso; ifnot]
        | Return _ -> Basic_vec.push (Hashtbl.find exit_fn fn) name; []
        | _ -> failwith "riscv_opt.ml: malformed SSA")
      in
      Basic_vec.append block.succ (Basic_vec.of_list successors);
      List.iter find_succ successors
  in
  find_succ fn;

  (* Find predecessors *)
  Hashtbl.iter (fun name block ->
    Basic_vec.iter block.succ (fun succ -> Basic_vec.push (block_of succ).pred name) 
  ) basic_blocks


(** Similar to `List.iter`, but iterates on contents of functions. *)
let iter_fn f ssa = 
  let visit (toplevel: instruction) =
    match toplevel with
    | FnDecl { fn; body; _ } -> f fn
    | _ -> ()
  in
  List.iter visit ssa

(** Similar to `iter_fn`, but gives two arguments to `f`. *)
let iter_fn2 f ssa = 
  let visit (toplevel: instruction) =
    match toplevel with
    | FnDecl { fn; body; _ } -> f fn body
    | _ -> ()
  in
  List.iter visit ssa

(** Similar to `List.map`, but maps on contents of functions. *)
let map_fn f ssa =
  let map_aux (toplevel: instruction) = 
    match toplevel with
    | FnDecl { fn; body; args; } -> Riscv_ssa.FnDecl { fn; body = f fn; args }
    | x -> x
  in
  List.map map_aux ssa

(** Sets to store live variables or basic blocks. *)
module Varset = Set.Make(String)

(** Find all basic blocks in function `fn`. *)
let get_blocks fn =
  let blocks = Basic_vec.empty () in
  let visited = ref Varset.empty in
  let rec aux x = 
    if not (Varset.mem x !visited) then
      (Basic_vec.push blocks x;
      visited := Varset.add x !visited;
      Basic_vec.iter (block_of x).succ aux)
  in
  aux fn;
  blocks |> Basic_vec.to_list

(**
Liveness analysis.

Takes the entry block of a function, and returns a hash table:
for each basic block in this function,
this hash table gives all variables alive at the exit of it.
*)
let liveness_analysis fn =
  let live_in = Hashtbl.create 1024 in
  let live_out = Hashtbl.create 1024 in
  let blocks = get_blocks fn in

  (* Initialize live_in and live_out to empty *)
  List.iter (fun name ->
    Hashtbl.add live_in name Varset.empty;
    Hashtbl.add live_out name Varset.empty
  ) blocks;

  (* Keep doing until reaches fixed point *)
  let rec iterate worklist =
    let last_item = Basic_vec.pop_opt worklist in
    match last_item with
    | None -> ()
    | Some fn -> 
    List.iter (fun name ->
      let block = block_of name in
      let old_live_in = Hashtbl.find live_in name in

      (* Update live_out *)
      (* It should be the union of live_in of all successors *)
      let new_live_out =
        List.fold_left (fun x succ_name ->
          Varset.union x (Hashtbl.find live_in succ_name)
        ) Varset.empty (Basic_vec.to_list block.succ)
      in
      
      Hashtbl.replace live_out name new_live_out;

      (* Re-calculate live-in *)
      let body = Basic_vec.to_list block.body in
      let def_var = List.concat_map Riscv_ssa.def body in
      let use_var = List.concat_map Riscv_ssa.use body in
      let def = List.map (fun (x: Riscv_ssa.var) -> x.name) def_var |> Varset.of_list in
      let use = List.map (fun (x: Riscv_ssa.var) -> x.name) use_var |> Varset.of_list in
      let new_live_in = Varset.union use (Varset.diff new_live_out def) in

      (* If live-in has changed, then all predecessors are subject to change; *)
      (* Push all of them into worklist *)
      if not (Varset.equal old_live_in new_live_in) then
        Hashtbl.replace live_in name new_live_in;
        Basic_vec.append worklist block.pred;
      
      iterate worklist
    ) blocks;
  in
  iterate (Hashtbl.find exit_fn fn);
  
  live_out

(** Converts the optimized control flow graph back into SSA. *)
let ssa_of_cfg fn = 
  let inst = Basic_vec.empty () in
  let blocks = get_blocks fn in
  List.iter (fun x ->
    Basic_vec.push inst (Riscv_ssa.Label x);
    Basic_vec.append inst (block_of x).body
  ) blocks;
  inst |> Basic_vec.to_list

let opt ssa =
  iter_fn2 build_cfg ssa;
  let s = map_fn ssa_of_cfg ssa in
  let out = Printf.sprintf "%s.ssa" !Driver_config.Linkcore_Opt.output_file in
  Basic_io.write out (String.concat "\n" (List.map Riscv_ssa.to_string s));
  s