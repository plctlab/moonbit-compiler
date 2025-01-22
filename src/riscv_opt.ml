(** Does all sorts of optimizations. *)

open Riscv_ssa
module Vec = Basic_vec

(** Instruction in SSA form; feel free to change it to anything you'd like *)
type instruction = Riscv_ssa.t

(** Note: `body` does not include the label before instructions. *)
type basic_block = {
  mutable body: instruction Vec.t;
  succ: string Vec.t;
  mutable pred: string Vec.t;
}

let make () = 
  {
    body = Vec.empty ();
    succ = Vec.empty ();
    pred = Vec.empty ();
  }


(** We use the name of a basic block to refer to it. *)
let basic_blocks = Hashtbl.create 1024

(** The exit block(s) for each function `fn`, i.e. whose final instruction is `return`. *)
let exit_fn = Hashtbl.create 256

(** The parameters of each function. *)
let (params: (string, var list) Hashtbl.t) = Hashtbl.create 256

(** Get the basic block with label `name`. *)
let block_of name = Hashtbl.find basic_blocks name

(** Get the body of a basic block. *)
let body_of name = (block_of name).body |> Vec.to_list

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
  let vec = ref (Vec.empty ()) in

  (* There might be multiple jumps at end of each basic block. *)
  (* Clean them up. *)
  let tidy (vec: instruction Vec.t) = 
    let tidied = Vec.empty () in
    let len = Vec.length vec in
    let rec iter i =
      Vec.push tidied (Vec.get vec i);
      
      if i = len - 1 then ()

      else let x = Vec.get vec i in
      match x with
      | Jump _
      | Branch _
      | Return _ 
      | JumpIndirect _ -> ()
      | _ -> iter (i + 1)
    in
    iter 0;
    tidied
  in

  let separate_basic_block (inst: instruction) = 
    (match inst with
    | Label label ->
        Hashtbl.add basic_blocks !name (make ());
        Vec.append (block_of !name).body (tidy !vec);

        (* Clear the instructions; Vec does not offer clear() or something alike *)
        vec := Vec.empty ();
        name := label
    
    | x -> Vec.push !vec x)
  in
  List.iter separate_basic_block body;

  (* The last basic block is missed by `separate_basic_block` *)
  (* Manually add it *)
  Hashtbl.add basic_blocks !name (make ());
  Vec.append (block_of !name).body (!vec);

  Hashtbl.add exit_fn fn (Vec.empty ());

  (* Find successors of each block. *)

  (* From the generation of SSA, *)
  (* it is guaranteed that the structure of basic block is preserved; *)
  (* i.e. only the last instruction can be jump/branch/return. *)
  (* So we just look at them. *)
  let rec find_succ name =
    let block = block_of name in 
    if Vec.is_empty block.succ then
      let successors =
        (match Vec.last block.body with
        | Jump target -> [target]
        | Branch { ifso; ifnot } -> [ifso; ifnot]
        | Return _ -> Vec.push (Hashtbl.find exit_fn fn) name; []
        | JumpIndirect { possibilities; _ } -> possibilities
        | _ -> failwith "riscv_opt.ml: malformed SSA")
      in
      Vec.append block.succ (Vec.of_list successors);
      List.iter find_succ successors
  in
  find_succ fn;

  (* Find predecessors *)
  Hashtbl.iter (fun name block ->
    Vec.iter (fun succ -> Vec.push (block_of succ).pred name) block.succ
  ) basic_blocks;

  (* Deduplicate *)
  Hashtbl.iter (fun name block -> 
    block.pred <-
      block.pred |> Vec.to_list |> Stringset.of_list |> Stringset.to_seq |> List.of_seq |> Vec.of_list
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

(** Find all basic blocks in function `fn`, in depth-first order. *)
let get_blocks fn =
  let blocks = Vec.empty () in
  let visited = ref Stringset.empty in
  let rec aux x = 
    if not (Stringset.mem x !visited) then
      (Vec.push blocks x;
      visited := Stringset.add x !visited;
      Vec.iter aux (block_of x).succ)
  in
  aux fn;
  blocks |> Vec.to_list

(**
Liveness analysis.

Takes the entry block of a function, and returns a hash table:
for each basic block in this function,
this hash table gives all variables alive at the exit of it.
*)
let liveness_analysis fn =
  (* Variables alive at the beginning of basic block. *)
  let live_in = Hashtbl.create 1024 in

  (* Variables alive at the end of basic block. *)
  let live_out = Hashtbl.create 1024 in

  (* By `upward exposed`, we mean variables that are used but not defined in this block; *)
  (* they rely on blocks `upwards`, hence the name. *)
  (* See [The SSA Book](https://pfalcon.github.io/ssabook/latest/book-full.pdf), page 116. *)
  let upward_exposed = Hashtbl.create 1024 in

  (* Variables defined by phi-function. *)
  let phidefs = Hashtbl.create 1024 in

  (* Variables used by phi-function. *)
  let phiuses = Hashtbl.create 1024 in

  (* Variables defined in the basic block. *)
  let defs = Hashtbl.create 1024 in

  let blocks = get_blocks fn in

  (* Initialize live_in and live_out to empty *)
  List.iter (fun name ->
    Hashtbl.add live_in name Stringset.empty;
    Hashtbl.add live_out name Stringset.empty;
    Hashtbl.add upward_exposed name Stringset.empty
  ) blocks;

  (* Precompute upward exposed variables. *)
  List.iter (fun name ->
    let block = block_of name in

    let exposed = ref Stringset.empty in
    let defined = ref Stringset.empty in
    let phidef = ref Stringset.empty in
    let phiuse = ref Stringset.empty in

    Vec.iter (fun inst -> 
      match inst with
      | Phi { rd; rs } ->
          phidef += rd.name;
          List.iter (fun (v, _) -> phiuse += v.name) rs

      | _ ->
        Riscv_ssa.reg_iter
          (fun rd -> defined := Stringset.add rd.name !defined)
          (fun rs -> if not (Stringset.mem rs.name !defined) then
            exposed += rs.name)
        inst) block.body;

    Hashtbl.add defs name !defined;
    Hashtbl.add upward_exposed name !exposed;
    Hashtbl.add phidefs name !phidef;
    Hashtbl.add phiuses name !phiuse
  ) blocks;

  (* Keep doing until fixed point is reached *)
  let rec iterate worklist =
    let last_item = Vec.pop_opt worklist in
    match last_item with
    | None -> ()
    | Some fn ->
        List.iter (fun name ->
          let block = block_of name in
          let old_live_in = Hashtbl.find live_in name in

          (* Update live_out *)
          (* LiveOut(B) = \bigcup_{S\in succ(B)} (LiveIn(S) - PhiDefs(S)) \cup PhiUses(B) *)
          let new_live_out =
            Stringset.union (List.fold_left (fun x s ->
              Stringset.union x (Stringset.diff (Hashtbl.find live_in s) (Hashtbl.find phidefs s))
            ) Stringset.empty (Vec.to_list block.succ)) (Hashtbl.find phiuses name)
          in
          
          Hashtbl.replace live_out name new_live_out;

          (* Re-calculate live-in *)
          (* LiveIn(B) = PhiDefs(B) \cup UpwardExposed(B) \cup (LiveOut(B) - Defs(B))*)
          let new_live_in = Stringset.union
            (Stringset.union (Hashtbl.find phidefs name) (Hashtbl.find upward_exposed name))
            (Stringset.diff new_live_out (Hashtbl.find defs name)) in

          (* If live-in has changed, then all predecessors are subject to change; *)
          (* Push all of them into worklist *)
          if not (Stringset.equal old_live_in new_live_in) then
            (Hashtbl.replace live_in name new_live_in;
            Vec.append worklist block.pred)
        ) blocks;
        iterate worklist
  in
  (* Must clone a vector, otherwise `exit_fn` will become empty *)
  iterate (Hashtbl.find exit_fn fn |> Vec.to_list |> Vec.of_list);
  
  live_out

(** Converts the optimized control flow graph back into SSA. *)
let ssa_of_cfg fn = 
  let inst = Vec.empty () in
  let blocks = get_blocks fn in
  List.iter (fun x ->
    Vec.push inst (Riscv_ssa.Label x);
    Vec.append inst (block_of x).body
  ) blocks;
  inst |> Vec.to_list