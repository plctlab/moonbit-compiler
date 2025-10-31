open Riscv_reg
open Riscv_virtasm
open Riscv_reg_util

module Spill = Riscv_reg_spill

let rpo = ref RPO.empty

let vprog = ref VProg.empty

module DisjointSet = struct
  type 'a t = ('a, ('a * int) ref) Hashtbl.t

  let create size = Hashtbl.create size

  let make_set dsu element =
    if not (Hashtbl.mem dsu element) then
      Hashtbl.add dsu element (ref (element, 0))

  let rec find dsu element =
    let parent, _ = !(Hashtbl.find dsu element) in
    if parent = element then
      Hashtbl.find dsu element
    else
      let root_ref = find dsu parent in
      Hashtbl.find dsu element := !root_ref;
      root_ref

  let union dsu elem1 elem2 =
    let root1_ref = find dsu elem1 in
    let root2_ref = find dsu elem2 in
    let (root1, rank1) = !root1_ref in
    let (root2, rank2) = !root2_ref in
    if root1 <> root2 then
      if rank1 > rank2 then
        root2_ref := (root1, rank2)
      else begin
        root1_ref := (root2, rank1);
        if rank1 = rank2 then
          root2_ref := (root2, rank2 + 1)
      end

  let are_connected dsu elem1 elem2 =
    try
      let root1, _ = !(find dsu elem1) in
      let root2, _ = !(find dsu elem2) in
      root1 = root2
    with Not_found -> false
end

module AllocEnv = struct
  (**
  Data structure for data-flow analysis.
  Here W stands for working registers (those aren't spilled);
  S stands for spilled registers.
  *)
  type alloc_info =
    { entry_map : Slot.t SlotMap.t
    ; exit_map : Slot.t SlotMap.t
    }

  type t = alloc_info VBlockMap.t

  let empty_info : alloc_info =
    { entry_map = SlotMap.empty
    ; exit_map = SlotMap.empty
    }
  ;;

  let empty : t = VBlockMap.empty

  let get_allocinfo (alloc_env : t) (bl : VBlockLabel.t) : alloc_info =
    match VBlockMap.find_opt alloc_env bl with
    | Some x -> x
    | None -> empty_info
  ;;

  let update_allocinfo (alloc_env : t) (bl : VBlockLabel.t) (info : alloc_info) : t =
    VBlockMap.add alloc_env bl info
  ;;
end

let alloc_env : AllocEnv.t ref = ref AllocEnv.empty

let available_regs = SlotSet.of_list (List.map (fun r -> Slot.Reg r) Reg.callee_saved_regs)

let get_allocinfo (bl : VBlockLabel.t) : AllocEnv.alloc_info =
  AllocEnv.get_allocinfo !alloc_env bl
;;

let update_allocinfo (bl : VBlockLabel.t) (info : AllocEnv.alloc_info) : unit =
  alloc_env := AllocEnv.update_allocinfo !alloc_env bl info
;;

let incr_freq_by_one (freqs : (int SlotMap.t) SlotMap.t) (slot : Slot.t) (reg : Slot.t) : (int SlotMap.t) SlotMap.t =
  match SlotMap.find_opt freqs slot with
  | Some mp -> 
    (match SlotMap.find_opt mp reg with
    | Some x -> SlotMap.add freqs slot (SlotMap.add mp reg (x + 1)) 
    | None -> SlotMap.add freqs slot (SlotMap.add mp reg 1))
  | None -> SlotMap.add freqs slot (SlotMap.singleton reg 1)
;;

(* Helper function : Find the register used with the maximum frequency in predecessors *)
let find_max_freq (freq_map_opt : int SlotMap.t option) : Slot.t option =
  Option.bind freq_map_opt (fun freq_map ->
    if SlotMap.is_empty freq_map then
      None
    else
      let _, max_reg =
        SlotMap.fold freq_map (0, Slot.Unit) (fun key freq (max_so_far, reg_so_far) ->
          if freq > max_so_far then
            (freq, key)
          else
            (max_so_far, reg_so_far))
      in
      Some max_reg)

(* Helper function: Choose the least-used register from available registers *)
let choose_least_used_reg (available : SlotSet.t) (usage_map : int SlotMap.t) : Slot.t =
  (* Count how many times each available register is already used *)
  let min_reg, _ =
    SlotSet.fold available (None, max_int) (fun reg (best_reg, min_count) ->
      let count = SlotMap.find_default usage_map reg 0 in
      if count < min_count then
        (Some reg, count)
      else
        (best_reg, min_count)
    )
  in
  match min_reg with
  | Some r -> r
  | None -> SlotSet.choose available  (* Fallback to arbitrary choice if empty *)

(* 1. Allocate register for the entry part.
  For each variable, simply use the most frequent register from predecessors.
  Since a block may not be the begining of the loop back edge, for loop back edge predecessors, force them to use the same register.
*)
let alloc_entry (bl : VBlockLabel.t) =
  let binfo = Spill.get_spillinfo bl in
  let rinfo = get_allocinfo bl in
  let block = VProg.get_block !vprog bl in
  let freq : (int SlotMap.t) SlotMap.t ref = ref SlotMap.empty in

  (* The block maybe a begining of a loop back edge and some of the registers may be destined *)
  let reg_map = ref rinfo.entry_map in

  (* 1. Count frequencies *)
  List.iter
    (fun pred ->
       match pred with
       | VBlock.NormalEdge pred_bl ->
        let pred_info = get_allocinfo pred_bl in
        SlotMap.iter pred_info.exit_map (fun var reg ->
          freq := incr_freq_by_one !freq var reg;
        ())
       | VBlock.LoopBackEdge _ -> ()
    ) block.preds;

  (* 2. Allocate rest variable *)
  (* Gather unallocated variables *)
  let unalloc = ref SlotSet.empty in
  SlotSet.iter binfo.entryW
    (fun var ->
      if Option.is_none @@ SlotMap.find_opt !reg_map var then
      match find_max_freq (SlotMap.find_opt !freq var) with
      | Some reg -> reg_map := SlotMap.add !reg_map var reg;
      | None -> unalloc := SlotSet.add !unalloc var;
    );
  (* Allocate them using least-used register strategy *)
  let reg_used = SlotMap.fold !reg_map SlotSet.empty (fun _ reg used -> SlotSet.add used reg) in
  let reg_left = SlotSet.diff available_regs reg_used in
  (* Build usage map: count how many times each register is already assigned *)
  let usage_count = ref SlotMap.empty in
  SlotMap.iter !reg_map (fun _ reg ->
    usage_count := SlotMap.add !usage_count reg (SlotMap.find_default !usage_count reg 0 + 1)
  );
  let _ = SlotSet.fold !unalloc reg_left
    (fun var reg_left ->
      (* Choose the least-used register from available ones *)
      let reg = choose_least_used_reg reg_left !usage_count in
      reg_map := SlotMap.add !reg_map var reg;
      (* Update usage count *)
      usage_count := SlotMap.add !usage_count reg (SlotMap.find_default !usage_count reg 0 + 1);
      SlotSet.remove reg_left reg
    ) in

  (* 3. Destine registers for loop variables *)
  List.iter
    (fun pred ->
       match pred with
       | VBlock.LoopBackEdge pred_bl ->
        let pred_alloc = get_allocinfo pred_bl in
        if pred_alloc.entry_map <> SlotMap.empty && pred_bl.name <> bl.name then failwith (Printf.sprintf "reg_alloc : Multiple Loopback edge %s" (bl.name));
        if pred_bl.name <> bl.name then(
          let pred_info = Spill.get_spillinfo pred_bl in
          let pred_map = SlotSet.fold pred_info.entryW SlotMap.empty (fun var pred_map ->
            (match SlotMap.find_opt !reg_map var with
            | Some reg -> SlotMap.add pred_map var reg
            | None -> pred_map)
          ) in
          update_allocinfo pred_bl { pred_alloc with entry_map = pred_map })
       | VBlock.NormalEdge pred_bl -> ()
    ) block.preds;
  update_allocinfo bl { rinfo with entry_map = !reg_map }

(* 2. Allocate registers for the body of the block *)
let alloc_body (bl : VBlockLabel.t) = 
  let rinfo = get_allocinfo bl in
  let block = VProg.get_block !vprog bl in
  let reg_map = ref rinfo.entry_map in
  let reg_used = SlotMap.fold !reg_map SlotSet.empty (fun _ reg used -> SlotSet.add used reg) in
  let reg_left_init = SlotSet.diff available_regs reg_used in
  let reg_left = ref reg_left_init in
  reg_map := SlotMap.add !reg_map Slot.Unit Slot.Unit;
  reg_map := SlotMap.add !reg_map (Slot.Reg Zero) (Slot.Reg Zero);
  
  (* Reproduce spill information by Reload and Spill instructions *)
  (* Allocate for new produced variables *)
  Vec.iteri block.body (fun i inst ->
    match inst with
    | Reload { origin } (*| FReload { origin }*) -> 
      let reg = SlotSet.choose !reg_left in
      reg_map := SlotMap.add !reg_map origin reg;
      reg_left := SlotSet.remove !reg_left reg;
      Vec.set block.body i (Reload { target = reg ; origin })
    | Spill { origin } (*| FSpill { origin }*) ->
      let reg = SlotMap.find_exn !reg_map origin in
      reg_map := SlotMap.remove !reg_map origin;
      reg_left := SlotSet.add !reg_left reg;
      Vec.set block.body i (Spill { target = reg ; origin })
    | _ -> 
      let dests = Inst.get_dests inst in
      List.iter (fun var -> match SlotMap.find_opt !reg_map var with
      | Some _ -> ()
      | None -> 
        let reg = SlotSet.choose !reg_left in
        reg_map := SlotMap.add !reg_map var reg;
        reg_left := SlotSet.remove !reg_left reg;
      ) dests;
      Vec.set block.body i (Inst.inst_convert inst (fun slot -> SlotMap.find_exn !reg_map slot)) 
  );

  update_allocinfo bl { rinfo with exit_map = !reg_map };
  ()

(* Record new successors for blocks with multiple successors *)
let conv_map : VBlockLabel.t VBlockMap.t VBlockMap.t ref = ref VBlockMap.empty

(* Helper function : Solve the conflict with a normal edge by generate a series of move instructions *)
(* Consider a variable in reg1 in predecessor and reg2 in current block, we need a mv reg1 reg2 for it.
  However, if reg2 is also used in predecessor, we need to make sure the mv instructions are executed in the right order.
  Build a graph for the moves, add a edge from rs to rd for each move.
  Since each register can only be used once in predecessor and current block, the graph is a collection of chains and cycles.
  For each chain, we can simply execute the moves from tail to head.
  For each cycle, we need a temporary register to break the cycle.
  Here we use the spill register as the temporary register, since it is guaranteed to be free at this point.
*)
let update_normal_pred (bl : VBlockLabel.t) (pred : VBlockLabel.t) =
  let binfo = get_allocinfo bl in
  let pinfo = get_allocinfo pred in
  let addInst = Vec.empty () in
  let values map = SlotMap.fold map [] (fun _ v acc -> v :: acc) in

  (* Series of data structures to represent the graph *)
  (* A disjoint set to represent the connected components *)
  let move_graph = DisjointSet.create 16 in
  (* A set to represent the connected components *)
  let is_circle = ref SlotSet.empty in
  (* A map to represent the tail of each connected component *)
  (* Will also record all connected nodes since a disjoint set can only represent connectivity *)
  let tail = ref SlotMap.empty in
  (* A map to represent the predecessor of each node *)
  let pred_map = ref SlotMap.empty in
  (* A function to get the representative of a connected component *)
  let get_fa = fun var -> fst !(DisjointSet.find move_graph var) in

  (* Initialize the data structures *)
  List.iter (fun var ->
    DisjointSet.make_set move_graph var;
    tail := SlotMap.add !tail var var;
  ) (List.append (values binfo.entry_map) (values pinfo.exit_map));

  (* Build the graph *)
  SlotMap.iter pinfo.exit_map (fun var preg ->
    match SlotMap.find_opt binfo.entry_map var with
    | Some reg -> if preg <> reg then 
        (if DisjointSet.are_connected move_graph preg reg then (
          (* When the edge form a circle, no more edges will be added *)
          is_circle := SlotSet.add !is_circle (get_fa preg);
          pred_map := SlotMap.add !pred_map reg preg
        ) else (
          let new_tail = SlotMap.find_exn !tail (get_fa reg) in
          tail := SlotMap.remove !tail (get_fa reg);
          tail := SlotMap.remove !tail (get_fa preg);
          DisjointSet.union move_graph preg reg;
          tail := SlotMap.add !tail (get_fa preg) new_tail;
          pred_map := SlotMap.add !pred_map reg preg;
        )
        )
    | None -> ()
  );

  (* Generate the move instructions *)
  SlotMap.iter !tail (fun fa var_tail ->
    if SlotSet.mem !is_circle fa then (
      Vec.push addInst (Inst.Mv { rd = Slot.Reg Reg.spill_reg ; rs = fa });
      let rec loop var =
        if var <> fa then (
          let next = SlotMap.find_exn !pred_map var in
          Vec.push addInst (Inst.Mv { rd = var ; rs = next });
          if next <> fa then loop next
          else Vec.push addInst (Inst.Mv { rd = next ; rs = Slot.Reg Reg.spill_reg })
        )
      in
      loop fa
    ) else (
      let rec loop var =
        match SlotMap.find_opt !pred_map var with
        | Some next ->
          Vec.push addInst (Inst.Mv { rd = var ; rs = next });
          loop next
        | None -> () in
      loop var_tail
    )
  );
  addInst

(* 3. Solve the problem of conflicts with predecessors and convert terminator *)
(* For predecessors with single successor, insert move instructions at the end of it. This may change the registers in terminator. (case 1)
  For others, if the current block has only one predecessor, insert move instructions at the begining of current block. (case 2)
  Otherwise, we have no choice but to create a new block to hold the move instructions. (case 3)
*)
let solve_edge (bl : VBlockLabel.t) =
  let block = VProg.get_block !vprog bl in
  let preds = List.map (fun pred ->
    match pred with
    | VBlock.NormalEdge pred_bl -> 
      let pred_block = VProg.get_block !vprog pred_bl in
      let pred_info = get_allocinfo pred_bl in
      let reg_map = pred_info.exit_map in
      let addInst = update_normal_pred bl pred_bl in
      let convert_term term = Term.term_map_reg term (fun slot -> SlotMap.find_exn reg_map slot) in
      if List.length @@ VBlock.get_successors pred_block = 1 then (
        (* Case 1 *)
        Vec.append pred_block.body addInst;
        (* Calculate effect of the moves *)
        let move_map = ref SlotMap.empty in
        let spill_match = ref None in
        Vec.iter (fun inst ->
          match inst with
          | Inst.Mv { rd ; rs } -> (
            if rd = Slot.Reg Reg.spill_reg then (
              match !spill_match with
              | Some _ -> failwith "reg_alloc: multiple use of spill_reg in edge"
              | None -> spill_match := Some rs
            ) else (
              if rs = Slot.Reg Reg.spill_reg then (
                match !spill_match with
                | Some reg -> move_map := SlotMap.add !move_map reg rd
                | None -> failwith "reg_alloc: use of spill_reg without match in edge"
              ) else (
                move_map := SlotMap.add !move_map rs rd
              )
            );
          )
          | _ -> failwith "reg_alloc: unexpected instruction in edge solving"
        ) addInst;
        let convert_term_moved term = Term.term_map_reg term (fun slot -> Option.value (SlotMap.find_opt !move_map slot) ~default:slot) in
        vprog := VProg.update_block !vprog pred_bl { pred_block with term = pred_block.term |> convert_term |> convert_term_moved };
        pred
      ) else (
        if List.length block.preds = 1 then (
          (* Case 2 *)
          Vec.append addInst block.body;
          Vec.clear block.body;
          Vec.append block.body addInst;
          conv_map := VBlockMap.add !conv_map pred_bl @@ VBlockMap.add (VBlockMap.find_default !conv_map pred_bl VBlockMap.empty) bl bl;
          pred
        ) else (
          (* Case 3 *)
          let new_block_label = VBlockLabel.fresh "edge" in
          let term = Term.J bl in
          let preds = [VBlock.NormalEdge pred_bl] in
          let new_block : VBlock.t = { body = addInst; term; preds } in
          conv_map := VBlockMap.add !conv_map pred_bl @@ VBlockMap.add (VBlockMap.find_default !conv_map pred_bl VBlockMap.empty) bl new_block_label;
          vprog := VProg.update_block !vprog new_block_label new_block;
          VBlock.NormalEdge new_block_label
        )
      )
    | VBlock.LoopBackEdge _ -> pred
  ) block.preds in
  vprog := VProg.update_block !vprog bl { block with preds };
  ()

(* Main function: used to allocate register for a block and solve its conflict with predecessors *)
let alloc_block (bl : VBlockLabel.t) =
  (* 1. Allocate register for the entry part.*)
  alloc_entry bl;

  (* 2. Allocate registers for the body of the block *)
  alloc_body bl;

  (* 3. Solve the problem of conflicts with predecessors and convert terminator *)
  solve_edge bl

(* 4. Handle the problem of unprocessed successors --
  Since the algorithm runs in a rpo order, terminators of blocks with multiple successors will not be deal at once
*)
let update_multisucc_term (bl : VBlockLabel.t) =
  let block = VProg.get_block !vprog bl in
  let binfo = get_allocinfo bl in
  match VBlockMap.find_opt !conv_map bl with
  | None -> ()
  | Some block_map ->
    let convert_term_label term = Term.term_map_label term (fun label -> VBlockMap.find_exn block_map label) in
    let reg_map = binfo.exit_map in
    let convert_term term = Term.term_map_reg term (fun slot -> SlotMap.find_exn reg_map slot) in
    let new_term = block.term |> convert_term_label |> convert_term in
    vprog := VProg.update_block !vprog bl { block with term = new_term };
    ()

(* Main function: used to handle the entire program *)
(* TODO : currently no FP support *)
let alloc_func (f_label : VFuncLabel.t) (func : VFunc.t) =
  let rpo_func = RPO.get_func_rpo f_label !rpo in
  List.iter alloc_block rpo_func;

  List.iter update_multisucc_term rpo_func;
  ()

let reg_alloc (vprog_arg: VProg.t) =
  rpo := RPO.calculate_rpo vprog_arg;
  (* A. Spill pass. Decide which registers to spill *)
  Spill.spill_regs vprog_arg !rpo;

  vprog := vprog_arg;

  let out = Printf.sprintf "%s-spilled.vasm" !Driver_config.Linkcore_Opt.output_file in
  Basic_io.write out (VProg.to_string !vprog);

  (* B. Allocate registers *)
  VFuncMap.iter !vprog.funcs alloc_func;
  let out = Printf.sprintf "%s-allocated.vasm" !Driver_config.Linkcore_Opt.output_file in
  Basic_io.write out (VProg.to_string !vprog);

  (* TODO : Optimization *)
  !vprog
