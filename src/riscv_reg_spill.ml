open Riscv_reg
open Riscv_virtasm
open Riscv_reg_util

(* Operator overloading: for convenient setting of Vec values *)
let ( .![] ) vec i = Vec.get vec i

let ( .![]<- ) vec i v = Vec.set vec i v

(* Shortcut methods for List:
  1. To take the first n elements
  2. To drop the first n elements
*)
let rec list_take n lst =
  if n <= 0
  then []
  else (
    match lst with
    | [] -> []
    | x :: xs -> x :: list_take (n - 1) xs)
;;

let rec list_drop n lst =
  if n <= 0
  then lst
  else (
    match lst with
    | [] -> []
    | _ :: xs -> list_drop (n - 1) xs)
;;

(* Store the entire program variables *)
let vprog : VProg.t ref = ref VProg.empty

(* Liveness information: only needs to be computed once *)
let live_info : Liveness.t ref = ref Liveness.empty

(* Reverse Postorder used for Next Use Distance *)
let rpo : RPO.t ref = ref RPO.empty

(* Spill environment: used to build the basic environment, etc. *)
module SpillEnv = struct
  (**
  Data structure for data-flow analysis.
  Here W stands for working registers (those aren't spilled);
  S stands for spilled registers.
  *)
  type spill_info =
    { entryW : SlotSet.t
    ; exitW : SlotSet.t
    ; entryS : SlotSet.t
    ; exitS : SlotSet.t
    }

  type t = spill_info VBlockMap.t

  let empty_info : spill_info =
    { entryW = SlotSet.empty
    ; exitW = SlotSet.empty
    ; entryS = SlotSet.empty
    ; exitS = SlotSet.empty
    }
  ;;

  let empty : t = VBlockMap.empty

  let get_spillinfo (spill_env : t) (bl : VBlockLabel.t) : spill_info =
    match VBlockMap.find_opt spill_env bl with
    | Some x -> x
    | None -> empty_info
  ;;

  let update_spillinfo (spill_env : t) (bl : VBlockLabel.t) (info : spill_info) : t =
    VBlockMap.add spill_env bl info
  ;;
end

let spill_env : SpillEnv.t ref = ref SpillEnv.empty

(*
   Used to record blocks that have been spilled: to prevent duplicate processing - mainly used for situations that RPO cannot solve
*)
let spilled_visited = VBlockSet.create 128

let inserted_visited = VBlockSet.create 128

let get_spillinfo (bl : VBlockLabel.t) : SpillEnv.spill_info =
  SpillEnv.get_spillinfo !spill_env bl
;;

let update_spillinfo (bl : VBlockLabel.t) (info : SpillEnv.spill_info) : unit =
  spill_env := SpillEnv.update_spillinfo !spill_env bl info
;;

(* Increase usage distance by one *)
let incr_freq_by_one (freqs : int SlotMap.t) (slot : Slot.t) : int SlotMap.t =
  match SlotMap.find_opt freqs slot with
  | Some x -> SlotMap.add freqs slot (x + 1)
  | None -> SlotMap.add freqs slot 1
;;

(*
   Sort by next use distance
*)
let sort_by_nextUse
      (nextUse : int SlotMap.t)
      (vars : SlotSet.t)
      ?(protected : SlotSet.t = SlotSet.empty)
      ()
  : Slot.t list
  =
  let w_list = SlotSet.elements protected in
  let sort_list = SlotSet.elements (SlotSet.diff vars protected) in

  (* Sort by the value of nextUse, use max_int if not present *)
  let sorted =
    List.sort
      (fun var1 var2 ->
         let key1 =
           match SlotMap.find_opt nextUse var1 with
           | Some v -> v
           | None -> max_int
         in
         let key2 =
           match SlotMap.find_opt nextUse var2 with
           | Some v -> v
           | None -> max_int
         in
         compare key1 key2)
      sort_list
  in
  w_list @ sorted
;;

(* Helper function: used to insert Spill & Reload at the end *)
let generate_trailing_spill (need_spill : SlotSet.t) =
  let insts = Vec.empty () in
  SlotSet.iter need_spill (fun var -> Vec.push insts (Inst.generate_spill var));
  insts
;;

let generate_trailing_reload (need_reload : SlotSet.t) =
  let insts = Vec.empty () in
  SlotSet.iter need_reload (fun var -> Vec.push insts (Inst.generate_reload var));
  insts
;;

(* Helper function: used to compute NextUse at the instruction level *)
let compute_every_inst_nextuse (bl : VBlockLabel.t) : int SlotMap.t Vec.t =
  let block = VProg.get_block !vprog bl in
  let b_liveinfo = Liveness.get_liveinfo !live_info bl in
  let n = Vec.length block.body in
  let nextUse = Vec.of_list (List.init (n + 1) (fun _ -> SlotMap.empty)) in

  (* 1. Initialization *)
  SlotMap.iter b_liveinfo.exitNextUse (fun var dist ->
    nextUse.![n] <- SlotMap.add nextUse.![n] var dist);

  (* 2. Handle terminator *)
  let term = block.term in
  let term_srcs = Term.get_srcs term in
  nextUse.![n] <- reset_all_values_list nextUse.![n] term_srcs;

  (* 3. Compute nextUse -- traverse insns from back to front *)
  Vec.rev_iteri block.body (fun i inst ->
    let srcs = Inst.get_srcs inst in
    let dests = Inst.get_dests inst in
    nextUse.![i] <- incr_all_values_by_one nextUse.![i + 1];
    nextUse.![i] <- remove_all_values_list nextUse.![i] dests;
    nextUse.![i] <- reset_all_values_list nextUse.![i] srcs);
  nextUse
;;

let initLoopHeader_entryW
    (bl : VBlockLabel.t)
    (loop_pred_vec : VBlockLabel.t Vec.t)
    (cands : SlotSet.t)
    (entryNextUse : int SlotMap.t)
  : unit
  =
  (* let b_liveinfo = Liveness.get_liveinfo !live_info bl in *)
  let binfo = get_spillinfo bl in

  let loop_preds = Vec.to_list loop_pred_vec in

  let loop_liveinfos = List.map (fun lbl -> Liveness.get_liveinfo !live_info lbl) loop_preds in

  (* Merge liveIn sets *)
  let liveIn_I, liveIn_F =
    List.fold_left
      (fun (accI, accF) (liveinfo : Liveness.live_info) ->
        let li_I, li_F = SlotSet.split_vars liveinfo.liveIn in
        (SlotSet.union accI li_I, SlotSet.union accF li_F))
      (SlotSet.empty, SlotSet.empty)
      loop_liveinfos
  in

  (* Take the maximum register pressure seen on all back edge blocks *)
  let maxPressure_I =
    List.fold_left (fun acc (li : Liveness.live_info) -> max acc li.maxPressure_I) 0 loop_liveinfos
  in
  let maxPressure_F =
    List.fold_left (fun acc (li : Liveness.live_info) -> max acc li.maxPressure_F) 0 loop_liveinfos
  in

  let entryNextUse_I, entryNextUse_F = SlotMap.split_vars entryNextUse in

  let k_I, k_F = Reg.k, FReg.k in

    (*
     At this point, Slot and FSlot need to be handled separately
  *)
  let split_init_entryW
        (entryNextUse : int SlotMap.t)
        (alive : SlotSet.t)
        (liveIn : SlotSet.t)
        (cands : SlotSet.t)
        (maxPressure : int)
        (k : int)
    : SlotSet.t
    =
    let entryW = ref SlotSet.empty in
    let alive = SlotSet.union liveIn cands in
    let liveThrough = SlotSet.diff alive cands in
    if SlotSet.cardinal cands < k then (
      entryW := SlotSet.union !entryW cands;
      let freeLoopSlots = k - maxPressure + SlotSet.cardinal liveThrough in
      if freeLoopSlots > 0 then (
        let sorted_liveThrough = sort_by_nextUse entryNextUse liveThrough () in
        List.iter
          (fun var -> entryW := SlotSet.add !entryW var)
          (list_take freeLoopSlots sorted_liveThrough)
      )
    ) else (
      let sorted_cands = sort_by_nextUse entryNextUse cands () in
      List.iter (fun var -> entryW := SlotSet.add !entryW var) (list_take k sorted_cands)
    );
    !entryW
  in

  let entryW_I =
    split_init_entryW entryNextUse_I liveIn_I liveIn_I cands maxPressure_I k_I
  in

  let entryW_F =
    split_init_entryW entryNextUse_F liveIn_F liveIn_F cands maxPressure_F k_F
  in

  let entryW = SlotSet.union entryW_I entryW_F in
  update_spillinfo bl { binfo with entryW };
  ()
;;

(* PartB: Initialize entryW for normal edges *)
let initUsual_entryW
      (bl : VBlockLabel.t)
      (normal_pred : VBlockLabel.t Vec.t)
      (freqs : int SlotMap.t)
      (cands : SlotSet.t)
      (entryNextUse : int SlotMap.t)
  : unit
  =
  let binfo = get_spillinfo bl in
  let freqs_I, freqs_F = SlotMap.split_vars freqs in
  let cands_I, cands_F = SlotSet.split_vars cands in
  let entryNextUse_I, entryNextUse_F = SlotMap.split_vars entryNextUse in
  let k_I, k_F = Reg.k, FReg.k in
  let split_init_entryW
        (freqs : int SlotMap.t)
        (cands : SlotSet.t)
        (entryNextUse : int SlotMap.t)
        (k : int)
    : SlotSet.t
    =
    let cands = ref cands in
    let takes = ref SlotSet.empty in
    let removes = ref SlotSet.empty in
    let num_preds = Vec.length normal_pred in
    SlotSet.iter !cands (fun var ->

      (* 1. Exists in all predecessor blocks *)
      if SlotMap.find_exn freqs var = num_preds
      then (
        takes := SlotSet.add !takes var;
        removes := SlotSet.add !removes var));

    (* 2. Remove selected variables from candidate set *)
    cands := SlotSet.diff !cands (SlotSet.inter !removes !cands);

    (* 3. Sort by next-use distance at entry *)
    let sorted_cands = sort_by_nextUse entryNextUse !cands () in

    (* 4. Select candidates => take until k *)
    let entryW = ref !takes in
    let numSlots = k - SlotSet.cardinal !entryW in
    List.iter
      (fun var -> entryW := SlotSet.add !entryW var)
      (list_take numSlots sorted_cands);
    !entryW
  in
  let entryW_I = split_init_entryW freqs_I cands_I entryNextUse_I k_I in
  let entryW_F = split_init_entryW freqs_F cands_F entryNextUse_F k_F in
  let entryW = SlotSet.union entryW_I entryW_F in
  update_spillinfo bl { binfo with entryW };
  ()
;;

(* 1. Initialize in_regs set *)
let init_entryW (bl : VBlockLabel.t) (entryNextUse : int SlotMap.t) =
  let b_liveinfo = Liveness.get_liveinfo !live_info bl in
  let freqs = ref SlotMap.empty in
  let cands = ref SlotSet.empty in
  let loops = ref SlotSet.empty in
  let block = VProg.get_block !vprog bl in
  let binfo = get_spillinfo bl in
  let loop_pred = Vec.empty () in
  let normal_pred = Vec.empty () in

  (* 1. Compute frequency -- also check for loop edges *)
  List.iter
    (fun pred ->
       match pred with
       | VBlock.LoopBackEdge pred_bl ->

         (* Add to predecessor blocks *)
         Vec.push loop_pred pred_bl;
         let loop_slots = VProg.get_loop_vars !vprog pred_bl in

         (* Only care about the set of loop variables *)
         loops := SlotSet.union !loops loop_slots;
         ()
       | VBlock.NormalEdge pred_bl ->

         (* Add to predecessor blocks *)
         Vec.push normal_pred pred_bl;
         let pred_info = get_spillinfo pred_bl in
         SlotSet.iter pred_info.exitW (fun var ->
           match SlotMap.find_opt b_liveinfo.exitNextUse var with
           | Some dist ->
             if not_inf dist
             then (

               (* Add to candidate set cands, also compute frequency of predecessor blocks *)
               freqs := incr_freq_by_one !freqs var;
               cands := SlotSet.add !cands var)
           | None -> ());
         ())
    block.preds;

  (* 2. Handle variables naturally brought in by the parameter environment *)
  SlotSet.iter binfo.entryW (fun var ->
    freqs := SlotMap.add !freqs var (Vec.length normal_pred);
    (* ^^^^ The purpose is to give these variables naturally brought in by the parameter environment the highest frequency attribute *)
    cands := SlotSet.add !cands var);

  (* 3. Initialize according to the difference between loop and normal edges *)
  if not (Vec.is_empty loop_pred)
  then initLoopHeader_entryW bl loop_pred !loops entryNextUse
  else initUsual_entryW bl normal_pred !freqs !cands entryNextUse;
  ()
;;

(* 2. Initialize in_regs & spilled-set *)
let init_entryS (bl : VBlockLabel.t) =
  let binfo = get_spillinfo bl in
  let block = VProg.get_block !vprog bl in
  let entryS = ref SlotSet.empty in
  let preds = VBlock.get_preds block in
  List.iter
    (fun pred ->
       let pred_info = get_spillinfo pred in
       entryS := SlotSet.union !entryS pred_info.exitS)
    preds;
  entryS := SlotSet.inter !entryS binfo.entryW;
  update_spillinfo bl { binfo with entryS = !entryS };
  ()
;;

(* 3. Handle predecessor registers -- insert reload/spill instructions *)
let append_spill_to_pred_block (bl : VBlockLabel.t) =
  let binfo = get_spillinfo bl in
  let block = VProg.get_block !vprog bl in
  let preds = VBlock.get_preds block in
  let entryW = binfo.entryW in
  let entryS = binfo.entryS in
  List.iter
    (fun pred ->
       if VBlockSet.mem spilled_visited pred && not (VBlockSet.mem inserted_visited pred)
       then (
         VBlockSet.add inserted_visited pred;
         let pred_info = get_spillinfo pred in
         let block = VProg.get_block !vprog pred in

         (* a-Reload instructions at the end of the predecessor block *)
         let need_reload = SlotSet.diff entryW pred_info.exitW in
         let reload_insts = generate_trailing_reload need_reload in
         Vec.append block.body reload_insts;

         (* b-Spill instructions at the end of the predecessor block *)
         let need_spill =
           SlotSet.inter (SlotSet.diff entryS pred_info.exitS) pred_info.exitW
         in
         let spill_insts = generate_trailing_spill need_spill in
         Vec.append block.body spill_insts))
    preds;
  ()
;;

(* Helper function: limit the number of registers *)
let limit_func
      (nextUse : int SlotMap.t Vec.t)
      (w : SlotSet.t ref)
      (s : SlotSet.t ref)
      (spill : SlotSet.t ref)
      (protected : SlotSet.t)
      (i : int)
      (k : int)
  =
  (* a. Sort by nextUse *)
  let sorted_w = sort_by_nextUse nextUse.![i] !w ~protected () in

  (* b. Spill excess variables *)
  List.iter
    (fun var ->
       if
         (not (SlotSet.mem !s var))
         && SlotMap.find_default nextUse.![i] var max_int < max_int
       then spill := SlotSet.add !spill var;
       s := SlotSet.remove !s var;
       w := SlotSet.remove !w var;
       nextUse.![i] <- SlotMap.remove nextUse.![i] var)
    (list_drop k sorted_w)
;;

(* TODO: ^^^^^^^^^^^^ Must ensure that there is an empty register, otherwise the next insn cannot be spilled to the stack *)

(* Core algorithm: main implementation *)
let apply_min_algorithm (bl : VBlockLabel.t) (nextUse : int SlotMap.t Vec.t) =
  let binfo = get_spillinfo bl in
  let block = VProg.get_block !vprog bl in

  (*1. Obtain Next-Use Distance for each instruction through nextUse parameter *)
  (*2. Initialize -- entryW/entryS *)
  let w_I, w_F = SlotSet.split_vars binfo.entryW in
  let s_I, s_F = SlotSet.split_vars binfo.entryS in
  let w_I = ref w_I in
  let w_F = ref w_F in
  let s_I = ref s_I in
  let s_F = ref s_F in

  (*3. Reload/Spill to be inserted before each instruction, including before Term, so it is n+1 *)
  let body_size = Vec.length block.body in
  let addInsts = Vec.of_list (List.init (body_size + 1) (fun _ -> Vec.empty ())) in

  (* 4. Common apply_inner function, but for clarity, the function to adjust k is passed in *)
  let apply_inner
        (w : SlotSet.t ref)
        (s : SlotSet.t ref)
        (srcs : SlotSet.t)
        (dests : SlotSet.t)
        (i : int)
        (adjust_k : int)
    =
    let reload = ref (SlotSet.diff srcs !w) in
    let spill = ref SlotSet.empty in
    let protected = srcs in
    (* At this point, protected protects the registers being used *)

    (* a. Compute the variables that need to be reloaded *)
    SlotSet.iter !reload (fun var ->
      w := SlotSet.add !w var;
      s := SlotSet.add !s var);
    (* Adjust the maximum number of allocatable registers *)

    (* b. Leave registers for src *)
    let _ = limit_func nextUse w s spill protected i adjust_k in

    (* c. Leave registers for dest -- i+1: because after src writes its result, the use of src is no longer important *)
    let adjust_k = adjust_k - SlotSet.cardinal dests in
    (* Further reduce k *)

    (* d. Add defs to w_I *)
    SlotSet.iter dests (fun var -> w := SlotSet.add !w var);
    let protected = dests in
    (* At this point, protected protects the registers being defined *)
    if i <> body_size then
      limit_func nextUse w s spill protected (i + 1) adjust_k;

    (* e. Insert reload/spill instructions *)
    SlotSet.iter !reload (fun var -> Vec.push addInsts.![i] (Inst.generate_reload var));

    (* f. Insert spill instructions for spill_I *)
    SlotSet.iter !spill (fun var -> Vec.push addInsts.![i] (Inst.generate_spill var));
    ()
  in
  (*5. Traverse instructions *)
  Vec.iteri block.body (fun i inst ->
    let srcs_I, srcs_F = SlotSet.split_vars @@ SlotSet.of_list @@ Inst.get_srcs inst in
    let dests_I, dests_F = SlotSet.split_vars @@ SlotSet.of_list @@ Inst.get_dests inst in

    (* A. Handle integer variables *)
    let pre_k = Reg.k in
    let adjust_k = Inst.adjust_rec_alloc_I inst pre_k in
    let _ = apply_inner w_I s_I srcs_I dests_I i adjust_k in

    (* B. Handle floating-point variables *)
    let pre_k = FReg.k in
    let adjust_k = Inst.adjust_rec_alloc_F inst pre_k in
    let _ = apply_inner w_F s_F srcs_F dests_F i adjust_k in
    ());

  (*6. Handle terminator *)
  let term = block.term in
  let srcs_I, srcs_F = SlotSet.split_vars @@ SlotSet.of_list @@ Term.get_srcs term in
  let dests_I, dests_F = SlotSet.split_vars @@ SlotSet.of_list @@ Term.get_dests term in

  (* A. Handle integer variables *)
  let pre_k = Reg.k in
  let adjust_k = Term.adjust_rec_alloc_I term pre_k in
  let _ = apply_inner w_I s_I srcs_I dests_I body_size adjust_k in

  (* B. Handle floating-point variables *)
  let pre_k = FReg.k in
  let adjust_k = Term.adjust_rec_alloc_F term pre_k in
  let _ = apply_inner w_F s_F srcs_F dests_F body_size adjust_k in

  (*7. Update block.body *)
  let new_body : Inst.t Vec.t = Vec.empty () in
  Vec.iteri block.body (fun i inst ->
    Vec.append new_body addInsts.![i];
    Vec.push new_body inst);
  Vec.append new_body addInsts.![body_size];
  (* ^^^^ Do not forget the reload of term *)

  (* 8. Update exitW/exitS/body *)
  Vec.clear block.body;
  Vec.append block.body new_body;
  let exitW = SlotSet.union !w_I !w_F in
  let exitS = SlotSet.union !s_I !s_F in
  update_spillinfo bl { binfo with exitW; exitS };
  ()
;;

(* 5. Handle the problem of unprocessed predecessors --
  Mainly used for additional processing of terminator instructions that jump to loop headers
*)
let after_handle (bl : VBlockLabel.t) =
  let block = VProg.get_block !vprog bl in
  let next_bls = VBlock.get_successors block in
  List.iter
    (fun next_bl ->
       match VBlockMap.find_opt !spill_env next_bl with
       (* Process again *)
       | Some _ -> append_spill_to_pred_block next_bl
       | None -> ())
    next_bls
;;

(* Core algorithm: key steps *)
let min_algorithm (bl : VBlockLabel.t) =
  let nextUse = compute_every_inst_nextuse bl in
  let entryNextUse = nextUse.![0] in

  (* 1. Initialize in_regs set *)
  let _ = init_entryW bl entryNextUse in

  (* 2. Initialize in_regs & spilled-set *)
  let _ = init_entryS bl in

  (* 3. Handle predecessor registers -- insert reload/spill instructions *)
  let _ = append_spill_to_pred_block bl in

  (* 4. Core algorithm *)
  let _ = apply_min_algorithm bl nextUse in
  
  (* 5. Handle the problem of unprocessed predecessors *)
  let _ = VBlockSet.add spilled_visited bl in
  let _ = after_handle bl in
  ()
;;

(* Main function: handle each function *)
let spill_reload_func (f_label : VFuncLabel.t) (func : VFunc.t) =
  let binfo = get_spillinfo func.entry in
  (* Pre-insert entryW to create parameter environment *)
  let entryW = SlotSet.add_list binfo.entryW func.args in
  let entryW = SlotSet.add_list entryW func.fargs in
  let rpo_func = RPO.get_func_rpo f_label !rpo in
  update_spillinfo func.entry { binfo with entryW };
  List.iter min_algorithm rpo_func;
  ()
;;

(* Main function: used to handle the entire program *)
let spill_regs (vprog_in : VProg.t) (rpo_arg : RPO.t) =
  vprog := vprog_in;
  live_info := Liveness.liveness_analysis !vprog rpo_arg;
  rpo := rpo_arg;
  VFuncMap.iter !vprog.funcs spill_reload_func;
  ()
;;
