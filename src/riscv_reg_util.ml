open Riscv_reg
open Riscv_virtasm

(** Helper function: checks if an int is "not infinite" *)
let not_inf (x : int) = x < max_int

(** 
  Helper function: saturating addition
  Returns x + y if both are not max_int, otherwise returns max_int 
*)
let sat_add (x : int) (y : int) = if not_inf x && not_inf y then x + y else max_int

(** 
  Helper function: increment all values in map by one (with saturation)
*)
let incr_all_values_by_one (mp : int SlotMap.t) : int SlotMap.t =
  SlotMap.fold mp SlotMap.empty (fun slot dist acc ->
    let new_dist = sat_add dist 1 in
    SlotMap.add acc slot new_dist)
;;

let remove_all_values_list (mp : int SlotMap.t) (slots : Slot.t list) : int SlotMap.t =
  List.fold_left (fun acc slot -> SlotMap.remove acc slot) mp slots 
;;

let reset_all_values_list (mp : int SlotMap.t) (slots : Slot.t list) : int SlotMap.t =
  List.fold_left (fun acc slot -> SlotMap.add acc slot 0) mp slots
;;

(** RPO, Reverse Postorder used for 
  RPO (Reverse Postorder) is an ordering of basic blocks in a control flow graph, 
  used to respect control flow during program analysis and optimization.
*)
module RPO = struct
  type t = VBlockLabel.t list VFuncMap.t

  let calculate_rpo (vprog : VProg.t) =
    let visited = VBlockSet.create 128 in
    let cal_func_rpo (funn : VFuncLabel.t) (func : VFunc.t) (acc : t) : t =
      let order = Vec.empty () in
      let rec dfs (bl : VBlockLabel.t) =
        if VBlockSet.mem visited bl
        then ()
        else (
          VBlockSet.add visited bl;
          let block = VProg.get_block vprog bl in
          let succs = VBlock.get_successors block in
          List.iter dfs succs;
          Vec.push order bl)
      in
      dfs func.entry;
      order |> Vec.to_list |> List.rev |> VFuncMap.add acc funn
    in
    VFuncMap.fold vprog.funcs VFuncMap.empty cal_func_rpo
  ;;

  let get_func_rpo (funn : VFuncLabel.t) (rpo : t) : VBlockLabel.t list =
    match VFuncMap.find_opt rpo funn with
    | Some x -> x
    | None -> failwith "RPO.get_func_rpo: function not found"
  ;;

  let empty : t = VFuncMap.empty
end

module Liveness = struct
  (** 
    For maxPressure_I / maxPressure_F,
    we count the number of Int/Float slots by checking each slot in liveIn set.
  *)
  type live_info =
    { maxPressure_I : int (* Maximum pressure of integer registers *)
    ; maxPressure_F : int (* Maximum pressure of floating-point registers *)
    ; liveIn : SlotSet.t
    ; liveOut : SlotSet.t
    ; exitNextUse : int SlotMap.t
    }

  (** 
    Liveness information for all basic blocks in the program.
    VBlockMap maps VBlockLabel.t to live_info
  *)
  type t = live_info VBlockMap.t

  (* Empty*)
  let empty_info : live_info =
    { maxPressure_I = 0
    ; maxPressure_F = 0
    ; liveIn = SlotSet.empty
    ; liveOut = SlotSet.empty
    ; exitNextUse = SlotMap.empty
    }
  ;;

  let empty : t = VBlockMap.empty

  (**
    Get live_info for a specific block.
    Returns default values or fails if block not found in map, depending on context
  *)
  let get_liveinfo (liveness : t) (bl : VBlockLabel.t) : live_info =
    match VBlockMap.find_opt liveness bl with
    | Some x -> x
    | None -> empty_info
  ;;

  (**
    Helper function: count the number of int slots and float slots in liveIn set
    Returns a tuple (int_count, float_count)
  *)
  let count_int_and_float (s : SlotSet.t) : int * int =
    SlotSet.fold s (0, 0) (fun slot (cnt_i, cnt_f) ->
      if Slot.is_int slot
      then cnt_i + 1, cnt_f
      else if Slot.is_float slot
      then cnt_i, cnt_f + 1
      else cnt_i, cnt_f)
  ;;

  (**
    Main logic: Performs liveness analysis on the entire program
    Returns a VBlockMap.t where each block label maps to its live_info
  *)
  let liveness_analysis (vprog : VProg.t) (rpo : RPO.t) : t =
    (* Reference to store analysis results for all blocks, updated in each iteration *)
    let liveness_ref : t ref = ref VBlockMap.empty in
    (* Returns current live_info for specified block (default if not exists) *)
    let get_current_info (bl : VBlockLabel.t) : live_info =
      get_liveinfo !liveness_ref bl
    in
    (* Sets or updates live_info for a block *)
    let set_current_info (bl : VBlockLabel.t) (info : live_info) =
      liveness_ref := VBlockMap.add !liveness_ref bl info
    in
    let changed = ref true in
    (* Scan all basic blocks within a function f *)
    let cal_func (f_label : VFuncLabel.t) (bls : VBlockLabel.t list) =
      let process_block (bl : VBlockLabel.t) =
        let block = VProg.get_block vprog bl in
        let old_info = get_current_info bl in
        (**********************************************************)
        (* PartA: Update block's liveOut & exitNextUse based on successors *)
        (**********************************************************)
        let b_liveOut = ref SlotSet.empty in
        let b_exitNextUse = ref SlotMap.empty in
        let successors = VBlock.get_successors block in
        List.iter
          (fun succ ->
             let succ_info = get_current_info succ in
             (* liveOut = union of successors' liveIn *)
             b_liveOut := SlotSet.union !b_liveOut succ_info.liveIn;
             (* exitNextUse = accumulate based on successors' entryNextUse *)
             SlotMap.iter succ_info.exitNextUse (fun slot dist_in_succ ->
               let new_dist = sat_add dist_in_succ 1 in
               match SlotMap.find_opt !b_exitNextUse slot with
               | Some old_d ->
                 (* if old_d is less than new_dist, keep old_d as it's better; otherwise update *)
                 if not_inf old_d && old_d <= new_dist
                 then ()
                 else b_exitNextUse := SlotMap.add !b_exitNextUse slot new_dist
               | None -> b_exitNextUse := SlotMap.add !b_exitNextUse slot new_dist))
          successors;
        (**********************************************************)
        (* PartB: Backward propagation to compute block's liveIn & entryNextUse *)
        (**********************************************************)

        (* First add all sources from terminator instruction to liveIn *)
        let b_liveIn =
          ref (SlotSet.union !b_liveOut (Term.get_srcs block.term |> SlotSet.of_list))
        in
        (* Initialize entry nextUse by copying from exitNextUse and increment all by 1 *)
        let b_entryNextUse = ref (incr_all_values_by_one !b_exitNextUse) in
        (* Set nextUse = 0 for terminator's sources *)
        let term_srcs = Term.get_srcs block.term in
        List.iter
          (fun src -> b_entryNextUse := SlotMap.add !b_entryNextUse src 0)
          term_srcs;
        (* Initialize pressure counters *)
        let maxP_I = ref 0 in
        let maxP_F = ref 0 in
        (* Update initial pressure based on current liveIn *)
        let cnt_i, cnt_f = count_int_and_float !b_liveIn in
        maxP_I := max !maxP_I cnt_i;
        maxP_F := max !maxP_F cnt_f;
        (***********************************************)
        (* Traverse normal instructions (body) in reverse order *)
        (***********************************************)
        let body_insts = VBlock.get_body_insts block in
        let reversed = List.rev body_insts in
        List.iter
          (fun inst ->
             (* Increment all nextUse distances by 1 *)
             b_entryNextUse := incr_all_values_by_one !b_entryNextUse;
             (* Get dest/src from the current instruction *)
             let dests = Inst.get_dests inst in
             let srcs = Inst.get_srcs inst in
             (* 1) remove dest from liveIn & entryNextUse *)
             List.iter
               (fun d ->
                  b_liveIn := SlotSet.remove !b_liveIn d;
                  b_entryNextUse := SlotMap.remove !b_entryNextUse d)
               dests;
             (* 2) add src to liveIn, set entryNextUse for these src to 0 *)
             List.iter
               (fun s ->
                  b_liveIn := SlotSet.add !b_liveIn s;
                  b_entryNextUse := SlotMap.add !b_entryNextUse s 0)
               srcs;
             (* 3) update maxPressure *)
             let cnt_i, cnt_f = count_int_and_float !b_liveIn in
             maxP_I := max !maxP_I cnt_i;
             maxP_F := max !maxP_F cnt_f)
          reversed;
        (**********************************************************)
        (* Record new b_liveIn, b_liveOut, b_entryNextUse, and maximum pressures *)
        (**********************************************************)
        let new_info =
          { maxPressure_I = !maxP_I
          ; maxPressure_F = !maxP_F
          ; liveIn = !b_liveIn
          ; liveOut = !b_liveOut
          ; exitNextUse = !b_exitNextUse
          }
        in
        (*
           Compare new_info with old_info for changes
          If changes detected, mark changed = true in outer scope
        *)
        let diff_liveIn = not (SlotSet.equal new_info.liveIn old_info.liveIn) in
        let diff_liveOut = not (SlotSet.equal new_info.liveOut old_info.liveOut) in
        let diff_exitNextUse =
          not (SlotMap.equal ( = ) new_info.exitNextUse old_info.exitNextUse)
        in
        let diff_pressure_I = new_info.maxPressure_I <> old_info.maxPressure_I in
        let diff_pressure_F = new_info.maxPressure_F <> old_info.maxPressure_F in
        if
          diff_liveIn
          || diff_liveOut
          || diff_exitNextUse
          || diff_pressure_I
          || diff_pressure_F
        then (
          changed := true;
          set_current_info bl new_info)
        else ()
      in
      List.iter process_block bls
    in
    (* Outer loop: continue until no changes occur *)
    while !changed do
      changed := false;
      VFuncMap.iter rpo (fun fun_lbl block_list -> cal_func fun_lbl block_list)
    done;
    (* final liveness analysis results for the entire program *)
    !liveness_ref
  ;;
end
