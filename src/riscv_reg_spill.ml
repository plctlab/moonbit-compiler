open Riscv_reg
open Riscv_virtasm
open Riscv_reg_util

(* 存放整个程序变量 *)
let vprog : VProg.t ref = ref VProg.empty

(* Liveness信息: 只需要一次计算即可*)
let live_info : Liveness.t ref = ref Liveness.empty

(* Reverse Postorder used for Next Use Distance*)
let rpo : RPO.t ref = ref RPO.empty

(* 溢出环境: 用于构建基础环境等*)
module SpillEnv = struct
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

let get_spillinfo (bl : VBlockLabel.t) : SpillEnv.spill_info =
  SpillEnv.get_spillinfo !spill_env bl
;;

let update_spillinfo (bl : VBlockLabel.t) (info : SpillEnv.spill_info) : unit =
  spill_env := SpillEnv.update_spillinfo !spill_env bl info
;;

let incr_freq_by_one (freqs : int SlotMap.t) (slot : Slot.t) : int SlotMap.t =
  match SlotMap.find_opt freqs slot with
  | Some x -> SlotMap.add freqs slot (x + 1)
  | None -> SlotMap.add freqs slot 1
;;

(* 带循环回边的初始化entryW*)
let initLoopHeader_entryW
      (bl : VBlockLabel.t)
      (loop_pred_vec : VBlockLabel.t Vec.t)
      (cands : SlotSet.t)
  : unit
  =
  let b_liveinfo = Liveness.get_liveinfo !live_info bl in
  let binfo = get_spillinfo bl in
  (* TODO: 目前只支持有一个回边的循环块,之后会允许多个回边*)
  if Vec.length loop_pred_vec <> 1
  then failwith "initLoopHeader_entryW: loop_pred_vec length not equal to 1";
  let loop_pred = Vec.get loop_pred_vec 0 in
  let loop_liveinfo = Liveness.get_liveinfo !live_info loop_pred in
  let entryNextUse = ref b_liveinfo.en in

  ()
;;

(* 普通边的初始化entryW*)
let initUsual_entryW
      (bl : VBlockLabel.t)
      (normal_pred : VBlockLabel.t Vec.t)
      (freqs : int SlotMap.t)
      (cands : SlotSet.t)
  : unit
  =
  ()
;;


(*1. 初始化in_regs集合*)
let init_entryW (bl : VBlockLabel.t) =
  let b_liveinfo = Liveness.get_liveinfo !live_info bl in
  let freqs = ref SlotMap.empty in
  let cands = ref SlotSet.empty in
  let loops = ref SlotSet.empty in
  let block = VProg.get_block !vprog bl in
  let binfo = get_spillinfo bl in
  let loop_pred = Vec.empty () in
  let normal_pred = Vec.empty () in
  (* 1.计算频率 -- 顺带检查是否有循环边*)
  List.iter
    (fun pred ->
       match pred with
       | VBlock.LoopBackEdge pred_bl ->
         (* 添加到前置块*)
         Vec.push loop_pred pred_bl;
         let loop_slots = VProg.get_loop_vars !vprog pred_bl in
         (* 只关心循环变量集合*)
         loops := SlotSet.union !loops loop_slots;
         ()
       | VBlock.NormalEdge pred_bl ->
         (* 添加到前置块*)
         Vec.push normal_pred pred_bl;
         let pred_info = get_spillinfo pred_bl in
         SlotSet.iter pred_info.exitW (fun var ->
           match SlotMap.find_opt b_liveinfo.exitNextUse var with
           | Some dist ->
             if not_inf dist
             then (
               (* 将普通边的放入候选集合cands中，同时计算前置块出现频率*)
               freqs := incr_freq_by_one !freqs var;
               cands := SlotSet.add !cands var)
           | None -> ());
         ())
    block.preds;
  (* 2.处理参数环境中自然带有的变量 *)
  SlotSet.iter binfo.entryW (fun var ->
    freqs := incr_freq_by_one !freqs var;
    cands := SlotSet.add !cands var);
  (* 3.按照循环与普通边的差异进行初始化 *)
  if not (Vec.is_empty loop_pred)
  then initLoopHeader_entryW bl loop_pred !loops
  else initUsual_entryW bl normal_pred !freqs !cands;
  ()
;;

(* 2. 初始化in_regs & spilled-set *)
let init_entryS (bl : VBlockLabel.t) = ()

(* 3. 处理前驱寄存器 -- 插入reload/spill指令 *)
let append_spill_to_pred_block (bl : VBlockLabel.t) = ()

(* 4. 核心算法 *)
let apply_min_algorithm (bl : VBlockLabel.t) = ()

(* 5.处理前驱尚未处理的问题 *)
let after_handle (bl : VBlockLabel.t) = ()

(* 最小算法*)
let min_algorithm (bl : VBlockLabel.t) =
  (*1. 初始化in_regs集合*)
  let _ = init_entryW bl in
  (* 2. 初始化in_regs & spilled-set *)
  let _ = init_entryS bl in
  (* 3. 处理前驱寄存器 -- 插入reload/spill指令 *)
  let _ = append_spill_to_pred_block bl in
  (* 4. 核心算法 *)
  let _ = apply_min_algorithm bl in
  (* 5.处理前驱尚未处理的问题 *)
  let _ = after_handle bl in
  ()
;;

(* 按照函数溢出 : 这里的info默认指向Spill Info*)
let spill_reload_func (f_label : VFuncLabel.t) (func : VFunc.t) =
  let binfo = get_spillinfo func.entry in
  let entryW = SlotSet.add_list binfo.entryW func.args in
  let entryW = SlotSet.add_list entryW func.fargs in
  let rpo_func = RPO.get_func_rpo f_label !rpo in
  update_spillinfo func.entry { binfo with entryW };
  List.iter min_algorithm rpo_func;
  ()
;;

(* 实际执行移除*)
let spill_regs (vprog_in : VProg.t) (rpo : RPO.t) =
  vprog := vprog_in;
  live_info := Liveness.liveness_analysis !vprog rpo;
  VFuncMap.iter !vprog.funcs spill_reload_func;
  ()
;;
