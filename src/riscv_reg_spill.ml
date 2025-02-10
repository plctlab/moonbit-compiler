open Riscv_reg
open Riscv_virtasm
open Riscv_reg_util

(* 运算符重载： 用于便捷设置Vec的值*)
let ( .![] ) vec i = Vec.get vec i
let ( .![]<- ) vec i v = Vec.set vec i v

(* 针对于List的快捷方法: 
  1. 用于取前n个元素 
  2. 跳过前n个元素
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

(* 存放整个程序变量 *)
let vprog : VProg.t ref = ref VProg.empty

(* Liveness信息: 只需要一次计算即可 *)
let live_info : Liveness.t ref = ref Liveness.empty

(* Reverse Postorder used for Next Use Distance *)
let rpo : RPO.t ref = ref RPO.empty

(* 溢出环境: 用于构建基础环境等 *)
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

(*
   用于记录已经被spilled的块： 防止重复处理 —— 主要用于RPO不能解决的情况
*)
let spilled_visited = VBlockSet.create 128
let inserted_visited = VBlockSet.create 128

let get_spillinfo (bl : VBlockLabel.t) : SpillEnv.spill_info =
  SpillEnv.get_spillinfo !spill_env bl
;;

let update_spillinfo (bl : VBlockLabel.t) (info : SpillEnv.spill_info) : unit =
  spill_env := SpillEnv.update_spillinfo !spill_env bl info
;;

(* 使用距离增加1 *)
let incr_freq_by_one (freqs : int SlotMap.t) (slot : Slot.t) : int SlotMap.t =
  match SlotMap.find_opt freqs slot with
  | Some x -> SlotMap.add freqs slot (x + 1)
  | None -> SlotMap.add freqs slot 1
;;

(*
   按照使用距离排序
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
  (* 按 nextUse 的值排序，如果不存在则使用 max_int *)
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


(* 辅助函数： 用于在尾部插入Spill & Reload*)
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

(* 辅助函数： 用于计算程序点级别的NextUse*)
let compute_every_inst_nextuse (bl : VBlockLabel.t) : int SlotMap.t Vec.t =
  let block = VProg.get_block !vprog bl in
  let b_liveinfo = Liveness.get_liveinfo !live_info bl in
  let n = Vec.length block.body in
  let nextUse = Vec.make ~dummy:SlotMap.empty (n + 1) in
  (* 1. 初始化 *)
  SlotMap.iter b_liveinfo.exitNextUse (fun var dist ->
    nextUse.![n] <- SlotMap.add nextUse.![n] var dist);
  (* 2. 处理终结符*)
  let term = block.term in
  let term_srcs = Term.get_srcs term in
  nextUse.![n] <- reset_all_values_list nextUse.![n] term_srcs;
  (* 3. 计算nextUse -- 从后往前遍历insns *)
  Vec.rev_iteri block.body (fun i inst ->
    let srcs = Inst.get_srcs inst in
    let dests = Inst.get_dests inst in
    nextUse.![i] <- incr_all_values_by_one nextUse.![i + 1];
    nextUse.![i] <- remove_all_values_list nextUse.![i] dests;
    nextUse.![i] <- reset_all_values_list nextUse.![i] srcs);
  nextUse
;;

(* PartA: 带循环回边的初始化entryW *)
let initLoopHeader_entryW
      (bl : VBlockLabel.t)
      (loop_pred_vec : VBlockLabel.t Vec.t)
      (cands : SlotSet.t)
      (entryNextUse : int SlotMap.t)
  : unit
  =
  let b_liveinfo = Liveness.get_liveinfo !live_info bl in
  let binfo = get_spillinfo bl in
  (* TODO: 目前只支持有一个回边的循环块,之后会允许多个回边*)
  if Vec.length loop_pred_vec <> 1
  then failwith "initLoopHeader_entryW: loop_pred_vec length not equal to 1";
  let loop_pred = loop_pred_vec.![0] in
  let loop_liveinfo = Liveness.get_liveinfo !live_info loop_pred in
  (*
     这个时候需要把Slot和FSlot分开处理了
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
    (* 1. 获取活跃变量集合 alive*)
    let alive = SlotSet.union liveIn cands in
    (* 2. 尝试允许贯穿循环的变量*)
    let liveThrough = SlotSet.diff alive cands in
    if SlotSet.cardinal cands < k
    then (
      entryW := SlotSet.union !entryW cands;
      (* 计算可用于liveThrough_I的寄存器槽位数量*)
      let freeLoopSlots = k - maxPressure + SlotSet.cardinal liveThrough in
      (* TODO: 准确来说，这里应该是整个循环内部的最大压力^^^^^^^^^^^^^^*)
      if freeLoopSlots > 0
      then (
        let sorted_liveThrough = sort_by_nextUse entryNextUse liveThrough () in
        List.iter
          (fun var -> entryW := SlotSet.add !entryW var)
          (list_take freeLoopSlots sorted_liveThrough)))
    else (
      let sorted_cands = sort_by_nextUse entryNextUse cands () in
      List.iter (fun var -> entryW := SlotSet.add !entryW var) (list_take k sorted_cands));
    !entryW
  in
  let liveIn_I, liveIn_F = SlotSet.split_vars b_liveinfo.liveIn in
  let maxPressure_I, maxPressure_F =
    loop_liveinfo.maxPressure_I, loop_liveinfo.maxPressure_F
  in
  let entryNextUse_I, entryNextUse_F = SlotMap.split_vars entryNextUse in
  let k_I, k_F = Reg.k, FReg.k in
  (* A-处理整数变量*)
  let entryW_I =
    split_init_entryW entryNextUse_I liveIn_I liveIn_I cands maxPressure_I k_I
  in
  (* B-处理浮点变量*)
  let entryW_F =
    split_init_entryW entryNextUse_F liveIn_F liveIn_F cands maxPressure_F k_F
  in
  let entryW = SlotSet.union entryW_I entryW_F in
  update_spillinfo bl { binfo with entryW };
  ()
;;

(* PartB: 普通边的初始化entryW *)
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
      (* 1. 在所有前驱块中都存在 *)
      if SlotMap.find_exn freqs var = num_preds
      then (
        takes := SlotSet.add !takes var;
        removes := SlotSet.add !removes var));
    (* 2. 候选集移除已选变量 *)
    cands := SlotSet.diff !cands (SlotSet.inter !removes !cands);
    (* 3. 根据入口的next-use距离排序*)
    let sorted_cands = sort_by_nextUse entryNextUse !cands () in
    (* 4. 选择cand => take 直到 k *)
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

(*1. 初始化in_regs集合*)
let init_entryW (bl : VBlockLabel.t) (entryNextUse : int SlotMap.t) =
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
    freqs := SlotMap.add !freqs var (Vec.length normal_pred);
    (* ^^^^ 目的是让这些用于构造参数环境“天然就带进来的变量”直接拥有最高频率的属性*)
    cands := SlotSet.add !cands var);
  (* 3.按照循环与普通边的差异进行初始化 *)
  if not (Vec.is_empty loop_pred)
  then initLoopHeader_entryW bl loop_pred !loops entryNextUse
  else initUsual_entryW bl normal_pred !freqs !cands entryNextUse;
  ()
;;

(* 2. 初始化in_regs & spilled-set *)
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

(* 3. 处理前驱寄存器 -- 插入reload/spill指令 *)
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
         (* a-前驱块末尾的reload指令*)
         let need_reload = SlotSet.diff entryW pred_info.exitW in
         let reload_insts = generate_trailing_reload need_reload in
         Vec.append block.body reload_insts;
         (* b-前驱块末尾的spill指令 *)
         let need_spill = SlotSet.inter (SlotSet.diff entryS pred_info.exitS) pred_info.exitW in
         let spill_insts = generate_trailing_spill need_spill in
         Vec.append block.body spill_insts))
    preds;
  ()
;;

(* 辅助函数： 限制寄存器个数*)
let limit_func
      (nextUse : int SlotMap.t Vec.t)
      (w : SlotSet.t ref)
      (s : SlotSet.t ref)
      (spill : SlotSet.t ref)
      (protected : SlotSet.t)
      (i : int)
      (k : int)
  =
  (* a.按照nextUse排序 *)
  let sorted_w = sort_by_nextUse nextUse.![i] !w ~protected () in
  (* b.溢出多余的变量*)
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

(* TODO: ^^^^^^^^^^^^必须要确保有一个寄存器是空的，否则next insn根本无法释放到栈上*)

(* 核心算法： 主要实现*)
let apply_min_algorithm (bl : VBlockLabel.t) (nextUse : int SlotMap.t Vec.t) =
  let binfo = get_spillinfo bl in
  let block = VProg.get_block !vprog bl in
  (*1. 通过nextUse参数获得每一条指令的Next-Use Distacne*)
  (*2. 初始化 -- entryW/entryS*)
  let w_I, w_F = SlotSet.split_vars binfo.entryW in
  let s_I, s_F = SlotSet.split_vars binfo.entryS in
  let w_I = ref w_I in
  let w_F = ref w_F in
  let s_I = ref s_I in
  let s_F = ref s_F in
  (*3. 每一条指令前要插入的Reload/Spill 包含Term前，所以是n+1*)
  let body_size = Vec.length block.body in
  let addInsts = Vec.make ~dummy:(Vec.empty ()) (body_size + 1) in
  (* 4.共同的apply_inner函数 不过为了更清晰 调整k的函数是传入的*)
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
    (* 此时的protected保护的是被使用的寄存器*)
    (* a.计算需要reload的变量 *)
    SlotSet.iter !reload (fun var ->
      w := SlotSet.add !w var;
      s := SlotSet.add !s var);
    (* 调整最大可分配寄存器数量*)
    (* b.为src留下寄存器 *)
    let _ = limit_func nextUse w s spill protected i adjust_k in
    (* c.为dest留下寄存器 -- i+1: 因为当 src 写入其结果后，src 的使用已不再重要 *)
    let adjust_k = adjust_k - (SlotSet.cardinal dests) in (* 调整个数 进一步减小k*)
    (* d.将defs加入w_I *)
    SlotSet.iter dests (fun var -> w := SlotSet.add !w var);
    let protected = dests in
    (* 此时的protected保护的是定义的寄存器*)
    let _ = limit_func nextUse w s spill protected (i + 1) adjust_k in
    (* e.插入reload/spill指令*)
    SlotSet.iter !reload (fun var -> Vec.push addInsts.![i] (Inst.generate_reload var));
    (* f. 为spill_I插入spill指令 *)
    SlotSet.iter !spill (fun var -> Vec.push addInsts.![i] (Inst.generate_spill var));
    ()
  in
  (*5. 遍历指令*)
  Vec.iteri block.body (fun i inst ->
    let srcs_I, srcs_F = SlotSet.split_vars @@ SlotSet.of_list @@ Inst.get_srcs inst in
    let dests_I, dests_F = SlotSet.split_vars @@ SlotSet.of_list @@ Inst.get_dests inst in
    (* A. 处理整数变量*)
    let pre_k = Reg.k in
    let adjust_k = Inst.adjust_rec_alloc_I inst pre_k in
    let _ = apply_inner w_I s_I srcs_I dests_I i adjust_k in
    (* B. 处理浮点变量*)
    let pre_k = FReg.k in
    let adjust_k = Inst.adjust_rec_alloc_F inst pre_k in
    let _ = apply_inner w_F s_F srcs_F dests_F i adjust_k in
    ());
  (*6. 处理终结符*)
  let term = block.term in
  let srcs_I, srcs_F = SlotSet.split_vars @@ SlotSet.of_list @@ Term.get_srcs term in
  let dests_I, dests_F = SlotSet.split_vars @@ SlotSet.of_list @@ Term.get_dests term in
  (* A. 处理整数变量*)
  let pre_k = Reg.k in
  let adjust_k = Term.adjust_rec_alloc_I term pre_k in
  let _ = apply_inner w_I s_I srcs_I dests_I body_size adjust_k in
  (* B. 处理浮点变量*)
  let pre_k = FReg.k in
  let adjust_k = Term.adjust_rec_alloc_F term pre_k in
  let _ = apply_inner w_F s_F srcs_F dests_F body_size adjust_k in
  (*7. 更新block.body*)
  let new_body : Inst.t Vec.t = Vec.empty () in
  Vec.iteri block.body (fun i inst ->
    Vec.append new_body addInsts.![i];
    Vec.push new_body inst);
  Vec.append new_body addInsts.![body_size];
  (* ^^^^ 不要忘记term 的Reload*)
  (* 8. 更新 exitW/exitS/body*)
  Vec.clear block.body;
  Vec.append block.body new_body;
  let exitW = SlotSet.union !w_I !w_F in
  let exitS = SlotSet.union !s_I !s_F in
  update_spillinfo bl { binfo with exitW; exitS };
  ()
;;

(* 5. 处理前驱尚未处理的问题 -- 
  主要用于对终结指令跳转到循环头的额外处理
*)
let after_handle (bl : VBlockLabel.t) =
  let block = VProg.get_block !vprog bl in
  let next_bls = VBlock.get_successors block in
  List.iter (fun next_bl ->
      match VBlockMap.find_opt !spill_env next_bl with
      (* 再次进行处理 *)
      | Some _ -> append_spill_to_pred_block next_bl
      | None -> ()
    )
;;

(* 核心算法： 关键步骤*)
let min_algorithm (bl : VBlockLabel.t) =
  let nextUse = compute_every_inst_nextuse bl in
  let entryNextUse = nextUse.![0] in
  (* 1. 初始化in_regs集合 *)
  let _ = init_entryW bl entryNextUse in
  (* 2. 初始化in_regs & spilled-set *)
  let _ = init_entryS bl in
  (* 3. 处理前驱寄存器 -- 插入reload/spill指令 *)
  let _ = append_spill_to_pred_block bl in
  (* 4. 核心算法 *)
  let _ = apply_min_algorithm bl nextUse in
  (* 5. 处理前驱尚未处理的问题 *)
  let _ = VBlockSet.add spilled_visited bl in
  let _ = after_handle bl in
  ()
;;

(* 主函数： 处理每一个函数*)
let spill_reload_func (f_label : VFuncLabel.t) (func : VFunc.t) =
  let binfo = get_spillinfo func.entry in
  (* 提前放入entryW 创造参数环境*)
  let entryW = SlotSet.add_list binfo.entryW func.args in
  let entryW = SlotSet.add_list entryW func.fargs in
  let rpo_func = RPO.get_func_rpo f_label !rpo in
  update_spillinfo func.entry { binfo with entryW };
  List.iter min_algorithm rpo_func;
  ()
;;

(* 主函数： 用于处理整个程序*)
let spill_regs (vprog_in : VProg.t) (rpo : RPO.t) =
  vprog := vprog_in;
  live_info := Liveness.liveness_analysis !vprog rpo;
  VFuncMap.iter !vprog.funcs spill_reload_func;
  ()
;;
