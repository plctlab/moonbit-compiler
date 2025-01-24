open Riscv_reg
open Riscv_virtasm

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
end

(* let cal_next_use_distance (vprog: VProg.t) (rpo: RPO.t) =  *)
