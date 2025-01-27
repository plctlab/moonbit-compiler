open Riscv_reg
open Riscv_virtasm
open Riscv_reg_util


let reg_alloc (vprog: VProg.t) =
  let rpo = RPO.calculate_rpo vprog in
  let liveinfo = Liveness.liveness_analysis vprog rpo in
  vprog
;;
