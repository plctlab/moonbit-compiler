open Riscv_reg
open Riscv_virtasm
open Riscv_reg_util


let reg_alloc (vprog: VProg.t) =
  let rpo = RPO.calculate_rpo vprog in
  let liveinfo = Liveness.liveness_analysis vprog rpo in
  let spilled_vprog = Riscv_reg_spill.spill_regs vprog rpo in
  vprog
;;
