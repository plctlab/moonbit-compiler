open Riscv_reg
open Riscv_virtasm
open Riscv_reg_util


let reg_alloc (vprog: VProg.t) =
  let rpo = RPO.calculate_rpo vprog in
  Riscv_reg_spill.spill_regs vprog rpo;

  let out = Printf.sprintf "%s-spilled.vasm" !Driver_config.Linkcore_Opt.output_file in
  Basic_io.write out (VProg.to_string vprog);
  vprog
