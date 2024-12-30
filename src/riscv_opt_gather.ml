(** Gathers all optimizations. *)
open Riscv_opt

let opt ssa =
  iter_fn2 build_cfg ssa;
  
  (* Peephole *)
  Riscv_opt_peephole.peephole ssa;

  let s = map_fn ssa_of_cfg ssa in
  let out = Printf.sprintf "%s.ssa" !Driver_config.Linkcore_Opt.output_file in
  Basic_io.write out (String.concat "\n" (List.map Riscv_ssa.to_string s));
  s