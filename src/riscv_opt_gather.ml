(** Gathers all optimizations. *)
open Riscv_opt
open Riscv_ssa

let opt tac =
  Riscv_ssa.write_to_file "-no-opt.ssa" tac;

  List.iter (fun top -> match top with
  | FnDecl { fn; args } -> Hashtbl.add params fn args
  | _ -> ()) tac;

  iter_fn2 build_cfg tac;
  let ssa = Riscv_tac2ssa.ssa_of_tac tac in

  Riscv_ssa.write_to_file "-opt0.ssa" ssa;

  for i = 1 to 3 do
    Riscv_ssa.write_to_file (Printf.sprintf "-opt%d.ssa" i) ssa;
    Riscv_opt_inline.inline ssa;
    Riscv_opt_peephole.peephole ssa;
    Riscv_opt_escape.lower_malloc ssa;
  done;
  
  let s = map_fn ssa_of_cfg ssa in
  write_to_file ".ssa" s;
  s