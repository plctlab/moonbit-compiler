(** Gathers all optimizations. *)
open Riscv_opt
open Riscv_ssa

let opt tac =
  List.iter (fun top -> match top with
  | FnDecl { fn; args } -> Hashtbl.add params fn args
  | _ -> ()) tac;

  iter_fn2 build_cfg tac;
  let ssa = Riscv_tac2ssa.ssa_of_tac tac in
  
  for i = 1 to 3 do
    Riscv_opt_inline.inline ssa;
    Riscv_opt_peephole.peephole ssa;
    Riscv_opt_escape.lower_malloc ssa;
  done;
  
  let s = map_fn ssa_of_cfg ssa in
  let out = Printf.sprintf "%s.ssa" !Driver_config.Linkcore_Opt.output_file in
  Basic_io.write out (String.concat "\n" (List.map Riscv_ssa.to_string s));
  s