(** Gathers all optimizations. *)
open Riscv_opt
open Riscv_ssa

let opt ssa =
  List.iter (fun top -> match top with
  | FnDecl { fn; args } -> Hashtbl.add params fn args
  | _ -> ()) ssa;
  iter_fn2 build_cfg ssa;
  
  for i = 1 to 3 do
    Riscv_opt_inline.inline ssa;
    Riscv_opt_peephole.peephole ssa;
  done;

  let s = map_fn ssa_of_cfg ssa in
  let out = Printf.sprintf "%s.ssa" !Driver_config.Linkcore_Opt.output_file in
  Basic_io.write out (String.concat "\n" (List.map Riscv_ssa.to_string s));
  s