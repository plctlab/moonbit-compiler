open Riscv_virtasm
module Ssa = Riscv_ssa

let virtasm_of_ssa (ssa : Ssa.t list) =
  let vprog : vprog_t =
    { blocks = VBlockMap.empty
    ; funcs = VFuncMap.empty
    ; consts = VSymbolMap.empty
    ; loop_vars = VBlockMap.empty
    }
  in
  vprog
;;
