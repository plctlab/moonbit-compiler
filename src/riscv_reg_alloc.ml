open Riscv_virtasm

let reg_alloc (vprog : vprog_t) =
  let vprog : vprog_t =
    { blocks = VBlockMap.empty
    ; funcs = VFuncMap.empty
    ; consts = VSymbolMap.empty
    ; loop_vars = VBlockMap.empty
    }
  in
  vprog
;;
