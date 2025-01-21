open Riscv_virtasm

module Ssa = Riscv_ssa

let virtasm_of_ssa (ssa: Ssa.t list) = ssa