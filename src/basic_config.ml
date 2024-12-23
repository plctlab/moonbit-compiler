(*
   Copyright (C) 2024 International Digital Economy Academy.
   This program is licensed under the MoonBit Public Source
   License as published by the International Digital Economy Academy,
   either version 1 of the License, or (at your option) any later
   version. This program is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the MoonBit
   Public Source License for more details. You should have received a
   copy of the MoonBit Public Source License along with this program. If
   not, see
   <https://www.moonbitlang.com/licenses/moonbit-public-source-license-v1>.
*)


module Map_string = Basic_map_string

type target = Wasm_gc | Riscv

include struct
  let sexp_of_target target = match target with
  | Wasm_gc -> S.Atom "Wasm_gc"
  | Riscv -> S.Atom "Riscv"

  let hash_fold_target hsv arg =
    Ppx_base.hash_fold_int hsv (match arg with
    | Wasm_gc -> 1
    | Riscv -> 2)

  let hash_target arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in hash_fold_target hsv arg)

  let equal_target = (Stdlib.( = ) : target -> target -> bool)
end

type js_format = Esm | Cjs | Iife
type error_format = Human | Json

let parse_target_exn = function
  | "wasm-gc" -> Wasm_gc
  | "riscv" -> Riscv
  | other -> raise (Arg.Bad ("unsupported target: " ^ other))

let parse_error_format_exn = function
  | "human" -> Human
  | "json" -> Json
  | other -> raise (Arg.Bad ("unsupported parse error format: " ^ other))

let mi_magic_str = "MINTF230520"
let core_magic_str = "MCORE240123"
let magic_str_len = String.length mi_magic_str

let input_magic_str ic =
  try Some (Stdlib.really_input_string ic magic_str_len)
  with End_of_file -> None

let default_package_name = ""
let show_loc = ref false
let show_doc = ref true
let current_package = ref default_package_name
let current_import_map : string Basic_map_string.t ref = ref Map_string.empty
let builtin_package = "moonbitlang/core/builtin"
let debug = ref false
let use_block_params = ref true
let test_mode = ref false
let std_path = ref ""
let export_memory_name : string option ref = ref None
let import_memory_module : string option ref = ref None
let import_memory_name : string option ref = ref None
let heap_memory_start = ref 10_000
let target = ref Wasm_gc
let js_format = ref Esm
let error_format = ref Human
let leak_check = ref false
let memory_safety_check = ref false
let use_js_builtin_string = ref false
let blackbox_test_import_all = ref false
let const_string_module_name = ref "_"

let verbose = ref false

type env = Release | Debug
let env = Release
let block_line = "///|"
