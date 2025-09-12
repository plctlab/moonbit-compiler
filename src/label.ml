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

module Label = struct
  type t =
    { name : string [@ceh.ignore]
    ; stamp : int
    }

  include struct
    let sexp_of_t { name; stamp } =
      S.List [
        S.List [ S.Atom "name"; Moon_sexp_conv.sexp_of_string name ];
        S.List [ S.Atom "stamp"; Moon_sexp_conv.sexp_of_int stamp ]]

    let equal a b =
         if a == b then true
         else a.name = b.name && a.stamp = b.stamp

    let hash_fold_t hsv arg =
      let hsv = 
       Ppx_base.hash_fold_string hsv arg.name
      in Ppx_base.hash_fold_int hsv arg.stamp

    let hash arg =
      Ppx_base.get_hash_value (hash_fold_t (Ppx_base.create ()) arg)

    let compare a b =
        if a == b then 0
        else if a.name <> b.name then
          Stdlib.compare a.name b.name
        else Stdlib.compare a.stamp b.stamp
  end
end

include Label

let dummy : t = { name = ""; stamp = -1 }
let fresh name = { name; stamp = Basic_uuid.next () }
let rename t = { name = t.name; stamp = Basic_uuid.next () }

let to_wasm_name (t : t) =
  Stdlib.String.concat "" [ "$"; t.name; "/"; Int.to_string t.stamp ]
;;

let to_string t = t.name ^ "/" ^ string_of_int t.stamp
let basename t = t.name

let to_wasm_label_loop t =
  let x = t.stamp in
  ("$loop:" ^ Int.to_string x : Stdlib.String.t)
;;

let to_wasm_label_break t =
  let x = t.stamp in
  ("$break:" ^ Int.to_string x : Stdlib.String.t)
;;

(** Used for generating function label in RISCV asm. *)
let to_riscv_label_func t =
  let fun_name = t.name in
  let fun_version = t.stamp in
  ("_" ^ fun_name ^ Int.to_string fun_version : Stdlib.String.t)
;;

(** Used for generating block label in RISCV asm. *)
let to_riscv_label_block t =
  let bl_name = t.name in
  let bl_num = t.stamp in
  ("." ^ bl_name ^ Int.to_string bl_num : Stdlib.String.t)
;;

module Hash = Basic_hashf.Make (Label)
module Hashset = Basic_hashsetf.Make (Label)
module Map = Basic_mapf.Make (Label)
