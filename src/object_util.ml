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


module Type_path = Basic_type_path
module Ident = Basic_core_ident

type object_key = { trait : Type_path.t; type_ : string }

let sexp_of_object_key { trait; type_ } =
  let x = [S.List [ S.Atom "type_"; Moon_sexp_conv.sexp_of_string type_ ]] in
  let x = S.List [ S.Atom "trait"; Type_path.sexp_of_t trait ] :: x in
  S.List x

let equal_object_key a b =
      if a == b then true
      else (Type_path.equal a.trait b.trait) && (a.type_ = b.type_)

let hash_fold_object_key hsv arg =
  let hsv = Type_path.hash_fold_t hsv arg.trait in
  Ppx_base.hash_fold_string hsv arg.type_

let hash_object_key arg =
  Ppx_base.get_hash_value
    (let hsv = Ppx_base.create () in
      hash_fold_object_key hsv arg)

module Hash = Basic_hashf.Make (struct
  type t = object_key

  let sexp_of_t = sexp_of_object_key
  let equal = equal_object_key
  let hash_fold_t = hash_fold_object_key
  let hash = hash_object_key
end)

type object_method_item = {
  method_id : Ident.t;
  method_prim : Primitive.prim option;
  method_ty : Mtype.t;
}

let sexp_of_object_method_item { method_id; method_prim; method_ty; } =
  let x = [S.List [ S.Atom "method_ty"; Mtype.sexp_of_t method_ty ]] in
  let a = Moon_sexp_conv.sexp_of_option Primitive.sexp_of_prim method_prim in
  let x = S.List [ S.Atom "method_prim"; a ] :: x in
  let x = S.List [ S.Atom "method_id"; Ident.sexp_of_t method_id ] :: x in
  S.List x

let get_trait_methods ~(trait : Type_path.t) ~stype_defs =
  let trait =
    match trait with
    | Toplevel { pkg; id } ->
        let types = Basic_hash_string.find_exn stype_defs pkg in
        Typing_info.find_trait_exn types id
    | _ -> assert false
  in
  trait.closure_methods

type object_info = { self_ty : Mtype.t; methods : object_method_item list }

let sexp_of_object_info =
  fun { self_ty; methods } ->
      let a = Moon_sexp_conv.sexp_of_list sexp_of_object_method_item methods in
      let x = [S.List [ S.Atom "methods"; a ]] in
      let x = S.List [ S.Atom "self_ty"; Mtype.sexp_of_t self_ty ] :: x in
      S.List x

type t = object_info Hash.t

let sexp_of_t x = Hash.sexp_of_t sexp_of_object_info x