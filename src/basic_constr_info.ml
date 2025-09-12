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


module Index_set = Basic_diet.Make (Int)

type constr_repr = Non_constant | Constant | Integer of int

type constr_tag =
  | Constr_tag_regular of {
      total : Index_set.t; [@ceh.ignore]
      index : int;
      repr_ : constr_repr; [@ceh.ignore]
      name_ : string; [@ceh.ignore]
    }
  | Extensible_tag of {
      pkg : string;
      type_name : string;
      name : string;
      total : Index_set.t; [@ceh.ignore]
      index : int; [@ceh.ignore]
    }

include struct
  let compare_constr_tag a b = 
    if a == b then 0
    else
      match (a, b) with
      | Constr_tag_regular a1, Constr_tag_regular b1 ->
          Stdlib.compare (a1.index : int) b1.index
      | Constr_tag_regular _, _ -> -1
      | _, Constr_tag_regular _ -> 1
      | Extensible_tag a1, Extensible_tag b1 -> (
        match Stdlib.compare a1.pkg b1.pkg with
        | 0 -> (
          match Stdlib.compare a1.type_name b1.type_name with
          | 0 -> Stdlib.compare a1.name b1.name
          | n -> n)
        | n -> n)

  let equal_constr_tag a b =
    if a == b then true
    else
      match (a, b) with
      | Constr_tag_regular a1, Constr_tag_regular b1 ->
          a1.index = b1.index
      | Constr_tag_regular _, _ -> false
      | _, Constr_tag_regular _ -> false
      | Extensible_tag a1, Extensible_tag b1 ->
        (a1.pkg = b1.pkg) && 
        (a1.type_name = b1.type_name) && 
        (a1.name = b1.name)


  let hash_fold_constr_tag hsv (arg: constr_tag) =
    match arg with
    | Constr_tag_regular _ir ->
      let hsv = Ppx_base.hash_fold_int hsv 0 in
      let hsv = Ppx_base.hash_fold_int hsv _ir.index
      in hsv
    | Extensible_tag _ir ->
      let hsv = Ppx_base.hash_fold_int hsv 1 in
      let hsv = Ppx_base.hash_fold_string hsv _ir.pkg in
      let hsv = Ppx_base.hash_fold_string hsv _ir.type_name in
      let hsv = Ppx_base.hash_fold_string hsv _ir.name
      in hsv

  let hash_constr_tag arg =
      Ppx_base.get_hash_value
        (let hsv = Ppx_base.create () in
         hash_fold_constr_tag hsv arg)

end

let sexp_of_constr_tag (tag : constr_tag) =
  match tag with
  | Constr_tag_regular { name_; total = _; index = _; repr_ = _ } ->
      (List
         (List.cons
            (Atom "Constr_tag_regular" : S.t)
            ([ Atom name_ ] : S.t list))
        : S.t)
  | Extensible_tag { pkg; type_name; name; total = _; index = _ } ->
      (List
         (List.cons
            (Atom "Extensible_tag" : S.t)
            (List.cons
               (Atom pkg : S.t)
               (List.cons (Atom type_name : S.t) ([ Atom name ] : S.t list))))
        : S.t)

let equal = equal_constr_tag

let ext_tag_to_str ~(pkg : string) ~(type_name : string) ~(name : string) =
  if pkg = "" then (type_name ^ "." ^ name : Stdlib.String.t)
  else Stdlib.String.concat "" [ pkg; "."; type_name; "."; name ]

let get_name tag =
  match tag with
  | Constr_tag_regular { name_ = name; _ } | Extensible_tag { name; _ } -> name
