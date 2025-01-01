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


module Ident = Basic_core_ident
module Ident_set = Ident.Set

let free_vars ~(exclude : Ident.Set.t) (fn : Mcore.fn) : Mtype.t Ident.Map.t =
  let go_ident ~env ~acc (id : Ident.t) ty =
    match id with
    | Pident _ | Pmutable_ident _ ->
        if Ident_set.mem env id then acc else Ident.Map.add acc id ty
    | Pdot _ | Plocal_method _ -> acc
  in
  let rec go_func ~env ~acc (func : Mcore.fn) =
    let new_env =
      List.fold_left (fun env (p: Mcore.param) -> Ident_set.add env p.binder) env func.params
    in
    go ~env:new_env ~acc func.body
  and go ~env ~acc (expr : Mcore.expr) =
    match expr with
    | Cexpr_const _ -> acc
    | Cexpr_unit _ -> acc
    | Cexpr_var { id; ty; prim = _ } -> go_ident ~env ~acc id ty
    | Cexpr_prim { prim = _; args; ty = _ } ->
        List.fold_left (fun acc arg -> go ~env ~acc arg) acc args
    | Cexpr_let { name; rhs; body; ty = _ } ->
        let new_env = Ident_set.add env name in
        let new_acc = go ~env ~acc rhs in
        go ~env:new_env ~acc:new_acc body
    | Cexpr_letfn { name; fn; body; ty = _; kind = _ } ->
        let new_env = Ident_set.add env name in
        let new_acc = go_func ~env:new_env ~acc fn in
        go ~env:new_env ~acc:new_acc body
    | Cexpr_function { func; ty = _ } -> go_func ~env ~acc func
    | Cexpr_apply { func; args; kind; ty = _; prim = _ } ->
        let new_acc =
          match kind with
          | Normal { func_ty } -> go_ident ~env ~acc func func_ty
          | Join -> acc
        in
        List.fold_left (fun acc arg -> go ~env ~acc arg) new_acc args
    | Cexpr_object { self; methods_key = _; ty = _ } -> go ~env ~acc self
    | Cexpr_letrec { bindings; body; ty = _ } ->
        let new_env =
          List.fold_left (fun env (binder, _) -> Ident_set.add env binder) env bindings
        in
        let new_acc =
          List.fold_left (fun acc (_, fn) -> go_func ~env:new_env ~acc fn) acc bindings
        in
        go ~env:new_env ~acc:new_acc body
    | Cexpr_constr { constr = _; tag = _; args; ty = _ } ->
        List.fold_left (fun acc arg -> go ~env ~acc arg) acc args
    | Cexpr_tuple { exprs; ty = _ } ->
        List.fold_left (fun acc expr -> go ~env ~acc expr) acc exprs
    | Cexpr_record { fields; ty = _ } ->
        List.fold_left (fun acc (field: Mcore.field_def) -> go ~env ~acc field.expr) acc fields
    | Cexpr_record_update { record; fields; fields_num = _; ty = _ } ->
        let new_acc = go ~env ~acc record in
        List.fold_left (fun acc (field: Mcore.field_def) -> go ~env ~acc field.expr) new_acc fields
    | Cexpr_field { record; accessor = _; pos = _; ty = _ } ->
        go ~env ~acc record
    | Cexpr_mutate { record; label = _; field; pos = _; ty = _ } ->
        let new_acc = go ~env ~acc record in
        go ~env ~acc:new_acc field
    | Cexpr_array { exprs; ty = _ } ->
        List.fold_left (fun acc expr -> go ~env ~acc expr) acc exprs
    | Cexpr_assign { var; expr; ty = _ } ->
        let new_acc = go_ident ~env ~acc var (Mcore.type_of_expr expr) in
        go ~env ~acc:new_acc expr
    | Cexpr_sequence { expr1; expr2; ty = _ } ->
        let new_acc = go ~env ~acc expr1 in
        go ~env ~acc:new_acc expr2
    | Cexpr_if { cond; ifso; ifnot; ty = _ } -> (
        let acc1 = go ~env ~acc cond in
        let acc2 = go ~env ~acc:acc1 ifso in
        match ifnot with Some ifnot -> go ~env ~acc:acc2 ifnot | None -> acc2)
    | Cexpr_switch_constr { obj; cases; default; ty = _ } -> (
        let acc1 = go ~env ~acc obj in
        let acc2 =
          List.fold_left (fun acc (_tag, binder, case) ->
              let new_env =
                match binder with
                | None -> env
                | Some binder -> Ident_set.add env binder
              in
              go ~env:new_env ~acc case) acc1 cases
        in
        match default with
        | Some default -> go ~env ~acc:acc2 default
        | None -> acc2)
    | Cexpr_switch_constant { obj; cases; default; ty = _ } ->
        let acc1 = go ~env ~acc obj in
        let acc2 =
          List.fold_left (fun acc (_const, case) -> go ~env ~acc case) acc1 cases
        in
        go ~env ~acc:acc2 default
    | Cexpr_loop { params; body; args; label = _; ty = _ } ->
        let new_env =
          List.fold_left (fun env (p: Mcore.param) -> Ident_set.add env p.binder) env params
        in
        let new_acc =
          List.fold_left (fun acc arg -> go ~env ~acc arg) acc args
        in
        go ~env:new_env ~acc:new_acc body
    | Cexpr_break { arg; label = _; ty = _ } -> (
        match arg with Some arg -> go ~env ~acc arg | None -> acc)
    | Cexpr_continue { args; label = _; ty = _ } ->
        List.fold_left (fun acc arg -> go ~env ~acc arg) acc args
    | Cexpr_return { expr; _ } -> go ~env ~acc expr
    | Cexpr_handle_error { obj; _ } -> go ~env ~acc obj
  in
  let init_env =
    List.fold_left (fun env (p: Mcore.param) -> Ident_set.add env p.binder) exclude fn.params
  in
  go ~env:init_env ~acc:Ident.Map.empty fn.body
