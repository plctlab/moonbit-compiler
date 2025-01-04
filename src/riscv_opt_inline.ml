(**
Inlines a function.

Current inline condition:
  1. The function has only one basic block;
  2. The function is a leaf (i.e. not calling other functions)
  3. The function contains no more than 16 instructions
*)

open Riscv_ssa
open Riscv_opt

(**
Whether each function can get inlined.
This cache is to avoid repeatedly scanning through the same function body.
*)
let inline_cache = Hashtbl.create 64

let is_single fn = 
  List.length (get_blocks fn) = 1

let is_leaf fn =
  let blocks = get_blocks fn in

  let has_call = List.exists (fun block -> 
    let body = body_of block in
    
    List.exists (fun x -> match x with
    | Call _ 
    | CallIndirect _
    | CallExtern _ -> true

    | _ -> false) body
  ) blocks in

  not has_call

let can_inline fn =
  if Hashtbl.mem inline_cache fn then
    Hashtbl.find inline_cache fn
  else
    let inlineable = is_leaf fn && is_single fn && (List.length (body_of fn) <= 16) in
    Hashtbl.add inline_cache fn inlineable;
    inlineable

(** Scan through body of `fn` and see if any function call can be inlined. *)
let do_inline fn = 
  let blocks = get_blocks fn in
  List.iter (fun block ->
    let body = body_of block in
    let new_body = Basic_vec.empty () in

    List.iter (fun x -> match x with
    | Call { rd; fn; args } ->
        (* Check if the function is inlineable *)
        let able = can_inline fn in
        if able then (
          (* As `fn` is a single block, the content of it is the whole function body *)
          let fn_body = body_of fn in
          
          (* Record the correspondence between arguments and parameters *)
          let subst = Hashtbl.create 32 in
          List.iter2 (fun arg param ->
            Hashtbl.add subst param.name arg
          ) args (Hashtbl.find params fn);

          (* Begin substitution *)
          let replace var =
            if Hashtbl.mem subst var.name then Hashtbl.find subst var.name
            else (
              (* Construct a new correspondence, *)
              (* in order to fix the case where the same function is inlined twice *)
              let tmp = new_temp var.ty in
              Hashtbl.add subst var.name tmp;
              tmp
            )
          in
          let fn_new = List.map (fun x -> reg_map replace replace x) fn_body in

          (* Convert `return` to an assignment operation *)
          let fn_core = List.map
            (fun x -> match x with
            | Return return -> Assign { rd; rs = return }
            | x -> x) fn_new
          in

          (* Add it to the original function body *)
          Basic_vec.append new_body (Basic_vec.of_list fn_core))
        else Basic_vec.push new_body x

    | x -> Basic_vec.push new_body x) body;

    (block_of block).body <- new_body
  ) blocks

let inline ssa = 
  iter_fn do_inline ssa