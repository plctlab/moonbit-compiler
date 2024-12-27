(**
MoonBit core library relies on some internal functions.
We provide implementations here.
*)

open Mtype
open Riscv_ssa

let internals = Hashtbl.create 64

let add fn fn_decl = Hashtbl.add internals fn (fn_decl fn)

(**
Performs a byte copy from src to dst.

fn unsafe_bytes_blit (dst, dst_offset, src, src_offset, length) {
  ; copy
  add %1 dst dst_offset
  add %2 src src_offset
  call_libc %3 memcpy %1 %2 length
  return %3
}
*)
let unsafe_bytes_blit fn =
  (* Construct arguments *)
  let dst = new_temp T_bytes in
  let dst_offset = new_temp T_int in
  let src = new_temp T_bytes in
  let src_offset = new_temp T_int in
  let length = new_temp T_int in

  let args = [ dst; dst_offset; src; src_offset; length ] in

  (* Translate. For names, see the comment above *)
  let _1 = new_temp T_bytes in
  let _2 = new_temp T_bytes in
  let _3 = new_temp T_unit in
  let body = [
    Add { rd = _1; rs1 = dst; rs2 = dst_offset };
    Add { rd = _2; rs1 = src; rs2 = src_offset };
    CallExtern { rd = _3; fn = "memcpy"; args = [ _1; _2; length ] };
    Return _3;
  ] in
  (args, body)

(**
Performs a substring operation, but returns a new copy.
Note we need to store length at the beginning.

Since each string character is 2 bytes long, the string length
is actually half of the argument `length`.

fn unsafe_bytes_sub_string (src, offset, length) {
  ; allocate space
  li %1 4
  add %2 length %1
  call dst malloc %2
  
  ; divide length by 2
  li %6 1
  shr %7 length %6
  sw %7 dst

  ; copy
  add %3 dst %1
  add %4 src offset
  call %5 memcpy %3 %4 length
  return %3
}
*)
let unsafe_bytes_sub_string fn =
  (* Arguments *)
  let src = new_temp T_bytes in
  let offset = new_temp T_int in
  let length = new_temp T_int in
  let args = [ src; offset; length ] in

  (* Body *)
  let _1 = new_temp T_int in
  let _2 = new_temp T_int in
  let _3 = new_temp T_bytes in
  let _4 = new_temp T_bytes in
  let _5 = new_temp T_unit in
  let _6 = new_temp T_int in
  let _7 = new_temp T_int in
  let dst = new_temp T_bytes in
  let body = [
    (* Allocate space *)
    AssignInt { rd = _1; imm = 4L };
    Add { rd = _2; rs1 = length; rs2 = _1 };
    CallExtern { rd = dst; fn = "malloc"; args = [ _2 ] };

    (* Divide length by 2 *)
    AssignInt { rd = _6; imm = 1L };
    Shr { rd = _7; rs1 = length; rs2 = _6 };
    Store { rd = _7; rs = dst; offset = 0; byte = 4 };
    
    (* Copy *)
    Add { rd = _3; rs1 = dst; rs2 = _1 };
    Add { rd = _4; rs1 = src; rs2 = offset };
    CallExtern { rd = _5; fn = "memcpy"; args = [ _3; _4; length ] };
    Return _3;
  ] in
  (args, body)


(** Initialization of hashtable *)
let () = 
  add "$moonbit.unsafe_bytes_blit" unsafe_bytes_blit;
  add "$moonbit.unsafe_bytes_sub_string" unsafe_bytes_sub_string


let get name = Hashtbl.find internals name