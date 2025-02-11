open Riscv_virtasm
module Ssa = Riscv_ssa

open Riscv_virtasm
open Riscv_ssa
open Riscv_reg
open Riscv_opt

module Vec = Basic_vec

let slot = ref 0
let vfuncs: VFunc.t Vec.t = Vec.empty ()
let vblocks: VBlock.t Vec.t = Vec.empty ()
let slot_map: (string, Slot.t) Hashtbl.t = Hashtbl.create 2048

(** Phi nodes at the beginning of each block *)
let phis: (string, Riscv_ssa.phi Vec.t) Hashtbl.t = Hashtbl.create 256

let new_slot () =
  slot := !slot + 1;
  !slot

(** Retrieve slot of the variable. When non-existent, create another one. *)
let slot_v v =
  if v.name = "_" then
    Slot.Unit
  else if Hashtbl.mem slot_map v.name then
    Hashtbl.find slot_map v.name
  else (
    let s = new_slot () in
    let new_slot =
      match v.ty with
      | T_float | T_double -> Slot.FSlot s
      | _ -> Slot s
    in
    Hashtbl.add slot_map v.name new_slot;
    new_slot
  )

(** Terminator and body are output arguments *)
let convert_single name body terminator (inst: Riscv_ssa.t) =
  let die () = failwith "riscv_virtasm_generate.ml: unsupported" in

  let rslot ({ rd; rs1; rs2 }: Riscv_ssa.r_type) =
    ({
      rd = slot_v rd;
      rs1 = slot_v rs1;
      rs2 = slot_v rs2
    }: Slots.r_slot)
  in

  match inst with
  | Label l ->
      (* Don't care about labels. *)
      (* We're converting for a single basic block, *)
      (* and labels will be handled outside. *)
      ()

  | Add ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      Vec.push body (match rd.ty with
      | T_int -> Inst.Addw r
      | T_uint -> Inst.Adduw r
      | T_int64 -> Inst.Add r
      | T_uint64 -> Inst.Addu r
      | _ -> die())

  | Sub ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      Vec.push body (match rd.ty with
      | T_int -> Inst.Subw r
      | T_uint -> Inst.Subuw r
      | T_int64 -> Inst.Sub r
      | T_uint64 -> Inst.Subu r
      | _ -> die())

  | Mul ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      (match rd.ty with
      | T_int -> Vec.push body (Inst.Mulw r)
      | T_uint -> Vec.push body (Inst.Mulw r); Vec.push body (Zextw { rd = r.rd; rs = r.rd })
      (* As only the last 64-bit is needed, we don't care about signedness *)
      | T_uint64 | T_int64 -> Vec.push body (Inst.Mul r)
      | _ -> die())

  | Div ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      Vec.push body (match rd.ty with
      | T_int -> Inst.Divw r
      | T_uint -> Inst.Divuw r
      | T_int64 -> Inst.Div r
      | T_uint64 -> Inst.Divu r
      | _ -> die())

  | Less ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      Vec.push body (match rd.ty with
      | T_int | T_int64 -> Inst.Slt r
      | T_uint | T_uint64 -> Inst.Sltu r
      | _ -> die())

  | Great { rd; rs1; rs2 } ->
      (* Note rs1 and rs2 exchanged. *)
      let r = ({
        rd = slot_v rd;
        rs1 = slot_v rs2;
        rs2 = slot_v rs1;
      }: Slots.r_slot) in

      Vec.push body (match rd.ty with
      | T_int | T_int64 -> Inst.Slt r
      | T_uint | T_uint64 -> Inst.Sltu r
      | _ -> die())

  | Leq { rd; rs1; rs2 } ->
      (* rs1 <= rs2, means !(rs2 < rs1) *)
      
      let rd_s = slot_v rd in
      let r = ({
        rd = rd_s;
        rs1 = slot_v rs2;
        rs2 = slot_v rs1;
      }: Slots.r_slot) in

      Vec.push body (match rd.ty with
      | T_int | T_int64 -> Inst.Slt r
      | T_uint | T_uint64 -> Inst.Sltu r
      | _ -> die());

      (* We negate by xoring 1 *)
      Vec.push body (Inst.Xori { rd = rd_s; rs1 = rd_s; imm = 1 })

  | Geq { rd; rs1; rs2 } ->
      (* rs1 >= rs2, means !(rs1 < rs2) *)
      
      let r = ({
        rd = slot_v rd;
        rs1 = slot_v rs1;
        rs2 = slot_v rs2;
      }: Slots.r_slot) in

      Vec.push body (match rd.ty with
      | T_int | T_int64 -> Inst.Slt r
      | T_uint | T_uint64 -> Inst.Sltu r
      | _ -> die());

      (* We negate by xoring 1 *)
      Vec.push body (Inst.Xori { rd = slot_v rd; rs1 = slot_v rd; imm = 1 })

  | Eq { rd; rs1; rs2 } ->
      Vec.push body (Inst.Xor { rd = slot_v rd; rs1 = slot_v rs1; rs2 = slot_v rs2 });
      Vec.push body (Inst.Slti { rd = slot_v rd; rs1 = slot_v rd; imm = 1 })

  | Neq { rd; rs1; rs2 } ->
      Vec.push body (Inst.Xor { rd = slot_v rd; rs1 = slot_v rs1; rs2 = slot_v rs2 });
      Vec.push body (Inst.Sltu { rd = slot_v rd; rs1 = Slot.Reg Zero; rs2 = slot_v rd })

  | Call { rd; fn; args }
  | CallExtern { rd; fn; args } ->
      let r = slot_v rd in
      let int_args = Vec.empty () in
      let fp_args = Vec.empty () in
      
      List.iter (fun x -> match x.ty with
      | T_float | T_double -> Vec.push fp_args (slot_v x)
      | _ -> Vec.push int_args (slot_v x)) args;
      
      Vec.push body (Inst.Call {
        rd = r; fn = { name = fn; stamp = 0 };
        args = Vec.to_list int_args;
        fargs = Vec.to_list fp_args
      })

  | Load { rd; rs; offset; byte; } ->
      let mem_slot: Slots.mem_slot = { rd = slot_v rd; base = slot_v rs; offset } in
      Vec.push body (match byte with
      | 1 -> Inst.Lb mem_slot
      | 2 -> Inst.Lh mem_slot
      | 4 -> Inst.Lw mem_slot
      | 8 -> Inst.Ld mem_slot
      | _ -> die ())

  | Store { rd; rs; offset; byte; } ->
      let mem_slot: Slots.mem_slot = { rd = slot_v rd; base = slot_v rs; offset } in
      Vec.push body (match byte with
      | 1 -> Inst.Sb mem_slot
      | 2 -> Inst.Sh mem_slot
      | 4 -> Inst.Sw mem_slot
      | 8 -> Inst.Sd mem_slot
      | _ -> die ())

  | AssignInt { rd; imm } ->
      Vec.push body (Inst.Li { rd = slot_v rd; imm = IntImm imm })

  | AssignInt64 { rd; imm } ->
      Vec.push body (Inst.Li { rd = slot_v rd; imm = Int64Imm imm })

  | Assign { rd; rs } ->
      Vec.push body (Inst.Mv { rd = slot_v rd; rs = slot_v rs })

  | Phi phi ->
      Vec.push (Hashtbl.find phis name) phi

  | Return ret ->
      terminator := Term.Ret (slot_v ret)

  | Branch { cond; ifso; ifnot } ->
      terminator := Term.Beq {
        rs1 = slot_v cond; rs2 = Slot.Reg Zero;
        ifso = { name = ifso; stamp = 0 };
        ifnot = { name = ifnot; stamp = 0 } }

  | Jump label ->
      terminator := Term.J { name = label; stamp = 0 }
    
  | _ -> die()

let gen_fn (f: fn) =
  let int_args = Vec.empty () in
  let fp_args = Vec.empty () in
  
  (* Split arguments into integral and FP *)
  List.iter (fun x -> match x.ty with
  | T_float | T_double -> Vec.push fp_args (slot_v x)
  | _ -> Vec.push int_args (slot_v x)) f.args;

  Vec.push vfuncs {
    funn = { name = f.fn; stamp = 0 };
    args = Vec.to_list int_args;
    fargs = Vec.to_list fp_args;
    entry = { name = f.fn; stamp = 0 }
  };

  let blocks = get_blocks f.fn in
  List.iter (fun x ->
    let block = block_of x in
    let body = Vec.empty () in
    let term = ref (Term.Ret Unit) in
    Vec.iter (convert_single x body term) block.body;
    Vec.push vblocks {
      body; term = !term; preds =
        Vec.to_list block.pred
        |> List.map (fun x -> VBlock.NormalEdge { name = x; stamp = 0 })
    }
  ) blocks;
  ()

let gen_var v = ()

let gen_extarr arr = ()

(** On calling this function, `ssa` must be coherent with basic block information in `opt` *)
let virtasm_of_ssa (ssa : Riscv_ssa.t list) =
  List.iter (fun x -> match x with
  | FnDecl f -> gen_fn f
  | GlobalVarDecl var -> gen_var var
  | ExtArray arr -> gen_extarr arr
  | _ -> failwith "riscv_virtasm_generate.ml: bad toplevel SSA") ssa;
  ({
    funcs = Label.Map.empty;
    blocks = Label.Map.empty;
    consts = Label.Map.empty;
    loop_vars = Label.Map.empty;
  }: VProg.t)

