open Riscv_virtasm
module Ssa = Riscv_ssa

open Riscv_virtasm
open Riscv_ssa
open Riscv_reg
open Riscv_opt

module Vec = Basic_vec

let slot = ref 0
let vfuncs: VFunc.t Vec.t = Vec.empty ()
let vblocks: (string, VBlock.t) Hashtbl.t = Hashtbl.create 512
let slot_map: (string, Slot.t) Hashtbl.t = Hashtbl.create 2048

(* We don't need to use stamp. *)
(* The name is already unique from SSA generation. *)
let label_of fn = ({ name = fn; stamp = 0 }: Label.t)

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
  let die msg = failwith (Printf.sprintf "riscv_virtasm_generate.ml: %s" msg) in

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

  (* Add and sub cares also for pointers, so they default to i64. *)
  (* Mul and div don't have these, so they explicitly fails if types are incorrect. *)
  | Add ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      (match rd.ty with
      | T_int -> Vec.push body (Inst.Addw r)
      | T_uint -> Vec.push body (Inst.Addw r); Vec.push body (Zextw { rd = r.rd; rs = r.rd })
      | _ -> Vec.push body (Inst.Add r))

  | Addi { rd; rs; imm } ->
      let r = ({ rd = slot_v rd; rs1 = slot_v rs; imm }: Slots.i_slot) in
      (match rd.ty with
      | T_int -> Vec.push body (Inst.Addiw r)
      | T_uint -> Vec.push body (Inst.Addiw r); Vec.push body (Zextw { rd = r.rd; rs = r.rd })
      | _ -> Vec.push body (Inst.Addi r))

  | Sub ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      (match rd.ty with
      | T_int -> Vec.push body (Inst.Subw r)
      | T_uint -> Vec.push body (Inst.Subw r); Vec.push body (Zextw { rd = r.rd; rs = r.rd })
      | _ -> Vec.push body (Inst.Sub r))

  | Mul ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      (match rd.ty with
      | T_int -> Vec.push body (Inst.Mulw r)
      | T_uint -> Vec.push body (Inst.Mulw r); Vec.push body (Zextw { rd = r.rd; rs = r.rd })
      (* As only the last 64-bit is needed, we don't care about signedness *)
      | T_uint64 | T_int64 -> Vec.push body (Inst.Mul r)
      | _ -> die (Printf.sprintf "mul: unexpected type %s" (Mtype.to_string rd.ty)))

  | Div ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      Vec.push body (match rd.ty with
      | T_int -> Inst.Divw r
      | T_uint -> Inst.Divuw r
      | T_int64 -> Inst.Div r
      | T_uint64 -> Inst.Divu r
      | _ -> die (Printf.sprintf "div: unexpected type %s" (Mtype.to_string rd.ty)))

  | Mod ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      Vec.push body (match rd.ty with
      | T_int -> Inst.Remw r
      | T_uint -> Inst.Remuw r
      | T_int64 -> Inst.Rem r
      | T_uint64 -> Inst.Remu r
      | _ -> die (Printf.sprintf "rem: unexpected type %s" (Mtype.to_string rd.ty)))

  (* Arithmetic; assumes rs1 is signed *)
  | Sra ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      (* Note we use rs1 type in leftshift *)
      (match rs1.ty with
      | T_int -> Vec.push body (Inst.Sraw r)
      | T_int64 -> Vec.push body (Inst.Sra r)
      | _ -> die (Printf.sprintf "sra: unexpected type %s" (Mtype.to_string rs1.ty)))

  (* Logical; assumes rs1 is unsigned *)
  | Srl ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      (* Note we use rs1 type in leftshift *)
      (match rs1.ty with
      | T_uint -> Vec.push body (Inst.Srlw r)
      | T_uint64 -> Vec.push body (Inst.Srl r)
      | _ -> die (Printf.sprintf "srl: unexpected type %s" (Mtype.to_string rs1.ty)))

  | Srai ({ rd; rs; imm }) ->
      let r = ({ rd = slot_v rd; rs1 = slot_v rs; imm } : Slots.i_slot) in
      (match rs.ty with
      | T_int -> Vec.push body (Inst.Sraiw r)
      | T_int64 -> Vec.push body (Inst.Srai r)
      | _ -> die (Printf.sprintf "srai: unexpected type %s" (Mtype.to_string rs.ty)))
  
  | Srli ({ rd; rs; imm }) ->
      let r = ({ rd = slot_v rd; rs1 = slot_v rs; imm } : Slots.i_slot) in
      (match rs.ty with
      | T_uint -> Vec.push body (Inst.Sraiw r)
      | T_uint64 -> Vec.push body (Inst.Srai r)
      | _ -> die (Printf.sprintf "srai: unexpected type %s" (Mtype.to_string rs.ty)))
    
  | And ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      Vec.push body (Inst.And r)

  | Andi { rd; rs; imm } ->
      let r = ({ rd = slot_v rd; rs1 = slot_v rs; imm } : Slots.i_slot) in
      Vec.push body (Inst.Andi r)

  | Or ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      Vec.push body (Inst.Or r)

  | Ori { rd; rs; imm } ->
      let r = ({ rd = slot_v rd; rs1 = slot_v rs; imm } : Slots.i_slot) in
      Vec.push body (Inst.Ori r)

  | Xor ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      Vec.push body (Inst.Xor r)

  | Xori { rd; rs; imm } ->
      let r = ({ rd = slot_v rd; rs1 = slot_v rs; imm } : Slots.i_slot) in
      Vec.push body (Inst.Xori r)

  | Less ({ rd; rs1; rs2 } as x) ->
      let r = rslot x in
      Vec.push body (match rs1.ty with
      | T_int | T_int64 -> Inst.Slt r
      | T_uint | T_uint64 -> Inst.Sltu r
      | _ -> die (Printf.sprintf "less: unexpected type %s" (Mtype.to_string rs1.ty)))

  | Great { rd; rs1; rs2 } ->
      (* Note rs1 and rs2 exchanged. *)
      let r = ({
        rd = slot_v rd;
        rs1 = slot_v rs2;
        rs2 = slot_v rs1;
      }: Slots.r_slot) in

      Vec.push body (match rs1.ty with
      | T_int | T_int64 | T_bool -> Inst.Slt r
      | T_uint | T_uint64 -> Inst.Sltu r
      | _ -> die (Printf.sprintf "great: unexpected type %s" (Mtype.to_string rs1.ty)))

  | Leq { rd; rs1; rs2 } ->
      (* rs1 <= rs2, means !(rs2 < rs1) *)
      
      let rd_s = slot_v rd in
      let r = ({
        rd = rd_s;
        rs1 = slot_v rs2;
        rs2 = slot_v rs1;
      }: Slots.r_slot) in

      Vec.push body (match rs1.ty with
      | T_int | T_int64 -> Inst.Slt r
      | T_uint | T_uint64 -> Inst.Sltu r
      | _ -> die (Printf.sprintf "leq: unexpected type %s" (Mtype.to_string rs1.ty)));

      (* We negate by xoring 1 *)
      Vec.push body (Inst.Xori { rd = rd_s; rs1 = rd_s; imm = 1 })

  | Geq { rd; rs1; rs2 } ->
      (* rs1 >= rs2, means !(rs1 < rs2) *)
      
      let r = ({
        rd = slot_v rd;
        rs1 = slot_v rs1;
        rs2 = slot_v rs2;
      }: Slots.r_slot) in

      Vec.push body (match rs1.ty with
      | T_int | T_int64 -> Inst.Slt r
      | T_uint | T_uint64 -> Inst.Sltu r
      | _ -> die (Printf.sprintf "geq: unexpected type %s" (Mtype.to_string rs1.ty)));

      (* We negate by xoring 1 *)
      Vec.push body (Inst.Xori { rd = slot_v rd; rs1 = slot_v rd; imm = 1 })

  | Eq { rd; rs1; rs2 } ->
      Vec.push body (Inst.Xor { rd = slot_v rd; rs1 = slot_v rs1; rs2 = slot_v rs2 });
      Vec.push body (Inst.Slti { rd = slot_v rd; rs1 = slot_v rd; imm = 1 })

  | Neq { rd; rs1; rs2 } ->
      Vec.push body (Inst.Xor { rd = slot_v rd; rs1 = slot_v rs1; rs2 = slot_v rs2 });
      Vec.push body (Inst.Sltu { rd = slot_v rd; rs1 = Slot.Reg Zero; rs2 = slot_v rd })

  | Call { rd; fn; args } ->
      let r = slot_v rd in
      let int_args = Vec.empty () in
      let fp_args = Vec.empty () in
      
      List.iter (fun x -> match x.ty with
      | T_float | T_double -> Vec.push fp_args (slot_v x)
      | _ -> Vec.push int_args (slot_v x)) args;
      
      Vec.push body (Inst.Call {
        rd = r; fn = label_of fn;
        args = Vec.to_list int_args;
        fargs = Vec.to_list fp_args
      })

  | CallExtern { rd; fn; args } ->
      let r = slot_v rd in
      let int_args = Vec.empty () in
      let fp_args = Vec.empty () in
      
      List.iter (fun x -> match x.ty with
      | T_float | T_double -> Vec.push fp_args (slot_v x)
      | _ -> Vec.push int_args (slot_v x)) args;
      
      Vec.push body (Inst.Call {
        rd = r; fn = { name = fn; stamp = -1 };
        args = Vec.to_list int_args;
        fargs = Vec.to_list fp_args
      })

  | CallIndirect { rd; rs; args } ->
      let r = slot_v rd in
      let int_args = Vec.empty () in
      let fp_args = Vec.empty () in
      
      List.iter (fun x -> match x.ty with
      | T_float | T_double -> Vec.push fp_args (slot_v x)
      | _ -> Vec.push int_args (slot_v x)) args;
      
      Vec.push body (Inst.CallIndirect {
        rd = r; fn = slot_v rs;
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
      | _ -> die "load: unexpected size")

  | Store { rd; rs; offset; byte; } ->
      let mem_slot: Slots.mem_slot = { rd = slot_v rd; base = slot_v rs; offset } in
      Vec.push body (match byte with
      | 1 -> Inst.Sb mem_slot
      | 2 -> Inst.Sh mem_slot
      | 4 -> Inst.Sw mem_slot
      | 8 -> Inst.Sd mem_slot
      | _ -> die "store: unexpected size")

  | Malloc { rd; size } ->
      Vec.push body (Inst.Li { rd = slot_v rd; imm = IntImm size });
      Vec.push body (Inst.Call {
        rd = slot_v rd;
        fn = { name = "malloc"; stamp = -1 };
        args = [slot_v rd]; fargs = []
      })

  | Alloca { rd; size } ->
      Vec.push body (Inst.Alloca { rd = slot_v rd; size });

  | AssignInt { rd; imm } ->
      Vec.push body (Inst.Li { rd = slot_v rd; imm = IntImm imm })

  | AssignInt64 { rd; imm } ->
      Vec.push body (Inst.Li { rd = slot_v rd; imm = Int64Imm imm })

  | Assign { rd; rs } ->
      Vec.push body (Inst.Mv { rd = slot_v rd; rs = slot_v rs })

  | AssignLabel { rd; imm } ->
      Vec.push body (Inst.La { rd = slot_v rd; label = label_of imm })

  | Phi phi ->
      Vec.push (Hashtbl.find phis name) phi

  | Return ret ->
      terminator := Term.Ret (slot_v ret)

  | Branch { cond; ifso; ifnot } ->
      terminator := Term.Beq {
        rs1 = slot_v cond; rs2 = Slot.Reg Zero;
        ifso = label_of ifso;
        ifnot = label_of ifnot }

  | Jump label ->
      terminator := Term.J (label_of label)
    
  | _ -> failwith (Printf.sprintf "unsupported ssa: %s" (to_string inst))

let gen_fn (f: fn) =
  let int_args = Vec.empty () in
  let fp_args = Vec.empty () in
  
  (* Split arguments into integral and FP *)
  List.iter (fun x -> match x.ty with
  | T_float | T_double -> Vec.push fp_args (slot_v x)
  | _ -> Vec.push int_args (slot_v x)) f.args;

  Vec.push vfuncs {
    funn = label_of f.fn;
    args = Vec.to_list int_args;
    fargs = Vec.to_list fp_args;
    entry = label_of f.fn
  };

  let blocks = get_blocks f.fn in
  List.iter (fun x -> Hashtbl.add phis x (Vec.empty ())) blocks;
  List.iter (fun x ->
    let block = block_of x in
    let body = Vec.empty () in
    let term = ref (Term.Ret Unit) in
    Vec.iter (convert_single x body term) block.body;
    Hashtbl.add vblocks x {
      body; term = !term; preds =
        Vec.to_list block.pred
        |> List.map (fun pred ->
          (* The labels are fixed for loops in `riscv_generate.ml`. *)
          if String.starts_with ~prefix:"loophead_" x
            && not (String.starts_with ~prefix:"loopbefore_" pred) then
            VBlock.LoopBackEdge (label_of x)
          else
            VBlock.NormalEdge (label_of pred)
        )
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
  
  let funcs = Label.Map.of_list (
    Vec.to_list vfuncs |> List.map (fun (x: VFunc.t) -> (x.funn, x))
  ) in

  let blocks = Label.Map.of_list (
    Hashtbl.to_seq vblocks |> List.of_seq |> List.map (fun (k, v) -> (label_of k, v))
  ) in

  
  let out = Printf.sprintf "%s.vasm" !Driver_config.Linkcore_Opt.output_file in
  Basic_io.write out (String.concat "\n\n"
    (Hashtbl.to_seq vblocks |> List.of_seq |>
      List.map (fun (k, (v: VBlock.t)) -> Printf.sprintf "%s:\n%s%s%s" k (
        String.concat "\n" (Vec.to_list v.body |> List.map Inst.to_string)
      ) (if Vec.length v.body = 0 then "" else "\n") (Term.to_string v.term))
  ));
  
  ({
    funcs; blocks;
    consts = Label.Map.empty;
    loop_vars = Label.Map.empty;
  }: VProg.t)

