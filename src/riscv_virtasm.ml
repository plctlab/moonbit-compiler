open Riscv_reg

let outfile = Printf.sprintf "%s.deb" !Driver_config.Linkcore_Opt.output_file

let debshow (x : string) : unit =
  (* Basic_io.write outfile x *)
  Printf.printf "[DEBUG] %s\n" x

let debsexp (x : S.t) : unit =
  (* Basic_io.write_s outfile x *)
  Printf.printf "[DEBUG] %s\n" (S.to_string x)

let deblist (listn : string) (f : 'a -> string) (lst : 'a list) : unit =
  let list_str =
    lst
    |> List.map f (* Apply the function to each element *)
    |> String.concat "; " (* Concatenate the results with "; " *)
  in
  Printf.printf "[DEBUG] %s: [\n\t%s\n]\n" listn list_str

(** Slot types for different operations (integer, floating-point, etc.) *)
module Slots = struct
  (** Similar to R-type instructions in RISC-V. *)
  type r_slot =
    { rd : Slot.t
    ; rs1 : Slot.t
    ; rs2 : Slot.t
    }

  (** R-type instructions for floating-point registers. *)
  type r_fslot =
    { frd : Slot.t
    ; frs1 : Slot.t
    ; frs2 : Slot.t
    }

  (** I-type, with one destination register, one source and one immediate. *)
  type i_slot =
    { rd : Slot.t
    ; rs1 : Slot.t
    ; imm : int
    }

  (** Defines a single floating-point register assignment with a destination register `frd`. *)
  type single_fslot = { frd : Slot.t }

  (** Defines a direct assignment between general-purpose and floating-point register.*)
  type assign_direct =
    { frd : Slot.t
    ; rs : Slot.t
    }

  type assign_slot =
    { rd : Slot.t
    ; rs : Slot.t
    }

  type assign_fslot =
    { frd : Slot.t
    ; frs : Slot.t
    }

  (** For special floating-point operation*)
  type triple_fslot =
    { frd : Slot.t
    ; frs1 : Slot.t
    ; frs2 : Slot.t
    ; frs3 : Slot.t
    }

  (** Immediate value `imm` to the destination register `rd`.  *)
  type assign_int64 =
    { rd : Slot.t
    ; imm : Imm.t
    }

  (** Defines an assignment of a label (address or function) to a register `rd`.  *)
  type assign_label =
    { rd : Slot.t
    ; label : Label.t
    }

  (**
  Defines a conversion between floating-point and integer registers.
  Converts floating-point register `frs` to the destination integer register `rd`.
  *)
  type convert_slot =
    { rd : Slot.t
    ; frs : Slot.t
    }

  (**
  Defines a conversion from an integer register to a floating-point register.
  Converts the integer value from `rs` to the destination floating-point register `frd`.
  *)
  type convert_fslot =
    { frd : Slot.t
    ; rs : Slot.t
    }

  (**
  Defines a comparison between two floating-point registers `frs1` and `frs2`, 
    with the result stored in the destination register `rd`.
  *)
  type compare_fslot =
    { rd : Slot.t
    ; frs1 : Slot.t
    ; frs2 : Slot.t
    }

  (** Calls function named `fn` with arguments `args`, and store the result in `rd`. *)
  type call_data =
    { rd : Slot.t
    ; fn : Label.t (* Use a stamp of -1 for libc functions. *)
    ; args : Slot.t list
    ; fargs : Slot.t list
    }

  (** Call function pointer with address `rs` and arguments `args`, and returns in `rd` *)
  type call_indirect =
    { rd : Slot.t
    ; fn : Slot.t
    ; args : Slot.t list
    ; fargs : Slot.t list
    }

  (**
  Similar to `ld` and `st` in RISC-V.
  `rd` and `rs` have different meanings in loads and stores:
  We load `byte` bytes from `rs` into `rd`,
  and store `byte` bytes from `rd` into `rs`.
  *)
  type mem_slot =
    { rd : Slot.t
    ; base : Slot.t
    ; offset : int
    }

  type mem_fslot =
    { frd : Slot.t
    ; base : Slot.t
    ; offset : int
    }

  (**
  Defines a stack slot used for register spilling and reloading.
  The `target` is the register being spilled/reloaded, 
  and `origin` is the original register it corresponds to.
  Used in the final assembly generation to manage stack offsets.
  *)

  type stack_slot =
    { target : Slot.t
    ; origin : Slot.t
    }

  type stack_fslot =
    { target : Slot.t
    ; origin : Slot.t
    }

  type alloca =
    { rd: Slot.t
    ; size: int
    }
end

(** Virtual RISC-V Instructions *)
module Inst = struct
  open Slots

  type t =
    (* Integer Arithmetic Instructions *)
    | Add of r_slot
    | Addw of r_slot

    | Sub of r_slot
    | Subw of r_slot (* signed/unsigned are the same for add/sub *)

    (* signed/unsigned only matters for bits 127-64 in mul *)
    (* which we don't care. *)
    | Mul of r_slot
    | Mulw of r_slot 

    | Div of r_slot (* signed divide *)
    | Divw of r_slot
    | Divu of r_slot (* unsigned divide *)
    | Divuw of r_slot

    | Rem of r_slot (* signed remainder *)
    | Remw of r_slot
    | Remu of r_slot (* unsigned remainder *)
    | Remuw of r_slot

    | Sextw of assign_slot (* signed extend *)
    | Zextw of assign_slot (* unsigned extend *)

    | Addi of i_slot
    | Addiw of i_slot
    (* Logical and Shift Instructions *)
    | And of r_slot
    | Or of r_slot
    | Xor of r_slot

    | Andi of i_slot
    | Ori of i_slot
    | Xori of i_slot

    | Slt of r_slot (* set less than *)
    | Sltw of r_slot
    | Sltu of r_slot
    | Sltuw of r_slot
    | Slti of i_slot
    | Sltiw of i_slot

    | Sll of r_slot (* shift left logical *)
    | Sllw of r_slot

    | Srl of r_slot (* shift right logical *)
    | Srlw of r_slot

    | Sra of r_slot (* shift right arithmetic *)
    | Sraw of r_slot

    | Slli of i_slot (* shift left logical immediate *)
    | Slliw of i_slot

    | Srli of i_slot (* shift right logical immediate *)
    | Srliw of i_slot
    
    | Srai of i_slot (* shift right arithmetic immediate *)
    | Sraiw of i_slot
    (* Multiplication and Division Instructions *)
    (* Memory Access Instructions *)
    | Lb of mem_slot
    | Lbu of mem_slot
    | Lh of mem_slot
    | Lhu of mem_slot
    | Lw of mem_slot (* load word 32-bit *)
    | Ld of mem_slot (* load doubleword 64-bit *)

    | Sb of mem_slot
    | Sh of mem_slot
    | Sw of mem_slot (* store word 32-bit *)
    | Sd of mem_slot (* store doubleword 64-bit *)
    (* Floating-Point Arithmetic Instructions *)
    | FaddD of r_fslot
    | FsubD of r_fslot
    | FmulD of r_fslot
    | FdivD of r_fslot
    | FmaddD of triple_fslot (* fmadd.d => f[rd] = f[rs1]×f[rs2]+f[rs3] *)
    | FmsubD of triple_fslot (* fmsub.d => f[rd] = f[rs1]×f[rs2]-f[rs3] *)
    | FnmaddD of triple_fslot (* fnmadd.d => f[rd] = -f[rs1]×f[rs2]+f[rs3] *)
    | FnmsubD of triple_fslot (* fnmsub.d => f[rd] = -f[rs1]×f[rs2]-f[rs3] *)
    (* Floating-Point Compare Instructions *)
    | FeqD of compare_fslot (* == *)
    | FltD of compare_fslot (* < *)
    | FleD of compare_fslot (* <= *)
    (* Floating-Point Conversion *)
    | FcvtDW of convert_fslot (* convert int32 to float *)
    | FcvtDL of convert_fslot (* convert int64 to float *)
    | FcvtLD of convert_slot (* convert float to int64 *)
    | FcvtWDRtz of convert_slot (* convert float to int, round towards zero *)
    (* Floating-Point Misc Instructions *)
    | FsqrtD of assign_fslot (* square root *)
    | FabsD of assign_fslot (* absolute value *)
    (* Floating-Point Memory Instructions *)
    | Fld of mem_fslot (* load doubleword 64-bit *)
    | Fsd of mem_fslot (* store doubleword 64-bit *)
    (* Movement Instructions *)
    | La of assign_label (* load address *)
    | Li of assign_int64 (* load immediate *)
    | Mv of assign_slot
    | FnegD of assign_fslot
    | FmvD of assign_fslot
    | FmvDX of assign_direct (* move integer slot -> float slot (bitwise) *)
    | FmvDXZero of single_fslot (* move x0 -> float slot (bitwise), i.e. 0.0 *)
    (* Call / Function Invocation Instructions *)
    | Call of call_data
    | CallIndirect of call_indirect
    (* Register Allocation Directives *)
    | Spill of stack_slot
    | Reload of stack_slot
    | FSpill of stack_fslot
    | FReload of stack_fslot
    (* Stack Allocation Directive *)
    | Alloca of alloca

  (* TODO : Refactor *)
  let inst_convert (inst : t) (f : Slot.t -> Slot.t) : t = 
    match inst with
    | Add { rd; rs1; rs2 } -> Add { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Addw { rd; rs1; rs2 } -> Addw { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Sub { rd; rs1; rs2 } -> Sub { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Subw { rd; rs1; rs2 } -> Subw { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Mul { rd; rs1; rs2 } -> Mul { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Mulw { rd; rs1; rs2 } -> Mulw { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Div { rd; rs1; rs2 } -> Div { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Divw { rd; rs1; rs2 } -> Divw { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Divu { rd; rs1; rs2 } -> Divu { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Divuw { rd; rs1; rs2 } -> Divuw { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Rem { rd; rs1; rs2 } -> Rem { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Remw { rd; rs1; rs2 } -> Remw { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Remu { rd; rs1; rs2 } -> Remu { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Remuw { rd; rs1; rs2 } -> Remuw { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Sextw { rd; rs } -> Sextw { rd = f rd; rs = f rs }
    | Zextw { rd; rs } -> Zextw { rd = f rd; rs = f rs }
    | Addi { rd; rs1; imm } -> Addi { rd = f rd; rs1 = f rs1; imm }
    | Addiw { rd; rs1; imm } -> Addiw { rd = f rd; rs1 = f rs1; imm }
    | And { rd; rs1; rs2 } -> And { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Or { rd; rs1; rs2 } -> Or { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Xor { rd; rs1; rs2 } -> Xor { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Andi { rd; rs1; imm } -> Andi { rd = f rd; rs1 = f rs1; imm }
    | Ori { rd; rs1; imm } -> Ori { rd = f rd; rs1 = f rs1; imm }
    | Xori { rd; rs1; imm } -> Xori { rd = f rd; rs1 = f rs1; imm }
    | Slt { rd; rs1; rs2 } -> Slt { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Sltw { rd; rs1; rs2 } -> Sltw { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Sltu { rd; rs1; rs2 } -> Sltu { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Sltuw { rd; rs1; rs2 } -> Sltuw { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Slti { rd; rs1; imm } -> Slti { rd = f rd; rs1 = f rs1; imm }
    | Sltiw { rd; rs1; imm } -> Sltiw { rd = f rd; rs1 = f rs1; imm }
    | Sll { rd; rs1; rs2 } -> Sll { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Sllw { rd; rs1; rs2 } -> Sllw { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Srl { rd; rs1; rs2 } -> Srl { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Srlw { rd; rs1; rs2 } -> Srlw { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Sra { rd; rs1; rs2 } -> Sra { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Sraw { rd; rs1; rs2 } -> Sraw { rd = f rd; rs1 = f rs1; rs2 = f rs2 }
    | Slli { rd; rs1; imm } -> Slli { rd = f rd; rs1 = f rs1; imm }
    | Slliw { rd; rs1; imm } -> Slliw { rd = f rd; rs1 = f rs1; imm }
    | Srli { rd; rs1; imm } -> Srli { rd = f rd; rs1 = f rs1; imm }
    | Srliw { rd; rs1; imm } -> Srliw { rd = f rd; rs1 = f rs1; imm }
    | Srai { rd; rs1; imm } -> Srai { rd = f rd; rs1 = f rs1; imm }
    | Sraiw { rd; rs1; imm } -> Sraiw { rd = f rd; rs1 = f rs1; imm }
    | Lb { rd; base; offset } -> Lb { rd = f rd; base = f base; offset }
    | Lbu { rd; base; offset } -> Lbu { rd = f rd; base = f base; offset }
    | Lh { rd; base; offset } -> Lh { rd = f rd; base = f base; offset }
    | Lhu { rd; base; offset } -> Lhu { rd = f rd; base = f base; offset }
    | Lw { rd; base; offset } -> Lw { rd = f rd; base = f base; offset }
    | Ld { rd; base; offset } -> Ld { rd = f rd; base = f base; offset }
    | Sb { rd; base; offset } -> Sb { rd = f rd; base = f base; offset }
    | Sh { rd; base; offset } -> Sh { rd = f rd; base = f base; offset }
    | Sw { rd; base; offset } -> Sw { rd = f rd; base = f base; offset }
    | Sd { rd; base; offset } -> Sd { rd = f rd; base = f base; offset }
    | FaddD { frd; frs1; frs2 } -> FaddD { frd = f frd; frs1 = f frs1; frs2 = f frs2 }
    | FsubD { frd; frs1; frs2 } -> FsubD { frd = f frd; frs1 = f frs1; frs2 = f frs2 }
    | FmulD { frd; frs1; frs2 } -> FmulD { frd = f frd; frs1 = f frs1; frs2 = f frs2 }
    | FdivD { frd; frs1; frs2 } -> FdivD { frd = f frd; frs1 = f frs1; frs2 = f frs2 }
    | FmaddD { frd; frs1; frs2; frs3 } -> FmaddD { frd = f frd; frs1 = f frs1; frs2 = f frs2; frs3 = f frs3 }
    | FmsubD { frd; frs1; frs2; frs3 } -> FmsubD { frd = f frd; frs1 = f frs1; frs2 = f frs2; frs3 = f frs3 }
    | FnmaddD { frd; frs1; frs2; frs3 } -> FnmaddD { frd = f frd; frs1 = f frs1; frs2 = f frs2; frs3 = f frs3 }
    | FnmsubD { frd; frs1; frs2; frs3 } -> FnmsubD { frd = f frd; frs1 = f frs1; frs2 = f frs2; frs3 = f frs3 }
    | FeqD { rd; frs1; frs2 } -> FeqD { rd = f rd; frs1 = f frs1; frs2 = f frs2 }
    | FltD { rd; frs1; frs2 } -> FltD { rd = f rd; frs1 = f frs1; frs2 = f frs2 }
    | FleD { rd; frs1; frs2 } -> FleD { rd = f rd; frs1 = f frs1; frs2 = f frs2 }
    | FcvtDW { frd; rs } -> FcvtDW { frd = f frd; rs = f rs }
    | FcvtDL { frd; rs } -> FcvtDL { frd = f frd; rs = f rs }
    | FcvtLD { rd; frs } -> FcvtLD { rd = f rd; frs = f frs }
    | FcvtWDRtz { rd; frs } -> FcvtWDRtz { rd = f rd; frs = f frs }
    | FsqrtD { frd; frs } -> FsqrtD { frd = f frd; frs = f frs }
    | FabsD { frd; frs } -> FabsD { frd = f frd; frs = f frs }
    | FnegD { frd; frs } -> FnegD { frd = f frd; frs = f frs }
    | FmvD { frd; frs } -> FmvD { frd = f frd; frs = f frs }
    | FmvDX { frd; rs } -> FmvDX { frd = f frd; rs = f rs }
    | FmvDXZero { frd } -> FmvDXZero { frd = f frd }
    | Fld { frd; base; offset } -> Fld { frd = f frd; base = f base; offset }
    | Fsd { frd; base; offset } -> Fsd { frd = f frd; base = f base; offset }
    | La { rd; label } -> La { rd = f rd; label }
    | Li { rd; imm } -> Li { rd = f rd; imm }
    | Mv { rd; rs } -> Mv { rd = f rd; rs = f rs }
    | Call { rd; fn; args; fargs } -> Call { rd = f rd; fn; args = List.map f args; fargs = List.map f fargs }
    | CallIndirect { rd; fn; args; fargs } -> CallIndirect { rd = f rd; fn = f fn; args = List.map f args; fargs = List.map f fargs }
    | Spill { target; origin } -> Spill { target = f target; origin }
    | Reload { target; origin } -> Reload { target = f target; origin }
    | FSpill { target; origin } -> FSpill { target = f target; origin }
    | FReload { target; origin } -> FReload { target = f target; origin }
    | Alloca { rd; size } -> Alloca { rd = f rd; size }
  ;;
  
  let inst_map (inst : t) (rd : Slot.t -> Slot.t list) (rs : Slot.t -> Slot.t list) =
    match inst with
    | Add r_slot | Addw r_slot
    | Sub r_slot | Subw r_slot
    | And r_slot | Or r_slot   | Xor r_slot
    | Sll r_slot | Sllw r_slot
    | Srl r_slot | Srlw r_slot
    | Sra r_slot | Sraw r_slot
    | Mul r_slot | Mulw r_slot
    | Div r_slot | Divw r_slot | Divu r_slot | Divuw r_slot
    | Rem r_slot | Remw r_slot | Remu r_slot | Remuw r_slot
    | Slt r_slot | Sltw r_slot | Sltu r_slot | Sltuw r_slot
    -> rd r_slot.rd @ rs r_slot.rs1 @ rs r_slot.rs2

    | Addi i_slot | Slli i_slot | Srli i_slot | Srai i_slot
    | Andi i_slot | Xori i_slot | Slti i_slot | Ori i_slot
    | Addiw i_slot| Slliw i_slot| Srliw i_slot| Sltiw i_slot
    | Sraiw i_slot ->
      rd i_slot.rd @ rs i_slot.rs1
    | Lb mem_slot | Lh mem_slot | Lw mem_slot | Ld mem_slot
    | Lbu mem_slot| Lhu mem_slot
    | Sb mem_slot | Sh mem_slot | Sw mem_slot | Sd mem_slot ->
      rd mem_slot.rd @ rs mem_slot.base
    | FaddD r_fslot | FsubD r_fslot | FmulD r_fslot | FdivD r_fslot ->
      rd r_fslot.frd @ rs r_fslot.frs1 @ rs r_fslot.frs2
    | FmaddD triple_fslot
    | FmsubD triple_fslot
    | FnmaddD triple_fslot
    | FnmsubD triple_fslot ->
      rd triple_fslot.frd
      @ rs triple_fslot.frs1
      @ rs triple_fslot.frs2
      @ rs triple_fslot.frs3
    | FeqD compare_fslot | FltD compare_fslot | FleD compare_fslot ->
      rd compare_fslot.rd @ rs compare_fslot.frs1 @ rs compare_fslot.frs2
    | FcvtDW convert_fslot | FcvtDL convert_fslot ->
      rd convert_fslot.frd @ rs convert_fslot.rs
    | FcvtLD convert_slot | FcvtWDRtz convert_slot ->
      rd convert_slot.rd @ rs convert_slot.frs
    | FsqrtD assign_fslot | FabsD assign_fslot | FnegD assign_fslot | FmvD assign_fslot ->
      rd assign_fslot.frd @ rs assign_fslot.frs
    | Fld mem_fslot | Fsd mem_fslot -> rd mem_fslot.frd @ rs mem_fslot.base
    | La assign_label -> rd assign_label.rd
    | Li assign_int64 -> rd assign_int64.rd
    | Mv assign_slot | Sextw assign_slot | Zextw assign_slot ->
      rd assign_slot.rd @ rs assign_slot.rs
    | FmvDX assign_direct -> rd assign_direct.frd @ rs assign_direct.rs
    | FmvDXZero _single_fslot -> []
    | Call call_data ->
      rd call_data.rd
      @ List.concat_map rs call_data.args
      @ List.concat_map rs call_data.fargs
    | CallIndirect call_indirect ->
      rd call_indirect.rd
      @ rs call_indirect.fn
      @ List.concat_map rs call_indirect.args
      @ List.concat_map rs call_indirect.fargs
    | Spill stack_slot | Reload stack_slot -> rd stack_slot.target @ rs stack_slot.origin
    | FSpill stack_fslot | FReload stack_fslot ->
      rd stack_fslot.target @ rs stack_fslot.origin
    | Alloca alloca -> rd alloca.rd
  ;;

  let get_srcs (inst : t) : Slot.t list = inst_map inst (fun _ -> []) (fun x -> [ x ])
  let get_dests (inst : t) : Slot.t list = inst_map inst (fun x -> [ x ]) (fun _ -> [])
  let generate_reload (var : Slot.t) : t = Reload { target = var; origin = var }
  let generate_spill (var : Slot.t) : t = Spill { target = var; origin = var }

  let adjust_rec_alloc_I (inst : t) (pre_K : int) : int =
    match inst with
    (* | Call _ | CallIndirect _ -> pre_K - List.length Reg.caller_saved_regs *)
    | _ -> pre_K
  ;;

  let adjust_rec_alloc_F (inst : t) (pre_K : int) : int =
    match inst with
    (* | Call _ | CallIndirect _ -> pre_K - List.length FReg.caller_saved_fregs *)
    | _ -> pre_K
  
  let to_string x =
    let s = Slot.to_string in
    let rtype t (r: Slots.r_slot) = 
      Printf.sprintf "%s %s, %s, %s" t (s r.rd) (s r.rs1) (s r.rs2)
    in
    let itype t (i: Slots.i_slot) = 
      Printf.sprintf "%s %s, %s, %d" t (s i.rd) (s i.rs1) i.imm
    in
    let mem t (m: Slots.mem_slot) =
      Printf.sprintf "%s %s, %d(%s)" t (s m.rd) m.offset (s m.base)
    in
    match x with
    | Add r -> rtype "add" r
    | Addw r -> rtype "addw" r
    | Sub r -> rtype "sub" r
    | Subw r -> rtype "subw" r
    | Mul r -> rtype "mul" r
    | Mulw r -> rtype "mulw" r
    | Div r -> rtype "div" r
    | Divw r -> rtype "divw" r
    | Divu r -> rtype "divu" r
    | Divuw r -> rtype "divuw" r
    | Rem r -> rtype "rem" r
    | Remw r -> rtype "remw" r
    | Remu r -> rtype "remu" r
    | Remuw r -> rtype "remuw" r
    | Sll r -> rtype "sll" r
    | Sllw r -> rtype "sllw" r
    | Srl r -> rtype "srl" r
    | Srlw r -> rtype "srlw" r
    | Sra r -> rtype "sra" r
    | Sraw r -> rtype "sraw" r
    | Slt r -> rtype "slt" r
    | Sltw r -> rtype "sltw" r
    | Sltu r -> rtype "sltu" r
    | Sltuw r -> rtype "sltuw" r
    | And r -> rtype "and" r
    | Or r -> rtype "or" r
    | Xor r -> rtype "xor" r

    | Addi i -> itype "addi" i
    | Addiw i -> itype "addiw" i
    | Andi i -> itype "andi" i
    | Ori i -> itype "ori" i
    | Xori i -> itype "xori" i
    | Slti i -> itype "slti" i
    | Sltiw i -> itype "sltiw" i
    | Srai i -> itype "srai" i
    | Srli i -> itype "srli" i
    | Slli i -> itype "slli" i
    | Sraiw i -> itype "sraiw" i
    | Srliw i -> itype "srliw" i
    | Slliw i -> itype "slliw" i

    | Lb m -> mem "lb" m
    | Lh m -> mem "lh" m
    | Lw m -> mem "lw" m
    | Ld m -> mem "ld" m
    | Lbu m -> mem "lbu" m
    | Lhu m -> mem "lhu" m

    | Sb m -> mem "sb" m
    | Sh m -> mem "sh" m
    | Sw m -> mem "sw" m
    | Sd m -> mem "sd" m

    | Call { rd; fn; args; fargs } ->
        let args_list = String.concat ", " (List.map s args) in
        let fargs_list = String.concat ", " (List.map s fargs) in
        Printf.sprintf "call %s, %s(%s; %s)" (s rd) fn.name args_list fargs_list

    | CallIndirect { rd; fn; args; fargs } ->
        let args_list = String.concat " " (List.map s args) in
        let fargs_list = String.concat ", " (List.map s fargs) in
        Printf.sprintf "jr %s, %s(%s; %s)" (s rd) (s fn) args_list fargs_list

    | Li { rd; imm } ->
        (match imm with
        | IntImm x -> Printf.sprintf "li %s, %d" (s rd) x
        | Int64Imm x -> Printf.sprintf "li %s, %s" (s rd) (Int64.to_string x)
        | FloatImm x -> Printf.sprintf "li %s, %f" (s rd) x)

    | La { rd; label } -> Printf.sprintf "la %s, %s" (s rd) label.name
    | Mv { rd; rs } -> Printf.sprintf "mv %s, %s" (s rd) (s rs)
    | Sextw { rd; rs } -> Printf.sprintf "sext.w %s, %s" (s rd) (s rs)
    | Zextw { rd; rs } -> Printf.sprintf "zext.w %s, %s" (s rd) (s rs)
    | Alloca { rd; size } -> Printf.sprintf "alloca %s, %d" (s rd) size

    | Spill { target; origin } -> Printf.sprintf "spill %s %s" (s target) (s origin)
    | Reload { target; origin } -> Printf.sprintf "reload %s %s" (s target) (s origin)
    | _ -> failwith "riscv_virtasm.ml: unsupported"
end

(* Vector alias*)
module Vec = Basic_vec

(* VBlock *)
module VBlockLabel = Label
module VBlockSet = Label.Hashset
module VBlockMap = Label.Map

(* VFunc *)
module VFuncLabel = Label
module VFuncSet = Label.Hashset
module VFuncMap = Label.Map

(* VProg *)

(* VSymbol *)
module VSymbolLabel = Label
module VSymbolSet = Label.Hashset
module VSymbolMap = Label.Map

(** Control Flow module *)
module Term = struct
  open Slots

  (** Branch slot for conditional branches *)
  type branch_slot =
    { rs1 : Slot.t
    ; rs2 : Slot.t
    ; ifso : VBlockLabel.t
    ; ifnot : VBlockLabel.t
    }

  (** rd stores return address, label is the jump target *)
  type jal_label =
    { rd : Slot.t
    ; label : VBlockLabel.t
    }

  (** jump address is calculated by rs1 + offset, rd stores return address *)
  type jalr_label =
    { rd : Slot.t
    ; rs1 : Slot.t
    ; offset : int
    }

  (** These include conditional branches, unconditional jumps, function returns, and tail calls.  *)
  type t =
    | Beq of branch_slot (* Branch if equal *)
    | Bne of branch_slot (* Branch if not equal *)
    | Blt of branch_slot (* Branch if less than *)
    | Bge of branch_slot (* Branch if greater than or equal *)
    | Bltu of branch_slot (* Branch if less than unsigned *)
    | Bgeu of branch_slot (* Branch if greater than or equal unsigned *)
    | J of VBlockLabel.t (* jump (not stort return address) *)
    | Jal of VBlockLabel.t (* jump and link (store return address) *)
    | Jalr of jalr_label (* jump and link register (store return address) *)
    | TailCall of call_data
    | TailCallIndirect of call_indirect
    | Ret of Slot.t (* Unit for no return*)
  ;;

  let term_map_reg (term : t) (f : Slot.t -> Slot.t) : t =
    match term with
    | Beq { rs1; rs2; ifso; ifnot } -> Beq { rs1 = f rs1; rs2 = f rs2; ifso; ifnot }
    | Bne { rs1; rs2; ifso; ifnot } -> Bne { rs1 = f rs1; rs2 = f rs2; ifso; ifnot }
    | Blt { rs1; rs2; ifso; ifnot } -> Blt { rs1 = f rs1; rs2 = f rs2; ifso; ifnot }
    | Bge { rs1; rs2; ifso; ifnot } -> Bge { rs1 = f rs1; rs2 = f rs2; ifso; ifnot }
    | Bltu { rs1; rs2; ifso; ifnot } -> Bltu { rs1 = f rs1; rs2 = f rs2; ifso; ifnot }
    | Bgeu { rs1; rs2; ifso; ifnot } -> Bgeu { rs1 = f rs1; rs2 = f rs2; ifso; ifnot }
    | Jalr { rd; rs1; offset } -> Jalr { rd = f rd; rs1 = f rs1; offset }
    | TailCall { rd; fn; args; fargs } -> TailCall { rd = f rd; fn; args = List.map f args; fargs = List.map f fargs }
    | TailCallIndirect { rd; fn; args; fargs } -> TailCallIndirect { rd = f rd; fn = f fn; args = List.map f args; fargs = List.map f fargs }
    | Ret var -> Ret (f var)
    | x -> x
  ;;

  let term_map_label (term : t) (f : VBlockLabel.t -> VBlockLabel.t) : t =
    match term with
    | Beq { rs1; rs2; ifso; ifnot } -> Beq { rs1; rs2; ifso = f ifso; ifnot = f ifnot }
    | Bne { rs1; rs2; ifso; ifnot } -> Bne { rs1; rs2; ifso = f ifso; ifnot = f ifnot }
    | Blt { rs1; rs2; ifso; ifnot } -> Blt { rs1; rs2; ifso = f ifso; ifnot = f ifnot }
    | Bge { rs1; rs2; ifso; ifnot } -> Bge { rs1; rs2; ifso = f ifso; ifnot = f ifnot }
    | Bltu { rs1; rs2; ifso; ifnot } -> Bltu { rs1; rs2; ifso = f ifso; ifnot = f ifnot }
    | Bgeu { rs1; rs2; ifso; ifnot } -> Bgeu { rs1; rs2; ifso = f ifso; ifnot = f ifnot }
    | J label -> J (f label)
    | Jal label -> Jal (f label)
    | x -> x
  ;;


  let get_srcs (term : t) : Slot.t list =
    match term with
    | Beq branch_slot
    | Bne branch_slot
    | Blt branch_slot
    | Bge branch_slot
    | Bltu branch_slot
    | Bgeu branch_slot -> [ branch_slot.rs1; branch_slot.rs2 ]
    | Jalr jalr_label -> [ jalr_label.rs1 ]
    | TailCall call_data -> call_data.args
    | TailCallIndirect call_indirect -> call_indirect.args
    | Ret _ -> []
    | J _ | Jal _ -> []
  ;;

  (* Keep these functions for API consistency *)
  let get_dests (_term : t) : Slot.t list = []
  let adjust_rec_alloc_I (_term : t) (pre_K : int) : int = pre_K
  let adjust_rec_alloc_F (_term : t) (pre_K : int) : int = pre_K

  let to_string t =
    let s = Slot.to_string in
    let branch t { rs1; rs2; ifso; ifnot } =
      Printf.sprintf "%s %s, %s, %s, %s" t (s rs1) (s rs2) ifso.name ifnot.name
    in
    match t with
    | Beq b -> branch "beq" b
    | Bne b -> branch "bne" b
    | Blt b -> branch "blt" b
    | Bge b -> branch "bge" b
    | Bltu b -> branch "bltu" b
    | Bgeu b -> branch "bgeu" b
    | J label -> Printf.sprintf "j %s" label.name
    | Jal label -> Printf.sprintf "jal %s" label.name
    | Jalr { rd; rs1; offset } -> Printf.sprintf "jalr %s %d(%s)" (s rd) offset (s rs1)
    | Ret ret -> Printf.sprintf "return %s" (s ret)
    | TailCall { rd; fn; args; fargs } ->
        let args_list = String.concat ", " (List.map s args) in
        let fargs_list = String.concat ", " (List.map s fargs) in
        Printf.sprintf "call.tail %s, %s(%s; %s)" (s rd) fn.name args_list fargs_list

    | TailCallIndirect { rd; fn; args; fargs } ->
        let args_list = String.concat " " (List.map s args) in
        let fargs_list = String.concat ", " (List.map s fargs) in
        Printf.sprintf "jr.tail %s, %s(%s; %s)" (s rd) (s fn) args_list fargs_list
end

(** VirtRvBlock*)
module VBlock = struct
  type t =
    { body : Inst.t Vec.t
    ; term : Term.t (* Single Terminator*)
    ; preds : pred_t list (* Predecessors*)
    }

  and pred_t =
    | NormalEdge of VBlockLabel.t
    | LoopBackEdge of VBlockLabel.t

  let get_preds (block : t) : VBlockLabel.t list =
    List.map
      (fun pred ->
         match pred with
         | NormalEdge pred_bl | LoopBackEdge pred_bl -> pred_bl)
      block.preds
  ;;

  let get_pred (edge : pred_t) : VBlockLabel.t =
    match edge with
    | NormalEdge pred_bl | LoopBackEdge pred_bl -> pred_bl
  ;;

  let get_successors (block : t) : VBlockLabel.t list =
    match block.term with
    | Beq branch_slot
    | Bne branch_slot
    | Blt branch_slot
    | Bge branch_slot
    | Bltu branch_slot
    | Bgeu branch_slot -> [ branch_slot.ifso; branch_slot.ifnot ]
    | J label | Jal label -> [ label ]
    | Jalr _ ->
      [] (* Since Jalr is a computed jump, we might not have a static successor *)
    | TailCall _ | TailCallIndirect _ ->
      []
      (* Tail calls typically transfer control to another function, no direct successor *)
    | Ret _ -> [] (* Return terminates the current function, so no successor blocks *)
  ;;

  let get_body_insts (block : t) : Inst.t list = Vec.to_list block.body
end

(** VirtRvFunc*)
module VFunc = struct
  type t =
    { funn : VFuncLabel.t
    ; args : Slot.t list
    ; fargs : Slot.t list
    ; entry : VBlockLabel.t
    }
end

(** VirtRvProg*)
module VProg = struct
  type t =
    { blocks : VBlock.t VBlockMap.t
    ; funcs : VFunc.t VFuncMap.t
    ; consts : Imm.t VSymbolMap.t
    (* Loop internal variables - 
    used for register allocation special identification*)
    ; loop_vars : SlotSet.t VBlockMap.t

    (* Global variables; stores the label and the size *)
    ; globals: (string * int) list

    (* Global arrays; stores the label and initial data. *)
    
    (* Note we treat it opaquely. The arrays might hold *)
    (* a vtable or an int array with length at front;  *)
    (* they are too different to abstract a same interface. *)
    ; extarrs: Riscv_ssa.extern_array list
    }

  let get_block (vprog : t) (bl : VBlockLabel.t) : VBlock.t =
    match VBlockMap.find_opt vprog.blocks bl with
    | None -> failwith "get_block: block not found"
    | Some x -> x
  ;;

  let update_block (vprog : t) (bl : VBlockLabel.t) (b : VBlock.t) : t =
    { vprog with blocks = VBlockMap.add vprog.blocks bl b }
  ;;

  let get_func (vprog : t) (fn : VFuncLabel.t) : VFunc.t =
    match VFuncMap.find_opt vprog.funcs fn with
    | None -> failwith "get_func: function not found"
    | Some x -> x
  ;;

  let get_loop_vars (vprog : t) (bl : VBlockLabel.t) : SlotSet.t =
    match VBlockMap.find_opt vprog.loop_vars bl with
    | None -> failwith "get_loop_vars: loop variable not found"
    | Some x -> x
  ;;

  let empty : t =
    { blocks = VBlockMap.empty
    ; funcs = VFuncMap.empty
    ; consts = VSymbolMap.empty
    ; loop_vars = VBlockMap.empty
    ; globals = []
    ; extarrs = []
    }
  
  let to_string (vprog: t) = 
    (String.concat "\n\n"
    (Label.Map.to_sorted_array vprog.blocks |> Array.to_list |>
      List.map (fun (({ name = k; _}: Label.t), (v: VBlock.t)) ->
        Printf.sprintf "%s:\n%s%s%s" k (
          String.concat "\n" (Vec.to_list v.body |> List.map Inst.to_string)
        ) (if Vec.length v.body = 0 then "" else "\n") (Term.to_string v.term))
    ))
end

(* Note: *)
(* Riscv Virtual ASM still retains the structure of control flow (CFG),  *)
(* while its VirtualASM instructions are closer to real assembly.  *)
(* It also includes pseudo-instructions for convenient register allocation and defines the Slot.t type,  *)
(* which aims to allow virtual registers of Slots to coexist with real registers of Regs. *)
