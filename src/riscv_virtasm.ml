open Riscv_reg

let outfile = Printf.sprintf "%s.deb" !Driver_config.Linkcore_Opt.output_file

let debshow (x : string) : unit =
  (* Basic_io.write outfile x *)
  print_endline @@ "[DEBUG]" ^ x
;;

let debsexp (x : S.t) : unit =
  (* Basic_io.write_s outfile x *)
  print_endline @@ "[DEBUG]" ^ S.to_string x
;;

let deblist (listn : string) (f : 'a -> string) (lst : 'a list) : unit =
  let list_str =
    lst
    |> List.map f (* Apply the function to each element *)
    |> String.concat "; " (* Concatenate the results with "; " *)
  in
  print_endline @@ "[DEBUG] " ^ listn ^ ": [";
  print_endline @@ "    " ^ list_str ^ ";";
  print_endline @@ "]"
;;

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
    ; imm : Imm.t
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
    ; fn : Label.t
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
    ; offset : Imm.t
    }

  type mem_fslot =
    { frd : Slot.t
    ; base : Slot.t
    ; offset : Imm.t
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
end

(** Virtual RISC-V Instructions *)
module Inst = struct
  open Slots

  type t =
    (* Integer Arithmetic Instructions *)
    | Add of r_slot
    | Sub of r_slot
    | Addi of i_slot
    (* Logical and Shift Instructions *)
    | And of r_slot
    | Or of r_slot
    | Xor of r_slot
    | Sll of r_slot (* shift left logical *)
    | Srl of r_slot (* shift right logical *)
    | Sra of r_slot (* shift right arithmetic *)
    | Slli of i_slot (* shift left logical immediate *)
    | Srli of i_slot (* shift right logical immediate *)
    | Srai of i_slot (* shift right arithmetic immediate *)
    (* Multiplication and Division Instructions *)
    | Mul of r_slot
    | Div of r_slot (* signed divide *)
    | Divu of r_slot (* unsigned divide *)
    | Rem of r_slot (* signed remainder *)
    | Remu of r_slot (* unsigned remainder *)
    (* Memory Access Instructions *)
    | Lw of mem_slot (* load word 32-bit *)
    | Ld of mem_slot (* load doubleword 64-bit *)
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
    | Neg of assign_slot
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

  let inst_map (inst : t) (rd : Slot.t -> Slot.t list) (rs : Slot.t -> Slot.t list) =
    match inst with
    | Add r_slot
    | Sub r_slot
    | And r_slot
    | Or r_slot
    | Xor r_slot
    | Sll r_slot
    | Srl r_slot
    | Sra r_slot
    | Mul r_slot
    | Div r_slot
    | Divu r_slot
    | Rem r_slot
    | Remu r_slot -> rd r_slot.rd @ rs r_slot.rs1 @ rs r_slot.rs2
    | Addi i_slot | Slli i_slot | Srli i_slot | Srai i_slot ->
      rd i_slot.rd @ rs i_slot.rs1
    | Lw mem_slot | Ld mem_slot | Sw mem_slot | Sd mem_slot ->
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
    | La assign_label -> []
    | Li assign_int64 -> []
    | Neg assign_slot | Mv assign_slot -> rd assign_slot.rd @ rs assign_slot.rs
    | FmvDX assign_direct -> rd assign_direct.frd @ rs assign_direct.rs
    | FmvDXZero single_fslot -> []
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
  ;;

  let get_srcs (inst : t) : Slot.t list = inst_map inst (fun x -> []) (fun x -> [ x ])
  let get_dests (inst : t) : Slot.t list = inst_map inst (fun x -> [ x ]) (fun x -> [])
  let generate_reload (var : Slot.t) : t = Reload { target = var; origin = var }
  let generate_spill (var : Slot.t) : t = Spill { target = var; origin = var }

  let adjust_rec_alloc_I (inst : t) (pre_K : int) : int =
    match inst with
    | Call _ | CallIndirect _ -> pre_K - List.length Reg.caller_saved_regs
    | _ -> pre_K
  ;;

  let adjust_rec_alloc_F (inst : t) (pre_K : int) : int =
    match inst with
    | Call _ | CallIndirect _ -> pre_K - List.length FReg.caller_saved_fregs
    | _ -> pre_K
  ;;
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
    ; offset : Imm.t
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
  let get_dests (term : t) : Slot.t list = []
  let adjust_rec_alloc_I (term : t) (pre_K : int) : int = pre_K
  let adjust_rec_alloc_F (term : t) (pre_K : int) : int = pre_K
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
    ; loop_vars : SlotSet.t VBlockMap.t
      (* Loop internal variables - 
    used for register allocation special identification*)
    }

  let get_block (vprog : t) (bl : VBlockLabel.t) : VBlock.t =
    match VBlockMap.find_opt vprog.blocks bl with
    | None -> failwith "get_block: block not found"
    | Some x -> x
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
    }
  ;;
end

(* Note: *)
(* Riscv Virtual ASM still retains the structure of control flow (CFG),  *)
(* while its VirtualASM instructions are closer to real assembly.  *)
(* It also includes pseudo-instructions for convenient register allocation and defines the Slot.t type,  *)
(* which aims to allow virtual registers of Slots to coexist with real registers of Regs. *)
