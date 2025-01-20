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

(** Similar to R-type instructions in RISC-V. *)
type r_slot =
  { rd : slot_t
  ; rs1 : slot_t
  ; rs2 : slot_t
  }

(** R-type instructions for floating-point registers. *)
type r_fslot =
  { frd : slot_t
  ; frs1 : slot_t
  ; frs2 : slot_t
  }

(** I-type, with one destination register, one source and one immediate. *)
type i_slot =
  { rd : slot_t
  ; rs1 : slot_t
  ; imm : imm_t
  }

(** Defines a single floating-point register assignment with a destination register `frd`. *)
type single_fslot = { frd : slot_t }

(** Defines a direct assignment between general-purpose and floating-point register.*)
type assign_direct =
  { frd : slot_t
  ; rs : slot_t
  }

type assign_slot =
  { rd : slot_t
  ; rs : slot_t
  }

type assign_fslot =
  { frd : slot_t
  ; frs : slot_t
  }

(**
Defines a floating-point multiplication and addition operation.
The result of multiplying `rs1` with `rs2`, and `rs3` with `rs2` are added together, 
with the result stored in `frd`.
*)
type mul_add_fslot =
  { frd : slot_t
  ; frs1 : slot_t
  ; frs2 : slot_t
  ; frs3 : slot_t
  }

(** Immediate value `imm` to the destination register `rd`.  *)
type assign_int64 =
  { rd : slot_t
  ; imm : imm_t
  }

(** Defines an assignment of a label (address or function) to a register `rd`.  *)
type assign_label =
  { rd : slot_t
  ; label : label_t
  }

(**
Defines a conversion between floating-point and integer registers.

Converts the value in the source floating-point register `frs` to the destination integer register `rd`.
*)
type convert_slot =
  { rd : slot_t
  ; frs : slot_t
  }

(**
Defines a conversion from an integer register to a floating-point register.

Converts the integer value from `rs` to the destination floating-point register `frd`.
*)
type convert_fslot =
  { frd : slot_t
  ; rs : slot_t
  }

(**
Defines a comparison between two floating-point registers `frs1` and `frs2`, 
  with the result stored in the destination register `rd`.
*)
type compare_fslot =
  { rd : slot_t
  ; frs1 : slot_t
  ; frs2 : slot_t
  }

(** Calls function named `fn` with arguments `args`, and store the result in `rd`. *)
type call_data =
  { rd : slot_t
  ; fn : label_t
  ; args : slot_t list
  ; fargs : slot_t list
  }

(** Call function pointer with address `rs` and arguments `args`, and returns in `rd` *)
type call_indirect =
  { rd : slot_t
  ; fn : slot_t
  ; args : slot_t list
  ; fargs : slot_t list
  }

(**
Similar to `ld` and `st` in RISC-V.

`rd` and `rs` have different meanings in loads and stores:
We load `byte` bytes from `rs` into `rd`,
and store `byte` bytes from `rd` into `rs`.
*)
type mem_slot =
  { rd : slot_t
  ; base : slot_t
  ; offset : imm_t
  }

type mem_fslot =
  { frd : slot_t
  ; base : slot_t
  ; offset : imm_t
  }

(**
Defines a stack slot used for register spilling and reloading.
The `target` is the register being spilled/reloaded, 
  and `origin` is the original register it corresponds to.
Used in the final assembly generation to manage stack offsets.
*)

type stack_slot =
  { target : slot_t
  ; origin : slot_t
  }

type stack_fslot =
  { target : slot_t
  ; origin : slot_t
  }

(** Virtual RISC-V Instructions *)
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
  | FmaddD of mul_add_fslot (* fmadd.d rd, rs1, rs2, rs3 => rd=(a*b)+(c*d) *)
  (* Floating-Point Compare Instructions *)
  | FeqD of compare_fslot (* == *)
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

(** Branching is done based on the comparison of registers `rs1`, `rs2` or a single register `rs`.  *)
type branch_slot =
  { rs1 : slot_t
  ; rs2 : slot_t
  ; ifso : label_t
  ; ifnot : label_t
  }

type branch_single =
  { rs : slot_t
  ; ifso : label_t
  ; ifnot : label_t
  }

(** These include conditional branches, unconditional jumps, function returns, and tail calls.  *)
type term_t =
  | Beq of branch_slot (* Branch if equal *)
  | Ble of branch_slot (* Branch if less than or equal *)
  | Beqz of branch_single (* Branch if equal to zero *)
  | Blez of branch_single (* Branch if less than or equal to zero *)
  | Bltz of branch_single (* Branch if less than zero *)
  | J of label_t (* jump to label (unconditional) *)
  | Jal of label_t (* jump and link (store return address) *)
  | TailCall of call_data
  | TailCallIndirect of call_indirect
  | Ret of slot_t (* Unit for no return*)

(* Note: *)
(* Riscv Virtual ASM still retains the structure of control flow (CFG),  *)
(* while its VirtualASM instructions are closer to real assembly.  *)
(* It also includes pseudo-instructions for convenient register allocation and defines the slot_t type,  *)
(* which aims to allow virtual registers of Slots to coexist with real registers of Regs. *)

module Map_int = Basic_map_int
module Map_string = Basic_map_string

type vblock_label = int
type vfunc_label = string

(** VirtRvBlock*)
type vblock_t =
  { body : t Basic_vec.t
  ; term : term_t (* Single Terminator*)
  ; preds : vblock_label Basic_vec.t (* Predecessors*)
  }

(** VirtRvFunc*)
type vfunc_t =
  { result : ret_type option
  ; args : slot_t list
  ; fargs : slot_t list
  ; entry : vblock_label
  }

(** VirtRvProg*)
type vprog_t =
  { blocks : vblock_t Map_int.t
  ; funcs : vfunc_t Map_string.t
  ; const : imm_t Map_string.t
  ; loop_vars : slot_t Map_int.t
    (* Loop internal variables - 
    used for register allocation special identification*)
  }
