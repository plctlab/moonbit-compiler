open Riscv_ssa
open Mtype

(**
Some MoonBit IR instructions carry optional compiler primitives in strange places.
I haven't met a case when those are not `None`, but a check is put here in case.
*)
let warn prim = match prim with
| None -> ()
| Some _ -> prerr_endline "warning: prim is not null"

module Varset = Set.Make(String)

let global_vars = ref Varset.empty

(** Global instructions, i.e. GlobalVarDecl and ExtArray *)
let global_inst = Basic_vec.empty ()

(** Offset of each field in a record type. *)
let offset_table = Hashtbl.create 64

(** Size of each record type. *)
let size_table = Hashtbl.create 64

(** All methods in vtable for each type. *)
let trait_table = Hashtbl.create 64

(** The vtable offset for (type, trait). *)
let trait_offset = Hashtbl.create 64

(** Indices of arguments that are trait types for each function, along with their types. *)
let traited_args = Hashtbl.create 64

(** Get offset of the `pos`-th field in the record type called `name`. *)
let offsetof ty pos = Hashtbl.find offset_table (ty, pos)

let is_trait ty = match ty with
| T_trait _ -> true
| _ -> false

let pointer_size = 8

(** Label of current basic block. Used in phi-function generation. *)
let current_label = ref ""

let push_label ssa label =
  current_label := label;
  Basic_vec.push ssa (Label label)

let remove_space = String.map (fun c -> if c = ' ' then '_' else c)

(* This is the size of their representations, not the actual size. *)
let rec sizeof ty = match ty with
| T_bool -> 1
| T_byte -> 1
| T_bytes -> pointer_size
| T_char -> 2
| T_double -> 8
| T_float -> 4
| T_func _ -> pointer_size
| T_int -> 4
| T_int64 -> 8
| T_string -> pointer_size
| T_uint -> 4
| T_uint64 -> 8
| T_unit -> 0
| T_tuple _ -> pointer_size
| T_constr id -> pointer_size
| T_fixedarray _ -> pointer_size
| _ -> failwith "riscv_ssa.ml: cannot calculate size"

(** Push the correct sequence of instruction based on primitives. *)
let deal_with_prim ssa rd (prim: Primitive.prim) args =
  let die () =
    failwith "riscv_ssa.ml: bad primitive format"
  in

  match prim with
  | Pcomparison { operand_type; operator } ->
      let is_fp = (operand_type = F32 || operand_type = F64) in
      let op = (match is_fp, operator, args with
      | false, Lt, [rs1; rs2] -> (Less { rd; rs1; rs2 })
      | true, Lt, [rs1; rs2] -> (FLess { rd; rs1; rs2 })
      | false, Gt, [rs1; rs2] -> (Great { rd; rs1; rs2 })
      | true, Gt, [rs1; rs2] -> (FGreat { rd; rs1; rs2 })
      | false, Ne, [rs1; rs2] -> (Neq { rd; rs1; rs2 })
      | true, Ne, [rs1; rs2] -> (FNeq { rd; rs1; rs2 })
      | false, Eq, [rs1; rs2] -> (Eq { rd; rs1; rs2 })
      | true, Eq, [rs1; rs2] -> (FEq { rd; rs1; rs2 })
      | false, Le, [rs1; rs2] -> (Leq { rd; rs1; rs2 })
      | true, Le, [rs1; rs2] -> (FLeq { rd; rs1; rs2 })
      | false, Ge, [rs1; rs2] -> (Geq { rd; rs1; rs2 })
      | true, Ge, [rs1; rs2] -> (FGeq { rd; rs1; rs2 })
      | _ -> die ()) in
      Basic_vec.push ssa op
  
  | Parith { operand_type; operator } ->
      let is_fp = (operand_type = F32 || operand_type = F64) in
      let op = (match is_fp, operator, args with
      | false, Add, [rs1; rs2] -> (Add { rd; rs1; rs2 })
      | true, Add, [rs1; rs2] -> (FAdd { rd; rs1; rs2 })
      | false, Sub, [rs1; rs2] -> (Sub { rd; rs1; rs2 })
      | true, Sub, [rs1; rs2] -> (FSub { rd; rs1; rs2 })
      | false, Mul, [rs1; rs2] -> (Mul { rd; rs1; rs2 })
      | true, Mul, [rs1; rs2] -> (FMul { rd; rs1; rs2 })
      | false, Div, [rs1; rs2] -> (Div { rd; rs1; rs2 })
      | true, Div, [rs1; rs2] -> (FDiv { rd; rs1; rs2 })
      | false, Mod, [rs1; rs2] -> (Mod { rd; rs1; rs2 })
      | false, Neg, [rs1] -> (Neg { rd; rs1 })
      | true, Neg, [rs1] -> (FNeg { rd; rs1 })
      | _ -> die ()) in
      Basic_vec.push ssa op

  | Pbitwise { operand_type; operator } ->
      let op = (match operator, args with
      | Not, [rs1] -> (Not { rd; rs1 })
      | And, [rs1; rs2] -> (And { rd; rs1; rs2 })
      | Or, [rs1; rs2] -> (Or { rd; rs1; rs2 })
      | Xor, [rs1; rs2] -> (And { rd; rs1; rs2 })
      | Shr, [rs1; rs2] -> (Shr { rd; rs1; rs2 })
      | Shl, [rs1; rs2] -> (Shl { rd; rs1; rs2 })

      (* For ease of implementation, let's just rely on GCC builtins *)
      | Popcnt, _ -> (CallExtern { rd; fn = "__builtin_popcount"; args })
      | Ctz, _ -> (CallExtern { rd; fn = "__builtin_ctz"; args })
      | Clz, _ -> (CallExtern { rd; fn = "__builtin_clz"; args })
      | _ -> die()) in
      Basic_vec.push ssa op

  | Pconvert { kind; from; to_ } ->
      (* Reinterpret means the register value should remain the same *)
      if kind == Primitive.Reinterpret then
        Basic_vec.push ssa (Assign { rd; rs = List.hd args })

      (* But convert is where we must take some action *)
      else
        let arg = List.hd args in 
        (match from, to_ with
        | I32, U8 | U32, U8 ->
            (* Discard higher bits by masking them away *)
            Basic_vec.push ssa (Andi { rd; rs = arg; imm = 255 })
        
        | _ -> die())

  | Parray_make ->
      (* This should construct a struct like: *)
      (* struct { void* buf; int len; } *)
      let buf = new_temp T_bytes in
      let len = new_temp T_int in
      let length = List.length args in
      let buf_size = if length = 0 then 0 else length * (sizeof (List.hd args).ty) in
      Basic_vec.push ssa (Malloc { rd; size = 12 });
      Basic_vec.push ssa (Malloc { rd = buf; size = buf_size });
      Basic_vec.push ssa (AssignInt { rd = len; imm = length; });
      Basic_vec.push ssa (Store { rd = buf; rs = rd; offset = 0; byte = pointer_size });
      Basic_vec.push ssa (Store { rd = len; rs = rd; offset = pointer_size; byte = 4 });

  (* The argument is whether we perform bound checks. *)
  (* My observation is that this argument is always Unsafe; *)
  (* they are done in an earlier stage in MoonBit core IR. *)
  (* Hence we don't generate bound check code here. *)
  | Pfixedarray_get_item _ ->
      if List.length args != 2 then
        failwith "riscv_ssa.ml: bad call to 'Pfixedarray_get_item'"
      else
        let arr = List.nth args 0 in
        let index = List.nth args 1 in
        let ty =
          (match arr.ty with
          | T_fixedarray { elem } -> elem
          | _ -> failwith "riscv_ssa.ml: bad type in fixedarray_get_item")
        in
        let size = sizeof ty in
        let addr = new_temp T_bytes in
        let sz = new_temp T_int in
        let offset = new_temp T_int in

        (* Same as `rd = *(arr + sizeof(T) * index)` *)
        Basic_vec.push ssa (AssignInt { rd = sz; imm = size; });
        Basic_vec.push ssa (Mul { rd = offset; rs1 = sz; rs2 = index });
        Basic_vec.push ssa (Add { rd = addr; rs1 = arr; rs2 = offset });
        Basic_vec.push ssa (Load { rd; rs = addr; offset = 0; byte = size });

  | Pfixedarray_make { kind } ->
      (match kind with
      | Uninit ->
          if List.length args != 1 then
            failwith "riscv_ssa.ml: bad call to 'Pfixedarray_make'"
          else
            let len = List.hd args in
            let ty =
              (match rd.ty with
              | T_fixedarray { elem } -> elem
              | _ -> failwith "riscv_ssa.ml: bad type in fixedarray_make")
            in
            let size = sizeof ty in
            let sz = new_temp T_int in
            let length = new_temp T_int in
            (* Same as `rd = malloc(sizeof(T) * len)` *)
            Basic_vec.push ssa (AssignInt { rd = sz; imm = size; });
            Basic_vec.push ssa (Mul { rd = length; rs1 = sz; rs2 = len });
            Basic_vec.push ssa (CallExtern { rd; fn = "malloc"; args = [length] })

      (* Haven't observed occurrence of other values of `kind` *)
      | _ -> failwith "riscv_ssa.ml: unrecognized config of fixed array make")

  | Pcall_object_method { method_index; _ } ->
      (* We've guaranteed that the vtable pointer is pointing to the correct place. *)
      (* So it's just calling index * ptr_size + *vtb. *)
      let vtb_offset = method_index * pointer_size in
      let arg = List.hd args in

      (* Temporaries in SSA *)
      let vtb = new_temp T_bytes in
      let fn_addr = new_temp T_bytes in
      let load = new_temp T_int in

      Basic_vec.push ssa (Load { rd = vtb; rs = arg; offset = -pointer_size; byte = pointer_size });
      Basic_vec.push ssa (AssignInt { rd = load; imm = vtb_offset });
      Basic_vec.push ssa (Add { rd = fn_addr; rs1 = vtb; rs2 = load });
      (* The whole set of args (including self) is needed. *)
      Basic_vec.push ssa (CallIndirect { rd; rs = fn_addr; args })

  | Pgetbytesitem ->
      let str = List.nth args 0 in
      let i = List.nth args 1 in
      
      let altered = new_temp T_string in
      Basic_vec.push ssa (Add { rd = altered; rs1 = str; rs2 = i });
      Basic_vec.push ssa (Load { rd; rs = altered; offset = 0; byte = 1 })

  | Psetbytesitem ->
      let str = List.nth args 0 in
      let i = List.nth args 1 in
      let item = List.nth args 2 in
      
      let altered = new_temp T_string in
      Basic_vec.push ssa (Add { rd = altered; rs1 = str; rs2 = i });
      Basic_vec.push ssa (Store { rd = item; rs = altered; offset = 0; byte = 1 })

  (* Be cautious that each `char` is 2 bytes long, which is extremely counter-intuitive. *)
  | Pgetstringitem ->
      let str = List.nth args 0 in
      let i = List.nth args 1 in
      
      let two = new_temp T_int in
      let offset = new_temp T_int in
      let altered = new_temp T_string in
      Basic_vec.push ssa (AssignInt { rd = two; imm = 2 });
      Basic_vec.push ssa (Mul { rd = offset; rs1 = i; rs2 = two });
      Basic_vec.push ssa (Add { rd = altered; rs1 = str; rs2 = offset });
      Basic_vec.push ssa (Load { rd; rs = altered; offset = 0; byte = 2 })

  (* Length are both stored at the same place for these arrays. *)
  | Pstringlength
  | Pbyteslength ->
      let bytes = List.hd args in
      Basic_vec.push ssa (Load { rd; rs = bytes; offset = -4; byte = 4 })

  (* We must put length information in bytes when we make it, *)
  (* but for strings we rely on `strlen` *)
  | Pmakebytes ->
      let len = List.nth args 0 in
      let init = List.nth args 1 in

      (* Let the pointer point to beginning of data, rather than the length section *)
      let memory = new_temp T_bytes in
      let unused = new_temp T_unit in
      let new_len = new_temp T_int in
      Basic_vec.push ssa (Addi { rd = new_len; rs = len; imm = 4 });
      Basic_vec.push ssa (CallExtern { rd = memory; fn = "malloc"; args = [ new_len ] });
      Basic_vec.push ssa (Store { rd = len; rs = memory; offset = 0; byte = 4 });
      Basic_vec.push ssa (Addi { rd; rs = memory; imm = 4 });
      Basic_vec.push ssa (CallExtern { rd = unused; fn = "memset"; args = [ rd; init; len ] });

  | Pignore -> ()

  | Ppanic ->
      Basic_vec.push ssa (CallExtern { rd; fn = "abort"; args })

  | Pidentity ->
      Basic_vec.push ssa (Assign { rd; rs = List.hd args })
  
  | Pprintln ->
      Basic_vec.push ssa (CallExtern { rd; fn = "puts"; args })
  
  | _ -> Basic_vec.push ssa (Call { rd; fn = (Primitive.sexp_of_prim prim |> S.to_string); args })


(** Extract information from types and store them in global variables. *)
let update_types ({ defs; _ }: defs) =
  let types = Id_hash.to_list defs in

  let visit (name, info) =
    match info with
    | Placeholder -> ()
    | Externref -> ()

    (* We don't care about declarations in traits. *)
    | Trait _ -> ()

    (* Calculate offset of fields in record types. *)
    | Record { fields } -> 
        let ty = T_constr name in
        let extract (x: field_info) = x.field_type in
        let field_types = List.map extract fields in
        let field_sizes = List.map sizeof field_types in
        let offset = ref 0 in
        let offsets = List.map (fun x -> let y = !offset in offset := x + !offset; y) field_sizes in
        List.iteri (fun i x -> Hashtbl.add offset_table (ty, i) x) offsets;
        Hashtbl.add size_table ty !offset;
        Hashtbl.add trait_table ty (Basic_vec.empty ())
    
    | _ -> failwith "TODO: riscv_ssa.ml: cannot deal with this type"
  in
  List.iter visit types

(** Record, for each type that implements some trait, which methods of that type are for the trait *)
let record_traits (methods: Object_util.t) =
  Basic_hash_gen.iter methods (fun (key, value) -> 
    let trait_name = Basic_type_path.sexp_of_t key.trait |> S.to_string in
    let ty = value.self_ty in

    let get_method_name = fun (x: Object_util.object_method_item) -> Ident.to_string x.method_id in
    let methods = List.map get_method_name value.methods in

    let vtb_size = Hashtbl.find trait_table ty |> Basic_vec.length in

    (* Note: traits are originally converted from Stype.T_trait to T_trait, *)
    (* and the former takes a Basic_type_path, as expected. *)
    (* However, the conversion function needs additional information which is unknown at this stage. *)
    Hashtbl.add trait_offset (ty, T_trait trait_name) vtb_size;
    Basic_vec.append (Hashtbl.find trait_table ty) (Basic_vec.of_list methods)
  )

(**
This is reserved for `continue`s.
See the match case for `Cexpr_loop` in `do_convert` for more details.

It represents a list of continue clauses, each with a list of arguments and a label,
marking where the argument comes from.
*)
let conts: (var * string) list list ref = ref []

(**
This function stores the SSA generated in the given argument `ssa`.

It returns the variable in which the result of the last instruction pushed is stored.
*)
let rec do_convert ssa (expr: Mcore.expr) =
  match expr with
  | Cexpr_unit _ ->
      unit
  
  | Cexpr_var { id; ty; prim; _ } ->
      warn prim;

      let name = Ident.to_string id in
      let variable = { name; ty } in

      (* Global variables are pointers. *)
      (* Mutable variables are also pointers. *)
      let is_pointer = 
        (Varset.mem name !global_vars) ||
        (match id with Pmutable_ident _ -> true | _ -> false)
      in

      if not is_pointer then variable
      else
        let rd = new_temp ty in
        Basic_vec.push ssa (Load { rd; rs = variable; offset = 0; byte = sizeof ty });
        rd
  
      
  (* A cast from a type into some trait. *)
  | Cexpr_object { self; methods_key = { trait; _ } ; _ } ->
      let obj = do_convert ssa self in
      let ty = obj.ty in

      let trait_name = Basic_type_path.sexp_of_t trait |> S.to_string in
      let delta = Hashtbl.find trait_offset (ty, T_trait trait_name) in

      (* Temporary variables used in SSA *)
      let load = new_temp T_int in
      let vtb = new_temp T_bytes in
      let altered = new_temp T_bytes in

      (* Alter the vtable offset according to the trait *)
      Basic_vec.push ssa (Load { rd = vtb; rs = obj; offset = 0; byte = pointer_size });
      Basic_vec.push ssa (AssignInt { rd = load; imm = delta });
      Basic_vec.push ssa (Add { rd = altered; rs1 = vtb; rs2 = load });
      Basic_vec.push ssa (Store { rd = altered; rs = obj; offset = 0; byte = pointer_size });
      obj
  
  (* Primitives are intrinsic functions. *)
  (* We tidy some of these up, and compile others into functions. *)
  | Cexpr_prim { prim; args; ty; _ } ->
      let rd = new_temp ty in
      (match prim, args with
      | Psequand, [rs1; rs2] ->
          (* Short circuiting, compile into if-else *)
          (* rd = rs1 && rs2 -> rd = if (rs1) rs2 else false *)
          let ifso = new_label "sequand_if_" in
          let ifnot = new_label "sequand_else_" in
          let ifexit = new_label "sequand_exit_" in
          let t1 = new_temp T_bool in
          let t2 = new_temp T_bool in
          let cond = do_convert ssa rs1 in
          Basic_vec.push ssa (Branch { cond; ifso; ifnot });

          push_label ssa ifso;
          let rs = do_convert ssa rs2 in
          let ifso_from = !current_label in
          Basic_vec.push ssa (Assign { rd = t1; rs });
          Basic_vec.push ssa (Jump ifexit);

          push_label ssa ifnot;
          Basic_vec.push ssa (AssignInt { rd = t2; imm = 0; });
          Basic_vec.push ssa (Jump ifexit);

          push_label ssa ifexit;
          Basic_vec.push ssa (Phi { rd; rs = [(t1, ifso_from); (t2, ifnot) ]})

      | Psequor, [rs1; rs2] ->
          (* Short circuiting, compile into if-else *)
          (* rd = rs1 || rs2 -> rd = if (rs1) true else rs2 *)
          let ifso = new_label "sequor_if_" in
          let ifnot = new_label "sequor_else_" in
          let ifexit = new_label "sequor_exit_" in
          let t1 = new_temp T_bool in
          let t2 = new_temp T_bool in
          let cond = do_convert ssa rs1 in
          Basic_vec.push ssa (Branch { cond; ifso; ifnot });

          push_label ssa ifso;
          Basic_vec.push ssa (AssignInt { rd = t1; imm = 1; });
          Basic_vec.push ssa (Jump ifexit);

          push_label ssa ifnot;
          let rs = do_convert ssa rs2 in
          let ifnot_from = !current_label in
          Basic_vec.push ssa (Assign { rd = t2; rs });
          Basic_vec.push ssa (Jump ifexit);

          push_label ssa ifexit;
          Basic_vec.push ssa (Phi { rd; rs = [(t1, ifso); (t2, ifnot_from) ]})

      | _ -> 
          let args = List.map (fun expr -> do_convert ssa expr) args in
          deal_with_prim ssa rd prim args);
      rd

  | Cexpr_let { name; rhs; body; _ } ->
      let rs = do_convert ssa rhs in
      (match name with
      | Pmutable_ident _ ->
          (* We use `bytes` to represent arbitrary pointers. *)
          let space = new_temp T_bytes in
          let rd = { name = Ident.to_string name; ty = rs.ty } in
          Basic_vec.push ssa (Malloc { rd = space; size = sizeof rs.ty });
          Basic_vec.push ssa (Assign { rd; rs = space });
          Basic_vec.push ssa (Store { rd = rs; rs = rd; offset = 0; byte = sizeof rs.ty });
      
      | _ ->
          let rd = { name = Ident.to_string name; ty = rs.ty } in
          Basic_vec.push ssa (Assign { rd; rs }));
      do_convert ssa body

  | Cexpr_apply { func; args; ty; prim; _ } ->
      warn prim;
      let rd = new_temp ty in
      let fn = Ident.to_string func in
      let args = List.map (fun expr -> do_convert ssa expr) args in

      (* Alter the vtable offset to the corresponding trait *)
      let before = Basic_vec.empty () in
      let after = Basic_vec.empty () in
      (if Hashtbl.mem traited_args fn then
        let indices = Hashtbl.find traited_args fn in
        List.iter (fun (i, trait_ty) ->
          let arg = List.nth args i in
          if not (is_trait arg.ty) then (
            let delta = Hashtbl.find trait_offset (arg.ty, trait_ty) in

            (* Trait themselves can't derive another trait, *)
            (* so no worries about diamond inheritance *)
            let load = new_temp T_int in
            let vtb = new_temp T_bytes in
            let altered = new_temp T_bytes in
            let offset = -pointer_size in
            let byte = pointer_size in 

            (* Before calling, we must advance the pointer to the correct offset *)
            Basic_vec.push before (Load { rd = vtb; rs = arg; offset; byte });
            Basic_vec.push before (AssignInt { rd = load; imm = delta });
            Basic_vec.push before (Add { rd = altered; rs1 = vtb; rs2 = load });
            Basic_vec.push before (Store { rd = altered; rs = arg; offset; byte });

            (* After the function returns, we must put it back *)
            Basic_vec.push after (Store { rd = vtb; rs = arg; offset; byte })
          )
        ) indices
      );
      
      Basic_vec.append ssa before;
      Basic_vec.push ssa (Call { rd; fn; args });
      Basic_vec.append ssa after;
      rd


  | Cexpr_sequence { expr1; expr2; _ } ->
      do_convert ssa expr1 |> ignore;
      do_convert ssa expr2

  (* Meaning: access the `pos`-th field of `record` *)
  (* Here `record` might be a record type or a tuple *)
  | Cexpr_field { record; accessor; pos; ty; _ } ->
      let rd = new_temp ty in
      let rs = do_convert ssa record in
      let byte = sizeof ty in
      
      (match rs.ty with
        | T_constr _ ->
            let offset = offsetof rs.ty pos in
            Basic_vec.push ssa (Load { rd; rs; offset; byte });
            rd
          
        | T_tuple { tys } ->
            let precede = Basic_lst.take pos tys in
            let sizes = List.map sizeof precede in
            let offset = List.fold_left (fun acc x -> acc + x) 0 sizes in
            Basic_vec.push ssa (Load { rd; rs; offset; byte });
            rd
        
        | _ -> failwith "riscv_ssa.ml: bad record type");
    
  (* Meaning: set the `pos`-th field of `record` to `field` *)
  | Cexpr_mutate { record; pos; field } ->
    let rs = do_convert ssa record in
    let rd = do_convert ssa field in
    
    let offset = offsetof rs.ty pos in
    Basic_vec.push ssa (Store { rd; rs; offset; byte = sizeof rd.ty });
    unit

  (* TODO: Nested if's can cause wrong phi calls. *)
  (* Should move it to riscv_opt.ml, where all basic blocks are emitted. *)
  | Cexpr_if { cond; ifso; ifnot; ty; _ } ->
      let rd = new_temp ty in

      let cond = do_convert ssa cond in

      let ifso_ssa = Basic_vec.empty () in 
      let ifnot_ssa = Basic_vec.empty () in


      let ifso_label = new_label "ifso_" in
      let ifnot_label = new_label "ifnot_" in
      let ifexit_label = new_label "ifexit_" in

      (*
        Compiling into:
      
          br %cond true:%ifso false:%ifnot
        
        ifso:
          ...
          jump ifexit
        
        ifnot:
          ...
          jump ifexit
        
        ifexit:
          %rd = φ %ifso_result[ifso] %ifnot_result[ifnot]
      *)

      Basic_vec.push ssa (Branch { cond; ifso = ifso_label; ifnot = ifnot_label });

      push_label ssa ifso_label;
      let ifso_result = do_convert ifso_ssa ifso in
      let ifso_from = !current_label in
      Basic_vec.append ssa ifso_ssa;
      Basic_vec.push ssa (Jump ifexit_label);

      push_label ssa ifnot_label;
      let ifnot_result =
        (match ifnot with
        | None -> unit
        | Some x -> do_convert ifnot_ssa x)
      in
      let ifnot_from = !current_label in
      Basic_vec.append ssa ifnot_ssa;
      Basic_vec.push ssa (Jump ifexit_label);

      push_label ssa ifexit_label;
      Basic_vec.push ssa (Phi
        { rd; rs = [(ifso_result, ifso_from); (ifnot_result, ifnot_from)] });
      
      rd

  (*
    In MoonBit core IR, loops are by default not looping.
    They only jump to beginning when they meet `Cexpr_continue`,
    in which case their `args` will be substituted by the `args` provided there,
    and the loop entry condition will be tested again.

    Therefore the loop is compiled as follows:

      before:
        # evaluate args
        jump head

      loop:
        %arg = φ %arg[before] %arg1[cont1] ... %argn[contn]
        ...
        jump exit

      exit:

    A good thing is that loops don't return a value. We don't need to insert
    φ after the label `exit`.
  *)
  | Cexpr_loop { params; body; args; label; _ } ->
      (* We need to use the global variable `conts`. *)
      (* In case there's an outer loop, we might have tampered it; *)
      (* So we must store the contents somewhere. *)
      let old_conts = !conts in

      (* Get the labels *)
      let loop = Printf.sprintf "%s_%d" label.name label.stamp in
      let before = Printf.sprintf "before_%s" loop in
      let exit = Printf.sprintf "exit_%s" loop in

      (* Start generating according to the template described above. *)
      
      (* Generate `before`. *)

      Basic_vec.push ssa (Jump before);
      push_label ssa before;
      let before_from = !current_label in
      let results = List.map (do_convert ssa) args in
      let cont = List.map (fun x -> (x, before_from)) results in
      conts := cont :: !conts;
      Basic_vec.push ssa (Jump loop);

      (* Calculate the φ-call. *)

      push_label ssa loop;

      (* Generate loop body. `conts` will be filled by Cexpr_continue. *)
      let body_ssa = Basic_vec.empty () in
      let _ = do_convert body_ssa body in

      let rec transpose lst =
        match lst with
        | [] -> []
        | [] :: _ -> []
        | _ -> List.map List.hd lst :: transpose (List.map List.tl lst)
      in

      let grouped = transpose !conts in
      let gen_phi (par: Mcore.param) rs =
        Phi { rd = { name = Ident.to_string par.binder; ty = par.ty }; rs }
      in
      
      let phis = List.map2 gen_phi params grouped in
      List.iter (fun x -> Basic_vec.push ssa x) phis;

      (* Generate rest parts. *)

      Basic_vec.append ssa body_ssa;
      Basic_vec.push ssa (Jump exit);
      push_label ssa exit;

      (* Store `conts` back; let outer loop go on normally. *)
      conts := old_conts;
      unit

  (* See the explanation for Cexpr_loop. *)
  | Cexpr_continue { args; label } ->
      (* Generate a label, and let the previous block jump to this block. *)
      let cont = new_label "continue_" in
      Basic_vec.push ssa (Jump cont);
      push_label ssa cont;

      (* Evaluate arguments and update `conts`. *)
      let results = List.map (do_convert ssa) args in
      let cont_from = !current_label in
      let new_cont = List.map (fun x -> (x, cont_from)) results in
      conts := new_cont :: !conts;

      (* Jump back to the beginning of the loop. *)
      let loop_name = Printf.sprintf "%s_%d" label.name label.stamp in 
      Basic_vec.push ssa (Jump loop_name);
      unit

  (* Assigns mutable variables. *)
  | Cexpr_assign { var; expr; ty } ->
      let rd = do_convert ssa expr in
      let rs = { name = Ident.to_string var; ty = T_bytes } in
      Basic_vec.push ssa (Store { rd; rs; offset = 0; byte = sizeof rd.ty });
      unit

  (* Builds a record type. *)
  | Cexpr_record { fields; ty; } ->
      (* Allocate space for the record *)
      let rd = new_temp ty in

      let has_vtable = Hashtbl.mem trait_table ty in
      let size = Hashtbl.find size_table ty in

      (if has_vtable then
        let beginning = new_temp ty in
        let load = new_temp T_int in

        (* We construct vtable before every field *)
        (* and let `rd` point at where fields start *)
        (* in order to unite traited and untraited types *)
        Basic_vec.push ssa (Malloc { rd = beginning; size = size + pointer_size });
        Basic_vec.push ssa (AssignInt { rd = load; imm = pointer_size });
        Basic_vec.push ssa (Add { rd; rs1 = beginning; rs2 = load });

        (* Load in vtable *)
        let vtb = new_temp T_bytes in
        let label = Printf.sprintf "vtable_%s" (to_string ty |> remove_space) in
        Basic_vec.push ssa (AssignLabel { rd = vtb; imm = label });
        Basic_vec.push ssa (Store { rd = vtb; rs = rd; offset = -pointer_size; byte = pointer_size })
      else
        (* No vtable; everything normal *)
        Basic_vec.push ssa (Malloc { rd; size });
      );

      (* Construct all its fields *)
      let visit ({ pos; expr; _ }: Mcore.field_def) =
        let result = do_convert ssa expr in
        let offset = offsetof ty pos in 
        Basic_vec.push ssa (Store { rd = result; rs = rd; offset; byte = sizeof result.ty })
      in

      List.iter visit fields;
      rd

  | Cexpr_break { label; _ } ->
      (* Jumps to exit of the loop. *)
      let loop_name = Printf.sprintf "%s_%d" label.name label.stamp in
      Basic_vec.push ssa (Jump ("exit_" ^ loop_name));
      unit

  | Cexpr_tuple { exprs; ty; _ } ->
      let rd = new_temp ty in
      let tys =
        (match ty with
        | T_tuple { tys } -> tys
        | _ -> failwith "riscv_ssa.ml: bad tuple")
      in

      let size = List.fold_left (fun acc x -> sizeof x + acc) 0 tys in
      Basic_vec.push ssa (Malloc { rd; size });

      let args = List.map (fun x -> do_convert ssa x) exprs in
      let sizes = List.map (fun x -> sizeof x.ty) args |> Basic_lst.take (List.length args - 1) in
      let offsets = 0 :: Basic_lst.cumsum sizes in
      List.iter2 (fun arg offset ->
        Basic_vec.push ssa (Store { rd = arg; rs = rd; offset; byte = sizeof ty })
      ) args offsets;
      rd

  | Cexpr_return _ ->
      prerr_endline "return";
      unit

  | Cexpr_letfn _ ->
      prerr_endline "letfn";
      unit

  | Cexpr_function _ ->
      prerr_endline "function";
      unit

  | Cexpr_constr _ ->
      prerr_endline "constr";
      unit

  | Cexpr_letrec _ ->
      prerr_endline "letrec";
      unit

  | Cexpr_record_update _ ->
      prerr_endline "record_update";
      unit

  | Cexpr_switch_constr _ ->
      prerr_endline "switch constr";
      unit

  | Cexpr_switch_constant _ ->
      prerr_endline "switch constant";
      unit

  | Cexpr_handle_error _ ->
      prerr_endline "handle error";
      unit

  | Cexpr_array _ ->
      prerr_endline "array";
      unit

  | Cexpr_const { c; ty; _ } ->
      let rd = new_temp ty in
      (match c with
      (* Note each element of string is 2 bytes long. TODO *)
      | C_string v ->
          let label = Printf.sprintf "str_%d" !slot in
          let vals = String.to_seq v |> List.of_seq in
          let len = String.length v |> Int.to_string in
          let vec = Basic_vec.empty () in

          List.iter (fun x ->
            Basic_vec.push vec (Char.code x);
            Basic_vec.push vec 0) vals;
          let values = len :: Basic_vec.map_into_list vec Int.to_string in

          slot := !slot + 1;
          Basic_vec.push global_inst (ExtArray { label; values });

          (* Let the pointer point to beginning of data, rather than the length section *)
          let beginning = new_temp T_bytes in
          Basic_vec.push ssa (ExtArray { label; values });
          Basic_vec.push ssa (AssignLabel { rd = beginning; imm = label; });
          Basic_vec.push ssa (Addi { rd; rs = beginning; imm = 4 })

      | C_bytes { v; _ } ->
          let label = Printf.sprintf "bytes_%d" !slot in
          let vals = String.to_seq v |> List.of_seq |> List.map (fun x -> Char.code x |> Int.to_string) in
          let len = String.length v |> Int.to_string in
          let values = len :: vals in

          slot := !slot + 1;
          Basic_vec.push global_inst (ExtArray { label; values });

          (* Let the pointer point to beginning of data, rather than the length section *)
          let beginning = new_temp T_bytes in
          Basic_vec.push ssa (ExtArray { label; values });
          Basic_vec.push ssa (AssignLabel { rd = beginning; imm = label; });
          Basic_vec.push ssa (Addi { rd; rs = beginning; imm = 4 })
  
      | C_int64 { v; _ } | C_uint64 { v; _ }  ->
          Basic_vec.push ssa (AssignInt64 { rd; imm = v });
      | C_bool imm ->
          Basic_vec.push ssa (AssignInt { rd; imm = if imm then 1 else 0; })
      | C_char imm ->
          Basic_vec.push ssa (AssignInt { rd; imm = Uchar.to_int imm; })
      | C_int { v; _ } | C_uint { v; _ }  ->
          Basic_vec.push ssa (AssignInt { rd; imm = Int32.to_int v; })
      | C_float { v; _ } ->
          Basic_vec.push ssa (AssignFP { rd; imm = v; })
      | C_double { v; _ } ->
          Basic_vec.push ssa (AssignFP { rd; imm = v; })
      | C_bigint _ -> failwith "TODO: riscv_ssa.ml: bigint not supported"
      );
      rd

let generate_vtables () =
  Hashtbl.iter (fun ty methods ->
    let label_raw = Printf.sprintf "vtable_%s" (to_string ty) in
    let label = remove_space label_raw in
    Basic_vec.push global_inst (ExtArray { label; values = Basic_vec.to_list methods })
  ) trait_table

(**
Converts given `expr` into a list of SSA instructions,
along with the variable in which the result of this expression is stored.
*)
let convert_expr (expr: Mcore.expr) =
  let ssa = Basic_vec.empty () in
  let return = do_convert ssa expr in
  Basic_vec.push ssa (Return return);
  Basic_vec.map_into_list ssa (fun x -> x)


let convert_toplevel _start (top: Mcore.top_item) =
  let var_of_param ({ binder; ty; _ } : Mcore.param) =
    { name = Ident.to_string binder; ty }
  in

  match top with
  | Ctop_fn { binder; func; export_info_; _ } ->
      let fn = Ident.to_string binder in
      let args = List.map var_of_param func.params in
      let body = convert_expr func.body in

      (* Record the index of arguments that are traits *)
      let traited = Basic_vec.empty () in
      List.iteri (fun i x ->
        if is_trait x.ty then Basic_vec.push traited (i, x.ty))
      args;

      if Basic_vec.length traited != 0 then
        Hashtbl.add traited_args fn (Basic_vec.to_list traited);

      if export_info_ != None then
        prerr_endline "warning: export info is non-empty";
      [ FnDecl { fn; args; body } ]

  | Ctop_let { binder; expr; is_pub_; _ } ->
      let name = Ident.to_string binder in
      global_vars := Varset.add name !global_vars;

      let rd = do_convert _start expr in
      let var = { name = Ident.to_string binder; ty = rd.ty } in

      Basic_vec.push global_inst (Store { rd; rs = var; offset = 0; byte = sizeof rd.ty });
      [ GlobalVarDecl var ]

  (* Stubs are references to internal functions. *)
  | Ctop_stub { func_stubs; binder; _ } ->
      (match func_stubs with
      | Internal { func_name = fn; } ->
          let (args, body) = Riscv_internals.get fn in
          [ FnDecl { fn = Ident.to_string binder; args; body } ]

      | Inline_code_text _
      | Inline_code_sexp _ -> failwith "RISC-V target does not support inline WASM"
      | Import _ -> failwith "riscv_ssa.ml: import should have been eliminated in link stage")
  
  | _ -> failwith "TODO: riscv_ssa.ml: don't know this toplevel"

let ssa_of_mcore (core: Mcore.t) =
  let out = Printf.sprintf "%s.ir" !Driver_config.Linkcore_Opt.output_file  in
  Basic_io.write_s out (Mcore.sexp_of_t core);

  (* Body of the function `_start`, which is the entry point *)
  let _start = Basic_vec.empty () in

  (* Look through types and calculate their field offsets *)
  update_types core.types;

  (* Look through traits and their implementations *)
  record_traits core.object_methods;

  (* Deal with ordinary functions *)
  let body = List.map (fun x -> convert_toplevel _start x) core.body |> List.flatten in

  (* Deal with main *)
  let with_main = match core.main with
    | Some (main_expr, _) ->
        let main_body = convert_expr main_expr in
        let main_decl = FnDecl { fn = "main"; args = []; body = main_body } in
        main_decl :: body
      
    | None -> body
  in

  (* Add _start *)
  let unused = new_temp T_unit in
  Basic_vec.push _start (Call { rd = unused; fn = "main"; args = [] });
  Basic_vec.push _start (Return unused);

  let start_body = Basic_vec.to_list _start in
  let with_start = FnDecl { fn = "_start"; args = []; body = start_body } :: with_main in

  (* Add global declarations and variables *)
  generate_vtables ();
  let with_vtables = (Basic_vec.to_list global_inst) @ with_start in
  with_vtables
