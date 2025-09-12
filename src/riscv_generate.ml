(**
This file actually generates TAC (3-address code).
It is converted to tac in optimization phase.
*)

open Riscv_ssa
module Vec = Basic_vec

(** Names of all functions. *)
let fn_names = ref Stringset.empty

let global_vars = ref Stringset.empty

(** The function/closure we're currently dealing with *)
let current_function = ref ""

(** The origin current function name. Used for judge whether a closure calls itself *)
let current_base_name = ref ""

(** The environment (i.e. the pointer passed to the closure) of current closure *)
let current_env = ref unit

(** Global instructions, i.e. GlobalVarDecl and ExtArray *)
let global_inst = Vec.empty ()

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

(** All captured variables for each closure. *)
let captured = Hashtbl.create 64

(** Offsets for fields in each variant in each enum type. *)
let variants = Hashtbl.create 64

(** The enum type that each variant belongs to. *)
let belong = Hashtbl.create 64

(** Init function exprs**)
let init_exprs = Vec.empty ()

(** Get offset of the `pos`-th field in the record type called `name`. *)
(** Note that there are n+1 fields for offsets in a n-field variant; the last one is the total size. *)
let offsetof ty pos = Hashtbl.find offset_table (ty, pos)

let is_trait ty = match ty with
| Mtype.T_trait _ -> true
| _ -> false

(** Label of current Cexpr_letfn with kind = Join. *)
let current_join = ref ""

(** Return value of current_join. *)
let current_join_ret = ref unit

(** For a type, get its name without qualifier *)
let nameof (x: Mtype.t) = match x with
| T_constr id -> id
| T_trait id -> id
| _ -> failwith "riscv_generate.ml: type does not have a user-defined name"

let remove_space = String.map (fun c -> if c = ' ' then '_' else c)

let revert_optimized_option_type = function 
| Mtype.T_optimized_option { elem } -> 
    (match elem with 
    | Mtype.T_char | T_bool | T_unit | T_byte -> Mtype.T_int
    | Mtype.T_int | T_uint -> Mtype.T_int64
    | _ -> failwith (Printf.sprintf "riscv_generate.ml: bad optimized_option type %s in revert" (Mtype.to_string elem)))
| otherwise -> otherwise

(** Push the correct sequence of instruction based on primitives. *)
let deal_with_prim tac rd (prim: Primitive.prim) args =
  let die () =
    failwith "riscv_generate.ml: bad primitive format"
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
      Vec.push tac op
  
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
      Vec.push tac op

  | Pbitwise { operand_type; operator } ->
      let op = (match operator, args with
      | Not, [rs1] -> (Not { rd; rs1 })
      | And, [rs1; rs2] -> (And { rd; rs1; rs2 })
      | Or, [rs1; rs2] -> (Or { rd; rs1; rs2 })
      | Xor, [rs1; rs2] -> (Xor { rd; rs1; rs2 })
      | Shl, [rs1; rs2] -> (Sll { rd; rs1; rs2 })
      | Shr, [rs1; rs2] -> 
          if rs1.ty = Mtype.T_uint || rs1.ty = T_uint64 then
            (Srl { rd; rs1; rs2 })
          else
            (Sra { rd; rs1; rs2 })

      (* For ease of implementation, let's just rely on GCC builtins *)
      | Popcnt, _ -> (CallExtern { rd; fn = "__builtin_popcount"; args })
      | Ctz, _ -> (CallExtern { rd; fn = "__builtin_ctz"; args })
      | Clz, _ -> (CallExtern { rd; fn = "__builtin_clz"; args })
      | _ -> die()) in
      Vec.push tac op

  | Pconvert { kind; from; to_ } ->
      (* Reinterpret means the register value should remain the same *)
      if kind == Primitive.Reinterpret then
        Vec.push tac (Assign { rd; rs = List.hd args })

      (* But convert is where we must take some action *)
      else
        let arg = List.hd args in
        (* let arg = { arg with ty = revert_optimized_option_type arg.ty } in *)
        (match from, to_ with
        | I32, U8 | U32, U8 ->
            (* Discard higher bits by masking them away *)
            Vec.push tac (Andi { rd; rs = arg; imm = 255 })

        | U64, U32 ->
            (* Discard higher bits by shifting *)
            let temp = new_temp Mtype.T_uint64 in
            Vec.push tac (Slli { rd = temp; rs = arg; imm = 32 });
            Vec.push tac (Srli { rd; rs = temp; imm = 32 })

        | I64, I32 | U64, I32 ->
            (* Discard higher bits by shifting, but arithmetic *)
            let temp = new_temp Mtype.T_uint64 in
            Vec.push tac (Slli { rd = temp; rs = arg; imm = 32 });
            Vec.push tac (Srai { rd; rs = temp; imm = 32 })

        | I32, U32 | U32, I32 | I32, I64 ->
            (* Simply do nothing *)
            Vec.push tac (Assign { rd; rs = arg });

        | U32, U64 | U32, I64 ->
            (* Must erase the signed bits *)
            let temp = new_temp Mtype.T_uint64 in
            Vec.push tac (Slli { rd = temp; rs = arg; imm = 32 });
            Vec.push tac (Srli { rd; rs = temp; imm = 32 })
        
        | _ -> die())

  (* The argument is whether we perform bound checks. *)
  (* My observation is that this argument is always Unsafe; *)
  (* they are done in an earlier stage in MoonBit core IR. *)
  (* Hence we don't generate bound check code here. *)
  | Pfixedarray_get_item _ ->
      if List.length args <> 2 then
        failwith "riscv_ssa.ml: bad call to 'Pfixedarray_get_item'"
      else
        let arr = List.nth args 0 in
        let index = List.nth args 1 in
        let ty =
          (match arr.ty with
          | Mtype.T_fixedarray { elem } -> elem
          | _ -> failwith "riscv_ssa.ml: bad type in fixedarray_get_item")
        in
        let size = sizeof ty in
        let addr = new_temp Mtype.T_bytes in
        let sz = new_temp Mtype.T_int in
        let offset = new_temp Mtype.T_int in

        (* Same as `rd = *(arr + sizeof(T) * index)` *)
        Vec.push tac (AssignInt { rd = sz; imm = size; });
        Vec.push tac (Mul { rd = offset; rs1 = sz; rs2 = index });
        Vec.push tac (Add { rd = addr; rs1 = arr; rs2 = offset });
        Vec.push tac (Load { rd; rs = addr; offset = 0; byte = size });

  | Pfixedarray_set_item _ ->
      let arr = List.nth args 0 in
      let index = List.nth args 1 in
      let value = List.nth args 2 in
      let size = sizeof value.ty in

      (* Temporary variables *)
      let addr = new_temp Mtype.T_bytes in
      let sz = new_temp Mtype.T_int in
      let offset = new_temp Mtype.T_int in
      Vec.push tac (AssignInt { rd = sz; imm = size });
      Vec.push tac (Mul { rd = offset; rs1 = index; rs2 = sz });
      Vec.push tac (Add { rd = addr; rs1 = arr; rs2 = offset });
      Vec.push tac (Store { rd = value; rs = addr; offset = 0; byte = size });
      Vec.push tac (Assign { rd; rs = unit })

  | Pfixedarray_make { kind } ->
      (match kind with
      | Uninit ->
          let len = List.hd args in
          let ty =
            (match rd.ty with
            | Mtype.T_fixedarray { elem } -> elem
            | _ -> failwith "riscv_ssa.ml: bad type in fixedarray_make")
          in
          let size = sizeof ty in
          
          let sz = new_temp Mtype.T_int in
          let datlen = new_temp Mtype.T_int in
          let total = new_temp Mtype.T_int in
          let space = new_temp Mtype.T_bytes in

          (* Malloc 4 extra bytes aprat from the data part *)
          Vec.push tac (AssignInt { rd = sz; imm = size; });
          Vec.push tac (Mul { rd = datlen; rs1 = sz; rs2 = len });
          Vec.push tac (Addi { rd = total; rs = datlen; imm = 4 });
          Vec.push tac (CallExtern { rd = space; fn = "malloc"; args = [total] });

          (* Store the length *)
          Vec.push tac (Store { rd = len; rs = space; offset = 0; byte = 4 });
          Vec.push tac (Addi { rd; rs = space; imm = 4 })


      | LenAndInit ->
          let len = List.nth args 0 in
          let init = List.nth args 1 in
          let size = sizeof init.ty in

          let sz = new_temp Mtype.T_int in
          let datlen = new_temp Mtype.T_int in
          let total = new_temp Mtype.T_int in
          let space = new_temp Mtype.T_bytes in
          
          (* First do a `malloc` same as the previous branch *)
          (* Malloc 4 extra bytes aprat from the data part *)
          Vec.push tac (AssignInt { rd = sz; imm = size; });
          Vec.push tac (Mul { rd = datlen; rs1 = sz; rs2 = len });
          Vec.push tac (Addi { rd = total; rs = datlen; imm = 4 });
          Vec.push tac (CallExtern { rd = space; fn = "malloc"; args = [total] });

          (* Store the length *)
          Vec.push tac (Store { rd = len; rs = space; offset = 0; byte = 4 });
          Vec.push tac (Addi { rd; rs = space; imm = 4 });

          (* Then generate a loop to fill things in:
            before:
              li %1 0
              j loop

            loop:
              phi i %1 before %2 body
              le %3 i len
              br %3 body exit

            body:
              mul %4 i sz
              add %5 rd %4
              s? init %5 0
              addi %2 i %1
              j loop

            exit:
          *)
          let _1 = new_temp Mtype.T_int in
          let _2 = new_temp Mtype.T_int in
          let _3 = new_temp Mtype.T_bool in
          let _4 = new_temp Mtype.T_int in
          let _5 = new_temp Mtype.T_bytes in
          let i = new_temp Mtype.T_int in

          let before = new_label "before_" in
          let loop = new_label "loop_" in
          let body = new_label "body_" in
          let exit = new_label "exit_" in

          Vec.push tac (Jump before);

          Vec.push tac (Label before);
          Vec.push tac (AssignInt { rd = _1; imm = 0 });
          Vec.push tac (Jump loop);

          Vec.push tac (Label loop);
          Vec.push tac (Phi { rd = i; rs = [(_1, before); (_2, body)] });
          Vec.push tac (Less { rd = _3; rs1 = i; rs2 = len });
          Vec.push tac (Branch { cond = _3; ifso = body; ifnot = exit });

          Vec.push tac (Label body);
          Vec.push tac (Mul { rd = _4; rs1 = i; rs2 = sz });
          Vec.push tac (Add { rd = _5; rs1 = rd; rs2 = _4 });
          Vec.push tac (Store { rd = init; rs = _5; offset = 0; byte = size });
          Vec.push tac (Addi { rd = _2; rs = i; imm = 1 });
          Vec.push tac (Jump loop);

          Vec.push tac (Label exit);

      | _ -> failwith "riscv_ssa.ml: unrecognized config of fixed array make")
  
  (* The list of arguments are the contents of the array. *)
  (* This primitive constructs a `@builtin.Array` record; *)
  (* It is like { void* buf; int len; }. *)
  | Parray_make ->
      let length = List.length args in
      if length = 0 then
        (* We can't get element size, so special treatment. *)
        let len = new_temp Mtype.T_int in
        let buf = new_temp Mtype.T_bytes in
        Vec.push tac (Malloc { rd; size = 12 });
        Vec.push tac (AssignInt { rd = len; imm = 0 });
        Vec.push tac (Store { rd = len; rs = rd; offset = 8; byte = 4 });
        Vec.push tac (Malloc { rd = buf; size = 0 });
        Vec.push tac (Store { rd = buf; rs = rd; offset = 0; byte = 8 })
      else (
        let elem_size = sizeof (List.hd args).ty in
        let buf_size = if List.length args = 0 then 0 else length * (sizeof (List.hd args).ty) in

        let len = new_temp Mtype.T_int in
        let buf = new_temp Mtype.T_bytes in

        (* Construct a @builtin.Array *)
        Vec.push tac (Malloc { rd; size = 12 });

        (* Store length *)
        Vec.push tac (AssignInt { rd = len; imm = length });
        Vec.push tac (Store { rd = len; rs = rd; offset = 8; byte = 4 });

        (* Generate a buffer and fill in its content with given arguments *)
        (* The buffer must also store a length *)
        let space = new_temp Mtype.T_bytes in
        Vec.push tac (Malloc { rd = space; size = buf_size + 4 });

        (* Store length into buffer *)
        Vec.push tac (Store { rd = len; rs = space; offset = 0; byte = 4 });

        (* Advance the pointer and store all elements *)
        Vec.push tac (Addi { rd = buf; rs = space; imm = 4 });
        List.iteri (fun i x ->
          Vec.push tac (Store { rd = x; rs = buf; offset = i * elem_size; byte = elem_size })
        ) args;
        Vec.push tac (Store { rd = buf; rs = rd; offset = 0; byte = 8 })
      )


  | Pcall_object_method { method_index; _ } ->
      (* We've guaranteed that the vtable pointer is pointing to the correct place. *)
      (* So it's just calling index * ptr_size + *vtb. *)
      let vtb_offset = method_index * pointer_size in
      let arg = List.hd args in

      (* Temporaries in SSA *)
      let vtb = new_temp Mtype.T_bytes in
      let fn_addr = new_temp Mtype.T_bytes in
      let fptr = new_temp Mtype.T_bytes in

      (* Here fptr = vtb[offset] = *(vtb + offset) *)
      Vec.push tac (Load { rd = vtb; rs = arg; offset = -pointer_size; byte = pointer_size });
      Vec.push tac (Addi { rd = fn_addr; rs = vtb; imm = vtb_offset });
      Vec.push tac (Load { rd = fptr; rs = fn_addr; offset = 0; byte = pointer_size });

      (* The whole set of args (including self) is needed. *)
      Vec.push tac (CallIndirect { rd; rs = fptr; args })

  | Pgetbytesitem { safe = _ } ->
      let str = List.nth args 0 in
      let i = List.nth args 1 in
      
      let altered = new_temp Mtype.T_string in
      Vec.push tac (Add { rd = altered; rs1 = str; rs2 = i });
      Vec.push tac (Load { rd; rs = altered; offset = 0; byte = 1 })

  | Psetbytesitem { safe = _ } ->
      let str = List.nth args 0 in
      let i = List.nth args 1 in
      let item = List.nth args 2 in
      
      let altered = new_temp Mtype.T_string in
      Vec.push tac (Add { rd = altered; rs1 = str; rs2 = i });
      Vec.push tac (Store { rd = item; rs = altered; offset = 0; byte = 1 });
      Vec.push tac (Assign { rd; rs = unit })

  (* Be cautious that each `char` is 2 bytes long, which is extremely counter-intuitive. *)
  | Pgetstringitem { safe = _ } ->
      let str = List.nth args 0 in
      let i = List.nth args 1 in
      
      let two = new_temp Mtype.T_int in
      let offset = new_temp Mtype.T_int in
      let altered = new_temp Mtype.T_string in
      Vec.push tac (AssignInt { rd = two; imm = 2 });
      Vec.push tac (Mul { rd = offset; rs1 = i; rs2 = two });
      Vec.push tac (Add { rd = altered; rs1 = str; rs2 = offset });
      Vec.push tac (Load { rd; rs = altered; offset = 0; byte = 2 })

  (* Length are all stored at the same place for these arrays. *)
  | Pstringlength
  | Pbyteslength
  | Pfixedarray_length ->
      let bytes = List.hd args in
      Vec.push tac (Load { rd; rs = bytes; offset = -4; byte = 4 })

  (* We must put length information in bytes when we make it, *)
  (* but for strings we rely on `strlen` *)
  | Pmakebytes ->
      let len = List.nth args 0 in
      let init = List.nth args 1 in

      (* Let the pointer point to beginning of data, rather than the length section *)
      let memory = new_temp Mtype.T_bytes in
      let unused = new_temp Mtype.T_unit in
      let new_len = new_temp Mtype.T_int in
      Vec.push tac (Addi { rd = new_len; rs = len; imm = 4 });
      Vec.push tac (CallExtern { rd = memory; fn = "malloc"; args = [ new_len ] });
      Vec.push tac (Store { rd = len; rs = memory; offset = 0; byte = 4 });
      Vec.push tac (Addi { rd; rs = memory; imm = 4 });
      Vec.push tac (CallExtern { rd = unused; fn = "memset"; args = [ rd; init; len ] });

  (* Get the field of a certain enum variant *)
  (* The type of variant is in `tag`, and `index` indicates the field of it *)
  | Penum_field { index; tag } ->
      (* First find all the offsets *)
      let arg = List.hd args in
      let name = nameof arg.ty in
      let name = Hashtbl.find belong name in
      let tag_offsets = Hashtbl.find variants name in

      (* Get the index of the variant within the enum type *)
      let ind =
        (match tag with Constr_tag_regular { index } | Extensible_tag { index } -> index)
      in
      let offsets = List.nth tag_offsets ind in
      let offset = List.nth offsets index in
      (* This won't go off bound because n-fielded variant has n+1 numbers in the list *)
      let size = List.nth offsets (index + 1) - offset in

      (* Load from the offset plus 4 for the tag *)
      Vec.push tac (Load { rd; rs = arg; offset = offset + 4; byte = size })

  (* There is a bunch of `Pccall`s used in primitive.ml *)
  (* We'll match each function name separately *)
  | Pccall { func_name = "add_string" } ->
      let s1 = List.nth args 0 in
      let s2 = List.nth args 1 in

      (* First load their lengths *)
      let l1 = new_temp Mtype.T_int in
      let l2 = new_temp Mtype.T_int in
      Vec.push tac (Load { rd = l1; rs = s1; offset = -4; byte = 4 });
      Vec.push tac (Load { rd = l2; rs = s2; offset = -4; byte = 4 });

      (* Multiply them by sizeof(char) *)
      let char_size = new_temp Mtype.T_int in
      let new_len = new_temp Mtype.T_int in
      Vec.push tac (AssignInt { rd = char_size; imm = sizeof Mtype.T_char });
      Vec.push tac (Add { rd = new_len; rs1 = l2; rs2 = l1 });

      (* Calculate the new length *)
      let space_size = new_temp Mtype.T_int in
      Vec.push tac (Mul { rd = space_size; rs1 = new_len; rs2 = char_size });
      Vec.push tac (Addi { rd = space_size; rs = space_size; imm = sizeof Mtype.T_int });

      (* Then allocate the correct amount of space *)
      let space = new_temp Mtype.T_bytes in
      Vec.push tac (CallExtern { rd = space; fn = "malloc"; args = [space_size] });

      (* Store the length and add the offset *)
      Vec.push tac (Store { rd = new_len; rs = space; offset = 0; byte = 4 });
      Vec.push tac (Addi { rd = space; rs = space; imm = 4 });

      (* Now copy the values into it *)
      Vec.push tac (Mul { rd = l1; rs1 = l1; rs2 = char_size });
      Vec.push tac (Mul { rd = l2; rs1 = l2; rs2 = char_size });

      Vec.push tac (CallExtern { rd = unit; fn = "memcpy"; args = [space; s1; l1] });

      let ptr = new_temp Mtype.T_bytes in
      Vec.push tac (Add { rd = ptr; rs1 = space; rs2 = l1 });
      Vec.push tac (CallExtern { rd = unit; fn = "memcpy"; args = [ptr; s2; l2] });

      (* Store the space into rd *)
      Vec.push tac (Assign { rd; rs = space })

  | Pignore ->
      Vec.push tac (Assign { rd; rs = unit })

  (* Calculates whether two references are equal; gets a boolean value *)
  | Prefeq ->
      let a = List.nth args 0 in
      let b = List.nth args 1 in
      
      let temp = new_temp Mtype.T_uint64 in
      Vec.push tac (Xor { rd = temp; rs1 = a; rs2 = b });
      Vec.push tac (Slti { rd; rs = temp; imm = 1 })

  (* Create a null-pointer. *)
  | Pnull ->
      Vec.push tac (AssignInt64 { rd; imm = 0L })

  | Pis_null ->
      let zero = new_temp Mtype.T_bytes in
      Vec.push tac (AssignInt64 { rd = zero; imm = 0L });
      Vec.push tac (Eq { rd; rs1 = List.hd args; rs2 = zero });

  | Ppanic ->
      Vec.push tac (CallExtern { rd; fn = "abort"; args })

  (* ref.as_non_null in WASM is just a copy *)
  | Pas_non_null
  | Pidentity ->
      Vec.push tac (Assign { rd; rs = List.hd args })
  
  | Pprintln ->
      Vec.push tac (CallExtern { rd; fn = "puts"; args })
  | Pnot -> 
      Vec.push tac (Not { rd; rs1 = List.hd args })
  | Pstringequal -> 
      let cmp_res = new_temp Mtype.T_int in
      let zero = new_temp Mtype.T_bytes in
      Vec.push tac (CallExtern { rd = cmp_res; fn = "strcmp"; args });
      Vec.push tac (AssignInt64 { rd = zero; imm = 0L });
      Vec.push tac (Eq { rd; rs1 = cmp_res; rs2 = zero });

  (* | Primitive.Pccall { func_name = "";_} -> _
  | Primitive.Praise -> _
  | Primitive.Punreachable -> _
  | Primitive.Pcatch -> _
  | Primitive.Pclosure_to_extern_ref -> _
  | Primitive.Pnull_string_extern -> _
  | Primitive.Perror_to_string -> _
  | Primitive.Pany_to_string -> _
  | Primitive.Pintrinsic -> _
  | Primitive.Pcast -> _
  | Primitive.Pcompare -> _
  | Primitive.Pset_enum_field -> _
  | Primitive.Pmake_value_or_error *)
  | _ -> failwith (Printf.sprintf "unknown primitive %s" (Primitive.sexp_of_prim prim |> S.to_string))


(** Extract information from types and store them in global variables. *)
let update_types ({ defs; _ }: Mtype.defs) =
  let types = Mtype.Id_hash.to_list defs in

  let visit (name, (info: Mtype.info)) =
    match info with
    | Placeholder -> ()
    | Externref -> ()

    (* We don't care about declarations in traits. *)
    | Trait _ -> ()

    (* Calculate offset of fields in record types. *)
    | Record { fields } -> 
        let ty = Mtype.T_constr name in
        let extract (x: Mtype.field_info) = x.field_type in
        let field_types = List.map extract fields in
        let field_sizes = List.map sizeof field_types in
        let offset = ref 0 in
        let offsets = List.map (fun x -> let y = !offset in offset := x + !offset; y) field_sizes in
        List.iteri (fun i x -> Hashtbl.add offset_table (ty, i) x) offsets;
        Hashtbl.add size_table ty !offset;
        Hashtbl.add trait_table ty (Vec.empty ())

    (* Variant is a single possibility of Variant_constr *)
    | Variant_constr -> ()

    (* An enum type, also called sum type *)
    | Variant { constrs } ->
        let tag_offsets = Vec.empty () in
        List.iteri (fun i ({ payload; tag }: Mtype.constr_info) ->
          let types = List.map (fun (x: Mtype.field_info) -> x.field_type) payload in
          let sizes = List.map (fun x -> sizeof x) types in
          let offsets = 0 :: Basic_lst.cumsum sizes in

          (* Record the correspondence between nameand index *)
          let variant_name = Printf.sprintf "%s.%s" name tag.name_ in
          Hashtbl.add belong variant_name name;
          Vec.push tag_offsets offsets
        ) constrs;
        Hashtbl.add variants name (tag_offsets |> Vec.to_list)

    | Constant_variant_constr -> failwith "riscv_generate.ml: cannot handle this type"
  in
  List.iter visit types

(** Record, for each type that implements some trait, which methods of that type are for the trait *)
let record_traits (methods: Object_util.t) =
  Basic_hash_gen.iter methods (fun (key, value) -> 
    let trait_name = Basic_type_path.sexp_of_t key.trait |> S.to_string in
    let ty = value.self_ty in

    let get_method_name = fun (x: Object_util.object_method_item) -> Ident.to_string x.method_id in
    let methods = List.map get_method_name value.methods in

    let vtb_size = Hashtbl.find trait_table ty |> Vec.length in

    (* Note: traits are originally converted from Stype.Mtype.T_trait to T_trait, *)
    (* and the former takes a Basic_type_path, as expected. *)
    (* However, the conversion function needs additional information which is unknown at this stage. *)
    Hashtbl.add trait_offset (ty, Mtype.T_trait trait_name) vtb_size;
    Vec.append (Hashtbl.find trait_table ty) (Vec.of_list methods)
  )

(** Loop variables of the current loop. Used by Cexpr_continue. *)
let loop_vars: var list ref = ref []

(** Loop result value of the current loop. Used by Cexpr_break. *)
let loop_result_value: var ref = ref { name = ""; ty = Mtype.T_unit } 

(**
This function stores the SSA generated in the given argument `tac`.

It returns the variable in which the result of the last instruction pushed is stored.
*)
let rec do_convert tac (expr: Mcore.expr) =
  match expr with
  | Cexpr_unit _ ->
      unit
  
  | Cexpr_var { id; ty; _ } ->
      let name = Ident.to_string id in
      let variable = { name; ty } in

      let is_global = Stringset.mem name !global_vars in

      let is_fn = Stringset.mem name !fn_names in

      (* Mutable variables are treated as pointers. *)
      let is_pointer =
        (match id with Pmutable_ident _ -> true | _ -> false)
      in

      if is_global then 
        let rd = new_temp ty in
        let label = new_temp Mtype.T_bytes in
        Vec.push tac (AssignLabel { rd = label; imm = name });
        Vec.push tac (Load { rd; rs = label; offset = 0; byte = sizeof ty });
        rd
      else if is_fn then 
        let closure = new_temp Mtype.T_bytes in
        let fptr = new_temp Mtype.T_bytes in
        Vec.push tac (Malloc { rd = closure; size = sizeof Mtype.T_bytes });
        Vec.push tac (AssignLabel { rd = fptr; imm = name });
        Vec.push tac (Store { rd = fptr; rs = closure; offset = 0; byte = pointer_size });
        closure
      else if is_pointer then 
        let rd = new_temp ty in
        Vec.push tac (Load { rd; rs = variable; offset = 0; byte = sizeof ty });
        rd
      else 
        variable

      
  (* A cast from a type into some trait. *)
  | Cexpr_object { self; methods_key = { trait; _ } ; _ } ->
      let obj = do_convert tac self in
      let ty = obj.ty in

      let trait_name = Basic_type_path.sexp_of_t trait |> S.to_string in
      let delta = Hashtbl.find trait_offset (ty, Mtype.T_trait trait_name) in

      (* Temporary variables used in SSA *)
      let load = new_temp Mtype.T_int in
      let vtb = new_temp Mtype.T_bytes in
      let altered = new_temp Mtype.T_bytes in

      (* Alter the vtable offset according to the trait *)
      Vec.push tac (Load { rd = vtb; rs = obj; offset = 0; byte = pointer_size });
      Vec.push tac (AssignInt { rd = load; imm = delta });
      Vec.push tac (Add { rd = altered; rs1 = vtb; rs2 = load });
      Vec.push tac (Store { rd = altered; rs = obj; offset = 0; byte = pointer_size });
      obj
  
  (* Primitives are intrinsic functions. *)
  (* We tidy some of these up, and compile others into functions. *)
  | Cexpr_prim { prim; args; ty; _ } ->
      let rd = new_temp ty in
      (match prim, args with

      | _ -> 
          let args = List.map (fun expr -> do_convert tac expr) args in
          deal_with_prim tac rd prim args);
      rd

  | Cexpr_let { name; rhs; body; _ } ->
      let rs = do_convert tac rhs in
      (match name with
      | Pmutable_ident _ ->
          (* We use `bytes` to represent arbitrary pointers. *)
          let space = new_temp Mtype.T_bytes in
          let rd = { name = Ident.to_string name; ty = rs.ty } in
          Vec.push tac (Malloc { rd = space; size = sizeof rs.ty });
          Vec.push tac (Assign { rd; rs = space });
          Vec.push tac (Store { rd = rs; rs = rd; offset = 0; byte = sizeof rs.ty });
      
      | _ ->
        (* (match rs.ty with
        | Mtype.T_func _ -> 
          let rd = { name = Ident.to_string name; ty = rs.ty } in
          Vec.push tac (AssignLabel { rd; imm = rs.name });
        | _ ->
          let rd = { name = Ident.to_string name; ty = rs.ty } in
          Vec.push tac (Assign { rd; rs }))); *)
        let rd = { name = Ident.to_string name; ty = rs.ty } in
          Vec.push tac (Assign { rd; rs }));
      do_convert tac body

  | Cexpr_apply { func; args; ty; kind; _ } ->
      let rd = new_temp ty in
      let fn = Ident.to_string func in
      let args = List.map (fun expr -> do_convert tac expr) args in

      (* Alter the vtable offset to the corresponding trait *)
      let before = Vec.empty () in
      let after = Vec.empty () in
      (if Hashtbl.mem traited_args fn then
        let indices = Hashtbl.find traited_args fn in
        List.iter (fun (i, trait_ty) ->
          let arg = List.nth args i in
          if not (is_trait arg.ty) then (
            let delta = Hashtbl.find trait_offset (arg.ty, trait_ty) in

            (* Trait themselves can't derive another trait, *)
            (* so no worries about diamond inheritance *)
            let load = new_temp Mtype.T_int in
            let vtb = new_temp Mtype.T_bytes in
            let altered = new_temp Mtype.T_bytes in
            let offset = -pointer_size in
            let byte = pointer_size in 

            (* Before calling, we must advance the pointer to the correct offset *)
            Vec.push before (Load { rd = vtb; rs = arg; offset; byte });
            Vec.push before (AssignInt { rd = load; imm = delta });
            Vec.push before (Add { rd = altered; rs1 = vtb; rs2 = load });
            Vec.push before (Store { rd = altered; rs = arg; offset; byte });

            (* After the function returns, we must put it back *)
            Vec.push after (Store { rd = vtb; rs = arg; offset; byte })
          )
        ) indices
      );
      
      Vec.append tac before;
      (if Stringset.mem fn !fn_names then
        Vec.push tac (Call { rd; fn; args })
      else
        if fn = !current_base_name then
          let fptr = new_temp Mtype.T_bytes in
          Vec.push tac (Load { rd = fptr; rs = !current_env; offset = 0; byte = pointer_size });

          let args = args @ [!current_env] in
          Vec.push tac (CallIndirect { rd; rs = fptr; args });
        else
          (* Here `fn` is a closure *)
          let closure = { name = fn; ty = Mtype.T_bytes } in
          let fptr = new_temp Mtype.T_bytes in
          Vec.push tac (Load { rd = fptr; rs = closure; offset = 0; byte = pointer_size });

          (* Closure, along with environment, should be passed as argument *)
          let args = args @ [closure] in
          Vec.push tac (CallIndirect { rd; rs = fptr; args }));

      (* If this is a `Join`, then we must jump to the corresponding letfn *)
      if kind = Join then (
        Vec.push tac (Assign { rd = !current_join_ret; rs = rd });
        Vec.push tac (Jump !current_join);
        !current_join_ret
      ) else (
        Vec.append tac after;
        rd
      )
  
  | Cexpr_sequence { exprs; last_expr; _ } ->
      List.iter (fun expr -> do_convert tac expr |> ignore) exprs;
      do_convert tac last_expr

  (* Meaning: access the `pos`-th field of `record` *)
  (* Here `record` might be a record type or a tuple *)
  | Cexpr_field { record; accessor; pos; ty; _ } ->
      let rd = new_temp ty in
      let rs = do_convert tac record in
      let byte = sizeof ty in
      
      (match rs.ty with
        | Mtype.T_constr _ ->
            let offset = offsetof rs.ty pos in
            Vec.push tac (Load { rd; rs; offset; byte });
            rd
          
        | Mtype.T_tuple { tys } ->
            let precede = Basic_lst.take tys pos in
            let sizes = List.map sizeof precede in
            let offset = List.fold_left (fun acc x -> acc + x) 0 sizes in
            Vec.push tac (Load { rd; rs; offset; byte });
            rd
        
        | _ -> failwith "riscv_ssa.ml: bad record type");
    
  (* Meaning: set the `pos`-th field of `record` to `field` *)
  | Cexpr_mutate { record; pos; field } ->
    let rs = do_convert tac record in
    let rd = do_convert tac field in
    
    let offset = offsetof rs.ty pos in
    Vec.push tac (Store { rd; rs; offset; byte = sizeof rd.ty });
    unit

  (* TODO: Nested if's can cause wrong phi calls. *)
  (* Should move it to riscv_opt.ml, where all basic blocks are emitted. *)
  | Cexpr_if { cond; ifso; ifnot; ty; _ } ->
      let rd = new_temp ty in

      let cond = do_convert tac cond in

      let ifso_ssa = Vec.empty () in 
      let ifnot_ssa = Vec.empty () in


      let ifso_label = new_label "ifso_" in
      let ifnot_label = new_label "ifnot_" in
      let ifexit_label = new_label "ifexit_" in

      Vec.push tac (Branch { cond; ifso = ifso_label; ifnot = ifnot_label });

      Vec.push tac (Label ifso_label);
      let ifso_result = do_convert ifso_ssa ifso in
      Vec.append tac ifso_ssa;
      Vec.push tac (Assign { rd; rs = ifso_result });
      Vec.push tac (Jump ifexit_label);

      Vec.push tac (Label ifnot_label);
      let ifnot_result =
        (match ifnot with
        | None -> unit
        | Some x -> do_convert ifnot_ssa x)
      in
      Vec.append tac ifnot_ssa;
      Vec.push tac (Assign { rd; rs = ifnot_result });
      Vec.push tac (Jump ifexit_label);

      Vec.push tac (Label ifexit_label);
      
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
        # body (containing continue and break)
        # merge results to result
        jump exit

      exit:
        return result
  *)
  | Cexpr_loop { params; body; args; label; ty; _ } ->
      let old_vars = !loop_vars in
      let old_result = !loop_result_value in
      if ty <> T_unit then 
        loop_result_value := new_temp ty;

      (* Get the labels *)
      let loop = Printf.sprintf "loophead_%s_%d" label.name label.stamp in
      let before = Printf.sprintf "loopbefore_%s" loop in
      let exit = Printf.sprintf "loopexit_%s" loop in
      
      Vec.push tac (Jump before);
      
      (* Generate `before`: initialize loop variables. *)
      Vec.push tac (Label before);
      let results = List.map (do_convert tac) args in
      let param_vars =
        List.map (fun (param: Mcore.param) -> { name = Ident.to_string param.binder; ty = param.ty }) params
      in
      loop_vars := param_vars;

      (* Assign arguments to parameters. *)
      List.iter2 (fun param arg ->
        Vec.push tac (Assign { rd = param; rs = arg }))
      param_vars results;

      Vec.push tac (Jump loop);
      Vec.push tac (Label loop);

      let result = do_convert tac body in
      Vec.push tac (Assign { rd = !loop_result_value; rs = result });
      Vec.push tac (Jump exit);
      Vec.push tac (Label exit);

      let result =
        if ty = T_unit then unit
        else !loop_result_value
      in 

      (* If this is a `Join`, then we must jump to the corresponding letfn *)

      (* Store `loop_vars` and `loop_result_value` back; let outer loop go on normally. *)
      loop_vars := old_vars;
      if ty <> T_unit then 
        loop_result_value := old_result;

      result

  (* See the explanation for Cexpr_loop. *)
  | Cexpr_continue { args; label } ->
      (* Generate a label, and let the previous block jump to this block. *)
      let cont = new_label "continue_" in
      Vec.push tac (Jump cont);
      Vec.push tac (Label cont);

      (* Evaluate loop variables and assign them back. *)
      let results = List.map (do_convert tac) args in
      List.iter2 (fun rd rs -> Vec.push tac (Assign { rd; rs })) !loop_vars results;

      (* Jump back to the beginning of the loop. *)
      let loop_name = Printf.sprintf "loophead_%s_%d" label.name label.stamp in 
      Vec.push tac (Jump loop_name);
      unit

  | Cexpr_break { label; arg; ty; _ } ->
    (* Jumps to exit of the loop. *)
    Option.iter (fun arg -> Vec.push tac (Assign { rd = !loop_result_value; rs = do_convert tac arg})) arg;
    
    (* If this is a `Join`, then we must jump to the corresponding letfn *)
    let loop_name = Printf.sprintf "loophead_%s_%d" label.name label.stamp in
    Vec.push tac (Jump ("loopexit_" ^ loop_name));
    unit

  (* Assigns mutable variables. *)
  | Cexpr_assign { var; expr; ty } ->
      let rd = do_convert tac expr in
      let rs = { name = Ident.to_string var; ty = Mtype.T_bytes } in
      Vec.push tac (Store { rd; rs; offset = 0; byte = sizeof rd.ty });
      unit

  (* Builds a record type. *)
  | Cexpr_record { fields; ty; } ->
      (* Allocate space for the record *)
      let rd = new_temp ty in

      let has_vtable = Hashtbl.mem trait_table ty in
      let size = Hashtbl.find size_table ty in

      (if has_vtable then
        let beginning = new_temp ty in
        let load = new_temp Mtype.T_int in

        (* We construct vtable before every field *)
        (* and let `rd` point at where fields start *)
        (* in order to unite traited and untraited types *)
        Vec.push tac (Malloc { rd = beginning; size = size + pointer_size });
        Vec.push tac (AssignInt { rd = load; imm = pointer_size });
        Vec.push tac (Add { rd; rs1 = beginning; rs2 = load });

        (* Load in vtable *)
        let vtb = new_temp Mtype.T_bytes in
        let label = Printf.sprintf "vtable_%s" (Mtype.to_string ty |> remove_space) in
        Vec.push tac (AssignLabel { rd = vtb; imm = label });
        Vec.push tac (Store { rd = vtb; rs = rd; offset = -pointer_size; byte = pointer_size })
      else
        (* No vtable; everything normal *)
        Vec.push tac (Malloc { rd; size });
      );

      (* Construct all its fields *)
      let visit ({ pos; expr; _ }: Mcore.field_def) =
        let result = do_convert tac expr in
        let offset = offsetof ty pos in 
        Vec.push tac (Store { rd = result; rs = rd; offset; byte = sizeof result.ty })
      in

      List.iter visit fields;
      rd

  | Cexpr_tuple { exprs; ty; _ } ->
      let rd = new_temp ty in
      let tys =
        (match ty with
        | Mtype.T_tuple { tys } -> tys
        | _ -> failwith "riscv_ssa.ml: bad tuple")
      in

      let size = List.fold_left (fun acc x -> sizeof x + acc) 0 tys in
      Vec.push tac (Malloc { rd; size });

      let args = List.map (fun x -> do_convert tac x) exprs in
      let sizes = List.map (fun x -> sizeof x.ty) args |> fun list -> Basic_lst.take list (List.length args - 1) in
      let offsets = 0 :: Basic_lst.cumsum sizes in
      List.iter2 (fun arg offset ->
        Vec.push tac (Store { rd = arg; rs = rd; offset; byte = sizeof arg.ty })
      ) args offsets;
      rd

  | Cexpr_letfn { name; fn; body = afterwards; kind; ty; _ } ->
      let name = Ident.to_string name in
      let free_vars = Hashtbl.find captured name in

      let sizes = List.map (fun v -> sizeof v.ty) free_vars in
      let offset = Basic_lst.cumsum sizes |> List.map (fun x -> x + pointer_size) in
      (* It is possible that this function does not capture anything *)
      let total = if List.length offset = 0 then pointer_size else Basic_lst.last offset in

      (* Generate a global function *)
      let params =
        List.map (fun (x: Mcore.param) -> { name = Ident.to_string x.binder; ty = x.ty }) fn.params
      in
      let body = fn.body in
      let fn_ssa = Vec.empty () in
      let fn_env = new_temp Mtype.T_bytes in    (* This will be added to argument list *)

      (* Load environment *)
      List.iter2 (fun arg offset ->
        let size = sizeof arg.ty in
        Vec.push fn_ssa (Load { rd = arg; rs = fn_env; offset = offset - size; byte = size })
      ) free_vars offset;

      (* This is a different function from the current one, *)
      (* so we must protect all global variables before generating body *)
      let this_fn = !current_function in
      let this_base_fn = !current_base_name in
      let this_env = !current_env in
      let this_join = !current_join in
      let this_join_ret = !current_join_ret in

      (* Set the correct values for this new function *)
      let fn_name = Printf.sprintf "%s_closure_%s" !current_function name in
      current_function := fn_name;
      current_base_name := name;
      current_env := fn_env;
      current_join := "";
      current_join_ret := unit;

      (* Generate function body *)
      let return = do_convert fn_ssa body in
      Vec.push fn_ssa (Return return);

      (* Put them back *)
      current_function := this_fn;
      current_base_name := this_base_fn;
      current_env := this_env;
      current_join := this_join;
      current_join_ret := this_join_ret;

      (* Push it into global instructions *)
      let fn_body = Vec.to_list fn_ssa in
      let args = params @ [fn_env] in
      Vec.push global_inst (FnDecl { fn = name; args; body = fn_body });

      (* Make a closure *)
      (* First 8 bytes are function pointer; other bytes are environment *)

      (* Allocate space *)
      let closure = new_temp Mtype.T_bytes in
      Vec.push tac (Malloc { rd = closure; size = total });

      (* Store the function pointer *)
      let fptr = new_temp Mtype.T_bytes in
      Vec.push tac (AssignLabel { rd = fptr; imm = name });
      Vec.push tac (Store { rd = fptr; rs = closure; offset = 0; byte = pointer_size });

      (* Store environment variables *)
      List.iter2 (fun (arg: var) offset ->
        let size = sizeof arg.ty in
        if arg.name = !current_base_name then
          (* This closure captures myself, so I need to make myself a closure *)
          (* Fortunately my environment is just my closure *)
          Vec.push tac (Store { rd = !current_env; rs = closure; offset = offset - size; byte = size })
        else
          Vec.push tac (Store { rd = arg; rs = closure; offset = offset - size; byte = size })
      ) free_vars offset;

      (* The closure's done. Go on processing code afterwards. *)
      Vec.push tac (Assign { rd = { name; ty = Mtype.T_bytes }; rs = closure; });

      (* Go on and process things afterwards *)
      (match kind with
      | Rec | Nonrec -> do_convert tac afterwards
      | _ ->
          (* If this is a joined letfn, then we must push a `join` after things, *)
          (* for joined `apply`s to jump to *)
          let this_join = !current_join in
          let this_join_ret = !current_join_ret in
          let join = new_label "join_" in
          let rd = new_temp ty in
          
          current_join := join;
          current_join_ret := rd;
          
          let ret = do_convert tac afterwards in

          Vec.push tac (Assign { rd; rs = ret });
          Vec.push tac (Jump join);
          Vec.push tac (Label join);

          current_join := this_join;
          current_join_ret := this_join_ret;
          rd)

  | Cexpr_return { expr; return_kind } ->
      (match return_kind with
      | Error_result { is_error; return_ty } ->
          failwith "TODO: riscv_generate.ml: return error"

      | Single_value ->
          let return = do_convert tac expr in
          Vec.push tac (Return return);
          unit)

  | Cexpr_constr { tag; args } ->
      let args = List.map (do_convert tac) args in
      let len = List.length args in
      let index = tag.index in
      let sizes = List.map (fun x -> sizeof x.ty) args in
      let offsets = 0 :: Basic_lst.cumsum sizes in
      (* Plus the space for the tag (an int) *)
      let size = 4 + List.nth offsets len in

      let rd = new_temp Mtype.T_bytes in
      Vec.push tac (Malloc { rd; size });

      (* First store the tag *)
      let tag = new_temp Mtype.T_int in
      Vec.push tac (AssignInt { rd = tag; imm = index });
      Vec.push tac (Store { rd = tag; rs = rd; offset = 0; byte = 4 });

      (* Then store all arguments *)
      List.iter2 (fun arg offset ->
        Vec.push tac (Store { rd = arg; rs = rd; offset = 4 + offset; byte = sizeof arg.ty })  
      ) args (Basic_lst.take offsets len);
      rd

  | Cexpr_switch_constr { obj; cases; default; ty; _ } ->
      (* Compile into a jump table *)
      let obj = do_convert tac obj in

      (* Extract index *)
      let index = new_temp Mtype.T_int in
      Vec.push tac (Load { rd = index; rs = obj; offset = 0; byte = 4 });

      let tag_offsets = Hashtbl.find variants (nameof obj.ty) in

      (* Generate a jump table *)
      let label = new_label "jumptable_" in
      let jumps = List.init (List.length tag_offsets) (fun _ -> new_label "jumptable_") in
      let out = new_label "jumptable_out_" in
      let default_lbl = new_label "jumptable_default_" in

      (* Choose which place to jump to *)
      let jtable = new_temp Mtype.T_bytes in
      let ptr_sz = new_temp Mtype.T_int in
      let off = new_temp Mtype.T_int in
      let altered = new_temp Mtype.T_bytes in
      let target = new_temp Mtype.T_bytes in
      
      (* Assign all these different possibilities into rd *)
      let rd = new_temp ty in

      (* Load the address *)
      Vec.push tac (AssignLabel { rd = jtable; imm = label });
      Vec.push tac (AssignInt { rd = ptr_sz; imm = pointer_size });
      Vec.push tac (Mul { rd = off; rs1 = index; rs2 = ptr_sz });
      Vec.push tac (Add { rd = altered; rs1 = jtable; rs2 = off });
      Vec.push tac (Load { rd = target; rs = altered; offset = 0; byte = pointer_size });

      let visited = Vec.empty () in
      let correspondence = Array.make (List.length tag_offsets) "_uninit" in

      (* For each label, generate the code of it *)
      let tac_cases = Vec.empty () in

      List.iter (fun ((tag: Tag.t), ident, expr) ->
        let lbl = List.nth jumps tag.index in

        Vec.push tac_cases (Label lbl);
        (match ident with
        | None -> ()
        | Some x ->
            Vec.push tac_cases (Assign { rd = { name = Ident.to_string x; ty = obj.ty }; rs = obj }));
        let ret = do_convert tac_cases expr in
        Vec.push tac_cases (Assign { rd; rs = ret });
        Vec.push tac_cases (Jump out);
        Vec.push visited tag.index;
        correspondence.(tag.index) <- lbl
      ) cases;

      (match default with
      | None -> ()
      | Some x ->
          let visited = visited |> Vec.to_list in
          
          Vec.push tac_cases (Label default_lbl);
          let ret = do_convert tac_cases x in
          Vec.push tac_cases (Assign { rd; rs = ret });
          Vec.push tac_cases (Jump out);
          
          List.iteri (fun i x ->
            if not (List.mem i visited) then (
              correspondence.(i) <- default_lbl
            )
          ) tag_offsets;);

      Vec.push tac_cases (Label out);

      (* Deduplicate all possible targets *)
      let possibilities =
        Array.to_list correspondence |> Stringset.of_list |> Stringset.to_seq |> List.of_seq
      in
      (* Jump to the correct target *)
      Vec.push tac (JumpIndirect { rs = target; possibilities });

      (* Emit all match cases *)
      Vec.append tac tac_cases;

      (* Record the correct label order *)
      Vec.push global_inst (ExtArray { label; values = Array.to_list correspondence; elem_size = 8 });
      rd

  | Cexpr_letrec _ ->
      prerr_endline "letrec";
      unit

  | Cexpr_record_update _ ->
      prerr_endline "record_update";
      unit

  | Cexpr_switch_constant { obj; cases; default; ty; _ } ->
      let index = do_convert tac obj in
      let len = List.length cases in
      
      if len = 0 then (
        (* Only default case is present. No match needs to be generated. *)
        do_convert tac default
      ) else (
        let rd = new_temp ty in 

        let values =
          List.map (fun (t, _) -> 
            match t with
            | Constant.C_bool b -> Bool.to_int b
            | Constant.C_int { v } -> Int32.to_int v
            | Constant.C_char v -> Uchar.to_int v
            | _ -> failwith "TODO: unsupported switch constant type"
          ) cases
        in
        
        let mx = List.fold_left (fun mx x -> max mx x) (-2147483647-1) values in
        let mn = List.fold_left (fun mn x -> min mn x) 2147483647 values in

        (* Sparse values, convert to if-else *)
        if mx - mn >= 256 || len < 16 then (
          let ifexit = new_label "match_ifexit_" in
          List.iter2 (fun x (_, expr) ->
            let equal = new_temp Mtype.T_bool in
            let v = new_temp Mtype.T_int in
            let ifso = new_label "match_ifso_" in
            let ifnot = new_label "match_ifnot_" in

            Vec.push tac (AssignInt { rd = v; imm = x });
            Vec.push tac (Eq { rd = equal; rs1 = index; rs2 = v });
            Vec.push tac (Branch { cond = equal; ifso; ifnot });
            
            (* Generate the match case *)
            Vec.push tac (Label ifso);
            let ret = do_convert tac expr in
            Vec.push tac (Assign { rd; rs = ret });
            Vec.push tac (Jump ifexit);

            Vec.push tac (Label ifnot);
            ()
          ) values cases;

          (* The last ifnot corresponds to the default case *)
          let ret = do_convert tac default in
          Vec.push tac (Assign { rd; rs = ret });
          Vec.push tac (Jump ifexit);

          Vec.push tac (Label ifexit)
        )
      
        (* Dense values, emit a jump table *)
        else (
          let table = new_label "jumptable_int_" in
          let jump = new_label "do_jump_int_" in
          let jumps = List.init (mx - mn + 1) (fun _ -> new_label "jumptable_int_") in
          let out = new_label "jumptable_int_out_" in
          let default_lbl = new_label "jumptable_default_" in

          (* If the value is outside the min/max range, jump to default *)
          let inrange = new_temp Mtype.T_bool in
          let maximum = new_temp Mtype.T_int in
          let minimum = new_temp Mtype.T_int in
          let _1 = new_temp Mtype.T_bool in
          let _2 = new_temp Mtype.T_bool in

          (* Evaluate (x < max) && (x > min), which is the range where we can use jump table *)
          Vec.push tac (AssignInt { rd = maximum; imm = mx });
          Vec.push tac (AssignInt { rd = minimum; imm = mn });
          Vec.push tac (Leq { rd = _1; rs1 = index; rs2 = maximum });
          Vec.push tac (Geq { rd = _2; rs1 = index; rs2 = minimum });
          Vec.push tac (And { rd = inrange; rs1 = _1; rs2 = _2 });
          Vec.push tac (Branch { cond = inrange; ifso = jump; ifnot = default_lbl });

          (* Load the address *)
          Vec.push tac (Label jump);

          let jtable = new_temp Mtype.T_bytes in
          let ptr_sz = new_temp Mtype.T_int in
          let off = new_temp Mtype.T_int in
          let altered = new_temp Mtype.T_bytes in
          let target = new_temp Mtype.T_bytes in

          Vec.push tac (AssignLabel { rd = jtable; imm = table });
          Vec.push tac (AssignInt { rd = ptr_sz; imm = pointer_size });
          
          (* We must also minus the minimum, unlike switch_constr *)
          let min_var = new_temp Mtype.T_int in
          let ind_2 = new_temp Mtype.T_int in

          Vec.push tac (AssignInt { rd = min_var; imm = mn });
          Vec.push tac (Sub { rd = ind_2; rs1 = index; rs2 = min_var });

          (* Now find which address to jump to *)
          Vec.push tac (Mul { rd = off; rs1 = ind_2; rs2 = ptr_sz });
          Vec.push tac (Add { rd = altered; rs1 = jtable; rs2 = off });
          Vec.push tac (Load { rd = target; rs = altered; offset = 0; byte = pointer_size });

          let visited = Vec.empty () in
          let correspondence = Array.make (mx - mn + 1) "_uninit" in
    
          (* For each label, generate the code of it *)
          let tac_cases = Vec.empty () in

          List.iter2 (fun value (_, expr) ->
            let lbl = List.nth jumps (value - mn) in
    
            Vec.push tac_cases (Label lbl);
            let ret = do_convert tac_cases expr in
            Vec.push tac_cases (Assign { rd; rs = ret });
            Vec.push tac_cases (Jump out);
            Vec.push visited value;
            correspondence.(value - mn) <- lbl
          ) values cases;

          (* For each values in the (min, max) range, redirect them into default *)
          let visited = visited |> Vec.to_list in
          
          Vec.push tac_cases (Label default_lbl);
          let ret = do_convert tac_cases default in
          Vec.push tac_cases (Assign { rd; rs = ret });
          Vec.push tac_cases (Jump out);

          List.iter (fun i ->
            if not (List.mem i visited) then (
              correspondence.(i - mn) <- default_lbl
            )
          ) (List.init (mx - mn) (fun i -> i + mn));

          (* Store the correct order of jump table *)
          Vec.push tac_cases (Label out);
          Vec.push global_inst (ExtArray
            { label = table; values = Array.to_list correspondence; elem_size = 8 });

          (* Deduplicate possibilities and jump there *)
          let possibilities =
            Array.to_list correspondence |> Stringset.of_list |> Stringset.to_seq |> List.of_seq
          in

          Vec.push tac (JumpIndirect { rs = target; possibilities });
          Vec.append tac tac_cases;);
        
        rd
      )

  | Cexpr_handle_error _ ->
      prerr_endline "handle error";
      unit

  | Cexpr_array { exprs; ty } ->
      let elem_ty = 
        (match ty with
        | Mtype.T_fixedarray { elem } -> elem
        | _ -> failwith "riscv_generate.ml: bad array type in Cexpr_array")
      in
      let elem_sz = sizeof elem_ty in

      let rd = new_temp Mtype.T_bytes in
      let space = new_temp Mtype.T_bytes in
      (* Make a space with 4 extra bytes *)
      Vec.push tac (Malloc { rd = space; size = elem_sz * (List.length exprs) + 4 });

      (* Store the length and advance to the data part *)
      let _1 = new_temp Mtype.T_int in
      Vec.push tac (AssignInt { rd = _1; imm = List.length exprs });
      Vec.push tac (Store { rd = _1; rs = space; offset = 0; byte = 4 });
      Vec.push tac (Addi { rd; rs = space; imm = 4 });

      (* Store each value in the correct position *)
      List.iteri (fun i x ->
        let return = do_convert tac x in
        Vec.push tac (Store { rd = return; rs = rd; offset = i * elem_sz; byte = elem_sz })
      ) exprs;
      rd

  | Cexpr_const { c; ty; _ } ->
      let rd = new_temp ty in
      (match c with
      (* Note each element of string is 2 bytes long. TODO *)
      | C_string v ->
          let label = Printf.sprintf "str_%d" !slot in
          let vals = String.to_seq v |> List.of_seq in
          let len = String.length v |> Int.to_string in
          let vec = Vec.empty () in

          List.iter (fun x ->
            Vec.push vec (Char.code x);
            Vec.push vec 0) vals;
          let values = len :: Vec.map_into_list vec ~unorder:Int.to_string in

          slot := !slot + 1;
          (* Each of them is still a single byte, since we separated strings into two bytes *)
          Vec.push global_inst (ExtArray { label; values; elem_size = 1 });

          (* Let the pointer point to beginning of data, rather than the length section *)
          let beginning = new_temp Mtype.T_bytes in
          Vec.push tac (AssignLabel { rd = beginning; imm = label; });
          Vec.push tac (Addi { rd; rs = beginning; imm = 4 })

      | C_bytes { v; _ } ->
          let label = Printf.sprintf "bytes_%d" !slot in
          let vals = String.to_seq v |> List.of_seq |> List.map (fun x -> Char.code x |> Int.to_string) in
          let len = String.length v |> Int.to_string in
          let values = len :: vals in

          slot := !slot + 1;
          Vec.push global_inst (ExtArray { label; values; elem_size = 1 });

          (* Let the pointer point to beginning of data, rather than the length section *)
          let beginning = new_temp Mtype.T_bytes in
          Vec.push tac (AssignLabel { rd = beginning; imm = label; });
          Vec.push tac (Addi { rd; rs = beginning; imm = 4 })
  
      | C_int64 { v; _ } | C_uint64 { v; _ }  ->
          Vec.push tac (AssignInt64 { rd; imm = v });
      | C_bool imm ->
          Vec.push tac (AssignInt { rd; imm = if imm then 1 else 0; })
      | C_char imm ->
          Vec.push tac (AssignInt { rd; imm = Uchar.to_int imm; })
      | C_int { v; _ } | C_uint { v; _ }  ->
          Vec.push tac (AssignInt { rd; imm = Int32.to_int v; })
      | C_float { v; _ } ->
          Vec.push tac (AssignFP { rd; imm = v; })
      | C_double { v; _ } ->
          Vec.push tac (AssignFP { rd; imm = v; })
      | C_byte { v; _ } ->
          Vec.push tac (AssignInt { rd; imm = v })
      | C_bigint _ -> failwith "TODO: riscv_ssa.ml: bigint not supported"
      );
      rd

  | Cexpr_function _ ->
      Printf.printf "unconverted: Cexpr_function\n";
      failwith "riscv_generate.ml: Cexpr_function should have been converted into letfn"

let generate_vtables () =
  Hashtbl.iter (fun (ty: Mtype.t) methods ->
    let label_raw = Printf.sprintf "vtable_%s" (Mtype.to_string ty) in
    let label = remove_space label_raw in
    Vec.push global_inst (ExtArray { label; values = Vec.to_list methods; elem_size = 8 })
  ) trait_table

(**
Converts given `expr` into a list of SSA instructions,
along with the variable in which the result of this expression is stored.
*)
let convert_expr_no_ret (expr: Mcore.expr) =
  let tac = Vec.empty () in
  let _ = do_convert tac expr in
  Vec.map_into_list tac (fun x -> x)

let convert_expr (expr: Mcore.expr) =
  let tac = Vec.empty () in
  let return = do_convert tac expr in
  Vec.push tac (Return return);
  Vec.map_into_list tac (fun x -> x)

(** Traverse the whole expression tree. *)
let rec iter_expr f (expr: Mcore.expr) =
  f expr;
  let go = iter_expr f in
  match expr with
  | Cexpr_prim { args } -> List.iter go args
  | Cexpr_let { rhs; body } -> go rhs; go body
  | Cexpr_letfn { fn; body } -> go fn.body; go body
  | Cexpr_function { func } -> go func.body
  | Cexpr_apply { args } -> List.iter go args
  | Cexpr_object { self } -> go self
  | Cexpr_letrec { body } -> go body
  | Cexpr_constr { args } -> List.iter go args
  | Cexpr_tuple { exprs } -> List.iter go exprs
  | Cexpr_record_update { record } -> go record
  | Cexpr_field { record } -> go record
  | Cexpr_mutate { record; field } -> go record; go field
  | Cexpr_array { exprs } -> List.iter go exprs
  | Cexpr_assign { expr } -> go expr
  | Cexpr_sequence { exprs; last_expr; _ } -> List.iter go exprs; go last_expr
  | Cexpr_if { cond; ifso; ifnot } -> go cond; go ifso; Option.iter go ifnot
  | Cexpr_switch_constr { obj; cases; default } -> go obj; List.iter (fun (a, b, c) -> go c) cases; Option.iter go default
  | Cexpr_switch_constant { obj; cases; default } -> go obj; List.iter (fun (a, b) -> go b) cases; go default
  | Cexpr_loop { body; args } -> go body; List.iter go args
  | Cexpr_break { arg } -> Option.iter go arg
  | Cexpr_continue { args } -> List.iter go args
  | Cexpr_handle_error { obj } -> go obj
  | Cexpr_return { expr } -> go expr
  | _ -> ()

let rec map_expr f (expr: Mcore.expr) =
  let go = map_expr f in
  (match expr with
  | Cexpr_prim ({ args } as x) ->
      Mcore.Cexpr_prim { x with args = List.map go args }

  | Cexpr_let ({ rhs; body } as x) ->
      Mcore.Cexpr_let { x with rhs = go rhs; body = go body }

  | Cexpr_letfn ({ fn; body } as x) ->
      Mcore.Cexpr_letfn { x with fn = { fn with body = go fn.body }; body = go body }

  | Cexpr_function ({ func } as x) ->
      Mcore.Cexpr_function { x with func = { func with body = go func.body } }

  | Cexpr_apply ({ args } as x) ->
      Mcore.Cexpr_apply { x with args = List.map go args }
    
  | Cexpr_object ({ self } as x) ->
      Mcore.Cexpr_object { x with self = go self }

  | Cexpr_letrec ({ body } as x) ->
      Mcore.Cexpr_letrec { x with body = go body }

  | Cexpr_constr ({ args } as x) ->
      Mcore.Cexpr_constr { x with args = List.map go args }

  | Cexpr_tuple ({ exprs } as x) ->
      Mcore.Cexpr_tuple { x with exprs = List.map go exprs }

  | Cexpr_record_update ({ record } as x) ->
      Mcore.Cexpr_record_update { x with record = go record }

  | Cexpr_field ({ record } as x) ->
      Mcore.Cexpr_field { x with record = go record }

  | Cexpr_mutate ({ record; field } as x) ->
      Mcore.Cexpr_mutate { x with record = go record; field = go field }

  | Cexpr_array ({ exprs } as x) ->
      Mcore.Cexpr_array { x with exprs = List.map go exprs }

  | Cexpr_assign ({ expr } as x) ->
      Mcore.Cexpr_assign { x with expr = go expr }

  | Cexpr_sequence ({ exprs; last_expr; _ } as x) ->
      Mcore.Cexpr_sequence { x with exprs = List.map go exprs; last_expr = go last_expr }

  | Cexpr_if ({ cond; ifso; ifnot } as x) ->
      Mcore.Cexpr_if { x with cond = go cond; ifso = go ifso; ifnot = Option.map go ifnot }

  | Cexpr_switch_constr ({ obj; cases; default } as x) ->
      Mcore.Cexpr_switch_constr {
        x with obj = go obj;
        cases = List.map (fun (a, b, c) -> (a, b, go c)) cases;
        default = Option.map go default
      }

  | Cexpr_switch_constant ({ obj; cases; default } as x) ->
      Mcore.Cexpr_switch_constant {
        x with obj = go obj;
        cases = List.map (fun (a, b) -> (a, go b)) cases;
        default = go default
      }

  | Cexpr_loop ({ body; args } as x) ->
      Mcore.Cexpr_loop { x with body = go body; args = List.map go args }

  | Cexpr_break ({ arg } as x) ->
      Mcore.Cexpr_break { x with arg = Option.map go arg }

  | Cexpr_continue ({ args } as x) -> 
      Mcore.Cexpr_continue { x with args = List.map go args }

  | Cexpr_handle_error ({ obj } as x) ->
      Mcore.Cexpr_handle_error { x with obj = go obj }

  | Cexpr_return ({ expr } as x) ->
      Mcore.Cexpr_return { x with expr = go expr }

  | w -> w) |> f

(* Converts anonymous functions into named functions, i.e. `letfn` *)
let convert_lambda (expr: Mcore.expr) =
  match expr with
  | Cexpr_function { func; ty; loc_ } ->
      let lambda = Printf.sprintf "lambda_%d" !slot in
      let ident = Core.Ident.Pident { name = lambda; stamp = !slot } in
      slot := !slot + 1;
      Mcore.Cexpr_letfn {
        name = ident;
        kind = Mcore.Rec;
        fn = func;
        body = Mcore.Cexpr_var { id = ident; prim = None; ty; loc_ };
        ty; loc_
      }

  | w -> w

(** Store captured variables for each closure in `captured` *)
let process_closure ((fn: Mcore.fn), (name: Ident.t)) = 
  let free_ident = Mcore_util.free_vars ~exclude:(Ident.Set.singleton name) fn in
  let free_vars =
    Array.map
      (fun (ident, ty) -> { name = Ident.to_string ident; ty })
      (free_ident |> Ident.Map.to_sorted_array)
  in
  Hashtbl.add captured (Ident.to_string name) (free_vars |> Array.to_list)

(** Finds all free variables inside a closure. *)
let analyze_closure (top: Mcore.top_item) =
  (* A list of all closures *)
  let worklist = Vec.empty () in

  let find_closures (expr: Mcore.expr) =
    match expr with
    | Cexpr_letfn { fn; name; } ->
        Vec.push worklist (fn, name)
    | _ -> ()
  in

  match top with
  | Ctop_fn { func; _ } ->
      iter_expr find_closures func.body;
      Vec.iter process_closure worklist

  | Ctop_expr { expr } ->
      iter_expr find_closures expr;
      Vec.iter process_closure worklist

  | _ -> ()

let convert_toplevel _start (top: Mcore.top_item) =
  let var_of_param ({ binder; ty; _ } : Mcore.param) =
    { name = Ident.to_string binder; ty }
  in

  match top with
  (* Init function *)
  | Ctop_expr { expr; _ } ->
    let expr = convert_expr_no_ret expr in
    Vec.append init_exprs @@ Vec.of_list expr;
    []
  | Ctop_fn { binder; func; export_info_; _ } ->
      let fn = Ident.to_string binder in
      let args = List.map var_of_param func.params in

      current_function := fn;
      current_base_name := fn;
      let body = convert_expr func.body in

      (* Record the index of arguments that are traits *)
      let traited = Vec.empty () in
      List.iteri (fun i x ->
        if is_trait x.ty then Vec.push traited (i, x.ty))
      args;

      if Vec.length traited <> 0 then
        Hashtbl.add traited_args fn (Vec.to_list traited);

      if export_info_ <> None then
        prerr_endline "warning: export info is non-empty";
      [ FnDecl { fn; args; body } ]

  | Ctop_let { binder; expr; is_pub_; _ } ->
      let name = Ident.to_string binder in
      global_vars := Stringset.add name !global_vars;

      let rd = do_convert _start expr in
      let label = new_temp Mtype.T_bytes in

      Vec.push _start (AssignLabel { rd = label; imm = name });
      Vec.push _start (Store { rd; rs = label; offset = 0; byte = sizeof rd.ty });
      [ GlobalVarDecl { name; ty = rd.ty } ]

  (* Stubs are references to internal functions. *)
  | Ctop_stub { func_stubs; binder; _ } ->
      (match func_stubs with
      | Internal { func_name = fn; } ->
          let (args, body) = Riscv_internals.get fn in
          [ FnDecl { fn = Ident.to_string binder; args; body } ]

      | Inline_code_text _
      | Inline_code_sexp _ -> failwith "RISC-V target does not support inline WASM"
      | Import _ -> failwith "riscv_ssa.ml: import should have been eliminated in link stage")

let find_functions (top: Mcore.top_item) =
  match top with
  | Ctop_fn { binder; _ }
  | Ctop_stub { binder; _ } ->
      fn_names += Ident.to_string binder

  | _ -> ()

let ssa_of_mcore (core: Mcore.t) =
  let out = Printf.sprintf "%s.ir" !Driver_config.Linkcore_Opt.output_file  in
  Basic_io.write_s out (Mcore.sexp_of_t core);

  (* Body of the function `_start`, which is the entry point *)
  let _start = Vec.empty () in

  (* Look through types and calculate their field offsets *)
  update_types core.types;

  (* Look through traits and their implementations *)
  record_traits core.object_methods;

  (* Deal with ordinary functions *)
  (* First rewrite lambdas into named closures *)
  let dislambda (top: Mcore.top_item) =
    match top with
    | Ctop_fn ({ func } as x) ->
        Mcore.Ctop_fn { x with func = { func with body = map_expr convert_lambda func.body } }
    | w -> w
  in

  let bodies = List.map dislambda core.body in
  List.iter find_functions bodies;
  List.iter analyze_closure bodies;
  let body = List.map (fun x -> convert_toplevel _start x) bodies |> List.flatten in

  (* Deal with main *)
  let with_main = match core.main with
    | Some (main_expr, _) ->
        let lambda_removed = map_expr convert_lambda main_expr in
        
        (* Find closures in main *)
        let closures = Vec.empty () in
        let find_closure (expr: Mcore.expr) =
          match expr with
          | Cexpr_letfn { name; fn } ->
              Vec.push closures (fn, name)
          | _ -> ()
        in
        iter_expr find_closure lambda_removed;
        List.iter process_closure (closures |> Vec.to_list);

        (* Do conversion *)
        let main_body = convert_expr lambda_removed in
        let main_decl = FnDecl { fn = "main"; args = []; body = main_body } in
        main_decl :: body
      
    | None -> body
  in

  (* Add _start *)
  let unused = new_temp Mtype.T_unit in
  Vec.append _start init_exprs;
  Vec.push _start (Call { rd = unused; fn = "main"; args = [] });
  Vec.push _start (Return unused);

  let start_body = Vec.to_list _start in
  let with_start = FnDecl { fn = "_start"; args = []; body = start_body } :: with_main in

  (* Add global declarations and variables *)
  generate_vtables ();
  let with_vtables = (Vec.to_list global_inst) @ with_start in
  with_vtables
