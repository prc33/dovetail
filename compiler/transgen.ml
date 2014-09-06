(* Dovetail JCAM
 *
 * Copyright 2014      Peter Calvert <peter.calvert@cl.cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)

open Jcam
open Core.Std

exception Undef_Label of string
exception Undef_Var of string
exception Undef_Const of string

let fold  f l s = List.fold_left l ~init:s ~f:(fun s x -> f x s)
let foldi f l s = snd (List.fold_left l ~init:(0,s) ~f:(fun (i,s) x -> (i+1, f (i,x) s)))

let context = Llvm.global_context ()
let llmod = Function.llmod

let void_type = Llvm.void_type context
let make_int = Llvm.const_int (Llvm.i32_type context)

let pattern_types types = List.map ~f:(function (c, ts) -> Typegen.structure (fun t -> types (fst t)) ts)

type state = {
  types: Typegen.t;
  values: Llvm.llvalue String.Map.t;
  blocks: Llvm.llbasicblock String.Map.t;
  undef: Llvm.llvalue String.Map.t;
  constructors: Llvm.llvalue String.Map.t;
  constants: Llvm.llvalue String.Map.t;
}

let get_state types constructors values constants = { types=types; values=values; blocks=String.Map.empty; undef=String.Map.empty; constructors=constructors; constants=constants }

let init_state f blocks s =
  let init_bb = Llvm.builder_at_end context (Llvm.append_block context "placeholders" f) in

  let new_s = fold (fun b state -> (match b.label with
    | None   ->   state
    | Some l -> { state with blocks=Map.add state.blocks l (Llvm.append_block context l f) }
    ) |>


    (* Placeholder values for any Phi-nodes *)
    fold (fun (v,t,_) state ->
      let placeholder = Llvm.build_load (Llvm.declare_global (state.types t) ("fref." ^ v) llmod) ("fval." ^ v) init_bb in
      { state with values=Map.add state.values v placeholder; undef=Map.add state.undef v placeholder }
    ) b.phis |>

    (* Placeholder values for all assignments *)
    fold (fun i state -> (match i with
      | Instruction.Assign(v,e) ->
          let placeholder = Llvm.build_load (Llvm.declare_global (state.types (Typing.return_type e)) ("fref." ^ v) llmod) ("fval." ^ v) init_bb in
          { state with values=Map.add state.values v placeholder; undef=Map.add state.undef v placeholder }
      | _ -> state
      )
    ) b.instrs
  ) blocks s in

  Llvm.build_ret_void init_bb |> ignore;

  new_s

let generate_block f instance block state =
  let value t v s = match v with
    | Value.Var(v)      -> (match Map.find s.values v with Some x -> x | None -> raise (Undef_Var v))
    | Value.Integer(x)  -> Llvm.const_int t x
    | Value.Float(x)    -> Llvm.const_float t x
    | Value.Constant(v) -> (match Map.find s.constants v with Some x -> x | None -> raise (Undef_Const v))
    | Value.Null        -> Llvm.const_null t
  in

  let get_block l = (match Map.find state.blocks l with Some x -> x | None -> raise (Undef_Label l)) in

  (* Position builder at correct block *)
  let bb = Llvm.builder_at_end context (match block.label with
    | Some l -> get_block l
    | None   -> Llvm.entry_block f)
  in

  (* Worker structure (always first parameter for both slow and fast *)
  let worker = Llvm.param f 0 in

  let build_expr v s = function
    | Expr.Add(t,a,b) -> let op = match t with
                                  | Type.Integer(_) -> Llvm.build_add
                                  | Type.Float(_) -> Llvm.build_fadd in
                         let t = (s.types t) in op (value t a s) (value t b s) v bb
    | Expr.Sub(t,a,b) -> let op = match t with
                                  | Type.Integer(_) -> Llvm.build_sub
                                  | Type.Float(_) -> Llvm.build_fsub in
                         let t = (s.types t) in op (value t a s) (value t b s) v bb
    | Expr.Mul(t,a,b) -> let op = match t with
                                  | Type.Integer(_) -> Llvm.build_mul
                                  | Type.Float(_) -> Llvm.build_fmul in
                         let t = (s.types t) in op (value t a s) (value t b s) v bb
    | Expr.Div(t,a,b) -> let op = match t with
                                  | Type.Integer(_) -> Llvm.build_sdiv
                                  | Type.Float(_) -> Llvm.build_fdiv in
                         let t = (s.types t) in op (value t a s) (value t b s) v bb
    | Expr.Compare(o,t,a,b) ->
      let llt = (s.types t) in (match t with
        | Type.Float(_) -> Llvm.build_fcmp (match o with
          | Eq -> Llvm.Fcmp.Oeq | Ne -> Llvm.Fcmp.One
          | UGt -> Llvm.Fcmp.Ogt | UGe -> Llvm.Fcmp.Oge | ULt -> Llvm.Fcmp.Olt | ULe -> Llvm.Fcmp.Ole  (* FIXME: correct float comparisons? signed/unsigned doesn't make sense *)
          | SGt -> Llvm.Fcmp.Ogt | SGe -> Llvm.Fcmp.Oge | SLt -> Llvm.Fcmp.Olt | SLe -> Llvm.Fcmp.Ole)
        | Type.Integer(_) -> Llvm.build_icmp (match o with
          | Cmp.Eq -> Llvm.Icmp.Eq | Cmp.Ne -> Llvm.Icmp.Ne
          | Cmp.UGt -> Llvm.Icmp.Ugt | Cmp.UGe -> Llvm.Icmp.Uge | Cmp.ULt -> Llvm.Icmp.Ult | Cmp.ULe -> Llvm.Icmp.Ule
          | Cmp.SGt -> Llvm.Icmp.Sgt | Cmp.SGe -> Llvm.Icmp.Sge | Cmp.SLt -> Llvm.Icmp.Slt | Cmp.SLe -> Llvm.Icmp.Sle)
      ) (value llt a s) (value llt b s) v bb
    | Expr.Array(t, l, vs) ->
      let t      = s.types t in
      let len,vs = (match vs with
                    | Arrays.InitList(vs) -> (make_int (List.length vs), List.map vs (fun e -> value t e s))
                    | Arrays.Length(l) -> (value Runtime.int_type l s, [])) in
      let data   = if l then Llvm.build_array_alloca t len (v ^ ".data") bb
                        else begin
                          let size = Llvm.build_gep (Llvm.const_null (Llvm.pointer_type t)) [| len |] "" bb in
                          let int_size = Llvm.build_ptrtoint size Runtime.size_type "" bb in
                          Llvm.build_call (Runtime.malloc t) [| int_size |] (v ^ ".data") bb 
                        end in
      let tmparr = Llvm.build_insertvalue (Llvm.undef (Runtime.array_type t)) len 0 "" bb in
      let arr    = Llvm.build_insertvalue tmparr data 1 v bb in
      List.iteri vs (fun i e ->
        Llvm.build_store e (Llvm.build_in_bounds_gep data [| make_int i |] (v ^ "." ^ (string_of_int i)) bb) bb |> ignore
      );
      arr
    | Expr.Length(a) -> Llvm.build_extractvalue (value void_type a s) 0 v bb (* TODO: void_type is a hack here since we know the value must be a Var or Constant... *)
    | Expr.Load(Type.Array(t) as arr_t,a,i) ->
      let data = Llvm.build_extractvalue (value (s.types arr_t) a s) 1 (v ^ ".data") bb in
      let ptr  = Llvm.build_gep data [| value (Llvm.i32_type context) i s |] (v ^ ".ptr") bb in
      Llvm.build_load ptr v bb
    | Expr.Split(Type.Array(t) as arr_t,l,a,b) ->
      let t      = s.types t in
      let arr_t  = s.types arr_t in
      let a      = value arr_t a s in
      let b      = (value (Runtime.array_type Runtime.int_type) b s) in
      let length = Llvm.build_extractvalue b 0 "" bb in
      let data   = if l then Llvm.build_array_alloca arr_t length (v ^ ".data") bb
                        else begin
                          let size = Llvm.build_gep (Llvm.const_null (Llvm.pointer_type arr_t)) [| length |] "" bb in
                          let int_size = Llvm.build_ptrtoint size Runtime.size_type "" bb in
                          Llvm.build_call (Runtime.malloc arr_t) [| int_size |] (v ^ ".data") bb 
                        end in
      let tmparr = Llvm.build_insertvalue (Llvm.undef (Runtime.array_type arr_t)) length 0 "" bb in
      let arr    = Llvm.build_insertvalue tmparr data 1 v bb in
      Llvm.build_call (Runtime.arrays_split t) [| data; a; b; (Llvm.size_of t) |] "" bb |> ignore;
      arr
    | Expr.Merge(Type.Array(t) as arr_t,x) ->
      let t      = s.types t in
      let arr_t  = s.types arr_t in
      let x      = (value (Runtime.array_type arr_t) x s) in
      Llvm.build_call (Runtime.arrays_merge t) [| x; (Llvm.size_of t) |] v bb
  in

  let state = state |>

  (* Phi Nodes *)
  fold (fun (v,t,incoming) s ->
      let t = (s.types t) in
      let old_value = (match Map.find s.undef v with Some x -> x | None -> raise (Stream.Error ("re-def of " ^ v))) in
      let new_value = Llvm.build_phi (List.map incoming (fun (i,l) -> (value t i s, get_block l))) v bb in
      Llvm.replace_all_uses_with old_value new_value;
      (*Llvm.delete_global old_value;*)
    { s with values=Map.add s.values v new_value; undef=Map.remove s.undef v }  
  ) block.phis |>

  (* Other Instructions *)
  fold (fun i s -> match i with
    (* Assignments and Expressions *)
    | Instruction.Assign(v,e) ->
        let old_value = (match Map.find s.undef v with Some x -> x | None -> raise (Stream.Error ("re-def of " ^ v))) in
        let new_value = build_expr v s e in
        Llvm.replace_all_uses_with old_value new_value;
        (*Llvm.delete_global old_value;*)
        { s with values=Map.add s.values v new_value; undef=Map.remove s.undef v }
    (* Array Stores *)
    | Instruction.Store(Type.Array(t) as arr_t,a,e,i) ->
      let data = Llvm.build_extractvalue (value (s.types arr_t) a s) 1 "" bb in (* TODO: names for these temporaries? *)
      let ptr  = Llvm.build_gep data [| value (Llvm.i32_type context) i s |] "" bb in
      let e    = value (s.types t) e s in
      Llvm.build_store e ptr bb |> ignore;
      s
    (* Instance Constructions *)
    | Instruction.Construct(c,ps) ->
        let msg  = Llvm.undef (Typegen.structure s.types (List.map ps (fun (t,v) -> t))) |>
                   foldi (fun (i,(t,v)) msg -> Llvm.build_insertvalue msg (value (s.types t) v s) i "" bb) ps in
        Function.fast_call (match Map.find s.constructors c with Some x -> x | None -> raise (Undef_Const c)) [| worker; msg |] "" true bb |> ignore;
        s
    (* Message Emissions *)
    | Instruction.Emit(v,ps) ->
        let channel = value void_type v s in (* TODO: void_type is a hack here since we know the value must be a Var or Constant... *)
        let func    = Llvm.build_extractvalue channel 0 "" bb in (* TODO: names for these temporaries? *)
        let inst    = Llvm.build_extractvalue channel 1 "" bb in
        let msg     = Llvm.undef (Typegen.structure s.types (List.map ps (fun (t,v) -> t))) |>
                      foldi (fun (i,(t,v)) msg -> Llvm.build_insertvalue msg (value (s.types t) v s) i "" bb) ps in
        (* TODO: should assert that this is type safe. I.e. msg might be anon struct when func expects named *)
        let cast_func = Llvm.build_bitcast func (Llvm.pointer_type (Llvm.function_type void_type [| Llvm.type_of worker; Llvm.type_of inst; Llvm.type_of msg |])) "" bb in
        Function.fast_call cast_func [| worker; inst; msg |] "" true bb |> ignore;
        s
  ) block.instrs in

  (* Terminator instruction *)
  begin match block.terminator with
  | Terminator.Finish      -> Llvm.build_ret_void bb |> ignore
  | Terminator.Goto(l)     -> Llvm.build_br (get_block l) bb |> ignore
  | Terminator.Cond(v,a,b) -> Llvm.build_cond_br (value Runtime.bool_type v state) (get_block a) (get_block b) bb |> ignore
  end;

  state


let generate_body f blocks instance state =
  let bb = Llvm.builder_at_end context (Llvm.entry_block f) in
  let raw_instance = Llvm.build_bitcast instance Runtime.instance_t "raw_inst" bb in

  (* Adjusts channel values to include current instance *)
  let adjust_channel ~key:name ~data:v = match Llvm.classify_value v with
    | Llvm.ValueKind.Function -> Llvm.build_insertvalue (Llvm.const_struct context [| v; Llvm.undef Runtime.instance_t |]) raw_instance 1 name bb
    | _ -> v
  in

  (* Modify all channel values in state to include current instance *)
  { state with values=Map.mapi state.values adjust_channel } |>

  (* Create empty basic blocks and placeholder values *)
  init_state f blocks |>

  (* Actual code generation *)
  fold (generate_block f instance) blocks |> ignore


(* Generates match builder of type (Runtime.worker_t, instance_t, message+) -> void *)
(* Generates internal transition of type (Runtime.worker_t; [instance_t, message+]) -> void *)
(* If passing in non-None inst_load then the value passed to the builder for instance is ignored *)
let generate_queued_transition state instance_t t inst_load s =
  (* Initial match structure definition *)
  let match_type = Llvm.named_struct_type context ("match." ^ (string_of_int t.tid) ^ s) in
  let match_parts = instance_t :: (pattern_types state.types t.pattern) in

  (* Define transition body functions *)
  let slow_type = Llvm.function_type void_type [| Runtime.worker_t; Llvm.pointer_type match_type |] in
  let build_type = Llvm.function_type void_type (Array.of_list (Runtime.worker_t::match_parts)) in
  let slow = Function.fast ("slow." ^ (string_of_int t.tid) ^ s) slow_type in

  (* Finalise match data structure type *)
  Llvm.struct_set_body match_type (Array.of_list ((Llvm.pointer_type slow_type)::match_parts)) false;

  (* Generate match building function (using slow version of transition) *)
  let build = Function.inlined ("build_match." ^ (string_of_int t.tid) ^ s) build_type
  in (
    let bb = Llvm.builder_at_end context (Llvm.entry_block build) in

    Llvm.add_function_attr build Llvm.Attribute.Nounwind;
    Llvm.add_function_attr build Llvm.Attribute.Alwaysinline;

    (* Allocate the match *)
    let m = Llvm.build_call (Runtime.match_alloc match_type) [| Llvm.param build 0 |] "m" bb in

    (* Populate match structure [note that match structure is same as        *)
    (*   except that first argument is worker and first field is transition  *)
    (*   function pointer.                                                   *)
    Array.iteri (fun i -> fun p -> Llvm.build_store
      (if i = 0 then slow else p)
      (Llvm.build_struct_gep m i ("m." ^ string_of_int i) bb)
      bb |> ignore) (Llvm.params build);

    (* Push match onto worker's queue *)
    Llvm.build_call Runtime.match_push [| Llvm.param build 0 |] "" bb |> ignore;
    Llvm.build_ret_void bb |> ignore
  );

  (* Initialise state *)
  let bb = Llvm.builder_at_end context (Llvm.entry_block slow) in
  let m = Llvm.param slow 1 in

  (* Initialise arguments in state by extracting from match *)
  state |> foldi (fun (i,(_,ts)) -> foldi (fun (j,(t,var)) s -> { s with values=Map.add s.values
    var
    (
      (* TODO: Refactor so that this does a load on the whole struct, then extractvalue for each element - probably more efficient *)
      (* TODO: Try to push the resulting common stuff into generate_body *)
      let ptr = Llvm.build_in_bounds_gep m [| make_int 0; make_int (i+2); make_int j |] (var ^ "_ptr") bb in
      Llvm.build_load ptr var bb
    )
  }) ts) t.pattern |>
  
  (* Define body for slow version *)
  generate_body slow t.blocks (
    match inst_load with
    | Some il -> il bb
    | None    -> let ptr = Llvm.build_struct_gep m 1 "inst_ptr" bb in
                 Llvm.build_load ptr "inst" bb
  );
  build

(* Generates LLVM function of type (Runtime.worker_t, instance_t, message+) -> void *)
let generate_immediate_transition state instance_t t =
  let match_parts = instance_t :: (pattern_types state.types t.pattern) in
  let fast_type = Llvm.function_type void_type (Array.of_list (Runtime.worker_t::match_parts)) in
  let fast = Function.inlined ("fast." ^ string_of_int t.tid) fast_type in
  let bb = Llvm.builder_at_end context (Llvm.entry_block fast) in

  (* Initialise arguments in state with by extracting values from parameter structures *)
  state |> foldi (fun (i,(_,ts)) -> foldi (fun (j,(t,var)) s -> { s with values=Map.add s.values
    var
    (Llvm.build_extractvalue (Llvm.param fast (i+2)) j var bb)
  }) ts) t.pattern |>

  (* Define body for fast version *)
  generate_body fast t.blocks (Llvm.param fast 1);
  fast
