(* Dovetail JCAM
 *
 * Copyright 2013      Peter Calvert <prc33@cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)
 
open Jcam

module SMap = Map.Make(String)

exception No_value
let option_get x = match x with
    Some x -> x
  | None -> raise No_value

let fold  f l s = List.fold_left (fun s x -> f x s) s l
let foldi f l s = snd (List.fold_left (fun (i,s) x -> (i+1, f (i,x) s)) (0, s) l)

(** LLVM Code Generation                                                      *)

let context = Llvm.global_context ()
let llmod = Runtime.llmod

let void_type = Llvm.void_type context
let make_int = Llvm.const_int (Llvm.i32_type context)

let cmp_null v = Llvm.build_icmp Llvm.Icmp.Eq v (Llvm.const_null (Llvm.type_of v))

type state = {
  types: Llvm.lltype SMap.t;
  values: Llvm.llvalue SMap.t;
  blocks: Llvm.llbasicblock SMap.t;
  undef: Llvm.llvalue SMap.t;
  constructors: Llvm.llvalue SMap.t;
  constants: Llvm.llvalue SMap.t;
}

let blank = { types=SMap.empty; values=SMap.empty; blocks=SMap.empty; undef=SMap.empty; constructors=SMap.empty; constants=SMap.empty }

let rec convert_type types t = match t with
  | Type.Alias   s  -> SMap.find s types
  | Type.Array   e  -> Runtime.array_type (convert_type types e)
  | Type.Float   w  -> (match w with
                  | 32 -> Llvm.float_type context
                  | 64 -> Llvm.double_type context
                  | _  -> failwith ("No floating point type of width " ^ (string_of_int w)))
  | Type.Integer w  -> Llvm.integer_type context w
  | Type.Struct  ts -> make_struct types ts (* TODO: should these be passed around by reference or value??? *)
  | Type.Channel ts -> Llvm.struct_type context [|
                    Llvm.pointer_type (Llvm.function_type void_type [| Runtime.worker_t; Runtime.instance_t; make_struct types ts |]);
                    Runtime.instance_t
                  |]
    and make_struct types ts = Llvm.struct_type context (Array.of_list (List.map (convert_type types) ts))

let populate_type (name, t) types = match t with
  | Type.Struct  _ -> SMap.add name (Llvm.named_struct_type context ("named_struct." ^ name)) types
  | Type.Channel _ -> SMap.add name (Llvm.named_struct_type context ("named_channel." ^ name)) types
  | _ -> types

let generate_type (name, t) types = match t with
  | Type.Struct _ ->
      Llvm.struct_set_body (SMap.find name types) (Llvm.struct_element_types (convert_type types t)) false;
      types
  | Type.Channel _ ->
      Llvm.struct_set_body (SMap.find name types) (Llvm.struct_element_types (convert_type types t)) false;
      types
  | _ ->
      SMap.add name (convert_type types t) types

let pattern_types types = List.map (function (c, ts) -> make_struct types (List.map fst ts))

module Function = struct
  let fastcc = Llvm.CallConv.fast

  let inlined f = 
    Llvm.add_function_attr f Llvm.Attribute.Nounwind;
    Llvm.add_function_attr f Llvm.Attribute.Alwaysinline;
    f

  let fast f =
    Llvm.add_function_attr f Llvm.Attribute.Nounwind;
    Llvm.set_function_call_conv fastcc f;
    f

  let zext f =
    Llvm.add_function_attr f Llvm.Attribute.Zext;
    f
end

let init_state f = fold (fun b state -> (match b.label with
    | None   ->   state
    | Some l -> { state with blocks=SMap.add l (Llvm.append_block context l f) state.blocks }
    ) |>

    (* Placeholder values for any Phi-nodes *)
    fold (fun (v,t,_) state ->
      let placeholder = Llvm.declare_global (convert_type state.types t) ("fref." ^ v) llmod in
      { state with values=SMap.add v placeholder state.values; undef=SMap.add v placeholder state.undef }
    ) b.phis |>

    (* Placeholder values for all assignments *)
    fold (fun i state -> (match i with
      | Instruction.Assign(v,e) ->
          let placeholder = Llvm.declare_global (convert_type state.types (Typing.return_type e)) ("fref." ^ v) llmod in
          { state with values=SMap.add v placeholder state.values; undef=SMap.add v placeholder state.undef }
      | _ -> state
      )
    ) b.instrs
  )

let generate_block f instance block state =
  let value t v s = match v with
    | Value.Var(v)      -> SMap.find v s.values
    | Value.Integer(x)  -> Llvm.const_int t x
    | Value.Float(x)    -> Llvm.const_float t x
    | Value.Constant(v) -> SMap.find v s.constants
    | Value.Null        -> Llvm.const_null t
  in

  (* Position builder at correct block *)
  let bb = Llvm.builder_at_end context (match block.label with
    | Some l -> SMap.find l state.blocks
    | None   -> Llvm.entry_block f)
  in

  (* Worker structure (always first parameter for both slow and fast *)
  let worker = Llvm.param f 0 in

  let build_expr v s = function
    | Expr.Add(t,a,b) -> let t = (convert_type s.types t) in Llvm.build_add (value t a s) (value t b s) v bb
    | Expr.Sub(t,a,b) -> let t = (convert_type s.types t) in Llvm.build_sub (value t a s) (value t b s) v bb
    | Expr.Mul(t,a,b) -> let op = match t with
                                  | Type.Integer(_) -> Llvm.build_mul
                                  | Type.Float(_) -> Llvm.build_fmul in
                         let t = (convert_type s.types t) in op (value t a s) (value t b s) v bb
    | Expr.Div(t,a,b) -> let op = match t with
                                  | Type.Integer(_) -> Llvm.build_sdiv
                                  | Type.Float(_) -> Llvm.build_fdiv in
                         let t = (convert_type s.types t) in op (value t a s) (value t b s) v bb
    | Expr.Compare(o,t,a,b) ->
      let llt = (convert_type s.types t) in (match t with
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
      let t      = convert_type s.types t in
      let len,vs = (match vs with
                    | Arrays.InitList(vs) -> (make_int (List.length vs), List.map (fun e -> value t e s) vs)
                    | Arrays.Length(l) -> (value Runtime.int_type l s, [])) in
      let data   = if l then Llvm.build_array_alloca t len (v ^ ".data") bb
                        else begin
                          let size = Llvm.build_gep (Llvm.const_null (Llvm.pointer_type t)) [| len |] "" bb in
                          let int_size = Llvm.build_ptrtoint size Runtime.size_type "" bb in
                          Llvm.build_call (Runtime.malloc t) [| int_size |] (v ^ ".data") bb 
                        end in
      let tmparr = Llvm.build_insertvalue (Llvm.undef (Runtime.array_type t)) len 0 "" bb in
      let arr    = Llvm.build_insertvalue tmparr data 1 v bb in
      List.iteri (fun i e ->
        Llvm.build_store e (Llvm.build_in_bounds_gep data [| make_int i |] (v ^ "." ^ (string_of_int i)) bb) bb |> ignore
      ) vs;
      arr
    | Expr.Length(a) -> Llvm.build_extractvalue (value void_type a s) 0 v bb (* TODO: void_type is a hack here since we know the value must be a Var or Constant... *)
    | Expr.Load(Type.Array(t) as arr_t,a,i) ->
      let data = Llvm.build_extractvalue (value (convert_type s.types arr_t) a s) 1 (v ^ ".data") bb in
      let ptr  = Llvm.build_gep data [| value (Llvm.i32_type context) i s |] (v ^ ".ptr") bb in
      Llvm.build_load ptr v bb
    | Expr.Split(Type.Array(t) as arr_t,l,a,b) ->
      let t      = convert_type s.types t in
      let arr_t  = convert_type s.types arr_t in
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
    | Merge(Type.Array(t) as arr_t,x) ->
      let t      = convert_type s.types t in
      let arr_t  = convert_type s.types arr_t in
      let x      = (value (Runtime.array_type arr_t) x s) in
      Llvm.build_call (Runtime.arrays_merge t) [| x; (Llvm.size_of t) |] v bb
  in

  let state = state |>

  (* Phi Nodes *)
  fold (fun (v,t,incoming) s ->
      let t = (convert_type s.types t) in
      let old_value = SMap.find v s.undef in
      let new_value = Llvm.build_phi (List.map (fun (i,l) -> (value t i s, SMap.find l s.blocks)) incoming) v bb in
      Llvm.replace_all_uses_with old_value new_value;
      Llvm.delete_global old_value;
    { s with values=SMap.add v new_value s.values; undef=SMap.remove v s.undef }  
  ) block.phis |>

  (* Other Instructions *)
  fold (fun i s -> match i with
    (* Assignments and Expressions *)
    | Instruction.Assign(v,e) ->
        let old_value = SMap.find v s.undef in
        let new_value = build_expr v s e in
        Llvm.replace_all_uses_with old_value new_value;
        Llvm.delete_global old_value;
        { s with values=SMap.add v new_value s.values; undef=SMap.remove v s.undef }
    (* Array Stores *)
    | Instruction.Store(Type.Array(t) as arr_t,a,e,i) ->
      let data = Llvm.build_extractvalue (value (convert_type s.types arr_t) a s) 1 "" bb in (* TODO: names for these temporaries? *)
      let ptr  = Llvm.build_gep data [| value (Llvm.i32_type context) i s |] "" bb in
      let e    = value (convert_type s.types t) e s in
      Llvm.build_store e ptr bb |> ignore;
      s
    (* Instance Constructions *)
    | Instruction.Construct(c,ps) ->
        let msg  = Llvm.undef (make_struct s.types (List.map (fun (t,v) -> t) ps)) |>
                   foldi (fun (i,(t,v)) msg -> Llvm.build_insertvalue msg (value (convert_type s.types t) v s) i "" bb) ps in
        let call = Llvm.build_call (SMap.find c s.constructors) [| worker; msg |] "" bb in
        Llvm.set_instruction_call_conv Function.fastcc call;
        Llvm.set_tail_call true call;
        s
    (* Message Emissions *)
    | Instruction.Emit(v,ps) ->
        let channel = value (void_type) v s in (* TODO: void_type is a hack here since we know the value must be a Var or Constant... *)
        let func    = Llvm.build_extractvalue channel 0 "" bb in (* TODO: names for these temporaries? *)
        let inst    = Llvm.build_extractvalue channel 1 "" bb in
        let msg     = Llvm.undef (make_struct s.types (List.map (fun (t,v) -> t) ps)) |>
                      foldi (fun (i,(t,v)) msg -> Llvm.build_insertvalue msg (value (convert_type s.types t) v s) i "" bb) ps in
        let call    = Llvm.build_call func [| worker; inst; msg |] "" bb in
        Llvm.set_instruction_call_conv Function.fastcc call;
        Llvm.set_tail_call true call;
        s
  ) block.instrs in

  (* Terminator instruction *)
  begin match block.terminator with
  | Terminator.Finish      -> Llvm.build_ret_void bb |> ignore
  | Terminator.Goto(l)     -> Llvm.build_br (SMap.find l state.blocks) bb |> ignore
  | Terminator.Cond(v,a,b) -> Llvm.build_cond_br (value Runtime.bool_type v state) (SMap.find a state.blocks) (SMap.find b state.blocks) bb |> ignore
  end;

  state


let generate_body f blocks instance state =
  let bb = Llvm.builder_at_end context (Llvm.entry_block f) in
  let raw_instance = Llvm.build_bitcast instance Runtime.instance_t "raw_inst" bb in

  (* Adjusts channel values to include current instance *)
  let adjust_channel name v = match Llvm.classify_value v with
    | Llvm.ValueKind.Function -> Llvm.build_insertvalue (Llvm.const_struct context [| v; Llvm.undef Runtime.instance_t |]) raw_instance 1 name bb
    | _ -> v
  in

  (* Modify all channel values in state to include current instance *)
  { state with values=SMap.mapi adjust_channel state.values } |>

  (* Create empty basic blocks and placeholder values *)
  init_state f blocks |>

  (* Actual code generation *)
  fold (generate_block f instance) blocks |> ignore


let generate_slow_transition state instance_t t =
  (* Initial match structure definition *)
  let match_type = Llvm.named_struct_type context ("match." ^ string_of_int t.tid) in
  let match_parts = instance_t :: (pattern_types state.types t.pattern) in

  (* Define transition body functions *)
  let slow_type = Llvm.function_type void_type [| Runtime.worker_t; Llvm.pointer_type match_type |] in
  let build_type = Llvm.function_type void_type (Array.of_list (Runtime.worker_t::match_parts)) in
  let slow = Function.fast (Llvm.define_function ("slow." ^ string_of_int t.tid) slow_type llmod) in

  (* Finalise match data structure type *)
  Llvm.struct_set_body match_type (Array.of_list ((Llvm.pointer_type slow_type)::match_parts)) false;

  (* Generate match building function (using slow version of transition) *)
  let build = Function.inlined (Llvm.define_function ("build_match." ^ string_of_int t.tid) build_type llmod)
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
  state |> foldi (fun (i,(_,ts)) -> foldi (fun (j,(t,var)) s -> { s with values=SMap.add
    var
    (
      let ptr = Llvm.build_in_bounds_gep m [| make_int 0; make_int (i+2); make_int j |] (var ^ "_ptr") bb in
      Llvm.build_load ptr var bb
    )
  s.values }) ts) t.pattern |>
  
  (* Define body for slow version *)
  generate_body slow t.blocks (
    let ptr = Llvm.build_struct_gep m 1 "inst_ptr" bb in
    Llvm.build_load ptr "inst" bb
  );
  build

let generate_fast_transition state instance_t t =
  let match_parts = instance_t :: (pattern_types state.types t.pattern) in
  let fast_type = Llvm.function_type void_type (Array.of_list (Runtime.worker_t::match_parts)) in
  let fast = Function.inlined (Llvm.define_function ("fast." ^ string_of_int t.tid) fast_type llmod) in
  let bb = Llvm.builder_at_end context (Llvm.entry_block fast) in

  (* Initialise arguments in state with by extracting values from parameter structures *)
  state |> foldi (fun (i,(_,ts)) -> foldi (fun (j,(t,var)) s -> { s with values=SMap.add
    var
    (Llvm.build_extractvalue (Llvm.param fast (i+2)) j var bb)
  s.values }) ts) t.pattern |>

  (* Define body for fast version *)
  generate_body fast t.blocks (Llvm.param fast 1);
  fast

let generate_def state def =
  (* Function to calculate channel implementations and types. *)
  let make_channels f instance =
    let rec mc i ts = function
      | (c::cs) -> (match f c.cattrs with
                    | Some impl_f ->
                        let (t,impl) = impl_f (make_struct state.types c.args) in
                        SMap.add c.name (i,impl) (mc (i+1) (t::ts) cs)
                    | None -> mc i ts cs)
      | []      -> Llvm.struct_set_body instance (Array.of_list (List.rev ts)) false; SMap.empty
    in mc 0 [] def.channels
  in

  (* Declares functions for constructors and channels, with the given prefix and adds to the state *)
  let declare_channels version = fold (fun c state ->
      let chan_type = make_struct state.types c.args in
      if c.constructor then
        let t = Llvm.function_type void_type [| Runtime.worker_t; chan_type |] in
        let f = Function.fast (Llvm.define_function ("construct." ^ version ^ "." ^ c.name) t llmod) in
        { state with constructors=SMap.add c.name f state.constructors }
      else
        let t = Llvm.function_type void_type [| Runtime.worker_t; Runtime.instance_t; chan_type |] in
        let f = Function.fast (Llvm.define_function ("emit." ^ version ^ "." ^ c.name) t llmod) in
        { state with values=SMap.add c.name f state.values }
    ) def.channels
  in

  (* Fast version of definition (keeping construct functions for use in slow version) *)
  let fast_constructors = if List.mem DAttribute.Closed def.dattrs then
    let fast_instance = Llvm.named_struct_type context ("instance.fast." ^ string_of_int def.did) in
    let fast_channels = make_channels Runtime.fast_channel fast_instance in

    (* Declare construct/emit functions, adding them to the state. *)
    let fast_state = state |> (declare_channels "fast") in

    (* Compile transitions (keeping track of functions for emit functions later) *)
    let compiled_transitions = List.map (fun x -> (x, generate_fast_transition fast_state (Llvm.pointer_type fast_instance) x)) def.transitions in

    (* Compilation function for fast emit bodies *)
    let generate_fast_emit f c inst channels value bb tail_call = 
      (* Worker structure (always first parameter) *)
      let worker = Llvm.param f 0 in

      (* Normal Channels *)
      if SMap.mem c.name fast_channels then begin
        let (_, this_impl) = (SMap.find c.name fast_channels) in

        (* TODO: this pattern matching can be forgotten for lower_bound=upper_bound, head channels (as the enqueue in those cases does not change anything) *)

        (* Consider every transition matching on c. Builder is passed through in fold manner *)
        let bb = bb |> fold (fun (transition,func) bb ->
          let next_transition = Llvm.append_block context ("post." ^ (string_of_int transition.tid)) f in

          (* Find pending messages *)
          let (bb,msgs) = (bb, []) |> List.fold_right (fun (name, _) (bb,msgs) ->
            if name = c.name then (
              bb, (name,value,this_impl)::msgs (* for the current channel we simply use the value passed in, message is only enqueued in case of no match *)
            ) else (
              let (_,impl)  = SMap.find name fast_channels in
              let next_chan = Llvm.append_block context ("post.find." ^ (string_of_int transition.tid) ^ "." ^ name) f in
              let msg       = impl.Runtime.fast_find (SMap.find name channels) ("msg." ^ (string_of_int transition.tid) ^ "." ^ name) bb in
              let check     = cmp_null msg ("fcheck." ^ (string_of_int transition.tid) ^ "." ^ name) bb in
              Llvm.build_cond_br check next_transition next_chan bb |> ignore;
              
              (Llvm.builder_at_end context next_chan, (name,msg,impl)::msgs)
            )
          ) transition.pattern in

          (* Deal with success. *)
          let data = List.map (fun (name,msg,impl) ->
            if name = c.name then (
              value
            ) else (
              impl.Runtime.fast_consume msg "" bb |> ignore;
              let ptr = impl.Runtime.fast_data msg ("data_ptr." ^ (string_of_int transition.tid) ^ "." ^ name) bb in
              Llvm.build_load ptr ("data." ^ (string_of_int transition.tid) ^ "." ^ name) bb
            )
          ) msgs in

          Llvm.build_call func (Array.of_list (worker::inst::data)) "" bb |> Llvm.set_tail_call tail_call;
          Llvm.build_ret_void bb |> ignore;

          (* Pass next_transition block for the next transition (or end of loop if this is the last) *)
          Llvm.builder_at_end context next_transition
        ) (List.filter (fun (t,b) -> List.mem c.name (List.map fst t.pattern)) compiled_transitions) in

        (* After all transitions fail, actually enqueue message. *)
        this_impl.Runtime.fast_enqueue (SMap.find c.name channels) value ("msg." ^ c.name) bb |> ignore;
        Llvm.build_ret_void bb |> ignore
      (* Channels where pattern matching is not required *)
      end else begin
        let (transition,func) = List.find (fun (t,b) -> (List.map fst t.pattern) = [ c.name ]) compiled_transitions in
        Llvm.build_call func [| worker; inst; value |] "" bb |> Llvm.set_tail_call tail_call;
        Llvm.build_ret_void bb |> ignore
      end

    in

    (* Actual produce emit and construct function bodies *)
    List.iter (fun c ->
      (* Construtors (worker, value): allocate instance on stack, initialise channels then build body *)
      if c.constructor then
        let f  = SMap.find c.name fast_state.constructors in
        let bb = Llvm.builder_at_end context (Llvm.entry_block f) in
        let inst = Llvm.build_alloca fast_instance "inst" bb in
        
        let channels = SMap.mapi (fun name (i,impl) ->
          let raw_ptr = Llvm.build_struct_gep inst i (name ^ ".raw") bb in
          let ptr     = impl.Runtime.fast_build_cast raw_ptr name bb in
          impl.Runtime.fast_init ptr "" bb |> ignore;
          ptr
        ) fast_channels in

        generate_fast_emit f c inst channels (Llvm.param f 1) bb false
      (* Normal (worker, instance, value): bitcast instance pointer, extract channel pointers, then build body *)
      else
        let f    = SMap.find c.name fast_state.values in
        let bb   = Llvm.builder_at_end context (Llvm.entry_block f) in
        let inst = Llvm.build_bitcast (Llvm.param f 1) (Llvm.pointer_type fast_instance) "inst" bb in

        let channels = SMap.mapi (fun name (i,impl) ->
          let raw_ptr = Llvm.build_struct_gep inst i (name ^ ".raw") bb in
          impl.Runtime.fast_build_cast raw_ptr name bb
        ) fast_channels in

        generate_fast_emit f c inst channels (Llvm.param f 2) bb true
    ) def.channels;

    fast_state.constructors
  else
    SMap.empty
  in

  (* Slow version of definition *)
  let slow_instance = Llvm.named_struct_type context ("instance.slow." ^ string_of_int def.did) in
  let slow_channels = make_channels Runtime.slow_channel slow_instance in

  (* Declare construct/emit functions, adding them to the state. *)
  let slow_state = state |> (declare_channels "slow") in

  (* Compile transitions (keeping track of functions for emit functions later) *)
  let compiled_transitions = List.map (fun x -> (x, generate_slow_transition slow_state (Llvm.pointer_type slow_instance) x)) def.transitions in
  
  (* Compilation function for slow emit bodies *)
  let generate_slow_emit f c inst channels value bb =
    (* Worker structure (always first parameter) *)
    let worker = Llvm.param f 0 in

    (* Normal Channels *)
    if SMap.mem c.name slow_channels then begin
      let (_, this_impl) = (SMap.find c.name slow_channels) in
      let retry_false = Llvm.const_int (Llvm.i8_type context) 0 in

      (* Enqueuing of new message *)
      let new_msg = this_impl.Runtime.slow_enqueue (SMap.find c.name channels) value ("msg." ^ c.name) bb in

      (* Allocate retry flag *)
      let retry = Llvm.build_alloca (Llvm.i8_type context) "retry" bb in

      (* Create exit block *)
      let exit = Llvm.append_block context "exit" f in
      Llvm.build_ret_void (Llvm.builder_at_end context exit) |> ignore;

      (* Create loop header and branch to it *)
      let loop = Llvm.append_block context "loop" f in
      Llvm.build_br loop bb |> ignore;

      (* This is the first block of the loop body *)
      let body = Llvm.append_block context "body" f in

      (* Loop Header Condition: Is new message consumed? *)
      let loop_bb = Llvm.builder_at_end context loop in begin
        Llvm.build_cond_br
          (this_impl.Runtime.slow_is_consumed new_msg "consumed" loop_bb)
          exit
          body
          loop_bb
        |> ignore
      end;

      (* Initialise retry flag to 0 *)
      let bb = Llvm.builder_at_end context body in
      Llvm.build_store retry_false retry bb |> ignore;

      (* Consider every transition matching on c. Builder is passed through in fold manner *)
      let bb = bb |> fold (fun (transition,build) bb ->
        let next_transition = Llvm.append_block context ("post." ^ (string_of_int transition.tid)) f in

        (* Find pending messages *)
        let (bb,msgs) = (bb, []) |> List.fold_right (fun (name, _) (bb,msgs) ->
          if name = c.name then (
            bb, (name,new_msg,this_impl)::msgs
          ) else (
            let (_,impl)  = SMap.find name slow_channels in
            let next_chan = Llvm.insert_block context ("post.find." ^ (string_of_int transition.tid) ^ "." ^ name) next_transition in
            let msg       = impl.Runtime.slow_find (SMap.find name channels) retry ("msg." ^ (string_of_int transition.tid) ^ "." ^ name) bb in
            let check     = cmp_null msg ("fcheck." ^ (string_of_int transition.tid) ^ "." ^ name) bb in
            Llvm.build_cond_br check next_transition next_chan bb |> ignore;
            
            (Llvm.builder_at_end context next_chan, (name,msg,impl)::msgs)
          )
        ) transition.pattern in

        let (bb,_) = (bb,next_transition) |> List.fold_right (fun (name,msg,impl) (bb,prev_revert) ->
          let new_commit = Llvm.insert_block context ("post.commit." ^ (string_of_int transition.tid) ^ "." ^ name) next_transition in
          let new_revert = Llvm.insert_block context ("revert." ^ (string_of_int transition.tid) ^ "." ^ name) next_transition in

          (* Try to claim message *)
          let check = impl.Runtime.slow_try_claim msg ("ccheck." ^ (string_of_int transition.tid) ^ "." ^ name) bb in
          Llvm.build_cond_br check new_commit prev_revert bb |> ignore;

          (* Reverts claimed message to pending in case of failed commit - chains with previous reverts *)
          let revert_bb = Llvm.builder_at_end context new_revert in begin
            impl.Runtime.slow_revert msg "" revert_bb |> ignore;
            Llvm.build_br prev_revert revert_bb |> ignore
          end;

          (Llvm.builder_at_end context new_commit, new_revert)
        ) msgs in

        (* Complete block that deals with success. *)
        let data = List.map (fun (name,msg,impl) ->
          impl.Runtime.slow_consume msg "" bb |> ignore;
          if name = c.name then
            value
          else
            let ptr = impl.Runtime.slow_data msg ("data_ptr." ^ (string_of_int transition.tid) ^ "." ^ name) bb in
            Llvm.build_load ptr ("data." ^ (string_of_int transition.tid) ^ "." ^ name) bb
        ) msgs in

        Llvm.build_call build (Array.of_list (worker::inst::data)) "" bb |> ignore;
        Llvm.build_ret_void bb |> ignore;

        (* Pass next_transition block for the next transition (or end of loop if this is the last) *)
        Llvm.builder_at_end context next_transition
      ) (List.filter (fun (t,b) -> List.mem c.name (List.map fst t.pattern)) compiled_transitions) in

      (* Loop Footer Condition: Are we meant to retry? *)
      let retry_value = Llvm.build_load retry "retry_value" bb in
      let retry_check = Llvm.build_icmp Llvm.Icmp.Eq retry_value retry_false "retry_check" bb in
      Llvm.build_cond_br retry_check exit loop bb |> ignore
    (* Channels where pattern matching is not required *)
    end else begin
      let (transition,build) = List.find (fun (t,b) -> (List.map fst t.pattern) = [ c.name ]) compiled_transitions in
      Llvm.build_call build [| worker; inst; value |] "" bb |> ignore;
      Llvm.build_ret_void bb |> ignore
    end
  in

  (* Actual produce emit and construct function bodies *)
  List.iter (fun c ->
    (* Construtors (worker, value): check for fast mode, allocate instance on heap, initialise channels then build body *)
    if c.constructor then      
      let f         = SMap.find c.name slow_state.constructors in
      let bb        = Llvm.builder_at_end context (Llvm.entry_block f) in
      let slow_mode = Llvm.append_block context "slow_mode" f in
      
      if SMap.mem c.name fast_constructors then (
        let fast_mode = Llvm.append_block context "fast_mode" f in
        let check = Llvm.build_call Runtime.fast_check [| Llvm.param f 0 |] "check" bb in
        Llvm.build_cond_br check fast_mode slow_mode bb |> ignore;

        let bb = Llvm.builder_at_end context fast_mode in
        let call = Llvm.build_call (SMap.find c.name fast_constructors) (Llvm.params f) "" bb in
        Llvm.set_instruction_call_conv Function.fastcc call;
        Llvm.set_tail_call true call;
        Llvm.build_ret_void bb |> ignore
      ) else (
        Llvm.build_br slow_mode bb |> ignore
      );

      (* Heap allocation *)
      let bb   = Llvm.builder_at_end context slow_mode in
      let inst = Llvm.build_call (Runtime.malloc slow_instance) [| Llvm.size_of slow_instance |] "inst" bb in
      
      let channels = SMap.mapi (fun name (i,impl) ->
        let raw_ptr = Llvm.build_struct_gep inst i (name ^ ".raw") bb in
        let ptr     = impl.Runtime.slow_build_cast raw_ptr name bb in
        impl.Runtime.slow_init ptr "" bb |> ignore;
        ptr
      ) slow_channels in

      generate_slow_emit f c inst channels (Llvm.param f 1) bb
    (* Normal (worker, instance, value): bitcast instance pointer, extract channel pointers, then build body *)
    else
      let f    = SMap.find c.name slow_state.values in
      let bb   = Llvm.builder_at_end context (Llvm.entry_block f) in
      let inst = Llvm.build_bitcast (Llvm.param f 1) (Llvm.pointer_type slow_instance) "inst" bb in

      let channels = SMap.mapi (fun name (i,impl) ->
        let raw_ptr = Llvm.build_struct_gep inst i (name ^ ".raw") bb in
        impl.Runtime.slow_build_cast raw_ptr name bb
      ) slow_channels in

      generate_slow_emit f c inst channels (Llvm.param f 2) bb
  ) def.channels

let generate_externs (name, ts, kt) state =
  (* TODO: these should probably all be queued up as matches to keep with the parallel ethos *)
  match kt with
  (* Return with Value Case *)
  | Type.Return(result_type) -> 
      (* External Function Declaration *)
      let func_type = Llvm.function_type (convert_type state.types result_type) (Array.of_list (List.map (convert_type state.types) ts)) in
      let func      = Llvm.declare_function name func_type llmod in
      (* Emit Function *)
      let chan_type = make_struct state.types (ts @ [Type.Channel([result_type])]) in
      let emit_type = Llvm.function_type void_type [| Runtime.worker_t; Runtime.instance_t; chan_type |] in
      let emit      = Function.fast (Llvm.define_function ("emit.extern." ^ name) emit_type llmod) in
      let bb        = Llvm.builder_at_end context (Llvm.entry_block emit) in
      (* Extract function parameters and do call *)
      let msg_in    = Llvm.param emit 2 in
      let params    = Array.init (List.length ts) (fun i -> Llvm.build_extractvalue msg_in i "" bb) in
      let result    = Llvm.build_call func params "result" bb in
      (* Emit result to continuation *)
      let channel   = Llvm.build_extractvalue msg_in (List.length ts) "k" bb in
      let k_func    = Llvm.build_extractvalue channel 0 "k_func" bb in
      let k_inst    = Llvm.build_extractvalue channel 1 "k_inst" bb in
      let msg_out   = Llvm.build_insertvalue (Llvm.undef (make_struct state.types [ result_type ])) result 0 "msg" bb in
      let call      = Llvm.build_call k_func [| (Llvm.param emit 0); k_inst; msg_out |] "" bb in
      Llvm.set_instruction_call_conv Function.fastcc call;
      Llvm.set_tail_call true call;
      Llvm.build_ret_void bb |> ignore;
      { state with constants=SMap.add name (Llvm.const_struct context [| emit; Llvm.const_null Runtime.instance_t |]) state.constants }
  (* Return without Value Case *)
  | Type.Void ->
      (* External Function Declaration *)
      let func_type = Llvm.function_type void_type (Array.of_list (List.map (convert_type state.types) ts)) in
      let func      = Llvm.declare_function name func_type llmod in
      (* Emit Function *)
      let chan_type = make_struct state.types (ts @ [Type.Channel([])]) in
      let emit_type = Llvm.function_type void_type [| Runtime.worker_t; Runtime.instance_t; chan_type |] in
      let emit      = Function.fast (Llvm.define_function ("emit.extern." ^ name) emit_type llmod) in
      let bb        = Llvm.builder_at_end context (Llvm.entry_block emit) in
      (* Extract function parameters and do call *)
      let msg_in    = Llvm.param emit 2 in
      let params    = Array.init (List.length ts) (fun i -> Llvm.build_extractvalue msg_in i "" bb) in
      let result    = Llvm.build_call func params "result" bb in
      (* Emit result to continuation *)
      let channel   = Llvm.build_extractvalue msg_in (List.length ts) "k" bb in
      let k_func    = Llvm.build_extractvalue channel 0 "k_func" bb in
      let k_inst    = Llvm.build_extractvalue channel 1 "k_inst" bb in
      let msg_out   = Llvm.const_struct context [| |] in
      let call      = Llvm.build_call k_func [| (Llvm.param emit 0); k_inst; msg_out |] "" bb in
      Llvm.set_instruction_call_conv Function.fastcc call;
      Llvm.set_tail_call true call;
      Llvm.build_ret_void bb |> ignore;
      { state with constants=SMap.add name (Llvm.const_struct context [| emit; Llvm.const_null Runtime.instance_t |]) state.constants }
  (* TODO: Async case *)
  | _ -> state (* enqueue a match that calls a helper that calls the function (helper needed to avoid fastcc) *)

let generate_module p =
  let top_types = SMap.empty |> fold populate_type p.named_types |> fold generate_type p.named_types in
  let state = { blank with types=top_types} |> fold generate_externs p.externs in
  List.iter (generate_def state) p.definitions;
  Llvm.dump_module llmod; (* Useful for debugging *)
  llmod
