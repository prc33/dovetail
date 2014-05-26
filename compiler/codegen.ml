(* Dovetail JCAM
 *
 * Copyright 2013      Peter Calvert <prc33@cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)
 
open Jcam
open Core.Std

let fold  f l s = List.fold_left l ~init:s ~f:(fun s x -> f x s)

(** LLVM Code Generation                                                      *)

let context = Llvm.global_context ()
let llmod = Function.llmod

let void_type = Llvm.void_type context
let make_int = Llvm.const_int (Llvm.i32_type context)

let cmp_null v = Llvm.build_icmp Llvm.Icmp.Eq v (Llvm.const_null (Llvm.type_of v))

let compile_program p =
  (* Compile named types *)
  let types = Typegen.generate p.named_types in

  (* Declare all constructors - both fast and slow *)
  let declare_constructors definitions version =
    let f c =
        if c.constructor then
          let t = Llvm.function_type void_type [| Runtime.worker_t; Typegen.structure types c.args |] in
          Some (c.name, Function.fast ("construct." ^ version ^ "." ^ c.name) t)
        else
          None
    in String.Map.of_alist_exn (List.concat_map definitions (fun def -> (List.filter_map def.channels f)))
  in

  let slow_constructors = declare_constructors p.definitions "slow" in
  let fast_constructors = declare_constructors (List.filter p.definitions (fun def -> List.mem def.dattrs DAttribute.Closed)) "fast" in

  let generate_externs (name, ts, kt) constants =
    (* TODO: these should probably all be queued up as matches to keep with the parallel ethos *)
    match kt with
    (* Return with Value Case *)
    | Type.Return(result_type) -> 
        (* External Function Declaration *)
        let func_type = Llvm.function_type (types result_type) (Array.of_list (List.map ts (types))) in
        let func      = Function.declare name func_type in
        (* Emit Function *)
        let chan_type = Typegen.structure types (ts @ [Type.Channel([result_type])]) in
        let emit_type = Llvm.function_type void_type [| Runtime.worker_t; Runtime.instance_t; chan_type |] in
        let emit      = Function.fast ("emit.extern." ^ name) emit_type in
        let bb        = Llvm.builder_at_end context (Llvm.entry_block emit) in
        (* Extract function parameters and do call *)
        let msg_in    = Llvm.param emit 2 in
        let params    = Array.init (List.length ts) (fun i -> Llvm.build_extractvalue msg_in i "" bb) in
        let result    = Llvm.build_call func params "result" bb in
        (* Emit result to continuation *)
        let channel   = Llvm.build_extractvalue msg_in (List.length ts) "k" bb in
        let k_func    = Llvm.build_extractvalue channel 0 "k_func" bb in
        let k_inst    = Llvm.build_extractvalue channel 1 "k_inst" bb in
        let msg_out   = Llvm.build_insertvalue (Llvm.undef (Typegen.structure types [ result_type ])) result 0 "msg" bb in
        let call      = Llvm.build_call k_func [| (Llvm.param emit 0); k_inst; msg_out |] "" bb in
        Llvm.set_instruction_call_conv Function.fastcc call;
        Llvm.set_tail_call true call;
        Llvm.build_ret_void bb |> ignore;
        Map.add constants name (Llvm.const_struct context [| emit; Llvm.const_null Runtime.instance_t |])
    (* Return without Value Case *)
    | Type.Void ->
        (* External Function Declaration *)
        let func_type = Llvm.function_type void_type (Array.of_list (List.map ts types)) in
        let func      = Function.declare name func_type in
        (* Emit Function *)
        let chan_type = Typegen.structure types (ts @ [Type.Channel([])]) in
        let emit_type = Llvm.function_type void_type [| Runtime.worker_t; Runtime.instance_t; chan_type |] in
        let emit      = Function.fast ("emit.extern." ^ name) emit_type in
        let bb        = Llvm.builder_at_end context (Llvm.entry_block emit) in
        (* Extract function parameters and do call *)
        let msg_in    = Llvm.param emit 2 in
        let params    = Array.init (List.length ts) (fun i -> Llvm.build_extractvalue msg_in i "" bb) in
        let result    = Llvm.build_call func params "" bb in
        (* Emit result to continuation *)
        let channel   = Llvm.build_extractvalue msg_in (List.length ts) "k" bb in
        let k_func    = Llvm.build_extractvalue channel 0 "k_func" bb in
        let k_inst    = Llvm.build_extractvalue channel 1 "k_inst" bb in
        let msg_out   = Llvm.const_struct context [| |] in
        let call      = Llvm.build_call k_func [| (Llvm.param emit 0); k_inst; msg_out |] "" bb in
        Llvm.set_instruction_call_conv Function.fastcc call;
        Llvm.set_tail_call true call;
        Llvm.build_ret_void bb |> ignore;
        Map.add constants name (Llvm.const_struct context [| emit; Llvm.const_null Runtime.instance_t |])
    (* TODO: Async case *)
    | _ -> constants (* enqueue a match that calls a helper that calls the function (helper needed to avoid fastcc) *)
  in
  let constants = List.fold_right p.externs ~f:generate_externs ~init:String.Map.empty in

  let generate_def def =
    (* Function to calculate channel implementations and types. *)
    let make_channels f instance =
      let rec mc i ts = function
        | (c::cs) -> (match f c.cattrs with
                      | Some impl_f ->
                          let (t,impl) = impl_f (Typegen.structure types c.args) in
                          Map.add (mc (i+1) (t::ts) cs) c.name (i,impl)
                      | None -> mc i ts cs)
        | []      -> Llvm.struct_set_body instance (Array.of_list (List.rev ts)) false; String.Map.empty
      in mc 0 [] def.channels
    in

    (* Declares functions for non-constructor channels, with the given prefix and returns a map. *)
    let declare_emits version = List.fold_left def.channels ~init:String.Map.empty ~f:(fun values c ->
        if c.constructor then
          values
        else
          let t = Llvm.function_type void_type [| Runtime.worker_t; Runtime.instance_t; Typegen.structure types c.args |] in
          let f = Function.fast ("emit." ^ version ^ "." ^ c.name) t in
          Map.add values c.name f
      )
    in

    (* Fast version of definition (keeping construct functions for use in slow version) *)
    if List.mem def.dattrs DAttribute.Closed then begin
      let fast_instance = Llvm.named_struct_type context ("instance.fast." ^ string_of_int def.did) in
      let fast_channels = make_channels Runtime.fast_channel fast_instance in

      (* Declare construct/emit functions, adding them to the state. *)
      let fast_state = Transgen.get_state types fast_constructors (declare_emits "fast") constants in

      (* Compile transitions (keeping track of functions for emit functions later) *)
      let compiled_transitions = List.map def.transitions (fun x -> (x, Transgen.generate_fast_transition fast_state (Llvm.pointer_type fast_instance) x)) in

      (* Compilation function for fast emit bodies *)
      let generate_fast_emit f c inst channels value bb tail_call = 
        (* Worker structure (always first parameter) *)
        let worker = Llvm.param f 0 in

        match Map.find channels c.name with
        (* Normal Channels *)
        | Some this_chan ->
          (* This pattern matching can be forgotten for lower_bound=upper_bound, head channels (as the enqueue in those cases does not change anything) *)
          let upperbound = List.find_map c.cattrs (function CAttribute.UpperBound x -> Some x | _ -> None) in
          let lowerbound = List.find_map c.cattrs (function CAttribute.LowerBound x -> Some x | _ -> None) in
          let headchan   = List.mem c.cattrs CAttribute.Head in

          let transitions_to_check = match (headchan, upperbound, lowerbound) with
            | (true, x, y) -> if x = y then [] else List.filter compiled_transitions (fun (t,b) -> List.mem (List.map t.pattern fst) c.name)
            | _ -> List.filter compiled_transitions (fun (t,b) -> List.mem (List.map t.pattern fst) c.name)
          in

          (* Consider every transition matching on c. Builder is passed through in fold manner *)
          let bb = bb |> fold (fun (transition,func) bb ->
            let next_transition = Llvm.append_block context ("post." ^ (string_of_int transition.tid)) f in

            (* Find pending messages *)
            let (bb,msgs) = List.fold_right transition.pattern ~f:(fun (name, _) (bb,msgs) ->
              if name = c.name then (
                bb, (name,value,this_chan)::msgs (* for the current channel we simply use the value passed in, message is only enqueued in case of no match *)
              ) else (
                let chan      = Map.find_exn channels name in
                let next_chan = Llvm.append_block context ("post.find." ^ (string_of_int transition.tid) ^ "." ^ name) f in
                let msg       = chan.Runtime.fast_find ("msg." ^ (string_of_int transition.tid) ^ "." ^ name) bb in
                let check     = cmp_null msg ("fcheck." ^ (string_of_int transition.tid) ^ "." ^ name) bb in
                Llvm.build_cond_br check next_transition next_chan bb |> ignore;
                
                (Llvm.builder_at_end context next_chan, (name,msg,chan)::msgs)
              )
            ) ~init:(bb, []) in

            (* Deal with success. *)
            let data = List.map msgs (fun (name,msg,chan) ->
              if name = c.name then (
                value
              ) else (
                chan.Runtime.fast_consume msg bb;
                let ptr = chan.Runtime.fast_data msg ("data_ptr." ^ (string_of_int transition.tid) ^ "." ^ name) bb in
                Llvm.build_load ptr ("data." ^ (string_of_int transition.tid) ^ "." ^ name) bb
              )
            ) in

            Llvm.build_call func (Array.of_list (worker::inst::data)) "" bb |> Llvm.set_tail_call tail_call;
            Llvm.build_ret_void bb |> ignore;

            (* Pass next_transition block for the next transition (or end of loop if this is the last) *)
            Llvm.builder_at_end context next_transition
          ) transitions_to_check in

          (* After all transitions fail, actually enqueue message. *)
          this_chan.Runtime.fast_enqueue value ("msg." ^ c.name) bb |> ignore;
          Llvm.build_ret_void bb |> ignore
        (* Channels where pattern matching is not required *)
        | None ->
          let (transition,func) = List.find_exn compiled_transitions ~f:(fun (t,b) -> (List.map t.pattern fst) = [ c.name ]) in
          Llvm.build_call func [| worker; inst; value |] "" bb |> Llvm.set_tail_call tail_call;
          Llvm.build_ret_void bb |> ignore
      in

      (* Actual produce emit and construct function bodies *)
      List.iter def.channels (fun c ->
        (* Construtors (worker, value): allocate instance on stack, initialise channels then build body *)
        if c.constructor then
          let f  = Map.find_exn fast_constructors c.name in
          let bb = Llvm.builder_at_end context (Llvm.entry_block f) in
          let inst = Llvm.build_alloca fast_instance "inst" bb in
          
          let channels = Map.mapi fast_channels (fun ~key ~data:(i,impl) ->
            let chan = impl (Llvm.build_struct_gep inst i (key ^ ".raw") bb) key bb in
            chan.Runtime.fast_init bb;
            chan
          ) in

          generate_fast_emit f c inst channels (Llvm.param f 1) bb false
        (* Normal (worker, instance, value): bitcast instance pointer, extract channel pointers, then build body *)
        else
          let f    = Map.find_exn fast_state.values c.name in
          let bb   = Llvm.builder_at_end context (Llvm.entry_block f) in
          let inst = Llvm.build_bitcast (Llvm.param f 1) (Llvm.pointer_type fast_instance) "inst" bb in

          let channels = Map.mapi fast_channels (fun ~key ~data:(i,impl) ->
            impl (Llvm.build_struct_gep inst i (key ^ ".raw") bb) key bb
          ) in

          generate_fast_emit f c inst channels (Llvm.param f 2) bb true
      )
    end;

    (* Slow version of definition *)
    let slow_instance = Llvm.named_struct_type context ("instance.slow." ^ string_of_int def.did) in
    let slow_channels = make_channels Runtime.slow_channel slow_instance in

    (* Declare construct/emit functions, adding them to the state. *)
    let slow_state = Transgen.get_state types slow_constructors (declare_emits "slow") constants in

    (* Compile transitions (keeping track of functions for emit functions later) *)
    let compiled_transitions = List.map def.transitions (fun x -> (x, Transgen.generate_slow_transition slow_state (Llvm.pointer_type slow_instance) x)) in
    
    (* Compilation function for slow emit bodies *)
    let generate_slow_emit f c inst channels value bb =
      (* Worker structure (always first parameter) *)
      let worker = Llvm.param f 0 in

      match Map.find channels c.name with
      (* Normal Channels *)
      Some this_chan ->
        let retry_false = Llvm.const_int (Llvm.i8_type context) 0 in

        (* Enqueuing of new message *)
        let new_msg = this_chan.Runtime.slow_enqueue value ("msg." ^ c.name) bb in

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
            (this_chan.Runtime.slow_is_consumed new_msg "consumed" loop_bb)
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
          let (bb,msgs) = List.fold_right transition.pattern ~f:(fun (name, _) (bb,msgs) ->
            if name = c.name then (
              bb, (name,new_msg,this_chan)::msgs
            ) else (
              let chan      = Map.find_exn channels name in
              let next_chan = Llvm.insert_block context ("post.find." ^ (string_of_int transition.tid) ^ "." ^ name) next_transition in
              let msg       = chan.Runtime.slow_find retry ("msg." ^ (string_of_int transition.tid) ^ "." ^ name) bb in
              let check     = cmp_null msg ("fcheck." ^ (string_of_int transition.tid) ^ "." ^ name) bb in
              Llvm.build_cond_br check next_transition next_chan bb |> ignore;
              
              (Llvm.builder_at_end context next_chan, (name,msg,chan)::msgs)
            )
          ) ~init:(bb, []) in

          let (bb,_) = List.fold_right msgs ~f:(fun (name,msg,chan) (bb,prev_revert) ->
            let new_commit = Llvm.insert_block context ("post.commit." ^ (string_of_int transition.tid) ^ "." ^ name) next_transition in
            let new_revert = Llvm.insert_block context ("revert." ^ (string_of_int transition.tid) ^ "." ^ name) next_transition in

            (* Try to claim message *)
            let check = chan.Runtime.slow_try_claim msg ("ccheck." ^ (string_of_int transition.tid) ^ "." ^ name) bb in
            Llvm.build_cond_br check new_commit prev_revert bb |> ignore;

            (* Reverts claimed message to pending in case of failed commit - chains with previous reverts *)
            let revert_bb = Llvm.builder_at_end context new_revert in begin
              chan.Runtime.slow_revert msg revert_bb;
              Llvm.build_br prev_revert revert_bb |> ignore
            end;

            (Llvm.builder_at_end context new_commit, new_revert)
          ) ~init:(bb, next_transition) in

          (* Complete block that deals with success. *)
          let data = List.map msgs (fun (name,msg,chan) ->
            chan.Runtime.slow_consume msg bb;
            if name = c.name then
              value
            else
              let ptr = chan.Runtime.slow_data msg ("data_ptr." ^ (string_of_int transition.tid) ^ "." ^ name) bb in
              Llvm.build_load ptr ("data." ^ (string_of_int transition.tid) ^ "." ^ name) bb
          ) in

          Llvm.build_call build (Array.of_list (worker::inst::data)) "" bb |> ignore;
          Llvm.build_ret_void bb |> ignore;

          (* Pass next_transition block for the next transition (or end of loop if this is the last) *)
          Llvm.builder_at_end context next_transition
        ) (List.filter compiled_transitions (fun (t,b) -> List.mem (List.map t.pattern fst) c.name)) in

        (* Loop Footer Condition: Are we meant to retry? *)
        let retry_value = Llvm.build_load retry "retry_value" bb in
        let retry_check = Llvm.build_icmp Llvm.Icmp.Eq retry_value retry_false "retry_check" bb in
        Llvm.build_cond_br retry_check exit loop bb |> ignore
      (* Channels where pattern matching is not required *)
      | None ->
        let (transition,build) = List.find_exn compiled_transitions ~f:(fun (t,b) -> (List.map t.pattern fst) = [ c.name ]) in
        Llvm.build_call build [| worker; inst; value |] "" bb |> ignore;
        Llvm.build_ret_void bb |> ignore
    in

    (* Actual produce emit and construct function bodies *)
    List.iter def.channels (fun c ->
      (* Construtors (worker, value): check for fast mode, allocate instance on heap, initialise channels then build body *)
      if c.constructor then      
        let f         = Map.find_exn slow_constructors c.name in
        let bb        = Llvm.builder_at_end context (Llvm.entry_block f) in
        let slow_mode = Llvm.append_block context "slow_mode" f in
        
        begin match Map.find fast_constructors c.name with
        (* Fast constructor available *)
        | Some fast_constructor ->
          let fast_mode = Llvm.append_block context "fast_mode" f in
          let check = Llvm.build_call Runtime.fast_check [| Llvm.param f 0 |] "check" bb in
          Llvm.build_cond_br check fast_mode slow_mode bb |> ignore;

          let bb = Llvm.builder_at_end context fast_mode in
          let call = Llvm.build_call fast_constructor (Llvm.params f) "" bb in
          Llvm.set_instruction_call_conv Function.fastcc call;
          Llvm.set_tail_call true call;
          Llvm.build_ret_void bb |> ignore
        (* Only slow mode *)
        | None ->
          Llvm.build_br slow_mode bb |> ignore
        end;

        (* Heap allocation *)
        let bb   = Llvm.builder_at_end context slow_mode in
        let inst = Llvm.build_call (Runtime.malloc slow_instance) [| Llvm.size_of slow_instance |] "inst" bb in
        
        let channels = Map.mapi slow_channels (fun ~key ~data:(i,impl) ->
          let chan = impl (Llvm.build_struct_gep inst i (key ^ ".raw") bb) key bb in
          chan.Runtime.slow_init bb;
          chan
        ) in

        generate_slow_emit f c inst channels (Llvm.param f 1) bb
      (* Normal (worker, instance, value): bitcast instance pointer, extract channel pointers, then build body *)
      else
        let f    = Map.find_exn slow_state.values c.name in
        let bb   = Llvm.builder_at_end context (Llvm.entry_block f) in
        let inst = Llvm.build_bitcast (Llvm.param f 1) (Llvm.pointer_type slow_instance) "inst" bb in

        let channels = Map.mapi slow_channels (fun ~key ~data:(i,impl) ->
          impl (Llvm.build_struct_gep inst i (key ^ ".raw") bb) key bb
        ) in

        generate_slow_emit f c inst channels (Llvm.param f 2) bb
    )
  in

  List.iter p.definitions generate_def;

  let main = Function.inlined "construct_main" (Llvm.function_type void_type [| Runtime.worker_t; Runtime.int_type |]) in begin
    let emit_end = Function.fast "emit.end" (Llvm.function_type void_type [| Runtime.worker_t; Runtime.instance_t; Llvm.struct_type context [| Runtime.int_type |] |]) in

    let end_bb = Llvm.builder_at_end context (Llvm.entry_block emit_end) in begin
      let dovetail_end = Function.declare "dovetail_end" (Llvm.function_type void_type [| Runtime.int_type |]) in
      let exit_code = Llvm.build_extractvalue (Llvm.param emit_end 2) 0 "exit_code" end_bb in
      Llvm.build_call dovetail_end [| exit_code |] "" end_bb |> ignore;
      Llvm.build_ret_void end_bb |> ignore
    end;

    let bb = Llvm.builder_at_end context (Llvm.entry_block main) in
    let msg = Llvm.const_struct context [| make_int 0; Llvm.const_struct context [| emit_end; Llvm.const_null Runtime.instance_t |] |] in
    let msg = Llvm.build_insertvalue msg (Llvm.param main 1) 0 "msg" bb in
    let call = Llvm.build_call (Map.find_exn slow_constructors "main") [| Llvm.param main 0; msg |] "" bb in
    Llvm.set_instruction_call_conv Function.fastcc call;
    Llvm.set_tail_call true call;
    Llvm.build_ret_void bb |> ignore
  end
