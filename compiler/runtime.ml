(* Dovetail JCAM
 *
 * Copyright 2013      Peter Calvert <prc33@cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)

let context = Llvm.global_context ()

let void_type = Llvm.void_type context
let bool_type = Llvm.i1_type context
let int_type  = Llvm.i32_type context
let size_type = Llvm.i64_type context
let ptr_type  = Llvm.pointer_type (Llvm.i8_type context)

let opaque n = Llvm.pointer_type (Llvm.named_struct_type context n)

let worker_t   = opaque "worker"
let match_t    = opaque "match"
let instance_t = opaque "instance"

let transition_t = Llvm.function_type void_type [| worker_t; match_t |]

let () = Llvm.struct_set_body (Llvm.element_type match_t) [| Llvm.pointer_type transition_t; instance_t |] false

let array_type t = Llvm.struct_type context [| int_type; Llvm.pointer_type t |]

let trampoline = Function.inlined "worker_trampoline" transition_t in begin
  let bb = Llvm.builder_at_end context (Llvm.entry_block trampoline) in
  let f_ptr = Llvm.build_struct_gep (Llvm.param trampoline 1) 0 "f.ptr" bb in
  let f = Llvm.build_load f_ptr "f" bb in
  Function.fast_call f (Llvm.params trampoline) "" true bb |> ignore;
  Llvm.build_ret_void bb |> ignore
end

let malloc = 
  let raw = Function.declare "GC_malloc" (Llvm.function_type ptr_type [| size_type |]) in
(*  Llvm.add_function_attr raw Llvm.Attribute.Noalias; *)
  function t -> Llvm.const_bitcast raw (Llvm.pointer_type (Llvm.function_type (Llvm.pointer_type t) [| size_type |]))

let arrays_split =
  let func_type t =
    let arr_t = array_type t in
    Llvm.function_type void_type [| Llvm.pointer_type arr_t; arr_t; array_type int_type; size_type |] in
  let raw = Function.declare "arrays_split" (func_type (Llvm.i8_type context)) in
  fun t -> Llvm.const_bitcast raw (Llvm.pointer_type (func_type t))

let arrays_merge =
  let func_type t =
    let arr_t = array_type t in
    Llvm.function_type arr_t [| array_type arr_t; size_type |] in
  let raw = Function.declare "arrays_merge" (func_type (Llvm.i8_type context)) in
  fun t -> Llvm.const_bitcast raw (Llvm.pointer_type (func_type t))

let match_alloc =
  let raw = Function.declare "match_alloc" (Llvm.function_type match_t [| worker_t |]) in
  function t -> Llvm.const_bitcast raw (Llvm.pointer_type (Llvm.function_type (Llvm.pointer_type t) [| worker_t |]))

let match_push = Function.declare "match_push" (Llvm.function_type void_type [| worker_t |])

let fast_check = Function.declare "dovetail_go_fast" (Llvm.function_type bool_type [| worker_t |]) (* TODO: rename? *)

type slow_channel = {
  slow_init        : Llvm.llbuilder -> unit;                                    (* () -> void      *)
  slow_data        : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* msg, int -> i8*  *)
  slow_enqueue     : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* msg -> void      *)
  slow_find        : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* i8* -> msg       *)
  slow_try_claim   : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* msg -> i1        *)
  slow_revert      : Llvm.llvalue -> Llvm.llbuilder -> unit;                    (* msg -> void      *)
  slow_consume     : Llvm.llvalue -> Llvm.llbuilder -> unit;                    (* msg -> void      *)
  slow_is_consumed : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* msg -> i1        *)
}

type fast_channel = {
  fast_init        : Llvm.llbuilder -> unit;                                    (* () -> void       *)
  fast_data        : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* msg, int -> i8*  *)
  fast_enqueue     : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* msg -> void      *)
  fast_find        : string -> Llvm.llbuilder -> Llvm.llvalue;                  (* () -> msg        *)
  fast_consume     : Llvm.llvalue -> Llvm.llbuilder -> unit;                    (* msg -> void      *)
}

let slow_impl impl msg_t tf =
  let channel_t = opaque ("channel." ^ impl) in

  let init        = Function.declare (impl ^ "_init")        (Llvm.function_type void_type [| channel_t; size_type |]) in
  let allocate    = Function.declare (impl ^ "_allocate")    (Llvm.function_type msg_t     [| channel_t; size_type |]) in
  let data        = Function.declare (impl ^ "_data")        (Llvm.function_type ptr_type  [| channel_t; msg_t;    |]) in
  let enqueue     = Function.declare (impl ^ "_enqueue")     (Llvm.function_type void_type [| channel_t; msg_t     |]) in
  let find        = Function.declare (impl ^ "_find")        (Llvm.function_type msg_t     [| channel_t; ptr_type  |]) in
  let try_claim   = Function.declare (impl ^ "_try_claim")   (Llvm.function_type bool_type [| channel_t; msg_t     |]) in
  let revert      = Function.declare (impl ^ "_revert")      (Llvm.function_type void_type [| channel_t; msg_t     |]) in
  let consume     = Function.declare (impl ^ "_consume")     (Llvm.function_type void_type [| channel_t; msg_t     |]) in
  let is_consumed = Function.declare (impl ^ "_is_consumed") (Llvm.function_type bool_type [| channel_t; msg_t     |]) in

  fun t -> let size = Llvm.size_of t in
           let casted = Llvm.const_bitcast data (Llvm.pointer_type (Llvm.function_type (Llvm.pointer_type t) [| channel_t; msg_t |])) in
  (tf t, fun raw name bb -> let channel = Llvm.build_bitcast raw channel_t name bb in {
    slow_init        = (fun bb  -> Llvm.build_call init   [| channel; size |] "" bb |> ignore);
    slow_data        = (fun msg -> Llvm.build_call casted [| channel; msg |]);
    slow_enqueue     =
      (fun value s bb ->
         let msg = Llvm.build_call allocate [| channel; size |] s bb in
         let ptr = Llvm.build_call casted [| channel; msg |] (s ^ ".data") bb in
         Llvm.build_store value ptr bb |> Llvm.set_volatile true;
         Llvm.build_call enqueue [| channel; msg |] "" bb |> ignore;
         msg);
    slow_find        = (fun retry -> Llvm.build_call find [| channel; retry |]);
    slow_try_claim   = (fun msg -> Llvm.build_call try_claim [| channel; msg |]);
    slow_revert      = (fun msg bb -> Llvm.build_call revert  [| channel; msg |] "" bb |> ignore);
    slow_consume     = (fun msg bb -> Llvm.build_call consume [| channel; msg |] "" bb |> ignore);
    slow_is_consumed = (fun msg -> Llvm.build_call is_consumed [| channel; msg |]);
  })

let slow_queue = slow_impl "slow_queue" (opaque "msg.slow_queue") (function t -> Llvm.struct_type context [| ptr_type; ptr_type |])
let slow_cell  = slow_impl "slow_cell"  (int_type)  (function t -> Llvm.struct_type context [| ptr_type; t |])

let slow_channel attrs = 
  if (List.mem Jcam.CAttribute.Functional attrs) then
    None
  else if (List.mem (Jcam.CAttribute.UpperBound(1)) attrs) then begin
    Some slow_cell
  end else
    Some slow_queue

let fast_impl impl tf =
  let channel_t = opaque ("channel." ^ impl) in
  let msg_t     = opaque ("msg." ^ impl) in

  let init        = Function.declare (impl ^ "_init")        (Llvm.function_type void_type [| channel_t; size_type |]) in
  let enqueue     = Function.declare (impl ^ "_enqueue")     (Llvm.function_type msg_t     [| channel_t; size_type |]) in
  let data        = Function.declare (impl ^ "_data")        (Llvm.function_type ptr_type  [| channel_t; msg_t     |]) in
  let find        = Function.declare (impl ^ "_find")        (Llvm.function_type msg_t     [| channel_t; size_type |]) in
  let consume     = Function.declare (impl ^ "_consume")     (Llvm.function_type void_type [| channel_t; msg_t; size_type |]) in 

  fun t -> let size = Llvm.size_of t in
           let casted = Llvm.const_bitcast data (Llvm.pointer_type (Llvm.function_type (Llvm.pointer_type t) [| channel_t; msg_t |])) in
  (tf t, fun raw name bb -> let channel = Llvm.build_bitcast raw channel_t name bb in {
    fast_init        = (fun bb -> Llvm.build_call init [| channel; size |] "" bb |> ignore);
    fast_data        = (fun msg -> Llvm.build_call casted [| channel; msg |]);
    fast_enqueue     =
      (fun value s bb ->
         let msg = Llvm.build_call enqueue [| channel; size |] s bb in
         let ptr = Llvm.build_call casted [| channel; msg |] (s ^ ".data") bb in
         Llvm.build_store value ptr bb |> ignore;
         msg);
    fast_find        = (Llvm.build_call find [| channel; size |]);
    fast_consume     = (fun msg bb -> Llvm.build_call consume [| channel; msg; size |] "" bb |> ignore);
  })

let fast_queue = fast_impl "fast_queue" (function t -> Llvm.struct_type context [| ptr_type; ptr_type; size_type; Llvm.array_type t 32 |])
let fast_cell  = fast_impl "fast_cell" (function t -> Llvm.struct_type context [| ptr_type; t |])
let fast_mem   = fast_impl "fast_mem" (function t -> Llvm.struct_type context [| t |])

let fast_channel attrs = 
  if (List.mem Jcam.CAttribute.Functional attrs) then
    None
  else if (List.mem (Jcam.CAttribute.UpperBound(1)) attrs) then begin
    if (List.mem (Jcam.CAttribute.LowerBound(1)) attrs) && (List.mem Jcam.CAttribute.Head attrs) then
      Some fast_mem
    else
      Some fast_cell
  end else
    Some fast_queue

(* NOTE: useful way of benchmarking is simply changing the above definitions to disable the faster versions *)
