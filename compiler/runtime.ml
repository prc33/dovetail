(* Dovetail JCAM
 *
 * Copyright 2013      Peter Calvert <prc33@cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)

let context = Llvm.global_context ()
let llmod = Llvm.create_module context "JCAM"

let worker_t   = Llvm.pointer_type (Llvm.named_struct_type context "worker")
let match_t    = Llvm.pointer_type (Llvm.named_struct_type context "match")
let instance_t = Llvm.pointer_type (Llvm.named_struct_type context "instance")

let void_type = Llvm.void_type context
let bool_type = Llvm.i1_type context
let int_type  = Llvm.i32_type context
let size_type = Llvm.i64_type context
let ptr_type  = Llvm.pointer_type (Llvm.i8_type context)

let malloc = 
  let raw = Llvm.declare_function "GC_malloc" (Llvm.function_type ptr_type [| size_type |]) llmod in
(*  Llvm.add_function_attr raw Llvm.Attribute.Noalias; *)
  function t -> Llvm.const_bitcast raw (Llvm.pointer_type (Llvm.function_type (Llvm.pointer_type t) [| size_type |]))

let match_alloc =
  let raw = Llvm.declare_function "match_alloc" (Llvm.function_type match_t [| worker_t |]) llmod in
  function t -> Llvm.const_bitcast raw (Llvm.pointer_type (Llvm.function_type (Llvm.pointer_type t) [| worker_t |]))

let match_push = Llvm.declare_function "match_push" (Llvm.function_type void_type [| worker_t |]) llmod

let fast_check = Llvm.declare_function "dovetail_go_fast" (Llvm.function_type bool_type [| worker_t |]) llmod (* TODO: rename? *)

type slow_channel = {
  slow_build_cast  : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;
  slow_init        : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* channel, int -> void *)
  slow_data        : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* msg, int     -> i8*  *)
  slow_enqueue     : Llvm.llvalue -> Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* channel, msg -> void *)
  slow_find        : Llvm.llvalue -> Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* channel, i8* -> msg  *)
  slow_try_claim   : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* msg -> i1            *)
  slow_revert      : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* msg -> void          *)
  slow_consume     : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* msg -> void          *)
  slow_is_consumed : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* msg -> i1            *)
}

type fast_channel = {
  fast_build_cast  : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;
  fast_init        : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* channel, int -> void *)
  fast_data        : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* msg, int     -> i8*  *)
  fast_enqueue     : Llvm.llvalue -> Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* channel, msg -> void *)
  fast_find        : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* channel -> msg  *)
  fast_consume     : Llvm.llvalue -> string -> Llvm.llbuilder -> Llvm.llvalue;  (* msg -> void          *)
}

let slow_impl impl tf =
  let channel_t = Llvm.pointer_type (Llvm.named_struct_type context ("channel." ^ impl)) in
  let msg_t     = Llvm.pointer_type (Llvm.named_struct_type context ("msg." ^ impl)) in

  let init        = Llvm.declare_function (impl ^ "_init")        (Llvm.function_type void_type [| channel_t; size_type |]) llmod in
  let allocate    = Llvm.declare_function (impl ^ "_allocate")    (Llvm.function_type msg_t     [| channel_t; size_type |]) llmod in
  let data        = Llvm.declare_function (impl ^ "_data")        (Llvm.function_type ptr_type  [| msg_t;     size_type |]) llmod in
  let enqueue     = Llvm.declare_function (impl ^ "_enqueue")     (Llvm.function_type void_type [| channel_t; msg_t     |]) llmod in
  let find        = Llvm.declare_function (impl ^ "_find")        (Llvm.function_type msg_t     [| channel_t; ptr_type  |]) llmod in
  let try_claim   = Llvm.declare_function (impl ^ "_try_claim")   (Llvm.function_type bool_type [| msg_t                |]) llmod in
  let revert      = Llvm.declare_function (impl ^ "_revert")      (Llvm.function_type void_type [| msg_t                |]) llmod in
  let consume     = Llvm.declare_function (impl ^ "_consume")     (Llvm.function_type void_type [| msg_t                |]) llmod in
  let is_consumed = Llvm.declare_function (impl ^ "_is_consumed") (Llvm.function_type bool_type [| msg_t                |]) llmod in

  fun t -> let size = Llvm.size_of t in
           let casted = Llvm.const_bitcast data (Llvm.pointer_type (Llvm.function_type (Llvm.pointer_type t) [| msg_t; size_type |])) in (tf t, {
    slow_build_cast  = (fun value -> Llvm.build_bitcast value channel_t);
    slow_init        = (fun channel -> Llvm.build_call init     [| channel; size |]);
    slow_data        = (function msg -> Llvm.build_call casted [| msg; size |]);
    slow_enqueue     =
      (fun channel value s bb ->
         let msg = Llvm.build_call allocate [| channel; size |] s bb in
         let ptr = Llvm.build_call casted [| msg; size |] (s ^ ".data") bb in
         Llvm.build_store value ptr bb |> ignore;
         Llvm.build_call enqueue [| channel; msg |] "" bb |> ignore;
         msg);
    slow_find        = (fun channel retry -> Llvm.build_call find [| channel; retry |]);
    slow_try_claim   = (fun msg -> Llvm.build_call try_claim [| msg |]);
    slow_revert      = (fun msg -> Llvm.build_call revert [| msg |]);
    slow_consume     = (fun msg -> Llvm.build_call consume [| msg |]);
    slow_is_consumed = (fun msg -> Llvm.build_call is_consumed [| msg |]);
  })

let slow_queue = slow_impl "slow_queue" (function t -> Llvm.struct_type context [| ptr_type; ptr_type |])
let slow_cell  = slow_impl "slow_cell" (function t -> Llvm.struct_type context [| ptr_type |])
let slow_mem   = slow_cell (* TODO: unclear whether mem optimisation is quite possible in slow mode: slow_impl "slow_mem" (function t -> Llvm.struct_type context [| t |]) *)

let slow_channel attrs = 
  if (List.mem Jcam.CAttribute.Functional attrs) then
    None
  else if (List.mem (Jcam.CAttribute.UpperBound(1)) attrs) then begin
    if (List.mem (Jcam.CAttribute.LowerBound(1)) attrs) then
      Some slow_mem
    else
      Some slow_cell
  end else
    Some slow_queue

let fast_impl impl tf =
  let channel_t = Llvm.pointer_type (Llvm.named_struct_type context ("channel." ^ impl)) in
  let msg_t     = Llvm.pointer_type (Llvm.named_struct_type context ("msg." ^ impl)) in

  let init        = Llvm.declare_function (impl ^ "_init")        (Llvm.function_type void_type [| channel_t; size_type |]) llmod in
  let allocate    = Llvm.declare_function (impl ^ "_allocate")    (Llvm.function_type msg_t     [| channel_t; size_type |]) llmod in
  let data        = Llvm.declare_function (impl ^ "_data")        (Llvm.function_type ptr_type  [| msg_t;     size_type |]) llmod in
  let enqueue     = Llvm.declare_function (impl ^ "_enqueue")     (Llvm.function_type void_type [| channel_t; msg_t     |]) llmod in
  let find        = Llvm.declare_function (impl ^ "_find")        (Llvm.function_type msg_t     [| channel_t            |]) llmod in
  let consume     = Llvm.declare_function (impl ^ "_consume")     (Llvm.function_type void_type [| msg_t                |]) llmod in 

  fun t -> let size = Llvm.size_of t in
           let casted = Llvm.const_bitcast data (Llvm.pointer_type (Llvm.function_type (Llvm.pointer_type t) [| msg_t; size_type |])) in (tf t, {
    fast_build_cast  = (fun value -> Llvm.build_bitcast value channel_t);
    fast_init        = (fun channel -> Llvm.build_call init [| channel; size |]);
    fast_data        = (function msg -> Llvm.build_call casted [| msg; size |]);
    fast_enqueue     =
      (fun channel value s bb ->
         let msg = Llvm.build_call allocate [| channel; size |] s bb in
         let ptr = Llvm.build_call casted [| msg; size |] (s ^ ".data") bb in
         Llvm.build_store value ptr bb |> ignore;
         Llvm.build_call enqueue [| channel; msg |] "" bb |> ignore;
         msg);
    fast_find        = (fun channel -> Llvm.build_call find [| channel |]);
    fast_consume     = (fun msg -> Llvm.build_call consume [| msg |]);
  })

let fast_queue = fast_impl "fast_queue" (function t -> Llvm.struct_type context [| ptr_type; ptr_type; ptr_type |])
let fast_cell  = fast_impl "fast_cell" (function t -> Llvm.struct_type context [| ptr_type; t |])
let fast_mem   = fast_cell (*TODO: fast_impl "fast_mem" (function t -> Llvm.struct_type context [| t |]) *)

let fast_channel attrs = 
  if (List.mem Jcam.CAttribute.Functional attrs) then
    None
  else if (List.mem (Jcam.CAttribute.UpperBound(1)) attrs) then begin
    if (List.mem (Jcam.CAttribute.LowerBound(1)) attrs) then
      Some fast_mem
    else
      Some fast_cell
  end else
    Some fast_queue

(* NOTE: useful way of benchmarking is simply changing the above definitions to disable the faster versions *)
