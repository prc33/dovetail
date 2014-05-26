(* Dovetail JCAM
 *
 * Copyright 2014      Peter Calvert <peter.calvert@cl.cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)

let context = Llvm.global_context ()
let llmod = Llvm.create_module context "JCAM"

let fastcc    = try  int_of_string (Sys.getenv "DOVETAIL_CALLCONV")
                with _ -> Llvm.CallConv.fast

let tailcalls = try bool_of_string (Sys.getenv "DOVETAIL_TAILCALLS")
                with _ -> true

let inlined n t =
  let f = Llvm.define_function n t llmod in
  Llvm.add_function_attr f Llvm.Attribute.Nounwind;
  Llvm.add_function_attr f Llvm.Attribute.Alwaysinline;
  f

let fast n t =
  let f = Llvm.define_function n t llmod in
  Llvm.add_function_attr f Llvm.Attribute.Nounwind;
  Llvm.set_function_call_conv fastcc f;
  f

let zext f =
  Llvm.add_function_attr f Llvm.Attribute.Zext;
  f

let declare n t = Llvm.declare_function n t llmod

let fast_call f args n tail bb =
  let call = Llvm.build_call f args n bb in
  Llvm.set_instruction_call_conv fastcc call;
  Llvm.set_tail_call (tail && tailcalls) call;
  call
