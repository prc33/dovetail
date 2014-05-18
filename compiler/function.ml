(* Dovetail JCAM
 *
 * Copyright 2014      Peter Calvert <peter.calvert@cl.cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)

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
