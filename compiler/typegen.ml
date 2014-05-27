(* Dovetail JCAM
 *
 * Copyright 2014      Peter Calvert <peter.calvert@cl.cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)

open Jcam
open Core.Std

exception Unknown of string

let context = Llvm.global_context ()

let void     = Llvm.void_type context

let int_type  = Llvm.i32_type context

type t = Jcam.Type.t -> Llvm.lltype

(* structure : ('a -> Llvm.lltype) -> 'a list -> Llvm.lltype *)
let structure f ts = Llvm.struct_type context (Array.of_list (List.map ts f))

(* convert : t -> t *)
let convert f t = match t with
  | Type.Alias   x  -> raise (Unknown x)
  | Type.Array   e  -> Runtime.array_type (f e)
  | Type.Float   w  -> (match w with
                        | 32 -> Llvm.float_type context
                        | 64 -> Llvm.double_type context
                        | _  -> failwith ("No floating point type of width " ^ (string_of_int w)))
  | Type.Integer w  -> Llvm.integer_type context w
  | Type.Struct  ts -> Llvm.pointer_type (structure f ts)
  | Type.Channel ts -> Llvm.struct_type context [|
                         Llvm.pointer_type (Llvm.function_type void [| Runtime.worker_t; Runtime.instance_t; structure f ts |]);
                         Runtime.instance_t
                       |]

(* generate : (string * Jcam.Type.t) list -> t *)
let generate ts =
  let ast = String.Map.of_alist_exn ts in
  let declarations = Map.filter_mapi ast (fun ~key ~data -> match data with
    | Type.Struct _  -> Some (Llvm.pointer_type (Llvm.named_struct_type context ("named_struct." ^ key)))
    | Type.Channel _ -> Some (Llvm.named_struct_type context ("named_channel." ^ key))
    | Type.Array _   -> Some (Llvm.named_struct_type context ("named_array." ^ key))
    | _ -> None)
  in

  let rec result = function
    | Type.Alias name -> (match Map.find declarations name with
                          | Some lltype -> lltype
                          | None -> result (Map.find_exn ast name))
    | t -> convert result t
  in

  ignore (Map.merge ast declarations (fun ~key -> function
    | `Both (Type.Struct  ts, lltype) -> Llvm.struct_set_body (Llvm.element_type lltype) (Array.of_list (List.map ts result)) false; None
    | `Both (Type.Channel ts, lltype) -> Llvm.struct_set_body lltype [|
                                           Llvm.pointer_type (Llvm.function_type void [| Runtime.worker_t; Runtime.instance_t; structure result ts |]);
                                           Runtime.instance_t
                                         |] false; None
    | `Both (Type.Array   t , lltype) -> Llvm.struct_set_body lltype [| int_type; Llvm.pointer_type (result t) |] false; None
    | _ -> None
  ));

  result
