(* Dovetail JCAM
 *
 * Copyright 2013      Peter Calvert <prc33@cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)
 
(** JCAM IR Representation                                                    *)

module StringMap = Map.Make(String)

let create_id =
  let n = ref 0 in
  fun () ->
    if !n = -1 then
      failwith "Out of unique IDs"
    else (
      incr n;
      !n
    )

(** Attributes on definitions, transitions and channels                       *)
module DAttribute = struct
  type t = 
    | Inline
    | Closed
    | Singleton (* TODO: currently unimplemented *)
end

module TAttribute = struct
  type t =
    | Inline
end
    
module CAttribute = struct
  type t =
    | Functional
    | LowerBound of int
    | UpperBound of int
end
  
(* Types                                                                      *)
module Type = struct
  type t =
    | Alias    of string                   (* Use of named type *)
    | Array    of t
    | Float    of int                      (* with specified bits *)
    | Integer  of int                      (* with specified bits *)
    | Struct   of t list
    | Channel  of t list
end

(** Comparisons                                                               *)
module Cmp = struct
  type t = Eq | Ne | UGt | UGe | ULt | ULe | SGt | SGe | SLt | SLe
end

module rec Expr : sig
  type binop = Type.t * Value.t * Value.t
  and t = 
    | Add     of binop
    | Sub     of binop
    | Compare of (Cmp.t * Type.t * Value.t * Value.t)
    (* TODO: more instructions *)
end = Expr
and Value : sig
  type t =
    | Var     of string
    | Integer of int
    | Float   of float
    | Null
    | Struct  of Value.t list
    | Array   of Value.t list
end = Value

type label = string

module Terminator = struct
  type t =
    | Finish
    | Goto   of label
    | Cond   of (Value.t * label * label)
end

module Instruction = struct
  type t =
    | Assign    of string * Expr.t
    | Construct of string * ((Type.t * Value.t) list)
    | Emit      of Value.t * ((Type.t * Value.t) list)
end

type phi = string * (label * Value.t) list

type block = {
  label       : label option;
  phis        : phi list;
  instrs      : Instruction.t list;
  terminator  : Terminator.t;
}

type channel = {
  name        : string;
  constructor : bool;
  args        : Type.t list;
  cattrs      : CAttribute.t list;
}

type pattern = (string * ((Type.t * string) list)) list

type transition = {
  tid         : int;
  tattrs      : TAttribute.t list;
  pattern     : pattern;
  blocks      : block list;
}

type definition = {
  did         : int;
  dattrs      : DAttribute.t list;
  channels    : channel list;
  transitions : transition list;
}
  
(* Top-Level Program                                                          *)
type program = {
  named_types : (string * Type.t) list;
  constants   : (string * Value.t) list;
  definitions : definition list;
}
