(* Dovetail JCAM
 *
 * Copyright 2014      Peter Calvert <peter.calvert@cl.cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)
 
(** CFA Constraints                                                           *)

(* Allocate a fresh flow variable *)
let fresh_var =
  let n = ref 0 in
  fun () ->
    if !n = -1 then
      failwith "Out of flow variables"
    else (
      incr n;
      !n
    )

type history = (int * (string option) * int) list

(* Background or foreground *)
type state = B | F

(* Constraints *)
type t =
  | Succ of int * state * int                              (* a_i >= F/B(a_j) *)
  | In of int * state * value                                (* a_i >= F/B(c) *)
  | Emit of (int list) * int * history                       (* [a_i] h-> a_j *)

(* Abstract values *)
and value =
  | Wildcard  
  | Closure of string * (t list) * (int list)

(* TODO restore substitute, solver needs one that creates fresh for all except Gamma *)
