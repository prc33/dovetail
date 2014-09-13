(* Dovetail JCAM
 *
 * Copyright 2014      Peter Calvert <peter.calvert@cl.cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)

open Core.Std
 
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

(* Solutions *)
type solution = (int, (value, state) Map.Poly.t) Hashtbl.Poly.t

(* Operations on value sets *)
let change ht i f =
  let old = match Hashtbl.find ht i with
    | Some x -> x
    | None   -> Map.Poly.empty
  in
  let neu = f old in
  Hashtbl.set ht i neu;
  not (Map.equal (=) old neu)

let add (sol,i) state value = change sol i (fun x ->
  Map.change x value (function
    | Some(F) -> Some(F)
    | _       -> Some(state)
  )
)

let add_multi (sol,i) sopt j = change sol i (fun x ->
  Map.merge x (Hashtbl.find_exn sol j) (fun ~key -> function
    | `Both(F, _) -> Some(F)
    | `Both(B, s) -> match sopt with
                     | Some state -> Some(state)
                     | None       -> Some(s)
    | `Left(s)    -> Some(s)
    | `Right(s)   -> match sopt with
                     | Some state -> Some(state)
                     | None       -> Some(s)
  )
)

(* Substitution with fresh variables for everything except those in exclude.
   Crucially this can be reused.                                              *)
let fresh_substitution exclude =
  let memo = Hashtbl.Poly.create () in
  (fun i -> match Hashtbl.find memo i with
    | Some j -> j
    | None   -> let j = if List.mem exclude i then i
                                              else fresh_var () in
                Hashtbl.add_exn memo i j;
                j
  )

(* Modifies a list of constraints applying the subtitution f, and appending
   the given call-string element (whilst keeping to length k)                 *)
let map f e k cs = List.map cs ~f:(function
  | Succ(i, s, j)  -> Succ(f i, s, f j)
  | In(i, s, v)    -> In(f i, s, v)
  | Emit(is, j, h) -> Emit(List.map is ~f, f j, if (List.length h) >= k then h else h @ [e])
)
