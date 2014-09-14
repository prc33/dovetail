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

type side = Inner | Outer | Prim

(* Constraints *)
type t =
  | Succ of int * (state option) * int                     (* a_i >= F/B(a_j) *)
  | In   of int * state * value                              (* a_i >= F/B(c) *)
  | Emit of (int list) * int * history                       (* [a_i] h-> a_j *)

(* Abstract values *)
and value =
  | Wildcard of side
  | Closure  of string * (t Stack.t) * (int list)

(* String conversion *)
let string_with_state state str = match state with
  | F -> "F(" ^ str ^ ")"
  | B -> "B(" ^ str ^ ")"

let rec string_of_constraint seen c = match c with
  | Succ (i, s, j) -> (string_of_int i) ^ " >= " ^ (match s with Some s -> string_with_state s (string_of_int j) | None -> string_of_int j)
  | In (i, s, v) -> (string_of_int i) ^ " >= " ^ (string_with_state s (string_of_value seen v))
  | Emit (is, j, _) -> (List.to_string string_of_int is) ^ " -> " ^ (string_of_int j)

and string_of_value seen v = match v with
  | Wildcard(Inner) -> "*_in"
  | Wildcard(Outer) -> "*_out"
  | Wildcard(Prim)  -> "PRIM"
  | Closure (f, cs, is) -> if Set.mem seen cs then "{" ^ f ^ "}" else (List.to_string (string_of_constraint (Set.add seen cs)) (Stack.to_list cs)) ^ "|" ^ f ^ ": " ^ (List.to_string string_of_int is)

let to_string = string_of_constraint Set.Poly.empty

(* Solutions *)
type solution = (int, (value, state) Map.Poly.t) Hashtbl.Poly.t

(* Operations on value sets *)
let get ht i = match Hashtbl.find ht i with
  | Some x -> x
  | None   -> Map.Poly.empty

let change ht i f =
  let old = get ht i in
  let neu = f old in
  Hashtbl.set ht i neu;
  not (Map.equal (=) old neu)

let add sol i state value = change sol i (fun x ->
  Map.change x value (function
    | Some(F) -> Some(F)
    | _       -> Some(state)
  )
)

let add_multi sol i sopt j = change sol i (fun x ->
  Map.merge x (get sol j) (fun ~key -> function
    | `Both(F, _) -> Some(F)
    | `Both(B, s) -> begin match sopt with
                     | Some state -> Some(state)
                     | None       -> Some(s)
                     end
    | `Left(s)    -> Some(s)
    | `Right(s)   -> begin match sopt with
                     | Some state -> Some(state)
                     | None       -> Some(s)
                     end
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
  | Emit(is, j, h) -> Emit(List.map is ~f, f j, if (List.length h) >= k then h else h @ e)
)
