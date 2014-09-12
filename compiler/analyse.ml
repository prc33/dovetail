(* Dovetail JCAM
 *
 * Copyright 2014      Peter Calvert <prc33@cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)
 
(** CFA Runner                                                                *)

open Core.Std

let string_with_state state str = match state with
  | Constraints.F -> "F(" ^ str ^ ")"
  | Constraints.B -> "B(" ^ str ^ ")"

let rec string_of_constraint c = match c with
  | Constraints.Succ (i, s, j) -> (string_of_int i) ^ " >= " ^ (string_with_state s (string_of_int j))
  | Constraints.In (i, s, v) -> (string_of_int i) ^ " >= " ^ (string_with_state s (string_of_value v))
  | Constraints.Emit (is, j, _) -> (List.to_string string_of_int is) ^ " -> " ^ (string_of_int j)

and string_of_value v = match v with
  | Constraints.Wildcard -> "*"
  | Constraints.Closure (f, cs, is) -> "{" ^ (List.to_string string_of_constraint cs) ^ "|" ^ f ^ ": " ^ (List.to_string string_of_int is)

let main f k = 
  let channel = open_in f in
  let stream = Lexer.lex (Stream.of_channel channel) in
  let parsed = try Parser.parse stream with e ->
                raise e
  in
    List.iter parsed.definitions (fun d ->
      let (gamma, cs) = Cfagen.generate d k in
      List.iter (Map.to_alist gamma) (fun (c,v) -> print_string ("Channel " ^ c ^ " -> "); print_string (List.to_string string_of_int v); print_newline() );
      List.iter cs (fun c -> print_endline (string_of_constraint c))
    )

let () = match Sys.argv with
  | [|_; f; k|] -> ignore (main f (int_of_string k))
  | _ as a -> Printf.eprintf "Usage: %s <input> <k>\n" a.(0)
