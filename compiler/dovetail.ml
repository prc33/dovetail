(* Dovetail JCAM
 *
 * Copyright 2013      Peter Calvert <prc33@cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)
 
(** Compiler driver                                                           *)

let print_token token = match token with
  | Token.Symbol c -> print_char c; print_newline ()
  | Token.Kwd s -> print_endline s
  | Token.Id s -> print_string "ID %"; print_endline s
  | Token.GId s -> print_string "GID @"; print_endline s
  | Token.Label s -> print_string "Label "; print_endline s
  | Token.Integer i -> print_string "Int "; print_int i; print_newline ()
  | Token.Float f -> print_string "Float "; print_float f; print_newline ()
  | Token.SizedType (p,s) -> print_string "SizedType "; print_char p; print_string "*"; print_int s; print_newline ()

let print_tokens stream = Stream.iter print_token stream

let main f1 f2 = 
  let channel = open_in f1 in
  let stream = Lexer.lex (Stream.of_channel channel) in
  let parsed = try Parser.parse stream with e ->
                begin match Stream.peek stream with
                | None -> ()
                | Some t -> print_token t; print_int (pos_in channel); print_newline ()
                end;
                raise e
  in
    Codegen.compile_program parsed;
    Llvm_bitwriter.write_bitcode_file Function.llmod f2

let () = match Sys.argv with
  | [|_; f1; f2|] -> ignore (main f1 f2)
  | _ as a -> Printf.eprintf "Usage: %s <input> <output>\n" a.(0)
