(* Dovetail JCAM
 *
 * Copyright 2013      Peter Calvert <prc33@cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)
 
(** Lexical analysis                                                          *)

let rec lex = parser
  (* Skip whitespace. *)
  | [< ' (' ' | '\n' | '\r' | '\t'); stream >] -> lex stream

  (* Comments: ; until end of line. *)
  | [< ' (';'); stream >] -> lex_comment stream
  
  (* Standard identifiers: %[a-zA-Z0-9_]* *)
  | [< ' ('%'); stream >] ->
    let buffer = Buffer.create 1 in
    lex_id buffer stream
  
  (* Global identifiers: @[a-zA-Z0-9_.]* *)
  | [< ' ('@'); stream >] ->
    let buffer = Buffer.create 1 in
    lex_gid buffer stream

  (* Keyword: [a-zA-Z][a-zA-Z0-9_]* *)
  | [< ' ('A'..'Z' | 'a'..'z' as c); stream >] ->
    lex_type c (Buffer.create 1) stream

  (* Constants: [0-9.]+ *)
  | [< ' ('0'..'9' as c); stream >] ->
    let buffer = Buffer.create 1 in
    Buffer.add_char buffer c;
    lex_int buffer stream

  (* Symbols: non-alphanumeric. *)
  | [< 'c; stream >] -> [< 'Token.Symbol c; lex stream >]

  (* End of stream. *)
  | [< >] -> [< >]

and lex_int buffer = parser
  | [< ' ('0'..'9' as c); stream >] ->
    Buffer.add_char buffer c;
    lex_int buffer stream
  | [< ' ('.' as c); stream >] ->
    Buffer.add_char buffer c;
    lex_float buffer stream
  | [< stream=lex >] ->
    [< 'Token.Integer (int_of_string (Buffer.contents buffer)); stream >]

and lex_float buffer = parser
  | [< ' ('0'..'9' | '.' as c); stream >] ->
    Buffer.add_char buffer c;
    lex_float buffer stream
  | [< stream=lex >] ->
    [< 'Token.Float (float_of_string (Buffer.contents buffer)); stream >]
  
and lex_type prefix buffer = parser
  | [< ' ('0'..'9' as c); stream >] ->
    Buffer.add_char buffer c;
    lex_type prefix buffer stream
  | [< ' ('A'..'Z' | 'a'..'z' | '_' as c); stream >] ->
    let new_buffer = Buffer.create 1 in
    Buffer.add_char   new_buffer prefix;
    Buffer.add_buffer new_buffer buffer;
    Buffer.add_char   new_buffer c;
    lex_word new_buffer stream
  | [< ' (':'); stream=lex >] -> [< 'Token.Label ((Char.escaped prefix) ^ (Buffer.contents buffer)); stream >]
  | [< stream=lex >] ->
    [< 'Token.SizedType (prefix, int_of_string (Buffer.contents buffer)); stream >]

and lex_word buffer = parser
  | [< ' ('A'..'Z' | 'a'..'z' | '0'..'9' | '_' as c); stream >] ->
    Buffer.add_char buffer c;
    lex_word buffer stream
  | [< ' (':'); stream=lex >] -> [< 'Token.Label (Buffer.contents buffer); stream >]
  | [< stream=lex >] -> [< 'Token.Kwd (Buffer.contents buffer); stream >]

and lex_id buffer = parser
  | [< ' ('A'..'Z' | 'a'..'z' | '0'..'9' | '_' as c); stream >] ->
    Buffer.add_char buffer c;
    lex_id buffer stream
  | [< stream=lex >] -> [< 'Token.Id (Buffer.contents buffer); stream >]

and lex_gid buffer = parser
  | [< ' ('A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '.' as c); stream >] ->
    Buffer.add_char buffer c;
    lex_gid buffer stream
  | [< stream=lex >] -> [< 'Token.GId (Buffer.contents buffer); stream >]

and lex_comment = parser
  | [< ' ('\n'); stream=lex >] -> stream
  | [< 'c; e=lex_comment >] -> e
  | [< >] -> [< >]
