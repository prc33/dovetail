(* Dovetail JCAM
 *
 * Copyright 2013      Peter Calvert <prc33@cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)
 
(** Lexical tokens                                                            *)

type token =
  | Symbol    of char              (* Single non-alphanumeric characters *)
  | Kwd       of string            (* Keywords                           *)
  | Label     of string            (* Basic block labels                 *)
  | Id        of string            (* Standard identifiers               *)
  | GId       of string            (* Global identifiers                 *)
  | Integer   of int               (* Integer constants                  *)
  | Float     of float             (* Float constants                    *)
  | SizedType of (char * int)      (* Sized Type names                   *)
