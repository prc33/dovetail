(* Dovetail JCAM
 *
 * Copyright 2014      Peter Calvert <prc33@cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)
 
(** CFA Runner                                                                *)

open Core.Std


let main f k = 
  let channel = open_in f in
  let stream = Lexer.lex (Stream.of_channel channel) in
  let parsed = try Parser.parse stream with e ->
                raise e
  in
    List.iter parsed.definitions (fun d ->
      print_endline "==================================================================";
      let (gamma, cs) = Cfagen.generate d k in
      List.iter (Map.to_alist gamma) (fun (c,v) -> print_string ("Channel " ^ c ^ " -> "); print_string (List.to_string string_of_int v); print_newline() );
      List.iter cs (fun c -> print_endline (Constraints.to_string c));
      print_endline "~~~~~";
      let (solution,inner,outer) = Cfasolve.solve gamma cs k in
      Hashtbl.iter solution ~f:(fun ~key ~data ->
        print_string (string_of_int key);
        print_string " = ";
        Map.iter data ~f:(fun ~key ~data ->
          print_string (Constraints.string_with_state data (Constraints.string_of_value (Set.Poly.empty) key));
          print_string ", "
        );
        print_endline ""
      );
      print_endline ("Inner: " ^ (List.to_string (fun x -> x) (Set.to_list inner)));
      print_endline ("Outer: " ^ (List.to_string (fun x -> x) (Set.to_list outer)))
    )

let () = match Sys.argv with
  | [|_; f; k|] -> ignore (main f (int_of_string k))
  | _ as a -> Printf.eprintf "Usage: %s <input> <k>\n" a.(0)
