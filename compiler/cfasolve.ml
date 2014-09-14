(* Dovetail JCAM
 *
 * Copyright 2014      Peter Calvert <peter.calvert@cl.cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)

open Jcam
open Constraints
open Core.Std

let solve gamma cs k =
  let solution = Hashtbl.Poly.create () in (* D *)
  let edges = Hashtbl.Poly.create () in (* E - should not contain In constraints *)
  let substitutions = Hashtbl.Poly.create () in
  let exclude = List.concat (Map.data gamma) in

  let inner_escape = Stack.create () in
  let outer_escape = Stack.create () in
  let collapse = ref false in

  let get_substitution h = match Hashtbl.Poly.find substitutions h with
    | Some x -> x
    | None -> let s = Constraints.fresh_substitution exclude in
              Hashtbl.add_exn substitutions h s; s
  in

  let get = Constraints.get solution in
  let add = Constraints.add solution in
  let add_multi = Constraints.add_multi solution in

  let add_edge i c =
    let old = match Hashtbl.find edges i with
              | Some x -> x
              | None   -> Set.Poly.empty
    in
    let neu = Set.add old c in
    Hashtbl.set edges i neu;
    not (Set.equal old neu) in

  let work = List.fold cs ~init:Int.Set.empty ~f:(fun work c -> match c with
    | Succ(_, _, j)  -> add_edge j c |> ignore; work
    | In(i, s, v)    -> if add i s v then (Set.add work i) else work
    | Emit(is, j, _) -> add_edge j c |> ignore; List.iter is (fun i -> add_edge i c |> ignore); work
  ) in

  let late_add work c = match c with
    | Succ(_, _, j)  -> if add_edge j c then (Set.add work j) else work
    | In(i, s, v)    -> if add i s v then (Set.add work i) else work
    | Emit(is, j, _) -> let work = List.fold is ~init:work ~f:(fun work i ->  if add_edge i c then (Set.add work i) else work) in if add_edge j c then (Set.add work j) else work (* Only need to add one of the vars to ensure this is calculated *)
  in

  let rec iterate work = match Set.choose work with
    | None   -> ()
    | Some w -> let cs = match Hashtbl.find edges w with
                  | Some x -> x
                  | None   -> Set.Poly.empty
                in
              (*  print_endline ("Doing " ^ (string_of_int w));
                print_endline ("with " ^ (List.to_string (Constraints.string_of_constraint) (Set.to_list cs)));
                print_endline ("Queue: " ^ (List.to_string string_of_int (Set.to_list (Set.remove work w))));*)
                let work = Set.fold cs ~init:(Set.remove work w) ~f:(fun work c -> match c with
                  | Succ(i, s, j)  -> if add_multi i s j then (Set.add work i) else work
                  | In(i, s, v)    -> failwith "Edges shouldn't contain In constraints..."
                  | Emit(is, j, h) -> Map.fold (get j) ~init:work ~f:(fun ~key ~data work -> match data,key with
                     | F, Wildcard(s) ->
                         (* foreach i in is, foreach Closure(f...) (B or F) in i,  add Gamma(f) >= F(Wildcard) *)
                         List.fold is ~init:work ~f:(fun work i ->
                           Map.fold (get i) ~init:work ~f:(fun ~key ~data work -> match key with
                             | Closure(f,_,_) -> begin match s with
                                                 | Inner -> Stack.push inner_escape f
                                                 | Outer -> Stack.push outer_escape f
                                                 | Prim  -> failwith "Emit to Prim???"
                                                 end;
                                                 List.fold (Map.find_exn gamma f) ~init:work ~f:(fun work g ->
                                                   late_add work (In(g, F, Wildcard(s)))
                                                 )
                             | Wildcard(s2) -> begin if (s2 <> Prim) && (s <> s2) then
                                                 collapse := true
                                               end;
                                               work
                           )
                         )
                     | F, Closure(f, inner_cs, js) ->
                         let sub = get_substitution h in
                         let inner_cs = Constraints.map sub h k (Stack.to_list inner_cs) in
                         (* Add in new constraints *)
                         let work = List.fold inner_cs ~init:work ~f:late_add in
                         (* Link with foreground path (i.e. sub(js) >= F(is)) *)
                         let work = List.fold2_exn js is ~init:work ~f:(fun work j i ->
                                      late_add work (Succ(sub j, Some F, i))
                                    ) in
                         (* Link with background 0-CFA variables (i.e. Gamma(f) >= B(is)) *)
                         List.fold2_exn (Map.find_exn gamma f) is ~init:work ~f:(fun work g i ->
                           late_add work (Succ(g, Some B, i))
                         )
                     | _ -> work
                    )
                ) in
  (*    Hashtbl.iter solution ~f:(fun ~key ~data ->
        print_string (string_of_int key);
        print_string " = ";
        Map.iter data ~f:(fun ~key ~data ->
          print_string (Constraints.string_with_state data (Constraints.string_of_value key));
          print_string ", "
        );
        print_endline ""
      );
                print_endline ("Queue: " ^ (List.to_string string_of_int (Set.to_list work)));
                print_endline "~~~~~";*)
                iterate work
  in

  iterate work;

  let to_set x = String.Set.of_list (Stack.to_list x) in

  if !collapse then
    let escaped = Set.union (to_set inner_escape) (to_set outer_escape) in
    (solution, escaped, escaped)
  else
    (solution, to_set inner_escape, to_set outer_escape)
