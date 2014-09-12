(* Dovetail JCAM
 *
 * Copyright 2014      Peter Calvert <peter.calvert@cl.cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)

open Jcam
open Constraints
open Core.Std

type env = int String.Map.t

(* transition_id, block label, env *)
type block_visit = (string option) * env
type transition_visit = int * env

let env_eq (k1,e1) (k2,e2) = (k1 = k2) && (Map.equal (=) e1 e2)

let no_repeat f =
  let visited = Stack.create () in
  let rec real_f x =
    if Stack.mem visited x then
      ()
    else
      Stack.push visited x;
      f real_f x
  in real_f

(* Jcam.definition -> Constraints.t list *)
let generate def k =
  let channels = Hashtbl.Poly.create () in

  (* Top level constraints *)
  let toplevel = Stack.create () in
  
  (* Create 0-CFA variables for all channels, and initial constraints for    *)
  (* constructors.                                                           *)
  let gamma = String.Map.of_alist_exn (List.map def.channels (fun c ->
    (c.name, List.map c.args (fun _ ->
      let v = fresh_var () in
      begin if c.constructor then
        Stack.push toplevel (In (v, F, Wildcard))
      end;
      v
    ))
  )) in

  (* Function to create constraints for a given transition/local environment *)
  let rec do_transition constraints t e = begin
    (* (label * string, int) Hashtbl.t *)
    let vars = Hashtbl.Poly.create () in

    Map.iter e (fun ~key ~data -> Hashtbl.add_exn vars key data);

    let do_block = no_repeat (fun do_block l ->
      let block = List.find_exn t.blocks ~f:(fun b -> b.label = l) in

      let get_var v =
        match Hashtbl.find vars v with
        | Some x -> x
        | None   -> let x = fresh_var () in Hashtbl.add_exn vars v x; x
      in

      let value v = match v with
        | Value.Var(v) -> begin match List.find def.channels (fun c -> c.name = v) with
                          | Some c -> let var = fresh_var () in 
                                      let (cs,is) = do_channel c in
                                      Stack.push constraints (In(var, F, Closure(c.name, cs, is)));
                                      var
                          | None -> get_var v
                          end
        | _            -> let var = fresh_var () in
                          Stack.push constraints (In(var, F, Wildcard));
                          var
      in
      (* Phi Nodes *)
      List.iter block.phis (fun (v,t,incoming) ->
          let var = get_var v in
          List.iter incoming (fun (income,_) -> Stack.push constraints (Succ(var, F, value income))); (* TODO TODO TODO this shouldn't do 'F' *)
      );

      (* Other Instructions *)
      List.iteri block.instrs (fun cnt i -> match i with
        | Instruction.Assign(v,_) -> let var = get_var v in Stack.push constraints (In(var, F, Wildcard))
        | Instruction.Store(_,_,_,_) -> failwith "STORE not supported"
        | Instruction.Construct(c,ps) -> 
            let var = fresh_var () in
            Stack.push constraints (In(var, F, Wildcard));
            Stack.push constraints (Emit(List.map ps (fun (t,v) -> value v), var, [(t.tid, l, cnt)]))
        | Instruction.Emit(v,ps) ->
            Stack.push constraints (Emit(List.map ps (fun (t,v) -> value v), value v, [(t.tid, l, cnt)]))
      );

      (* Terminator instruction *)
      begin match block.terminator with
      | Terminator.Finish      -> ()
      | Terminator.Goto(l)     -> do_block (Some l)
      | Terminator.Cond(v,a,b) -> do_block (Some a); do_block (Some b)
      end;
    ) in

    do_block None
  end

  (* Signal level enumerate *)
  and do_channel c = begin
    match Hashtbl.find channels c.name with
    | Some result -> result
    | None -> begin
        let new_constraints = Stack.create () in
        let args = List.map c.args (fun _ -> fresh_var ()) in
        List.iter def.transitions (fun t ->
          if List.mem (List.map t.pattern (fun (x,_) -> x)) c.name then begin
            do_transition new_constraints t (
              List.fold t.pattern ~init:String.Map.empty ~f:(fun e (x,params) ->
                List.fold2_exn params (if x = c.name then args else Map.find_exn gamma x) ~init:e ~f:(fun e (_,v) i -> Map.add e v i)
              )
            )
          end else begin
            ()
          end
        );
        let result = (Stack.to_list new_constraints, args) in
        Hashtbl.set channels c.name result;
        result
    end
  end in

  (* Do all constructor transitions *)
  List.iter def.channels (fun c ->
    if c.constructor then begin
      let t = List.find_exn def.transitions ~f:(fun t -> (List.map t.pattern (fun (x,_) -> x)) = [ c.name ]) in
      do_transition toplevel t (
        List.fold t.pattern ~init:String.Map.empty ~f:(fun e (x,params) ->
          List.fold2_exn params (Map.find_exn gamma x) ~init:e ~f:(fun e (_,v) i -> Map.add e v i)
        )
      )
    end
  );

  (gamma, Stack.to_list toplevel)
