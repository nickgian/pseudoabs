open Syntax
open Unsigned
open Debug
open Graph
open Console
open AbstractionLoop

(* assumes the program is well formed *)
let buildGraph (ds: declarations) : Graph.t =
  let rec buildGraph_aux ds (nodes, edges, roles) =
    match ds with
    | [] -> (nodes, edges, roles)
    | d :: ds ->
       match d with
       | DNodes n ->
          begin
          match nodes with
          | None -> buildGraph_aux ds (Some n, edges, roles)
          | Some _ -> Console.error "multiple node defs"
          end     
       | DEdges es ->
          begin
          match edges with
          | None -> buildGraph_aux ds (nodes, Some es, roles)
          | Some _ -> Console.error "multiple edge defs"
          end
       | DRole r ->
          buildGraph_aux ds (nodes, edges, r :: roles)
  in
  match buildGraph_aux ds (None, None, []) with
  | (Some n, Some es, roles) ->
     add_edges (add_roles (Graph.create n) roles) es
  | _, _, _ -> Console.error "No nodes or edges defined"

let main =
  let file = Sys.argv.(1) in
  let source = UInt32.of_string (Sys.argv.(2)) in
  let dst = UInt32.of_string (Sys.argv.(3)) in
  let ds, info = Input.parse file in
  let g = buildGraph ds in
  let goal = Printf.sprintf "Reachability of %s from %s" Sys.argv.(3) Sys.argv.(2) in
  (* let goal = Printf.sprintf "Reachability of 0 from 14" in *)
  (* Printf.printf "builded\n"; *)
  (* print_newline (); *)
  (* let f = Abstraction.findAbstraction g UInt32.zero in *)
  (* Printf.printf "%s" (AbstractionMap.printAbstractGroups f "\n") *)  
  match abstractionLoop g source dst true with
  | Yes -> show_message "Success" T.Green goal
  | No (failed, _) ->
     show_message ("Impossible with failures: " ^ (printEdges failed "\n")) T.Red goal

