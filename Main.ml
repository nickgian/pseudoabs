open Syntax
open Unsigned
open Debug
open Graph
open Console
open AbstractionLoop

(* assumes the program is well formed *)
let buildGraph (ds: declarations) : Graph.t =
  let rec buildGraph_aux ds (nodes, edges) =
    match ds with
    | [] -> (nodes, edges)
    | d :: ds ->
       match d with
       | DNodes n ->
          begin
          match nodes with
          | None -> buildGraph_aux ds (Some n, edges)
          | Some _ -> Console.error "multiple node defs"
          end     
       | DEdges es ->
          begin
          match edges with
          | None -> buildGraph_aux ds (nodes, Some es)
          | Some _ -> Console.error "multiple edge defs"
          end
  in
  match buildGraph_aux ds (None, None) with
  | (Some n, Some es) ->
     add_edges (Graph.create n) es
  | _, _ -> Console.error "No nodes or edges defined"

                
let main =
  Printf.printf "entering main\n";
  let file = Sys.argv.(1) in
  (* let source = UInt32.of_string (Sys.argv.(2)) in  *)
  (* let dst = UInt32.of_string (Sys.argv.(3)) in *)
  let source = UInt32.of_int 14 in
  let dst = UInt32.zero in
  Printf.printf "parsing...\n";
  print_newline ();
  let ds, info = Input.parse file in
  Printf.printf "building...\n";
  print_newline ();
  let g = buildGraph ds in
  (* let goal = Printf.sprintf "Reachability of %s from %s" Sys.argv.(3) Sys.argv.(2) in *)
  let goal = Printf.sprintf "Reachability of 0 from 14" in
  Printf.printf "builded\n";
  print_newline ();
  let f = Abstraction.findAbstraction g UInt32.zero in
  AbstractionMap.printAbstractGroups f
  
  (* match abstractionLoop g source dst false with *)
  (* | Yes -> show_message "Success" T.Green goal *)
  (* | No (failed, _) -> *)
  (*    show_message ("Impossible with failures: " ^ (printEdges failed "\n")) T.Red goal *)

