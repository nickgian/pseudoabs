open Syntax
open Unsigned
open Debug
open Graph
open Console
open AbstractionLoop

let expand_edge (e: Syntax.vertex * Syntax.vertex)
                 (roles: (UInt32.t list) RoleMap.t) : (UInt32.t * UInt32.t) list =
  match e with
  | Num i1, Num i2 -> [(i1, i2)]
  | Role s1, Role s2 ->
     let nodes1 = RoleMap.find s1 roles in
     let nodes2 = RoleMap.find s2 roles in
     List.fold_left (fun acc v1 ->
         List.fold_left (fun acc v2 -> (v1, v2) :: acc) acc nodes2) [] nodes1
  | Role s1, Num i ->
     let nodes1 = RoleMap.find s1 roles in
     List.fold_left (fun acc v1 -> (v1, i) :: acc) [] nodes1
  | Num i, Role s1 ->
     let nodes1 = RoleMap.find s1 roles in
     List.fold_left (fun acc v1 -> (i, v1) :: acc) [] nodes1

let expand_edges es roles =
  List.concat (List.map (fun e -> expand_edge e roles) es)
   
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
     let g = add_roles (Graph.create n) roles in
     let es = expand_edges es g.roles in
     add_edges g (List.map (fun (u,v) -> (get_vertex g u, get_vertex g v)) es)
  | _, _, _ -> Console.error "No nodes or edges defined"
             
let main =
  let file = Sys.argv.(1) in
  let source = UInt32.of_string (Sys.argv.(2)) in
  let dst = UInt32.of_string (Sys.argv.(3)) in
  let ds, info = Input.parse file in
  let g = buildGraph ds in
  let goal = Printf.sprintf "Reachability of %s from %s" Sys.argv.(3) Sys.argv.(2) in
  match abstractionLoop g (get_vertex g source) (get_vertex g dst) true with
  | Yes -> show_message "Success" T.Green goal
  | No (failed, _) ->
     show_message ("Impossible with failures: " ^ (printEdges failed "\n")) T.Red goal

