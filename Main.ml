open Graph
open Abstraction
open AbstractionMap
open Syntax
open Unsigned
   
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

let pathToGroupId f (path: Vertex.t list) : key list =
  List.map (fun v -> getGroupId f (getGroup f v)) path
  
let main =
  let file = Sys.argv.(1) in
  let ds, info = Input.parse file in
  let g = buildGraph ds in
  let f = findAbstraction g (UInt32.zero) in
  let groups = getAbstractGroups f in
  List.iter (fun us -> AbstractNode.printAbstractNode us) groups
