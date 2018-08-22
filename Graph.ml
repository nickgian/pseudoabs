open Unsigned

module Vertex = struct
  type t = UInt32.t * (string option)

  let vid (i,s) = i
  let vname (i,s) = s

  let printVertex (i, s) =
    match s with
    | None -> Printf.sprintf "%d" (UInt32.to_int i)
    | Some s -> Printf.sprintf "%s%d" s (UInt32.to_int i)
    
  let compare x y = UInt32.compare (fst x) (fst y)
end

module VertexMap = Map.Make (Vertex)
module VertexSet = Set.Make(Vertex)
module RoleMap = Map.Make(String)

module Edge = struct
  type t = Vertex.t * Vertex.t

  let compare (v1, w1) (v2, w2) =
    if Vertex.compare v1 v2 != 0 then Vertex.compare v1 v2
    else Vertex.compare w1 w2
end

module EdgeSet = Set.Make(Edge)

(* OCaml 4.06 contains find_opt and update built in. upgrade compiler. *)
let find_opt v m = try Some (VertexMap.find v m) with Not_found -> None

let update v f m =
  match f (find_opt v m) with
  | None -> VertexMap.remove v m
  | Some ns -> VertexMap.add v ns m

let vertex_map_to_string elem_to_string m =
  VertexMap.fold (fun k v s ->
      Vertex.printVertex k ^ ":" ^ elem_to_string v ^ "\n" ^ s) m ""

let print_vertex_map elem_to_string m =
  Printf.printf "%s" (vertex_map_to_string elem_to_string m)

(* a graph as adjacency list * # of vertices *)
type t =
  { adjacency: Vertex.t list VertexMap.t;
    roles: VertexSet.t RoleMap.t;
    size: UInt32.t
  }
    
(* create a graph with i vertices *)
let create i =
  let rec adj n acc =
    if UInt32.zero = n then
      VertexMap.add (n, None) [] acc
    else
      adj (UInt32.sub n (UInt32.one)) (VertexMap.add (n, None) [] acc)
  in
  { adjacency = adj (UInt32.sub i (UInt32.one)) VertexMap.empty;
    roles = RoleMap.empty;
    size = i }

(* vertices and edges *)
let num_vertices g = g.size

let getNodes g =
  VertexMap.fold (fun k _ acc -> VertexSet.add k acc) g.adjacency VertexSet.empty
                        
let edges g =
  let my_edges v neighbors =
    List.fold_left (fun a w -> (v, w) :: a) [] neighbors
  in
  List.rev
    (VertexMap.fold (fun v neighbors a -> my_edges v neighbors @ a) g.adjacency [])

(* a vertex v does not belong to a graph's set of vertices *)
exception BadVertex of Vertex.t

let good_vertex g v =
  try
    if UInt32.compare (Vertex.vid v) UInt32.zero < 0 ||
         not (UInt32.compare g.size (Vertex.vid v) > 0) then
      raise (BadVertex v)
  with
  | BadVertex v as ex ->
     Printf.printf "%s is not a valid vertex\n" (Vertex.printVertex v);
     raise ex

let good_graph g =
  List.iter (fun (v, w) -> good_vertex g v ; good_vertex g w) (edges g)

(* add_edge g e adds directed edge e to g *)
let add_edge (v, w) g =
  good_vertex g v ;
  good_vertex g w ;
  let f adj =
    match adj with
    | None -> Some [w]
    | Some ns -> if List.mem w ns then adj else Some (w :: ns)
  in
  {g with adjacency = update v f g.adjacency}

let add_uedge (v, w) g =
  add_edge (v,w) g |>
    add_edge (w,v)
  
let rec add_edges g edges =
  match edges with [] -> g | e :: edges -> add_edges (add_edge e g) edges

let rec add_uedges g edges =
  match edges with [] -> g | e :: edges -> add_uedges (add_uedge e g) edges

                                         
(* add_edge g e adds directed edge e to g *)
let remove_edge g (v, w) =
  good_vertex g v ;
  good_vertex g w ;
  let f adj =
    match adj with
    | None -> adj
    | Some ns ->
      match List.filter (fun a -> not (Vertex.compare a w = 0)) ns with
      | [] -> None
      | ns' -> Some ns'
  in
  {g with adjacency = update v f g.adjacency}

(* neighbors of v in g *)
let neighbors g v =
  good_vertex g v ;
  match find_opt v g.adjacency with None -> [] | Some ns -> ns

let printEdge (u,v) =
  Printf.sprintf "(%s,%s)" (Vertex.printVertex u) (Vertex.printVertex v)

let printEdges (es : EdgeSet.t) (sep : string) =
  let rec loop es =
    match es with
    | [] ->
       ""
    | [e] -> printEdge e
    | e :: es -> (printEdge e) ^ sep ^ loop es
  in
  loop (EdgeSet.elements es)

let add_role g (role, nodes) =
  {g with roles = RoleMap.add role (VertexSet.of_list nodes) g.roles}

let add_roles g rs =
  List.fold_left (fun acc r -> add_role acc r) g rs
                                                
let print g =
  Printf.printf "%d\n" (UInt32.to_int (num_vertices g)) ;
  List.iter
    (fun (v, w) ->
      Printf.printf "%s -> %s\n" (Vertex.printVertex v) (Vertex.printVertex w) )
    (edges g)

let to_string g =
  let b = Buffer.create 80 in
  let rec add_edges es =
    match es with
    | [] -> ()
    | (v, w) :: rest ->
        Buffer.add_string b
          (Printf.sprintf "%s -> %s\n" (Vertex.printVertex v) (Vertex.printVertex w)) ;
        add_edges rest
  in
  Buffer.add_string b (UInt32.to_string (num_vertices g) ^ "\n") ;
  add_edges (edges g) ;
  Buffer.contents b
