(** Implements shortest path routing *)

open Graph
open Batteries
open BatLazyList

type cost = int
type routingMap = (Vertex.t option * cost) VertexMap.t
type failureSet = EdgeSet.t

(* Not that routing here, does not work in the same direction as SRPs,
   e.g. it's (d,u) not (u,d) *)
let route (g: Graph.t) (d: Vertex.t) (failed: failureSet): routingMap =
  let sol = ref (VertexMap.singleton d (None, 0)) in
  let q = Queue.create () in
  let () = Queue.push (d,0) q in
  let visited u = VertexMap.mem u !sol in 
  let rec explore () =
    try
      let (u,c) = Queue.pop q in
      let ns = List.filter (fun v -> not (EdgeSet.mem (u,v) failed)) (neighbors g u) in
      List.iter (fun v -> if not (visited v) then
                            begin
                              sol := VertexMap.add v (Some u, c+1) !sol;
                              Queue.push (v, c+1) q
                            end) ns;
      explore ()
    with
    | Queue.Empty -> !sol
  in
  explore ()

(* Doing just one failure for simplicity *)
let routeWithFailures (g: Graph.t) (d: Vertex.t) (failures:bool) =
  let gedges = if failures then Graph.edges g else [] in
  let edgeStream = LazyList.of_list gedges in
  (EdgeSet.empty, route g d EdgeSet.empty) ^:^
    (LazyList.map
       (fun failed -> let fs = EdgeSet.singleton failed in
                      (fs, route g d fs)) edgeStream)

let pathOfRouting (sol: routingMap) (u: Vertex.t) : Vertex.t list =
  let rec traverse (u: Vertex.t) acc =
    match VertexMap.find u sol with
    | None, _ -> acc
    | Some v, _ -> traverse v (v :: acc)
  in
  try List.rev (traverse u [u])
  with Not_found -> []

  
