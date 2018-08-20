open Graph
open BatLazyList

type cost = int

(** The type of solutions, mapping each vertex to a node it forwards
   to and a cost to reach the destination *)
type routingMap = (Vertex.t option * cost) VertexMap.t

(** Represents the failed links of the network*)
type failureSet = EdgeSet.t

(** [route g d failedLinks] solves the routing problem for network [g]
   and destination [d] under the link failures given by [failedLinks]*)
val route: Graph.t -> Vertex.t -> failureSet -> routingMap

(** [routeWithFailures g d failedLinks] lazily solves multiple routing
   problems for network [g] and destination [d], one for each possible
   link failure if [failures = true]. *)
val routeWithFailures: Graph.t -> Vertex.t -> bool -> (failureSet * routingMap) BatLazyList.t

(** [pathOfRouting sol u] returns the path from [u] to the destination
   or the empty list if there isn't one*)  
val pathOfRouting: routingMap -> Vertex.t -> Vertex.t list
