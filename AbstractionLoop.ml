open Graph
open Abstraction
open AbstractionMap
open Syntax
open Unsigned
open Routing
open BatLazyList
open Console
open Debug
open Vertex
   
type result = Yes | No of failureSet * routingMap
                        
let pathToGroupId f (path: Vertex.t list) : key list =
  List.map (fun v -> getGroupId f (getGroup f v)) path
  
let getReachability (g: Graph.t) (s: Vertex.t) (d: Vertex.t) (failures: bool) : result =
  let rec loop sols =
    match BatLazyList.next sols with
    | Nil -> Yes
    | Cons ((failed, sol), sols) ->
       if VertexMap.mem s sol then
         loop sols
       else
         No (failed, sol)
  in
  loop (routeWithFailures g d failures)

  
(* Try to find a failure for which splitting would make the most
   sense. This is based on heuristics, currently: 
   
 * 1. Choose a u with |u| > 1, so we can split it.
 * 2. Choose failure (u,v) such that u can reach the destination and v cannot.
 * 3. Choose u with the biggest cost (thus distance from the destination). This is good
      for our splitting based on the path. *)
let findOptimalFailure (f: abstractionMap) (failedHat : EdgeSet.t)
                       (sol : routingMap) : Vertex.t =
  let candidate1 =
    EdgeSet.filter (fun (u,v) -> (AbstractNode.cardinal (getGroupById f (vid u))) > 1)
                   failedHat in
  let candidate2 =
    EdgeSet.filter (fun (u,v) -> (VertexMap.mem u sol)
                                 && (not (VertexMap.mem v sol))) candidate1 in
  match EdgeSet.is_empty candidate2 with
  | false ->
     let randMax = fst (EdgeSet.choose candidate2) in
     let maxu, _ = EdgeSet.fold (fun (u,v) (maxu,maxc) ->
                       if (snd (VertexMap.find u sol)) < maxc then
                         (u, snd (VertexMap.find u sol))
                       else
                         (maxu, maxc)) candidate2 (randMax, snd (VertexMap.find randMax sol))
     in
     maxu
  | true ->
     if EdgeSet.is_empty candidate1 then
       snd (EdgeSet.choose failedHat) (* if all our heuristics fail *)
     else
       fst (EdgeSet.choose candidate1)

let mapFailedSet (g: Graph.t) (f: abstractionMap) (failed: failureSet) : failureSet =
  EdgeSet.fold (fun ehat acc -> EdgeSet.union (abstractToConcreteEdge g f ehat) acc)
               failed EdgeSet.empty

let abstractionLoop_debug =
  let iterationNumber = ref 0 in
  fun (f: abstractionMap) (failed: EdgeSet.t)->
  if !(Debug.debugAbstractionLoop) then
    begin
      let groups = printAbstractGroups f "\n" in
      incr iterationNumber;
      show_message (string_of_int !iterationNumber) T.Blue "Iteration Number ";
      show_message (printEdges failed "\n") T.Blue "Failed Edges";
      show_message groups T.Blue "Abstract groups";
    end
  
let abstractionLoop (g: Graph.t) (s: Vertex.t) (d: Vertex.t) (failures: bool) : result =
  let rec loop (f: abstractionMap) =
    let ag = buildAbstractGraph g f in
    match getReachability ag (getId f s, vname s) (getId f d, vname d) failures with
    | Yes -> Yes
    | No (failed, sol) ->
       let concreteFailures = mapFailedSet g f failed in
       let numberOfFailures = EdgeSet.cardinal concreteFailures in
       abstractionLoop_debug f concreteFailures;
       if (numberOfFailures <= 1) then (* simple case*)
         No (concreteFailures, sol)
       else 
         begin
           (* refine abstraction and try again *)
           let uhat = findOptimalFailure f failed sol in
           let upath = pathOfRouting sol uhat in
           let f' = refineForFailures g f uhat upath in
           loop f'
         end
  in
  let f = findAbstraction g d in
  loop f

