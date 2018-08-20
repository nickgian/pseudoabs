open Graph
open Unsigned

module VertexSet = Set.Make(Vertex)
   
module AbstractNode =
  struct
    include VertexSet
    let printAbstractNode (us : t) =
      let rec printAux lst acc =
        match lst with
        | [] -> "}" :: acc
        | [u] -> printAux [] ((Printf.sprintf "%d" (UInt32.to_int u)) :: acc)
        | u :: lst -> printAux lst ((Printf.sprintf "%d," (UInt32.to_int u)) :: acc)
      in
      String.concat "" (List.rev (printAux (elements us) ["{"]))

    let randomSplit (us : t) : (t * t) =
      let u1, u2, _ =
        fold (fun u (s1,s2,b) ->
            if b then
              (add u s1, s2, false)
            else
              (s1, add u s2, true)) us (empty, empty, true)
      in
      (u1, u2)
  end

module UInts = struct
  type t = UInt32.t

  let compare = compare
end

module GroupMap = Map.Make(UInts)
type key = UInts.t

type abstractionMap =
  { mutable absGroups : AbstractNode.t GroupMap.t; (* mapping each id to a set of nodes *)
    mutable groupId   : key VertexMap.t;           (* mapping each node to an id *)
    mutable nextId    : key;                       (* next available id *)
  }

let getId (f: abstractionMap) (u: Vertex.t) : key =
  VertexMap.find u (f.groupId)

let getIdPartial (f: abstractionMap) (u: Vertex.t) : key option =
  VertexMap.find_opt u (f.groupId)

let getGroupById (f: abstractionMap) (idx: key) : AbstractNode.t =
  GroupMap.find idx (f.absGroups)

let getGroup (f: abstractionMap) (u: Vertex.t) : AbstractNode.t =
  getGroupById f (getId f u)

let getGroupRepresentative (f: abstractionMap) (u: AbstractNode.t) : Vertex.t =
  AbstractNode.min_elt u
  
let getGroupId (f: abstractionMap) (u: AbstractNode.t) : key =
  getId f (getGroupRepresentative f u)

let partitionNode (f: abstractionMap) (newId: key) (u: Vertex.t) : unit =
  let _ =  match getIdPartial f u with
    | Some idx ->
       let us = getGroupById f idx in
       let newUs = AbstractNode.remove u us in
       if AbstractNode.is_empty newUs then
           f.absGroups <- GroupMap.remove idx (f.absGroups)
       else
         f.absGroups <- GroupMap.add idx newUs (f.absGroups)
    | None -> ()
  in
  f.groupId <- VertexMap.add u newId (f.groupId)
    
let partitionNodes (f: abstractionMap) (i: key) (us: AbstractNode.t) : unit =
  AbstractNode.iter (fun u -> partitionNode f i u) us;
  f.absGroups <- GroupMap.add i us (f.absGroups)

let split (f: abstractionMap) (us: AbstractNode.t) : abstractionMap =
  let f' = {absGroups = f.absGroups;
            groupId = f.groupId;
            nextId = UInt32.add f.nextId UInt32.one} in
  partitionNodes f' (f'.nextId) us;
  f'

let getAbstractGroups (f: abstractionMap) : AbstractNode.t list =
  List.map (fun (k,v) -> v) (GroupMap.bindings f.absGroups)

let printAbstractGroups (f: abstractionMap) (sep: string) : string =
  List.fold_left (fun acc us -> (AbstractNode.printAbstractNode us) ^ sep ^ acc)
                 "" (getAbstractGroups f)


let createAbstractionMap g : abstractionMap =
  let f = { absGroups = GroupMap.empty; groupId = VertexMap.empty; nextId = UInt32.zero} in
  partitionNodes f (f.nextId) (Graph.getNodes g);
  f

let fold (g: Vertex.t -> AbstractNode.t -> 'a -> 'a) (f: abstractionMap) (acc: 'a) : 'a =
  VertexMap.fold (fun u idx acc -> g u (getGroupById f idx) acc) f.groupId acc

let size (f: abstractionMap) : int =
  VertexMap.cardinal f.groupId

let normalize (f: abstractionMap) =
  let (nextIdN, groupIdN, absGroupsN) =
    VertexMap.fold (fun u id (nextIdN, groupIdN, absGroupsN) ->
        (UInt32.add nextIdN UInt32.one,
         VertexMap.add u nextIdN groupIdN,
         GroupMap.add nextIdN (getGroup f u) absGroupsN))
                   f.groupId (UInt32.zero, VertexMap.empty, GroupMap.empty)
  in
  f.absGroups <- absGroupsN;
  f.groupId <- groupIdN;
  f.nextId <- nextIdN
    
