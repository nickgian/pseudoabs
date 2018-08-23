open Unsigned

type vertex = Num of UInt32.t | Role of string
   
type declaration =
  | DNodes of UInt32.t
  | DEdges of (vertex * vertex) list
  | DRole of (string * (UInt32.t list))

type declarations = declaration list
