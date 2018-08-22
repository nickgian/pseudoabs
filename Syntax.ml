open Unsigned

type declaration =
  | DNodes of UInt32.t
  | DEdges of (UInt32.t * UInt32.t) list
  | DRole of (string * (UInt32.t list))

type declarations = declaration list
