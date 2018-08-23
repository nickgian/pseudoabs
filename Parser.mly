%{
  open Syntax
  open Unsigned
    
%}

%token <Span.t * Unsigned.UInt32.t> NUM
%token <Span.t * string> ID
%token <Span.t> SUB
%token <Span.t> EQ
%token <Span.t> LET 
%token <Span.t> SEMI 
%token <Span.t> LBRACE 
%token <Span.t> RBRACE 
%token <Span.t> EDGES 
%token <Span.t> NODES
%token <Span.t> ROLE
%token EOF

%start prog
%type  <Syntax.declarations> prog


%%
  
component:
    | LET EDGES EQ LBRACE RBRACE             { DEdges [] }
    | LET EDGES EQ LBRACE edges RBRACE       { DEdges $5 }
    | LET NODES EQ NUM                       { DNodes (snd $4) }
    | LET ROLE ID EQ LBRACE nodes RBRACE   { DRole ((snd $3), $6) }
;
  
components:
    | component                         { [$1] }
    | component components              { $1 :: $2 }
;

nodes:
    | NUM                   { [snd $1] }
    | NUM SEMI nodes        { (snd $1) :: $3 }
;

vertex:
  | NUM { Num (snd $1) }
  | ID  { Role (snd $1) }

edge:
    | vertex SUB vertex SEMI                  { [($1, $3)] }
    | vertex EQ vertex SEMI                   { [($1, $3); ($3, $1)] }
;

edges:
    | edge                              { $1 }
    | edge edges                        { $1 @ $2 }
;

prog:
    | components EOF                    { $1 }
;
