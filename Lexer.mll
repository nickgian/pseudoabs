
{
  open Parser
  open Printf
  open Span
  exception Eof

  let position lexbuf =
    {start=Lexing.lexeme_start lexbuf; finish=Lexing.lexeme_end lexbuf}

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- 
      { pos with Lexing.pos_lnum = pos.Lexing.pos_lnum + 1; 
                 Lexing.pos_bol = pos.Lexing.pos_cnum; } ;;

}

let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let symbol = ['~' '`' '!' '@' '#' '$' '%' '^' '&' '|' ':' '?' '>' '<' '[' ']' '=' '-' '.']+
let num = ['0'-'9']+
let tid = ['\'']['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*
  
rule token = parse
  | "(*"         { comments 0 lexbuf }
  | "let"        { LET (position lexbuf) } 
  | "edges"      { EDGES (position lexbuf) }
  | "nodes"      { NODES (position lexbuf) }
  | num as n     { NUM (position lexbuf, Unsigned.UInt32.of_string n) }
  | "-"          { SUB (position lexbuf) }
  | "="          { EQ (position lexbuf) }
  | ";"          { SEMI (position lexbuf) }
  | "{"          { LBRACE (position lexbuf) }
  | "}"          { RBRACE (position lexbuf) }
  | [' ' '\t']   { token lexbuf }
  | '\n'         { incr_linenum lexbuf; token lexbuf}
  | _ as c       { printf "[Parse Error] Unrecognized character: %c\n" c; token lexbuf }
  | eof		       { EOF }

and comments level = parse
  | "*)"  { if level = 0 then token lexbuf else comments (level-1) lexbuf }
  | "(*"  { comments (level+1) lexbuf }
  | '\n'  { incr_linenum lexbuf; comments level lexbuf}
  | _     { comments level lexbuf }
  | eof   { raise End_of_file }
