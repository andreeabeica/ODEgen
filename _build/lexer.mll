(* ***************************************************************************)
(* *********************           LEXER		**********************)
(* ***************************************************************************)

{
open Lexing
open Parser
open Printf

exception Lexing_error of string

let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }


}

let letter = ['a' - 'z'  'A' - 'Z']
let digit = ['0' - '9']
let integer = ['1'-'9'] digit+
let ident = [ 'a' - 'z'  'A' - 'Z' '_'] ['a' - 'z'  'A' - 'Z' '0' - '9' '_']*
let space = [' ' '\t']
let digit_octal = ['0'-'7']
let digit_hexa = ['0'-'9' 'a'-'f' 'A'-'F']
let character =  ([' ' - '~'] # ['\\' '\'' '\"']) 
let coeff =  (((['0'-'9']+ | ['0'-'9']+ '.' ['0'-'9']*) | (['0'-'9']* '.' ['0'-'9']+)) ((['e' 'E'] ['+' '-'] ['0'-'9']+) | (['e' 'E'] ['0'-'9']+))) 
  | ((['0'-'9']+ '.' ['0'-'9']*) | (['0'-'9']* '.' ['0'-'9']+))
	     | (['1'-'9']['0'-'9']*)


rule token = parse
| '\n'			{newline lexbuf;token lexbuf}
| "PARAMS"		{PARAMS}
| "INIT"		{INIT}
| "EQN"			{EQN}
| "square"		{SQRT}
| space+		{token lexbuf}
| '#'| "//"		{comment lexbuf}
| integer as c		{CONST(c)}
| digit as c		{CONST(String.make 1 c)}
| coeff as c		{CONST(c)}
| ident as name		{ID(name)}
| '-'			{MINUS}
| '^'			{POW}
| '/'			{DIV}
| '*'			{TIMES}
| '('			{LPAR}
| ')'			{RPAR}
| '+'			{PLUS}
| '='			{EQUAL}
| "<->"			{REV}
| "->"			{IREV}
| "<=>"			{NMAREV}
| "=>"			{NMAIREV}
| ';'			{SEMI}
| eof			{EOF}
| _  as c		{raise (Lexing_error ("illegal character: " ^ 
                	(String.make 1 c )^ "\n"))}


and comment = parse
|'\n'			{newline lexbuf; token lexbuf}
| eof			{EOF}
| _			{comment lexbuf}


