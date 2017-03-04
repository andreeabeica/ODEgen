
(* The type of tokens. *)

type token = 
  | UNSCALABLE
  | TIMES
  | SQRT
  | SEMI
  | RPAR
  | REV
  | POW
  | PLUS
  | PARAMS
  | NMAREV
  | NMAIREV
  | MINUS
  | LPAR
  | IREV
  | INIT
  | ID of (string)
  | EQUAL
  | EQN
  | EOF
  | DIV
  | CONST of (string)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.file)
