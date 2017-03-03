/* ************************************************************************** */
/* **************************    PARSER    ********************************* */
/* ************************************************************************** */

%{

 open Lexing;;
 open Ast;;


 let fst l = if List.length l = 0 then [] else  [List.hd l] ;;
 let snd l = if List.length l = 0 then [] else  [List.hd l; List.hd (List.tl l)] ;;


%}

%token <string> CONST
%token <string> ID
%token PLUS MINUS TIMES DIV
%token LPAR RPAR
%token SQRT POW
%token EQUAL
%token REV IREV NMAREV NMAIREV
%token SEMI
%token EOF
%token EQN INIT PARAMS



%start file

%type <Ast.file> file

%%


file:
 | EQN; p = reaction_list; PARAMS; r = rate_def; INIT; i = init_cond; EOF	
							{List.append (p:file) (List.append (r:file) (i:file))}
;

init_cond:
 							{[]}
 | i = init; l = init_cond				{i :: l}
;

init:
 | i = ID; c = CONST					{([(i,c)], 0, [], [])}
 | i = ID; EQUAL; c = CONST				{([(i,c)], 0, [], [])}
;

rate_def:
 							{[]}
 | i = rate_val; l = rate_def				{i :: l}
;

rate_val:
 | i = ID; EQUAL; c = CONST				{([], 1, [], [(i, c)])}
 | i = ID; EQUAL; s = expr				{([], 1, [], [(i, s)])}
 | i = ID; EQUAL; j = ID				{([], 1, [], [(i, j)])}
;


reaction_list:
  							{[]}
  | i = reaction; l = reaction_list;			{i :: l}
;

reaction:

  | r = rp_list; REV; p = rp_list; SEMI; rt = rate_list	 {(r,2,p,(snd rt))} 
  | r = rp_list; IREV; p = rp_list; SEMI; rt = rate_list 	{(r, 3, p, (fst rt))} 
  | r = rp_list; NMAREV; p = rp_list; SEMI; rt = rate_list	{(r, 4,p,(snd rt))} 
  | r = rp_list; NMAIREV; p = rp_list; SEMI; rt = rate_list 	{(r, 5, p, (fst rt))} 
;


rp_list:
  | s = species						{[s]}
  | s = species; PLUS; l = rp_list			{s :: l}
;

species:
 | c = CONST; i = ID					{(i, c)}
 | c = CONST; TIMES; i=ID				{(i,c) }
 | c = ID; TIMES; i=ID					{(i,c)}
 | i = ID						{(i, "1")}
;

rate_list:
 							{[]}
 | r = rate						{[r]}
 | r = rate; SEMI; l = rate_list			{r :: l}

;

rate:
 | i = ID 						{(i, "nondef")}
 | i = ID; EQUAL; c = CONST				{(i, c)}
 | i = ID; EQUAL; s = expr				{(i, s)}
 | i = ID; EQUAL; j = ID				{(i, j)}
;

expr:
 | i = ID						{ i }
 | i = CONST						{ i }
 | LPAR; e = expr; RPAR					{ "("^e^")" }
 | e1= expr; PLUS; e2 = expr				{  e1 ^ "+"^e2}
 | e1= expr; MINUS; e2 = expr				{  e1 ^ "-"^e2}
 | e1= expr; TIMES; e2 = expr				{  e1 ^ "*"^e2}
 | e1= expr; DIV; e2 = expr				{  e1 ^ "/"^e2}
 | e1= expr; POW; e2 = expr				{  e1 ^ "^"^e2}
 | SQRT; e = expr					{ "square " ^ e }
 | MINUS; e = expr					{ "-" ^ e }
;








