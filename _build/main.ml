open Format
open Lexing
open Printf


module StringSet = Set.Make(String)





(* COMPILATION-RELATED *)

let parse_only = ref false
let type_only = ref false

let ifile = ref ""
let ofile = ref ""
let species = ref ""
let factor = ref ""

let set_file f s = f := s


let options =
  ["-parse-only", Arg.Set parse_only,
   " Syntactic analysis only";
  "-type-only", Arg.Set type_only,
" Lexical, syntactic and semantic analysis only";
   "-o", Arg.String (set_file ofile),
   "<file>  Output file name";
   "-scale", Arg.Tuple([Arg.Set_string species;Arg.Set_string factor]), "<species>, <factor> Do species scaling"]

let usage = "usage: compilateur [option] file.exp"

let localisation pos =
  let 
	l = pos.pos_lnum 
  in
  let 
	c = pos.pos_cnum - pos.pos_bol + 1 
  in
  	eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c





(*  GLOBAL VARIABLES *)

let count = ref 0
let scount = ref 0




(* HASHTABLES FOR: PARAMETERS, VARIABLES, DEPENDENCE *)

let params = Hashtbl.create 100
let vars = Hashtbl.create 100
let dep = Hashtbl.create 100
let dep_degree = Hashtbl.create 100






(* AUXILIARY FUNCTIONS *)

let print l = 
	List.iter (fun (name, coeff) -> printf "%s with coefficient %s, " name coeff) l


let expr l = 
	List.fold_left (fun acc (name, coeff) -> 
					if 
					  name ="null" 
					then 
					  "" 
					else 
					  if 
					    coeff <> "1"
					  then 
					    acc ^ "*" ^ name ^ "^" ^ coeff 
					  else 
					    acc ^ "*" ^ name
			) "" l



let first (a,b) = a
let snd (a,b) = b

(*determine if a tuple denotes a reaction, instead of a rate definition or initial condition*)

let is_react (a,b,c,d) = 
	not ((List.length a ==0 && List.length c ==0) || (List.length c == 0 && List.length d ==0))




(*counts number of reactions*)

let count_react prog =
	List.iter (fun  (react_list, rev, prod_list, rate_list) ->
				if rev = 2 || rev = 4 
				then scount := !scount +2
				else 
					if rev = 1 || rev = 5
					then scount := !scount +1
			  		else ()
			) prog

(* constructs the species-to-index association list*)
let construct_species prog =  let i = ref (-1) in
	let assoc_list = 
		List.fold_left (fun acc (react_list, rev, prod_list, rate_list) ->  
		     if List.length react_list > 0 && List.length rate_list > 0 
		     then 
			List.fold_left (fun acc (name, coeff) -> 
						if List.mem_assoc name acc || name ="null"
						then acc 
						else 
						     begin 
							i:= !i+1; 
							(name, !i) :: acc 
						     end
					) acc (List.append react_list prod_list)
		     else acc
		  ) [] prog 
	in
	List.rev assoc_list

(*decide if string is denoting a number; used for rate values*)

let is_int s = 
	       try
		ignore (int_of_string s);
		true
	       with _ ->
		false

let is_float s = 
		try
		 ignore (float_of_string s);
		 true
		with _ ->
		 false

let is_numeric s = is_int s || is_float s



(*constructs the J array*)

let construct_J prog = 
	let i = ref 0 in
	let j = Array.make !scount "0" in
        List.iter (fun (react_list, rev, prod_list, rate_list) -> 
			if List.length react_list > 0 && List.length rate_list > 0 
			then
			   begin 
				Array.set j !i (first (List.hd rate_list) ^ (if rev = 3 || rev = 2 then expr react_list else "")); 
				i:=!i+1 ;
			   	if rev = 2 || rev = 4
				then 
				  begin 
				   Array.set j !i (first (List.hd (List.tl rate_list)) ^ (if rev = 2 then expr prod_list else "")); 
				   i:=!i+1 
				  end
			   	else ()
			   end	
			else ()

		 ) prog;
	j
		
(*computes expression for "prod-cons"*)

let diff prod cons = 
	if (is_numeric cons && is_numeric prod)
	then string_of_float ((float_of_string prod)-.(float_of_string cons))	
	else
	if prod = "0."
	then ("-"^cons)
	else
	if cons="0."
	then
	prod
	else
	"("^prod^"-"^cons^")"
	
(*constructs the I matrix*)

let construct_I prog species columns=
	let aux = Array.make_matrix (List.length species) columns "0." in
	let _ = List.fold_left 
			(fun acc (react_list, rev, prod_list, rate_list)->
			    if List.length react_list > 0 && List.length rate_list > 0
			    then
				    begin
				    List.iter (fun (name,index) -> 
						let cons = try List.assoc name react_list 
							   with Not_found -> "0."
						in 
						let prod = try List.assoc name prod_list
							   with Not_found -> "0."
						in 
						aux.(index).(acc) <- (diff prod cons);
						if rev = 2 || rev = 4
				                then aux.(index).(acc+1) <- (diff cons prod)
						else ()
						) 
				    species;
                                    if rev = 2 || rev = 4 then acc +2 else acc+1
			          end
			    else acc 
			) 
		0 prog in
	aux




(*generates initial conditions file*)

let gen_init prog index =
	let aux=
	List.fold_left (fun acc (react_list, rev, prod_list, rates_list) ->
			if List.length prod_list + List.length rates_list = 0
			then
			List.fold_left (fun acc (species, init) -> (species, init) :: acc) acc react_list
			else 
			acc
		   ) [] prog
        in
	let file = "Matlab_code/initial.m"
	in
	let oc = open_out file
	in 
	fprintf oc "function init = initial () \n %%define initial conditions\n";
	List.iter (fun (name, index) -> let x = (if List.mem_assoc name aux then (List.assoc name aux) else "0") in fprintf oc " %s = %s;\n" name x) index;
	fprintf oc " init = [ ";
	List.iter (fun (name,index) -> fprintf oc "%s " name) index;
	fprintf oc "];\n";
	fprintf oc "end";
	close_out oc

(*check if rate is constant or expression*)

let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

let check s = 	String.contains s '+' ||
	      	String.contains s '-' ||
		String.contains s '/' ||
		String.contains s '^' ||
		String.contains s '(' ||
		String.contains s ')' ||
		String.contains s '*' ||
		contains s "square"

(*generates parameters and variables tables*)

let generate prog = List.iter (fun (r,rev,p,rates_list) -> 
				List.iter (fun (name,value) ->
						 if value <> "nondef" 
						 then 
						   if not (check value) 
						   then 
							Hashtbl.add params name value
						   else 
							Hashtbl.add vars name value
						 else () 
					   )  rates_list
			       ) prog




(*check that all parameters/variables appearing in rate expressions exist*)

(*TODO*)

(*generates parameter file*)
let gen_params table =
	let file = "Matlab_code/parameters.m"
	in
	let oc = open_out file
	in 
	fprintf oc "function params = parameters ()  \n %%define parameters\n";
	Hashtbl.iter (fun key value ->  fprintf oc " %s = %s;\n" key value) table;
	fprintf oc " params = [ ";
	Hashtbl.iter (fun key value -> fprintf oc "%s " key) table;
	fprintf oc "];\n";
	fprintf oc "end";
	close_out oc




(*SCALING PART*)

(*AUXILIARY FUNCTIONS*)

(*print list of reactions, as returned by the parser*)

let print_rlist rlist = List.iter (fun (a,b,c,d) -> 
					print_char '(';
					print_char '[';
					List.iter (fun (name,coeff) -> 
							print_char '(';
							print_string name;
							print_char ',';
							print_string coeff;
							print_char ')';
						  ) a;
					print_char ']';
					print_char ',';
					print_int b;
					print_char ',';
					print_char '[';
					List.iter (fun (name,coeff) -> 
							print_char '(';
							print_string name;
							print_char ',';
							print_string coeff;
							print_char ')';
						  ) c;
					print_char ']';
					print_char ',';
					print_char '[';
					List.iter (fun (name,coeff) -> 
							print_char '(';
							print_string name;
							print_char ',';
							print_string coeff;
							print_char ')';
						  ) d;
					print_char ']';
					print_char ')';
					print_newline ();
					
							
					
			) rlist


(*turns list into a set*)

let stringset_of_list l = List.fold_left 
				(fun set elem -> StringSet.add elem set) 
				StringSet.empty 
				l



(*MICHAELIS MENTEN PART*)
(*constructs Michaelis Menten regex using string s*)

let construct_MM s = 
	let id = "[A-Za-z0-9_]+"
	in
	Str.regexp(String.concat "" ["\\(";id;"\\*\\)*";s;"\\(\\*";id;"\\)*/(\\(";id;"\\+\\)*";s;"\\(\\+";id;"\\)*)"])



(*determines if rate is Michaelis Menten*)

let is_MM rate s= 
	let new_rate = Str.global_replace (Str.regexp " ") "" rate
	in
	let exp = construct_MM s	
	in 
	(Str.string_match exp new_rate 0) 

(*scale species in MM rate*)

let rec replace rate l factor = match l with
	[] -> rate
	| hd::tl -> let aux = Str.regexp hd
		    in replace (Str.replace_first aux ("("^factor^"*"^hd^")") rate) tl factor


let scale_MM rate species factor =
	match is_MM rate species 
	with              
		  true -> 
			  let last = Str.match_end()
			  in
			  let subrate = Str.matched_string rate
			  in
			  let index = Str.search_forward (Str.regexp "/") subrate 0
			  in
			  let s1 = Str.string_before subrate (index+1)
			  in
			  let s2 = Str.string_after subrate (index+1)
			  in 
			  let l = Str.split (Str.regexp "[+()]") s2
			  in
			  let ll = List.filter (fun x -> x <> species) l
		 	  in
			  s1^(replace s2 ll factor)^(Str.string_after rate last)
			   	
		| false -> rate
        

(*HILL PART*)

let construct_Hill s =
	let id = "[A-Za-z0-9_]+"
	in
	Str.regexp (String.concat "" ["\\(";id;"\\*\\)*";s;"\\^\\(";id;"\\)\\(\\*";id;"\\)*/(\\(";id;"\\^\\2\\+\\)*";s;"\\^\\2\\(\\+";id;"\\^\\2\\)*)"])

let is_Hill rate s =
	let new_rate = Str.global_replace (Str.regexp " ") "" rate
	in
	let exp = construct_Hill s
	in
	(Str.string_match exp new_rate 0)

let scale_Hill rate species factor =
	match is_Hill rate species
	with
		  true -> 
			  let last = Str.match_end()
			  in
			  let subrate = Str.matched_string rate
			  in
			  let index = Str.search_forward (Str.regexp "/") subrate 0
			  in
			  let s1 = Str.string_before subrate (index+1)
			  in
			  let s2 = Str.string_after subrate (index+1)
			  in 
			  let l = List.fold_left (fun acc x -> (List.hd (String.split_on_char '^' x))::acc) [] (Str.split (Str.regexp "[+()]") s2)
			  in
			  let ll = List.filter (fun x -> x <> species) l
		 	  in
			  s1^(replace s2 ll factor)^(Str.string_after rate last)

		| false -> rate
 
let add_rate_def rate =
	if (snd rate = "nondef")
	then []
	else [([],1,[],[rate])]

let scale_mass_action rate factor coeff  =
	String.concat "" [(fst rate);"/(";factor;"^";coeff;")"]
	

let scale prog species factor =
	List.fold_left 
	(
	fun new_list (a,b,c,d) ->
		if (b=0)  
		then try 
			let value = List.assoc species a 
			in 
			if is_numeric value 
			then new_list@[([(species, string_of_float((float_of_string factor) *.(float_of_string value)))],b,c,d)]
			else new_list@[([(species, factor^"*("^value^")")],b,c,d)]
		      with _ ->
			new_list@[(a,b,c,d)]
		else
		if (b=1)
		then let new_val = scale_MM (scale_Hill (snd (List.hd d)) species factor) species factor
		     in
		     new_list@[(a,b,c,[(fst (List.hd d),new_val)])]
		else
		if (b=2)
		then 
			try 
				let coeff1 = List.assoc species a
				in
				let aux = List.length new_list
				in
				let r1 = add_rate_def (List.hd d)
				in
				let new_forward_name = (fst (List.hd d))^(string_of_int aux)
				in
				let new_forward_rate = scale_mass_action (List.hd d) factor coeff1 
				in
				try
			  	  let coeff2 = List.assoc species c
				  in
				  let r2 = add_rate_def (List.nth d 1)
				  in
				  let new_backward_name = (fst (List.nth d 1))^(string_of_int (aux+1))
				  in
				  let new_backward_rate = scale_mass_action (List.nth d 1) factor coeff2 
				  in
				  new_list@[(a,b,c,[(new_forward_name,new_forward_rate);(new_backward_name,new_backward_rate)])]@r1@r2
				with _ ->
				  new_list@[(a,b,c,[(new_forward_name, new_forward_rate);List.nth d 1])]@r1
		     
			with _ -> new_list@[(a,b,c,d)]
				
			
		else
		if (b=3)
		then
		try 
			let coeff1 = List.assoc species a
			in
			let aux = List.length new_list
			in
			let r1 = add_rate_def (List.hd d)
			in
			let new_forward_name = (fst (List.hd d))^(string_of_int aux)
			in
			let new_forward_rate = scale_mass_action (List.hd d) factor coeff1 
			in
			new_list@[(a,b,c,[(new_forward_name, new_forward_rate)])]@r1
		     
		with Not_found -> new_list@[(a,b,c,d)]
		else
		if (b=4)
		then 
			let v1 = scale_MM (scale_Hill (snd (List.hd d)) species factor) species factor
			in
			let v2 = scale_MM (scale_Hill (snd (List.nth d 1)) species factor) species factor
			in
			new_list@[(a,b,c,[(fst (List.hd d),v1);(fst (List.nth d 1),v2)])]
		
		
		else    let v1 = scale_MM (scale_Hill (snd (List.hd d)) species factor) species (factor)
			in
			new_list@[(a,b,c,[(fst (List.hd d),v1)])]

			
		
	)
			

	[] prog

(* filters mass-action reactions where species "a" is a reactant AND non-mass action reactions where "species" appears in the reaction rate *)

let filter_list species list =  
	List.filter (
		     fun (a,b,c,d) ->		
			(b = 0 && List.mem_assoc species a) 		|| 
			(b = 1 && is_MM (snd(List.hd d)) species)    	|| 
			(b = 2 && (List.mem_assoc species a || List.mem_assoc species c)) ||
			(b = 3 && List.mem_assoc species a) ||
			(b = 4 && (is_MM (snd(List.hd d)) species || is_MM (snd(List.nth d 1)) species)) ||
			(b = 5 && is_MM (snd(List.hd d)) species)
					 
		   )
	list
	





(*constructs mass-action regex using string s*)

(*determines is rate is mass-action*)


(*DEPENDENCY PART*)
(*construct dependency hash table for vars*)

let gen_depth vars = 
	Hashtbl.iter 
	(
	 fun key value ->
	 	let dep_list = 
			stringset_of_list
			(List.filter 
				(fun x -> not (is_numeric x) && x<>"") 
				(Str.split (Str.regexp "[ +*-/\\(\\)\\^]") value)
			)
	 	in	
	 	Hashtbl.add dep key dep_list
	) 
	vars

let print_dep dep = Hashtbl.iter (fun key value -> 
					Printf.printf "%s depends on \n" key;
					StringSet.iter (fun x -> Printf.printf "%s \n" x) value;
					Printf.printf "\n\n";
					
				
			          ) dep


let rec depth key valueset index = 
	let aux = StringSet.fold
			(fun x acc -> if 
					Hashtbl.mem dep_degree x
				      then 
					max acc (1+Hashtbl.find dep_degree x)
				      else
					if
					  (Hashtbl.mem params x) || (List.mem_assoc x index)
					then
					  acc
					else
					  max acc (1+ depth x (Hashtbl.find dep x) index)
			) valueset 1
	in

	let _ = if (not (Hashtbl.mem dep_degree key)) then Hashtbl.add dep_degree key aux
	in 
        aux 

let gen_dep tbl index = Hashtbl.iter (fun key value ->  let _ = depth key value index in ()) tbl

let dep_list tbl = List.sort compare (Hashtbl.fold (fun key value acc -> (value, key)::acc) tbl [])

(*generates ODE file*)

let gen_ODE params vars index a b dep_order =
	let file = "Matlab_code/ODE.m"
	in
	let oc = open_out file
	in
	let c = ref 1 
	in
	fprintf oc "function dydt= ODE(t, y, parameters)\n";
	Hashtbl.iter (fun key value -> fprintf oc "%s = parameters(%d);\n" key !c; c:= !c +1 ) params;
	fprintf  oc "\n\n";
	List.iter (fun (name, value) -> fprintf  oc "%s = y(%d);\n" name (value+1)) index;
	fprintf oc "\n\n";
        (* print variables, in order of dep_degree *)
        List.iter (fun (lvl, name) -> let value = Hashtbl.find vars name in fprintf oc "%s = %s; \n" name value) dep_order;
	fprintf oc "\n\n";
	fprintf oc "dydt(size(y,1),1)= 0;\n";
	List.iter (
		fun (x,y) -> let i = List.assoc x index
			 in
			  begin
			   fprintf oc "dydt(%d)=" (y+1);
			   Array.fold_left (fun acc x -> 
						let aux = 
						   if (is_numeric x)
						   then
							if x= "-1." 
							then "-"
							else 
						  	   if x= "1." 
						  	   then "+" 
						           else 
							      if (float_of_string x) < 0. 
							      then x ^" * "
							      else " + " ^ x ^" * "
						  else 
						  if String.get x 0 = '-'
						  then
					          x^" * "
						  else " + "^x^" * "
						in 
						let _ =
						      if x <> "0."
						      then Some (fprintf oc (" %s ") (aux ^ b.(acc)))
						      else None
						in
						acc+1
						
					    ) 0 a.(i);
			   fprintf oc ";\n"
			  end

	      ) index;
	fprintf oc "end";
	close_out oc

(*generates main file*)

let gen_main index =
	let file = "Matlab_code/main.m"
	in
	let oc = open_out file 
	in
	fprintf oc ("%%parameters \n params = parameters ();\n") ;
	fprintf oc ("%%define initial conditions \n init = initial ();\n") ;
	fprintf oc ("%%call solver routine\n t0 = 0; \n tf = 1e5; \n [t,y] = ode15s(@(t,y) ODE(t, y, parameters), [t0,tf], init);\n");
	fprintf oc ("%%assign species names\n");
	List.iter (fun (name, value) -> fprintf  oc " %s = y(:, %d);\n" name (value+1)) index;
	close_out oc
   

let () =
  (* Parsing de la ligne de commande *)
  Arg.parse options (set_file ifile) usage;

  (* On vérifie que le nom du fichier source a bien été indiqué *)
  if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end;

 (* Ce fichier doit avoir l'extension .txt *)
  if not (Filename.check_suffix !ifile ".txt") then begin
    eprintf "Le fichier d'entrée doit avoir l'extension .txt\n@?"; 
    Arg.usage options usage;
    exit 1
  end;

  (* Par défaut, le fichier cible a le même nom que le fichier source, 
     seule l'extension change *)
  if !ofile="" then ofile := Filename.chop_suffix !ifile ".txt" ^ ".out";
  
  (* Ouverture du fichier source en lecture *)
  let f = open_in !ifile in

  (* Création d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in
  try
    begin
      let prog = if (!species<>"" && !factor<>"") 
		 then 
			(scale (Parser.file Lexer.token buf) !species !factor )
		 else 
			Parser.file Lexer.token buf 
	in
	let () = count_react prog in
	let s2i = construct_species prog in
	print_rlist prog;
	generate prog;
	gen_depth vars;
	gen_dep dep s2i;
        gen_init prog s2i;
	gen_params params;
	gen_ODE params vars s2i (construct_I prog s2i !scount) (construct_J prog) (dep_list dep_degree);
	gen_main s2i;
        close_in f
	
    end

  with
    | Lexer.Lexing_error s ->
	(* Erreur lexicale. On récupère sa position absolue et
	   on la convertit en numéro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf ("Erreur dans l'analyse lexicale\n");
        print_string s;
	exit 1
    | Parser.Error ->
	(* Erreur syntaxique. On récupère sa position absolue et on la
	   convertit en numéro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse syntaxique\n";
	exit 1
;;
