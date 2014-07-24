open Hashtbl;;
exception NotUnif ;;												   (* raised if unification of 2 terms fail *)

type term = A of atom | C of string | V of string | Z of int           (* types for abstract program. *)
and atom = string*(term list)										   (* vars are zeroary symbols(C x|Z x|V x)  *)
and head = atom
and body = atom list
and hclause = F of head | R of head * body
and program  = hclause list;; 

let  index = ref 1;;			                                       (* index for mapping V(x) to Z(index) *)						
let progList = ref ([]:program) ;;
let queryList = ref ([]:body) ;;
let (hashVar: (string,int) Hashtbl.t) = create 50 ;;
let queryOn = ref false ;;                                             (* bool for distinguishing b/w program and query *)
(*******************	some helper functions 	******************************)
let giveVar st =if(mem hashVar st) then Z(find hashVar st)				(* give next mappped Z(index) for given V(x) *)
				else let () = index:= !index +1 in let ()= replace hashVar st !index in Z(!index);;
let stOfC inp = match inp with 										(* string represntation of a term *)
	| V x | C x -> x
	| Z x -> string_of_int(x)
	| A x  -> "Not Used";;

let remove_duplicates l =                                           (* remove duplicates from a list *)
  let sl = List.sort compare l in
  let rec go l acc = match l with
    | [] ->  acc
    | [x] ->  (x::acc) 
    | (x1::x2::xs) -> if x1 = x2
      then go (x2::xs) acc
      else go (x2::xs) (x1::acc)
  in go sl [];;

(********************************* gives the set of all the vars in the program/query ***********************************************)

let rec varInTerm (trm : term):term list = match trm with
		| A (st,t_list) -> varsT(t_list)
		| C st -> []
		| q   -> [q]
and varsT  (t_list:term list)  = match t_list with
		| [] -> []
		| trm :: tl -> varInTerm(trm) @ varsT( tl )
and vars ( (st:string),t_list) =varsT(t_list);;

let rec varsInList lis = match lis with
	| [] -> []
	| hd::tl -> (vars hd) @ varsInList tl;;

(*************************  Functions for printing the program given in abstract syntax *******************************)
let rec printTerm trm = match trm with
  | A( hh ) -> let _ = printAtom hh in ()
  | V( st ) -> let _ = print_string("V("^st^")\n") in ()
  | Z (x) -> let _ = print_string("Z("^string_of_int(x)^")\n") in ()
  | C(st) -> let _= print_string("C("^st^")\n") in ()
and printAtom atm=match atm with
  | (st,lis) ->let _ = (let () = print_string(st^":") in List.map printTerm lis) in ();;

let tostr plist =match plist with
  | F hd -> let _ = printAtom hd in ()
  | R(hd,bd) -> let _ = (List.map printAtom bd) in ();; 

(**********************************checking well-formed *************************************************)

let rec wfInst sym_table inpTree = match inpTree with
		| F hd -> appWfAtom sym_table [hd]
		| R(hd,bd) -> appWfAtom sym_table (hd::bd)
and appWfAtom sym_table tail = match tail with
		| [] 	 -> true
		| hd::tl -> (wfAtom sym_table hd) && appWfAtom sym_table tl
and wfAtom sym_table (st,childs) = let len = List.length childs in
						if ( (mem sym_table st) && find sym_table st != len) 
						then false else let ()=replace sym_table st len in  appWfTerm sym_table childs
and appWfTerm sym_table tail = match tail with
		| [] -> true
		| hd::tl -> (wfTerm sym_table hd) && appWfTerm sym_table tl
and wfTerm sym_table inpTree = match inpTree with
		| V st | C st -> if ( (mem sym_table st) && find sym_table st != 0 ) then false else let ()=replace sym_table st 0 in true
		| A atm -> wfAtom sym_table atm
		| _ -> true;;

let checkWf prog = let hash = create 100 in (List.fold_left (fun x y-> x&&y) true (List.map (wfInst hash) prog) );;


(************************************ defining substitution ****************************************************)
type substitution = term -> term;;                       (* type of substitution *)

(**** any substitution can be defined as follow  **********)
let sbst (myhash:( term , term ) Hashtbl.t ) (v1) = if(mem myhash v1) then find myhash v1 else v1;;

let rec apply (s : substitution) (t : term) : term =match t with
		| (V x) as t -> s(t)
		| (Z x) as t  -> s(t) 
		| C x -> C x
		| A(st, lis ) -> A( st, List.map (apply s) lis );; 

(* composition of substitutoins: fog(x)  *)
let compose (f:substitution) (g:substitution) (v1:term) : term = apply g (f v1);;


let rec occurs x (t : term) : bool =                         (*  occur's check for mgu algorithm  *)
  match t with 
  | A (_, s) -> List.exists (occurs x) s
  | _ as t -> t=x;;

(*************************** unifies two atoms and gives (bool,sbst) ****************************************)
let unifier (tree1:atom) (tree2:atom) =
	let rec unify_single (t1:term) (t2:term)= let cmgu=create 10 in
		match (t1,t2) with
		| (C x,C y) -> if x=y then sbst cmgu else raise NotUnif
		| ( (V _ | Z _) as t , Z x ) ->  let () = replace cmgu (Z x) t in sbst cmgu
		| ( (V _ | Z _) as t , V x ) ->  let () = replace cmgu (V x) t in sbst cmgu
		| ((V _ | Z _) as t,C y) -> let ()=replace cmgu (t) (C y) in sbst cmgu
		| (C y,V x) -> let ()=replace cmgu (V x) (C y) in sbst cmgu
		| (C y,Z x) -> let ()=replace cmgu (Z x) (C y) in sbst cmgu
		| ((V _ | Z _) as tt, (A(_,_) as t) ) -> if occurs (tt) t then raise NotUnif else let ()=replace cmgu (t1) t in sbst cmgu
		| ( (A(_,_) as t),V x) -> if occurs (V x) t then raise NotUnif else let ()=replace cmgu (V x) t in sbst cmgu
		| ( A(st1,sc) , A(st2,tc) ) -> if(st1=st2 && List.length sc = List.length tc ) then unify_atom (List.combine sc tc) (sbst cmgu) else raise NotUnif
		|  _ -> raise NotUnif
	and unify_atom t_list fn= match t_list with
		| [] ->fn
		| (t1,t2)::tl -> let temp=unify_single (apply fn t1) (apply fn t2) in 
				(unify_atom tl (compose fn temp) ) in
	let unify (st1,t1) (st2,t2) =
		let table=create 10 in
		let fn = sbst table in 
		try
			if(st1<>st2) then  (false,fn) else 
			let t_list = List.combine t1 t2 in
			(true,unify_atom t_list fn)
		with
		| NotUnif ->  (false,fn);
	in unify tree1 tree2;;

(****************************************** some more helper functions  ************************************************)

let rec printStack (stck:(body*substitution) Stack.t) = 
	if(Stack.is_empty stck) then print_string("--------\n") (* exit(0) *) else let top=Stack.pop stck in let _ = (List.map printAtom (fst top) ) in printStack stck ;;  

let rec occursNot prog = match prog with                               (* returns true if "not" occurs in given atom *)
	| [] -> false
	| F(st,_)::tl -> st="not" ||  occursNot tl 
	| R(_,t_list)::tl -> (checkInBody t_list) || occursNot tl
and checkInBody t_list = match t_list with
	| [] -> false
	| (st,_)::tl -> st="not" || checkInBody tl;;


(*************************************** Solves the query (also negated queries) ************************************************)
let solveWithNot( querylist ) =
	let stck:((body*substitution) Stack.t) = Stack.create () in         (* stack for running the program *) 
	let _vars = if(List.length querylist=1) then vars(List.hd querylist) else remove_duplicates (varsInList querylist) in
	let varLength = List.length _vars in 						(* _vars is list of vars   *)
	let mapping = create 10 in
	let var_subst = sbst mapping in 							(*  creating the empty substitution  *)
	let success = ref false in 									(* true if negated queries fail *)
	let count = ref 0 in                                        (*  for counting the number of answers returned *)
	let allOn =ref false in 									(* used for printing *) 
	let ()=Stack.push (querylist,var_subst) stck in
	let begining = ref true in
	let applyAtom mysbst atm = match atm with
		| (st , t_list) ->  (st, List.map mysbst t_list) in
	let rec printAns _subst varSet=match varSet with 			(* for printing answers to command line *)
		| [] -> count:=1 + !count ;
		| hd::tl  ->print_string("\n"^stOfC(hd)^" = "^stOfC(_subst hd) ); 
					success:=true; printAns _subst tl in
	let rec pushStack lis tl = match lis with 					(* adding the partial atom list to stack *)
		| [] -> ()
		| (at,subst)::tl2 -> let () = Stack.push (at @ (List.map (applyAtom subst) tl),subst) stck in pushStack tl2 tl in 
	let rec tryUnify atm prog _subst=						(* tries to unifies 2 terms, returns (true,subst) if unifiable*)
		match prog with 									(* return (false,_) if terms are not unifiable *)
		| [] -> []
		| F(a1)::tl -> let res=unifier atm a1 in 	
					if(fst res) then let comp=compose (snd res) _subst in 
						(tryUnify atm tl _subst)@  [ [],comp] 
					else (tryUnify atm tl _subst)
		| R(a1,a1_list)::tl  ->
					let res=unifier atm a1 in
						let mysbst=compose (snd res) _subst in
						if (fst res) then let rhs = List.map (applyAtom (snd res) ) a1_list in
							(tryUnify atm tl _subst)@[ rhs, mysbst]
						else (tryUnify atm tl _subst)
			 in
	let handleNot _subst t_list =match t_list with 			(* for evaluating negated queries. *)
			| [v1;v2] -> _subst v1 <> _subst v2
			| _ -> false  in
	while Stack.is_empty stck=false do 						(* main loop *)
		let top = Stack.pop stck in
		let subGoals =fst top and ans=snd top in
		(match subGoals with
			| [] ->
				if(!begining) then (if(varLength>0) then let _ = begining:=false in printAns ans _vars else success:=true)
				else if(!allOn=false) then 
					let inp = read_line () in 
						if(inp="." ) then Stack.clear stck else 
						if(inp=";" ) then (if(varLength>0) then let _ = begining:=false in printAns ans _vars else success:=true) 
						else let _ = allOn:= true in if(varLength>0) then let _ = begining:=false in printAns ans _vars else success:=true 
				else (if(varLength>0) then let () =print_string("\n") in printAns ans _vars else success:=true)

			| (st,hd)::tl ->let lis= if(st="not") then (if(handleNot ans hd) then [[],ans] else [])
						else tryUnify (st,hd) !progList ans in
						pushStack lis tl );
		if(Stack.is_empty stck && (!success) ) then let () = print_string("\nyes\n") in
			(if(varLength>0) then print_string("count:"^string_of_int(!count)^"\n") ); flush stdout;
		if(Stack.is_empty stck && !success=false) then print_string("\nno\n"); flush stdout;
	done

(***********************************test queries**************************************************************)
(* let q1 = ["father",[C "pandu";V "P"];"father",[V "P";V "K"]] ;;
let q2 = ["halfsibling",[V "K";V "P"] ] ;;
let q3 = ["truesibling",[V "K";V "P"] ] ;;

let q4 = ["sibling",[C "arjun";V "P"] ] ;;

let q5 = ["brother",[V "G";V "P"] ] 
and q6 = ["sister",[V "G";V "P" ]]
and q7 = ["step_father",[V "G";V "P" ]]
and q8 = ["step_mother",[V "G";V "P" ]]
and q9 = ["half_brother",[V "G";V "P" ]]
and q10 = ["half_sister",[V "G";V "P" ]]
and q11 = ["uncleaunt",[V "G";C "arjun" ]]           (* ???????? *)
and q12 = ["aunt",[V "G";V "P" ]]				 (* ??????   *)
and q13 = ["sister_in_law",[V "G";C "arjun" ]]    (* ?????  *)
and q14 = ["ancester",[V "G";V "P" ]]
and q15 = ["cousin",[V "G";V "P" ]];;

let q16 = ["child",[C "arjun";C "pandu"] ] *)