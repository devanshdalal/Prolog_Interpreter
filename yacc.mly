/* yacc.mly */
%{

	open Printf;;
	open Hashtbl;;
	open Execute;;
%}

%token <string> VARIABLE
%token <string> IDEN
%token <string> PREDICATE
%token IFF DOT LPAREN RPAREN COMMA SEMICOLON EOF
%token NEWLINE NOTEQ IS

%start prog
%start query
%type <Execute.program> prog
%type <unit> query

%% 
/* Grammar rules and actions for query  */
query : query EOF 				{ raise End_of_file; }
	  |	atom_list DOT           { queryList := $1;   }


/* Grammar rules and actions for Parser */
prog :	 /*empty*/ 				{ 					[]  } 
	| prog relation			    { clear hashVar;  $1 @ [$2]	}
	| prog EOF      			{	 progList:=$1;
									(*let _ = (List.map tostr $1) in*) 
									let _ = raise End_of_file in $1  }
;

relation:	atom DOT			{   F( $1 ) 	}
	| atom IFF atom_list DOT	{   R($1,$3)  	}
;

atom_list : atom     { [$1] }
	| atom_list COMMA atom {  $1 @ [$3] }
;

atom : 
	IDEN LPAREN term_list RPAREN     		{ ($1,$3)  }
	| LPAREN VARIABLE NOTEQ VARIABLE RPAREN	{	("not",[giveVar $2;giveVar $4])	}
;

term_list : term              { [$1]      }
	| term_list COMMA term    { ($1)@[$3] }
;

term :
     atom   		{ A($1) }
	| IDEN          { C($1) }
	| VARIABLE  	{   (* choosing the variables for queries as well as input file. *)
						if( !queryOn ) then V($1)
					 	else giveVar $1
					}
;
%%