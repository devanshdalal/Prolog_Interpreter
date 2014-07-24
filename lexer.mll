{
	open Yacc
}

let capitalalpha=['A'-'Z']
let smallalpha=['a'-'z']
let digit=['0'-'9']
(*let dig=['1'-'9']*)
let dot=['.']
let comma=[',']
let underscore=['_']
let apostrophe="'"
let iff=":-"
let condLine = ['\n']*
let alphanumeric=digit | smallalpha | capitalalpha
let variables= capitalalpha (underscore* | alphanumeric* | apostrophe* | capitalalpha*)* 
let iden= smallalpha (underscore* | alphanumeric* | apostrophe* | smallalpha*)*

rule token = parse
	|	[' ' '\t' '\n' ]+    {  token lexbuf }
	(*|	[ '\n' ]+			 { NEWLINE }*)
	|	iden as id           {  IDEN id }
	|	iff		             {  IFF }
	|	variables as pd      {  VARIABLE pd }
	|	dot		             {  DOT }
	|	comma	             {  COMMA }
	|	"\\=="				 {  NOTEQ  }
	|	"("		             {  LPAREN }
	|	"is"				 {  IS    }
	|	")"		             {  RPAREN }
	|	"/*"                 { 	comments lexbuf  }
	|	"%"					 {  lineComments lexbuf}
	|	_		             { 	token lexbuf }
	|	eof		             { 	EOF } 

and comments = parse
	| "*/"  { token lexbuf  }
	| _   { comments lexbuf   }
	| eof   {   print_string("Parenthesis Not closed \n" ); raise End_of_file }

and lineComments = parse
	| "\n"| eof  { token lexbuf  }
	| _     	 { lineComments lexbuf   }
	