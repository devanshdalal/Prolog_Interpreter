all:
	ocamlc -c execute.ml
	ocamlyacc yacc.mly
	ocamlc -c yacc.mli
	ocamllex lexer.mll
	ocamlc -c yacc.ml
	ocamlc -c lexer.ml
	ocamlc -c main.ml
	ocamlc -o run execute.cmo lexer.cmo yacc.cmo main.cmo
 
clean:
	rm *.cmo
	rm *.cmi
	rm run
	rm *.mli
	rm yacc.ml
	rm lexer.ml
