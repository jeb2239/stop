# Makefile for Stop Language compiler

scanner.ml : scanner.mll
	ocamllex scanner.mll

.PHONY : clean
clean :
	rm -f parser.ml parser.mli scanner.ml *.cmo *.cmi
