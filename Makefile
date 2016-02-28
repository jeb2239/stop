# Makefile for Stop Language compiler

OBJS = parser.cmo scanner.cmo calc.cmo ast.cmo

calc : $(OBJS)
	ocamlc -o calc $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f calc parser.ml parser.mli scanner.ml *.cmo *.cmi

calc.cmo: scanner.cmo parser.cmi ast.cmi
calc.cmx: scanner.cmx parser.cmx ast.cmi
parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmo parser.cmi
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
parser.cmi: ast.cmo
