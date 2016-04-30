# Make sure ocamlbuild can find opam-managed packages: first run
#
# eval `opam config env`

# Easiest way to build: using ocamlbuild, which in turn uses ocamlfind

# See http://caml.inria.fr/pub/docs/manual-ocaml/comp.html for suppressed errors
# 44 & 45 In particular suppressed -- no errors, constantly filled up warning reports
stop:
	corebuild -use-ocamlfind -pkgs llvm,llvm.analysis \
		-cflags -w,+a-4-44-45 -I src/ stop.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	corebuild -clean
	rm -rf testall.log *.diff stop scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o *.ll *.out

.PHONY : clean_tests
clean_tests :
	rm -f *.ll *.out *.log
