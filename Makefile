# Make sure ocamlbuild can find opam-managed packages: first run
#
# eval `opam config env`

# Easiest way to build: using ocamlbuild, which in turn uses ocamlfind

stop:
	corebuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 \
		stop.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	corebuild -clean
	rm -rf testall.log *.diff stop scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o *.ll *.out
