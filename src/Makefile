INSTALLDIR=`ocamlc -where`
MLFILES=$(wildcard *.ml)
MLIFILES=$(wildcard *.mli)

all: _build/LambdaJS.cma _build/LambdaJS.cmxa META

_build/LambdaJS.cma: $(MLFILES) $(MLIFILES)
	ocamlbuild LambdaJS.cma

_build/LambdaJS.cmxa: $(MLFILES) $(MLIFILES)
	ocamlbuild LambdaJS.cmxa

install: all
	ocamlfind install LambdaJS META *.mli _build/LambdaJS.cma _build/LambdaJS.cmxa _build/*.cmi

