INSTALLDIR=`ocamlc -where`
MLFILES=$(wildcard *.ml)
MLIFILES=$(wildcard *.mli)

all: _build/LambdaJS.cma _build/LambdaJS.cmxa _build/LambdaJS.d.cma _build/LambdaJS.p.cmxa META

_build/LambdaJS.cma: $(MLFILES) $(MLIFILES)
	ocamlbuild LambdaJS.cma

_build/LambdaJS.cmxa: $(MLFILES) $(MLIFILES)
	ocamlbuild LambdaJS.cmxa

_build/LambdaJS.p.cmxa: $(MLFILES) $(MLIFILES)
	ocamlbuild LambdaJS.p.cmxa

_build/LambdaJS.d.cma: $(MLFILES) $(MLIFILES)
	ocamlbuild LambdaJS.d.cma

install: all
	ocamlfind install LambdaJS META *.mli _build/LambdaJS.cma _build/LambdaJS.cmxa _build/LambdaJS.d.cma _build/LambdaJS.p.cmxa _build/*.cmi

