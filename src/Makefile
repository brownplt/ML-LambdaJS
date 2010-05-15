INSTALLDIR=`ocamlc -where`
MLFILES=$(wildcard *.ml)
MLIFILES=$(wildcard *.mli)

.PHONY = clean builddir debug

OCAMLBUILD = ocamlbuild -build-dir ../build -quiet

debug: builddir
	$(OCAMLBUILD) jsc.d.byte

lib: builddir
	$(OCAMLBUILD) LambdaJS.cma
	$(OCAMLBUILD) LambdaJS.cmxa
	$(OCAMLBUILD) LambdaJS.p.cmxa
	$(OCAMLBUILD) LambdaJS.d.cma

clean:
	rm -rf ../build

builddir:
	@mkdir ../build 2> /dev/null || true

install: lib
	cd ../build && ocamlfind install LambdaJS ../src/META *.mli *.cmi LambdaJS.cma LambdaJS.cmxa LambdaJS.d.cma LambdaJS.p.cmxa LambdaJS.a LambdaJS.p.a

