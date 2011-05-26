.PHONY = clean builddir debug

OCAMLBUILD = ocamlbuild -build-dir ../build -quiet

debug: builddir
	$(OCAMLBUILD) jsc.d.byte

clean:
	rm -rf ../build

builddir:
	@mkdir ../build 2> /dev/null || true

