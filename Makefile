.DEFAULT_GOAL=build

PSH=src/parsetree_hack.ml
CP=cp -v
OB=ocamlbuild -use-ocamlfind #-classic-display
OCAML_COMPILER=$(shell opam config var compiler)

$(PSH):
	if [ "4.02.3-modular-implicits" = "$(OCAML_COMPILER)" ]; then $(CP) src/parsetree_hack.mi.ml $(PSH); else $(CP) src/parsetree_hack.normal.ml $(PSH); fi

build: $(PSH)
	cp pkg/META.in pkg/META
	$(OB) src/GT.cmo src/GT.cmx
	ocaml pkg/build.ml native=true native-dynlink=true

derive = $(shell ocamlfind query ppx_deriving.show)
dry_run: build
	ocamlfind ppx_tools/rewriter "ocamlfind ppx_deriving/ppx_deriving ${derive}/ppx_deriving_show.cma _build/src/ppx_deriving_gt.cma" src_test/test_ppx_gt.ml

test: build
	$(RM) -r _build/src_test/
	$(OB) -j 0 -classic-display src_test/test_ppx_gt.byte

clean:
	$(RM) $(PSH)
	ocamlbuild -clean

.PHONY: build test doc clean
