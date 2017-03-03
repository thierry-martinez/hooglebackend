OCAMLFLAGS=-package compiler-libs.common

.PHONY: all
all: hooglebackend

hooglebackend: hooglebackend.cmx
	ocamlfind ocamlopt $(OCAMLFLAGS) -linkpkg -o $@ $<

%.cmx: %.ml
	ocamlfind ocamlopt $(OCAMLFLAGS) -c $<
