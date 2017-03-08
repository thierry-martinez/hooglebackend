OCAMLFLAGS=-package compiler-libs.common
OCAMLOPT=$(OCAMLFIND) ocamlopt
MKDIRP=mkdir -p
PREFIX=/usr/local
INSTALL=install
INSTALL_PROGRAM=$(INSTALL)
bindir=$(PREFIX)/bin

.PHONY: all
all: hooglebackend

hooglebackend: hooglebackend.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) -linkpkg -o $@ $<

install: hooglebackend
	$(MKDIRP) $(bindir)
	$(INSTALL_PROGRAM) hooglebackend $(bindir)/hooglebackend

uninstall:
	- rm $(bindir)/hooglebackend

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<
