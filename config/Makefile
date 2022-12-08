# Makefile,v
# Copyright (c) INRIA 2007-2017

RM=rm

OCAMLTOPLEVEL=ocaml
CAMLP5LIB=$(shell $(OCAMLFIND) query camlp5)
LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5 -verbose -I +dynlink
MKCAMLP5OPT=$(LAUNCH) mkcamlp5.opt -verbose -I +dynlink
CAMLP5R=camlp5r -I $(CAMLP5LIB)
INCLUDES=
OCAMLCFLAGS=$(DEBUG) $(WARNERR) $(INCLUDES)
PACKAGES=unix,compiler-libs.common,camlp5,camlp5.extfun
PACKAGES := $(PACKAGES),camlp5.parser_quotations
#PACKAGES := $(PACKAGES),camlp5.quotations
SYNTAX = camlp5r

export PERL5LIB := $(TOP):$(PERL5LIB)

top: all

.mli.cmi:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmo:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmx:
	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmt:
	@mkdir -p .cmt
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_dock -syntax $(SYNTAX) -bin-annot -c $< -o .cmt/$*.cmo
	cp .cmt/$*.cmt $*.cmt

.mli.cmti:
	@mkdir -p .cmt
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_dock -syntax $(SYNTAX) -bin-annot -c $< -o .cmt/$*.cmi
	cp .cmt/$*.cmti $*.cmti

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti .byte .opt

clean::
	$(RM) -rf *.cm* *.o *.a *.byte *.log *.cache *.ppo *.ppo.ml _build/* .cmt *.odoc

realclean:: clean
