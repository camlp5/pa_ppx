# Makefile,v
# Copyright (c) INRIA 2007-2017

RM=rm

NOT_OCAMLFIND=not-ocamlfind
LAUNCH=env TOP=$(TOP) ocamlfind camlp5-buildscripts/LAUNCH --
OCAMLFIND=ocamlfind
JOINMETA=ocamlfind camlp5-buildscripts/join_meta
YAWRAP=ocamlfind camlp5-buildscripts/ya-wrap-ocamlfind
OCAMLTOPLEVEL=ocaml
CAMLP5LIB=$(shell $(OCAMLFIND) query camlp5)
MKCAMLP5=mkcamlp5 -verbose -I +dynlink
MKCAMLP5OPT=mkcamlp5.opt -verbose -I +dynlink
INCLUDES=
OCAMLCFLAGS= $(INCLUDES)
PACKAGES=unix,compiler-libs.common,camlp5,camlp5.extfun,camlp5.parser_quotations
PRPACKAGE = camlp5.pr_o

export PERL5LIB := $(TOP):$(PERL5LIB)

top: all

.mli.cmi:
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

.ml.cmo:
#	$(LAUNCH) $(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),$(PRPACKAGE) $< > $*_ppo.ml
#	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -c $*_ppo.ml
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

.ml.cmx:
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamlopt $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

.ml.cmt:
	@mkdir -p .cmt
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_dock -bin-annot -c $< -o .cmt/$*.cmo
	cp .cmt/$*.cmt $*.cmt

.mli.cmti:
	@mkdir -p .cmt
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_dock -bin-annot -c $< -o .cmt/$*.cmi
	cp .cmt/$*.cmti $*.cmti

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	$(RM) -rf *.cm* *.o *.a *.byte *.opt *.log *.cache *.ppo *ppo.ml _build/* .cmt *.odoc

realclean:: clean

EXTERNAL=$(shell $(LAUNCH) $(OCAMLFIND) query -predicates byte -format '%m' $(ALLPACKAGES) | grep local-install)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamldep -package $(ALLPACKAGES) -- $(ALLSRC) >> .depend.NEW
	mv .depend.NEW .depend

-include .depend
