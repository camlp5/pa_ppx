# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

LAUNCH=$(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5
PACKAGES := $(PACKAGES),fmt,pa_ppx_utils,pa_ppx_base,pa_ppx_base

TARGET=pa_deriving.cma
ML=pa_deriving.ml surveil.ml pa_rebindto.ml deriving_harness.ml pa_deriving_base.ml
MLI=surveil.mli pa_deriving_base.mli pa_deriving.mli
CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

OCAMLCFLAGS := $(OCAMLCFLAGS) -linkall

all: $(TARGET) $(TARGET:.cma=.cmxa) camlp5.pa_ppx_deriving camlp5.pa_ppx_deriving.opt
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

camlp5.pa_ppx_deriving: $(TARGET)
	$(MKCAMLP5) -verbose -package camlp5.pa_r,camlp5.pr_r,pa_ppx_base $(TARGET) -o $@

camlp5.pa_ppx_deriving.opt: $(TARGET:.cma=.cmxa)
	$(MKCAMLP5).opt -verbose -package camlp5.pa_r,camlp5.pr_r,pa_ppx_base $(TARGET:.cma=.cmxa) -o $@

META: META.pl
	./META.pl > META

install::
	mkdir -p $(DESTDIR)/lib
	./META.pl $(DESTDIR)/lib > META
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_deriving -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.a) $(MLI:.mli=.cmi) $(CMX) $(wildcard *.cmt*)
	$(RM) -f META

clean::
	rm -rf META camlp5.pa_ppx_deriving

$(TARGET): $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) $(CMO) -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) $(CMO:.cmo=.cmx) -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r *.ml *.mli >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
