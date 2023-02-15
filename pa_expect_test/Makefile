# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=

PACKAGES := $(PACKAGES),fmt,pa_ppx_base
TARGET := pa_ppx_expect_test.cma
ML := pa_expect_test.ml
ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

all: $(TARGET) $(TARGET:.cma=.cmxa) camlp5.pa_ppx_expect_test camlp5.pa_ppx_expect_test.opt
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

camlp5.pa_ppx_expect_test: $(TARGET)
	$(MKCAMLP5) -package fmt,camlp5.pa_r,camlp5.pr_r,pa_ppx_base $(TARGET) -o $@

camlp5.pa_ppx_expect_test.opt: $(TARGET:.cma=.cmxa)
	$(MKCAMLP5OPT) -package fmt,camlp5.pa_r,camlp5.pr_r,pa_ppx_base $(TARGET:.cma=.cmxa) -o $@

install:: META
	mkdir -p $(DESTDIR)/lib
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_expect_test -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.a) $(TARGET:.cma=.cmxa) $(wildcard *.cmt*)

clean::
	rm -rf META camlp5.pa_ppx_expect_test*

$(TARGET): $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) $(CMO) -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) $(CMO:.cmo=.cmx) -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
