# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := unix,camlp5,oUnit,fmt,pcre,rresult,compiler-libs.common,yojson,sexplib0
TARGET := pa_ppx_testutils.cma
ML := papr_util.ml testutil.ml testutil2.ml
ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

OCAMLCFLAGS := $(OCAMLCFLAGS) -for-pack Pa_ppx_testutils

all: $(TARGET) $(TARGET:.cma=.cmxa)
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

testutil2.cmo: testutil2.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5o -c $<

testutil2.cmx: testutil2.ml
	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5o -c $<

pa_ppx_testutils.cmo: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o pa_ppx_testutils.cmo $^

pa_ppx_testutils.cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o pa_ppx_testutils.cmx $^

install:: META
	mkdir -p $(DESTDIR)/lib
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_testutils -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.a) pa_ppx_testutils.cmx pa_ppx_testutils.cmi $(wildcard *.cmt*)

clean::
	rm -rf META

$(TARGET): pa_ppx_testutils.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $^ -a -o $(TARGET)

$(TARGET:.cma=.cmxa): pa_ppx_testutils.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $^ -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
