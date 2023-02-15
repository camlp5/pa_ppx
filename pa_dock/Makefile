# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=

PACKAGES := $(PACKAGES),fmt,bos,pa_ppx_base,pa_ppx_utils,camlp5.pa_r,camlp5.pa_lexer,pa_ppx_unmatched_vala
TARGET := pa_ppx_dock.cma
ML := comment_lexer.ml pa_dock.ml
MLI := pa_dock.mli
ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

OCAMLCFLAGS := $(OCAMLCFLAGS) -for-pack Pa_ppx_dock -linkall

all: $(TARGET) $(TARGET:.cma=.cmxa) camlp5.pa_ppx_dock camlp5.pa_ppx_dock.opt
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

pa_ppx_dock.cmo: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o pa_ppx_dock.cmo $^

pa_ppx_dock.cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o pa_ppx_dock.cmx $^

camlp5.pa_ppx_dock: $(TARGET)
	$(MKCAMLP5) -package fmt,bos,camlp5.pa_r,camlp5.pr_r,pa_ppx_base $(TARGET) -o $@

camlp5.pa_ppx_dock.opt: $(TARGET:.cma=.cmxa)
	$(MKCAMLP5OPT) -package fmt,bos,camlp5.pa_r,camlp5.pr_r,pa_ppx_base $(TARGET:.cma=.cmxa) -o $@

install:: META
	mkdir -p $(DESTDIR)/lib
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_dock -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.a) pa_ppx_dock.cmi $(wildcard *.cmt*)

clean::
	rm -rf META camlp5.pa_ppx_dock*

$(TARGET): $(TARGET:.cma=.cmo)
	$(OCAMLFIND) ocamlc $(DEBUG) $(TARGET:.cma=.cmo) -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(TARGET:.cma=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) $(TARGET:.cma=.cmx) -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
