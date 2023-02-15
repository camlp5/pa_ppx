# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=

PACKAGES := $(PACKAGES),pcre
TARGET := pa_ppx_utils.cma
ML := std.ml coll.ml std2.ml uf.ml tsort0.ml tsort.ml pack.ml hash.ml
MLI := std2.mli  std.mli  tsort0.mli  uf.mli pack.mli
ALLSRC = $(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

SYNTAX := camlp5o
OCAMLCFLAGS := $(OCAMLCFLAGS) -for-pack Pa_ppx_utils

all: $(TARGET) $(TARGET:.cma=.cmxa)
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

pa_ppx_utils.cmo pa_ppx_utils.cmi: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o pa_ppx_utils.cmo $^

pa_ppx_utils.cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o pa_ppx_utils.cmx $^

install:: META
	mkdir -p $(DESTDIR)/lib
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_utils -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.a) pa_ppx_utils.cmx pa_ppx_utils.cmi $(wildcard *.cmt*)

clean::
	rm -rf META

$(TARGET): pa_ppx_utils.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $^ -a -o $(TARGET)

$(TARGET:.cma=.cmxa): pa_ppx_utils.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $^ -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
