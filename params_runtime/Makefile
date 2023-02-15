# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

PACKAGES := rresult,fmt,camlp5,uint,pa_ppx_base,pa_ppx_deriving_plugins.std,camlp5.parser_quotations
TARGET := pa_ppx_params.cma
ML := runtime.ml
MLI := 
ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)
CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

OCAMLCFLAGS := $(OCAMLCFLAGS) -for-pack Pa_ppx_params

all: $(TARGET) $(TARGET:.cma=.cmxa)
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

pa_ppx_params.cmo: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o pa_ppx_params.cmo $^

pa_ppx_params.cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o pa_ppx_params.cmx $^

META: META.pl
	./META.pl > META

install:: META
	mkdir -p $(DESTDIR)/lib
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_params_runtime -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.a) pa_ppx_params.cmx pa_ppx_params.cmi $(wildcard *.cmt*)

clean::
	rm -rf META camlp5.pa_ppx_params

$(TARGET): pa_ppx_params.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) pa_ppx_params.cmo -a -o $(TARGET)

$(TARGET:.cma=.cmxa): pa_ppx_params.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) pa_ppx_params.cmx -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
