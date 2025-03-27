# Makefile,v
# Copyright (c) INRIA 2007-2017

ifeq ($(OS),Windows_NT)
WD=$(shell cygpath --absolute --windows .)
else
WD=$(shell pwd)
endif
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),fmt,pa_ppx_utils,pa_ppx_base,pa_ppx_base

TARGET_PACKAGE=pa_ppx_deriving
TARGET=$(TARGET_PACKAGE).cma
ML=pa_deriving.ml surveil.ml pa_rebindto.ml deriving_harness.ml pa_deriving_base.ml
MLI=surveil.mli pa_deriving_base.mli pa_deriving.mli
ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

OCAMLCFLAGS := $(OCAMLCFLAGS) -linkall -for-pack Pa_ppx_deriving

all: $(TARGET) $(TARGET:.cma=.cmxa) camlp5.pa_ppx_deriving camlp5.pa_ppx_deriving.opt
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

camlp5.pa_ppx_deriving: $(TARGET)
	$(LAUNCH) $(MKCAMLP5) -package camlp5.pa_r,camlp5.pr_r,pa_ppx_base $(TARGET) -o $@

camlp5.pa_ppx_deriving.opt: $(TARGET:.cma=.cmxa)
	$(LAUNCH) $(MKCAMLP5OPT) -package camlp5.pa_r,camlp5.pr_r,pa_ppx_base $(TARGET:.cma=.cmxa) -o $@

TARGET_INSTALL_FILES= $(PACKED_ARCHIVE_TARGET_INSTALL_FILES)
install:: default_install

clean::
	rm -rf META camlp5.pa_ppx_deriving

$(TARGET_PACKAGE).cmo $(TARGET_PACKAGE).cmi: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o $(TARGET_PACKAGE).cmo $^

$(TARGET_PACKAGE).cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o $(TARGET_PACKAGE).cmx $^

$(TARGET): $(TARGET_PACKAGE).cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $^ -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(TARGET_PACKAGE).cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $^ -a -o $(TARGET:.cma=.cmxa)
