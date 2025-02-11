# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := str,unix,camlp5,oUnit,fmt,pcre2,rresult,compiler-libs.common,yojson,sexplib0
TARGET_PACKAGE = pa_ppx_testutils
TARGET = $(TARGET_PACKAGE).cma
ML = papr_util.ml testutil.ml testutil2.ml
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

TARGET_INSTALL_FILES= $(PACKED_ARCHIVE_TARGET_INSTALL_FILES)
install:: default_install

clean::
	rm -rf META

$(TARGET_PACKAGE).cmo $(TARGET_PACKAGE).cmi: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o $(TARGET_PACKAGE).cmo $^

$(TARGET_PACKAGE).cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o $(TARGET_PACKAGE).cmx $^

$(TARGET): $(TARGET_PACKAGE).cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $^ -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(TARGET_PACKAGE).cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $^ -a -o $(TARGET:.cma=.cmxa)
