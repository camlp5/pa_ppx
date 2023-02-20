# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),fmt,pa_ppx_base
TARGET_PACKAGE = pa_ppx_expect_test
TARGET = $(TARGET_PACKAGE).cma
ML = pa_ppx_expect_test.ml
ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

all: $(TARGET) $(TARGET:.cma=.cmxa) camlp5.pa_ppx_expect_test camlp5.pa_ppx_expect_test.opt
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

camlp5.pa_ppx_expect_test: $(TARGET)
	$(LAUNCH) $(MKCAMLP5) -package fmt,camlp5.pa_r,camlp5.pr_r,pa_ppx_base $(TARGET) -o $@

camlp5.pa_ppx_expect_test.opt: $(TARGET:.cma=.cmxa)
	$(LAUNCH) $(MKCAMLP5OPT) -package fmt,camlp5.pa_r,camlp5.pr_r,pa_ppx_base $(TARGET:.cma=.cmxa) -o $@

TARGET_INSTALL_FILES= $(DEFAULT_TARGET_INSTALL_FILES)
install:: default_install

clean::
	rm -rf META camlp5.pa_ppx_expect_test*

$(TARGET): $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) $(CMO) -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) $(CMO:.cmo=.cmx) -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
