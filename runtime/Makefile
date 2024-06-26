# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES = rresult,fmt,camlp5,yojson,sexplib0
TARGET_PACKAGE = pa_ppx_runtime
TARGET = $(TARGET_PACKAGE).cma
ML = runtime.ml exceptions.ml
MLI = exceptions.mli
ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

OCAMLCFLAGS := $(OCAMLCFLAGS) -for-pack Pa_ppx_runtime -linkall

all: $(TARGET) $(TARGET:.cma=.cmxa)
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

TARGET_INSTALL_FILES= $(PACKED_ARCHIVE_TARGET_INSTALL_FILES)
install:: default_install

clean::
	rm -rf META camlp5.pa_ppx_runtime

initialize-exn-i:: exceptions.ORIG.mli
	echo "(**pp -syntax camlp5r *)" > $@.NEW
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.macro,camlp5.pr_r -syntax camlp5o $< >> $@.NEW
	mv $@.NEW exceptions.mli

bootstrap-exn-i: exceptions.ORIG.mli
	echo "(**pp -syntax camlp5r *)" > $@.NEW
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.macro,camlp5.pr_r,pa_ppx_import,pa_ppx_deriving_plugins.std,pa_ppx_deriving_plugins.yojson,pa_ppx_deriving_plugins.sexp \
	-syntax camlp5o \
	-ppopt -pa_ppx-runtime -ppopt "" \
	-ppopt -D -ppopt BOOTSTRAP \
	-ppopt -sep -ppopt "\n" \
	$< >> $@.NEW
	mv $@.NEW  exceptions.mli

refold-exn-i: exceptions.mli
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.pr_r,pa_ppx_undo_deriving \
	-syntax camlp5r \
	$< > $@.NEW
	mv $@.NEW  exceptions.REFOLD.mli

initialize-exn:: exceptions.ORIG.ml
	echo "(**pp -syntax camlp5r *)" > $@.NEW
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.macro,camlp5.pr_r -syntax camlp5o $< >> $@.NEW
	mv $@.NEW exceptions.ml

bootstrap-exn: exceptions.ORIG.ml
	echo "(**pp -syntax camlp5r *)" > $@.NEW
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.macro,camlp5.pr_r,pa_ppx_import,pa_ppx_deriving_plugins.show,pa_ppx_deriving_plugins.yojson,pa_ppx_deriving_plugins.sexp \
	-syntax camlp5o \
	-ppopt -pa_ppx-runtime -ppopt "" \
	-ppopt -D -ppopt BOOTSTRAP \
	-ppopt -sep -ppopt "\n" \
	$< >> $@.NEW
	mv $@.NEW  exceptions.ml

refold-exn: exceptions.ml
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.pr_r,pa_ppx_undo_deriving \
	-syntax camlp5r \
	$< > $@.NEW
	mv $@.NEW  exceptions.REFOLD.ml

$(TARGET_PACKAGE).cmo $(TARGET_PACKAGE).cmi: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o $(TARGET_PACKAGE).cmo $^

$(TARGET_PACKAGE).cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o $(TARGET_PACKAGE).cmx $^

$(TARGET): $(TARGET_PACKAGE).cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $^ -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(TARGET_PACKAGE).cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $^ -a -o $(TARGET:.cma=.cmxa)
