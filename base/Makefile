# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),rresult,fmt,pa_ppx_utils,pa_ppx_runtime,compiler-libs.common
TARGET := pa_ppx_base.cma
ML := base.ml pp_MLast.ml pp_parsetree.ml ppxutil.ml pa_passthru.ml pa_normalize.ml
MLI := ppxutil.mli pa_passthru.mli pp_MLast.mli pp_parsetree.mli

ALLSRC := $(ML) $(MLI)
ALLPACKAGES := $(PACKAGES)

OCAMLCFLAGS := $(OCAMLCFLAGS) -for-pack Pa_ppx_base -linkall

all: $(TARGET) $(TARGET:.cma=.cmxa) camlp5.pa_ppx_base camlp5.pa_ppx_base.opt
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

pa_ppx_base.cmo: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -bin-annot -o pa_ppx_base.cmo $^

pa_ppx_base.cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -bin-annot -o pa_ppx_base.cmx $^

initialize-pp-MLast-i:: pp_MLast.ORIG.mli
	echo "(** -syntax camlp5r *)" > $@.NEW
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package camlp5.macro,camlp5.pr_r -syntax camlp5o $< >> $@.NEW
	mv $@.NEW pp_MLast.mli

bootstrap-pp-MLast-i: pp_MLast.ORIG.mli
	echo "(** -syntax camlp5r *)" > $@.NEW
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.macro,camlp5.pr_r,pa_ppx_import,pa_ppx_deriving_plugins.std \
	-syntax camlp5o -ppopt -D -ppopt BOOTSTRAP \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-ppopt -sep -ppopt "\n" \
	$< >> $@.NEW
	mv $@.NEW  pp_MLast.mli

refold-pp-MLast-i: pp_MLast.mli
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.pr_r,pa_ppx_undo_deriving \
	-syntax camlp5r \
	$< > $@.NEW
	mv $@.NEW  pp_MLast.REFOLD.mli

initialize-pp-MLast:: pp_MLast.ORIG.ml
	echo "(** -syntax camlp5r *)" > $@.NEW
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package camlp5.macro,camlp5.pr_r -syntax camlp5o $< >> $@.NEW
	mv $@.NEW pp_MLast.ml

bootstrap-pp-MLast: pp_MLast.ORIG.ml
	echo "(** -syntax camlp5r *)" > $@.NEW
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.macro,camlp5.pr_r,pa_ppx_import,pa_ppx_deriving_plugins.std \
	-syntax camlp5o -ppopt -D -ppopt BOOTSTRAP \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-ppopt -sep -ppopt "\n" \
	$< >> $@.NEW
	mv $@.NEW  pp_MLast.ml

refold-pp-MLast: pp_MLast.ml
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.pr_r,pa_ppx_undo_deriving \
	-syntax camlp5r \
	$< > $@.NEW
	mv $@.NEW  pp_MLast.REFOLD.ml

initialize-pp-parsetree-i:: pp_parsetree.ORIG.mli
	echo "(** -syntax camlp5r *)" > $@.NEW
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package camlp5.macro,camlp5.pr_r -syntax camlp5o $< >> $@.NEW
	mv $@.NEW pp_parsetree.mli

bootstrap-pp-parsetree-i: pp_parsetree.ORIG.mli
	echo "(** -syntax camlp5r *)" > $@.NEW
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.macro,camlp5.pr_r,pa_ppx_import,pa_ppx_deriving_plugins.std \
	-syntax camlp5o -ppopt -D -ppopt BOOTSTRAP \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-ppopt -sep -ppopt "\n" \
	$< >> $@.NEW
	mv $@.NEW  pp_parsetree.mli

refold-pp-parsetree-i: pp_parsetree.mli
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.pr_r,pa_ppx_undo_deriving \
	-syntax camlp5r \
	$< > $@.NEW
	mv $@.NEW  pp_parsetree.REFOLD.mli

initialize-pp-parsetree:: pp_parsetree.ORIG.ml
	echo "(** -syntax camlp5r *)" > $@.NEW
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package camlp5.macro,camlp5.pr_r -syntax camlp5o $< >> $@.NEW
	mv $@.NEW pp_parsetree.ml

bootstrap-pp-parsetree: pp_parsetree.ORIG.ml
	echo "(** -syntax camlp5r *)" > $@.NEW
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.macro,camlp5.pr_r,pa_ppx_import,pa_ppx_deriving_plugins.std \
	-syntax camlp5o -ppopt -D -ppopt BOOTSTRAP \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-ppopt -sep -ppopt "\n" \
	$< >> $@.NEW
	mv $@.NEW  pp_parsetree.ml

refold-pp-parsetree: pp_parsetree.ml
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.pr_r,pa_ppx_undo_deriving \
	-syntax camlp5r \
	$< > $@.NEW
	mv $@.NEW  pp_parsetree.REFOLD.ml

camlp5.pa_ppx_base: $(TARGET)
	$(MKCAMLP5) -package fmt,rresult,camlp5.pa_r,camlp5.pr_r,pa_ppx_utils,pa_ppx_runtime,compiler-libs.common $(TARGET) -o $@

camlp5.pa_ppx_base.opt: $(TARGET:.cma=.cmxa)
	$(MKCAMLP5OPT) -package fmt,rresult,camlp5.pa_r,camlp5.pr_r,pa_ppx_utils,pa_ppx_runtime,compiler-libs.common $(TARGET:.cma=.cmxa) -o $@

install:: META
	mkdir -p $(DESTDIR)/lib
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_base -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.a) pa_ppx_base.cmx pa_ppx_base.cmi $(wildcard *.cmt*)

clean::
	rm -rf META camlp5.pa_ppx_base*

$(TARGET): pa_ppx_base.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $< -a -o $(TARGET)

$(TARGET:.cma=.cmxa): pa_ppx_base.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $< -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
