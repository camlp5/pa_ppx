# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

export PACKAGEPREFIX=pa_ppx_

DESTDIR=

export PACKAGES := $(PACKAGES),oUnit,fmt,str,pcre2,result,rresult,compiler-libs.common,yojson,sexplib,sexplib0

PPXPACKAGES:=ppx_import
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.show
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.eq
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.ord
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.enum
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.iter
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.map
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.fold
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.make
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving_yojson
PPXPACKAGES:=$(PPXPACKAGES),ppx_sexp_conv
export PPXPACKAGES:=$(PPXPACKAGES),ppx_here

PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)utils
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)dock.link
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.std
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.yojson
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.sexp
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.located_sexp
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)runtime
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)params_runtime
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)runtime_fat
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)located_sexp
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)hashrecons
export PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)testutils

ALLSRC=$(shell ls *.pappx.ml hCPassthru.ml test_passthru_hashrecons.ml test_import_camlp5.ml test_import0.ml test_self_import.ml test_self_import.mli)
ALLPACKAGES = $(PACKAGES),$(PAPACKAGES),$(PPXPACKAGES)

ML=papr_util.ml hCPassthru.ml

TESTS= \
	test_utils.byte \
	test_ppx_deriving.byte \
	test_ppx_deriving.ppx.byte \
	test_deriving_show.byte \
	test_deriving_show.ppx.byte \
	test_deriving_eq.byte \
	test_deriving_eq.ppx.byte \
	test_deriving_ord.byte \
	test_deriving_ord.ppx.byte \
	test_deriving_enum.byte \
	test_deriving_enum.ppx.byte \
	test_deriving_iter.byte \
	test_deriving_iter.ppx.byte \
	test_deriving_map.byte \
	test_deriving_map.ppx.byte \
	test_deriving_fold.byte \
	test_deriving_fold.ppx.byte \
	test_deriving_make.byte \
	test_deriving_make.ppx.byte \
	test_deriving_params.byte \
	test_deriving_sexp.byte \
	test_deriving_located_sexp.byte \
	test_here.byte \
	test_here_original.byte \
	test_here.ppx.byte \
	test_import0.byte \
	test_import_camlp5.byte \
	test_ppx_import.byte \
	test_ppx_import.ppx.byte \
	test_hashrecons.byte \
	test_passthru_hashrecons.byte \
	test_ppx_yojson.byte \
	test_ppx_yojson.ppx.byte \
	test_extensible_variants.byte \
	test_comment_lexer.byte \
	o_top_test.byte \
	test_linked_expander.byte

all: $(CMO) $(TESTS)

test: $(TESTS:.byte=)

% : %.byte
	mkdir -p _build && ./$<

.SUFFIXES: .ML
.SECONDARY:

test_%.ml.pappx.ml: test_%.ML
	cppo -D PAPPX $< > $@

test_%.ml.ppx.ml: test_%.ML
	cppo -U PAPPX $< > $@

test_%.byte: test_%.ml.pappx.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),$(PAPACKAGES) \
	-linkpkg -syntax camlp5o $< -o $@

test_%.ppx.byte: stuff.cmo test_self_import.cmo test_%.ml.ppx.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -custom $(WARNERR) -package $(PACKAGES),oUnit,$(PPXPACKAGES) -linkpkg $^ -o $@

test_ppx_yojson.ppx.byte: test_ppx_yojson.ML stuff.cmo test_self_import.cmo
	cppo test_ppx_yojson.ML > test_ppx_yojson.ppx.ml
	$(NOT_OCAMLFIND) preprocess -package $(PACKAGES),oUnit,$(PPXPACKAGES),ppx_sexp_conv test_ppx_yojson.ppx.ml >  test_ppx_yojson.ppx.ml.ppo
	$(OCAMLFIND) ocamlc $(DEBUG) -custom $(WARNERR) -package $(PACKAGES),oUnit,$(PPXPACKAGES),ppx_sexp_conv -linkpkg stuff.cmo test_self_import.cmo test_ppx_yojson.ppx.ml -o $@

test_passthru_hashrecons.byte: hCPassthru.cmo test_passthru_hashrecons.ml
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),$(PAPACKAGES) \
	-linkpkg -syntax camlp5o $< -o $@

test_deriving_params.byte: test_deriving_params.ml.pappx.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),$(PAPACKAGES),camlp5.pa_r.link \
	-linkpkg -syntax camlp5o $< -o $@

test_comment_lexer.byte: test_comment_lexer.ml.pappx.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),$(PAPACKAGES),camlp5.pa_o.link \
	-linkpkg -syntax camlp5o $< -o $@

test_here_original.ml: test_here.ml
	cp $< $@
	perl -p -i -e 's,test_here.ml.pappx.ml,test_here_original.ml.pappx.ml,' test_here_original.ml
	perl -p -i -e 's,pos_bol = 141,pos_bol = 159,' test_here_original.ml
	perl -p -i -e 's,pos_cnum = 152,pos_cnum = 170,' test_here_original.ml

test_import0.cmo: stuff.cmo test_self_import.cmi
test_import0.byte: test_import0.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),$(PACKAGEPREFIX)deriving_plugins.show,$(PACKAGEPREFIX)import,camlp5.pr_o,$(PACKAGEPREFIX)testutils \
	-linkall -linkpkg -syntax camlp5o $< -o $@

test_import_camlp5.cmo: stuff.cmo test_self_import.cmi
test_import_camlp5.byte: test_import_camlp5.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),$(PACKAGEPREFIX)import,$(PACKAGEPREFIX)deriving_plugins.show,$(PACKAGEPREFIX)testutils \
	-linkpkg -syntax camlp5o $< -o $@

test_linked_expander.byte: test_linked_expander.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package camlp5.pa_o.link,camlp5.pr_o.link,$(PACKAGES),$(PACKAGEPREFIX)base.link,$(PACKAGEPREFIX)here.link,$(PACKAGEPREFIX)testutils \
	-linkpkg -linkall -syntax camlp5o $< -o $@

test_ppx_import.byte: stuff.cmo test_self_import.cmo test_ppx_import.ml.pappx.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),$(PAPACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

o_top_test:: o_top_test.byte
	mkdir -p _build && ./o_top_test.byte

o_top_test.byte: o_top_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) -package camlp5,oUnit,fmt,compiler-libs.toplevel,camlp5.pa_op.link,camlp5.pr_official.link,camlp5.toploop.link,pa_ppx_base.link,pa_ppx_here.link,$(PACKAGEPREFIX)testutils -syntax camlp5r -linkpkg -impl o_top_test.ml -o o_top_test.byte

clean::
	rm -f *.corrected *.pappx.ml *.ppx.ml *.ppo.ml

test_self_import.cmo: test_self_import.ml test_self_import.cmi
