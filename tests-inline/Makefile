# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGEPREFIX=pa_ppx_

DESTDIR=

PACKAGES := $(PACKAGES),ounit2,fmt,pcre2,rresult,compiler-libs.common,sexplib0,str,threads.posix
OCAMLCFLAGS += -thread

TESTS= \
	test_inline_test.TEST failures.TEST order.TEST random_state.TEST \
	test_stderr.TEST test_output.TEST uncaught_exn.TEST unreachable.TEST \
	no_output_patterns_test.TEST \
	ppx_assert_test.TEST ppx_sexp_test.TEST

TEST_EXECUTABLES = $(TESTS:.TEST=.pa.byte) $(TESTS:.TEST=.ppx.byte)
all: $(CMO) test-executables

test-executables: $(TEST_EXECUTABLES)

test: $(TESTS)

PPXPACKAGES:=ppx_import
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.std
PPXPACKAGES:=$(PPXPACKAGES),ppx_here
PPXPACKAGES:=$(PPXPACKAGES),ppx_sexp_conv


PPXINLINEPACKAGES:=ppx_inline_test.runtime-lib
PPXINLINEPACKAGES:=$(PPXINLINEPACKAGES),ppx_assert
PPXINLINEPACKAGES:=$(PPXINLINEPACKAGES),$(PPXPACKAGES)
PPXINLINEPACKAGES:=$(PPXINLINEPACKAGES),ppx_expect
PPXINLINEPACKAGES:=$(PPXINLINEPACKAGES),expect_test_helpers
PPXINLINEPACKAGES:=$(PPXINLINEPACKAGES),ppx_inline_test.runner.lib
PPXINLINEPACKAGES:=$(PPXINLINEPACKAGES),ppx_expect.matcher
PPXINLINEPACKAGES:=$(PPXINLINEPACKAGES),ppx_expect.evaluator

PPXINLINEOPT = -ppxopt ppx_expect,-inline-test-lib  -ppxopt ppx_expect,foo

PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)import
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.std
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.sexp
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)here
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)assert

PAINLINEPACKAGES:=$(PAPACKAGES)
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),$(PACKAGEPREFIX)inline_test
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),$(PACKAGEPREFIX)expect_test
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),expect_test_helpers
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),ppx_expect.evaluator
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),ppx_inline_test.runtime-lib,ppx_inline_test.runner.lib

PAINLINEOPT=-ppopt -pa_inline_test-lib -ppopt foo

%.ppx.byte: %.ml
	$(NOT_OCAMLFIND) preprocess -package $(PACKAGES),$(PPXINLINEPACKAGES) $(PPXINLINEOPT) $< > $<.ppx.ppo
	$(OCAMLFIND) ocamlc $(DEBUG) -custom -thread $(WARNERR) -package $(PACKAGES),$(PPXINLINEPACKAGES) $(PPXINLINEOPT) -linkpkg $< ppx_inline_test_epilogue.ml -o $@

%.pa.byte: %.ml
	$(NOT_OCAMLFIND) preprocess -package $(PACKAGES),$(PAINLINEPACKAGES) -syntax camlp5o $(PAINLINEOPT) $< | $(OCAMLFIND) not-ocamlfind/papr_official.exe -binary-input -impl > $<.pappx.ppo
	$(OCAMLFIND) ocamlc $(DEBUG) -custom -thread $(WARNERR) -package $(PACKAGES),$(PAINLINEPACKAGES) -syntax camlp5o $(PAINLINEOPT) -linkpkg $< ppx_inline_test_epilogue.ml -o $@

ppx_sexp_test.ppx.byte: ppx_sexp_test.ml
	cppo ppx_sexp_test.ml > ppx_sexp_test.ppx.ml
	$(NOT_OCAMLFIND) preprocess -package $(PACKAGES),$(PPXINLINEPACKAGES) $(PPXINLINEOPT) ppx_sexp_test.ppx.ml > ppx_sexp_test.ppx.ml.ppo
	$(OCAMLFIND) ocamlc $(DEBUG) -custom -thread $(WARNERR) -package $(PACKAGES),$(PPXINLINEPACKAGES) $(PPXINLINEOPT) -linkpkg ppx_sexp_test.ppx.ml ppx_inline_test_epilogue.ml -o $@


ppx_sexp_test.pa.byte: ppx_sexp_test.ml
	cppo -D PAPPX ppx_sexp_test.ml > ppx_sexp_test.pappx.ml
	$(NOT_OCAMLFIND) preprocess -package $(PACKAGES),$(PAINLINEPACKAGES),camlp5.pr_o -ppopt -sep -ppopt ";;" -syntax camlp5o $(PAINLINEOPT) ppx_sexp_test.pappx.ml >  ppx_sexp_test.pappx.ml.ppo
	$(OCAMLFIND) ocamlc $(DEBUG) -custom -thread $(WARNERR) -package $(PACKAGES),$(PAINLINEPACKAGES),$(PACKAGEPREFIX)runtime_fat -syntax camlp5o $(PAINLINEOPT) -linkpkg ppx_sexp_test.pappx.ml ppx_inline_test_epilogue.ml -o $@

%.TEST: %.ppx.byte %.pa.byte
	./test-runner $^ foo $(VERBOSE)

.SUFFIXES: .TEST

clean::
	rm -f *.corrected *.ppo

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES),$(PAINLINEPACKAGES) | grep local-install)
$(TEST_EXECUTABLES): $(EXTERNAL)

depend::
	echo "$(TEST_EXECUTABLES): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5o test_*.ml >> .depend.NEW
	mv .depend.NEW .depend

-include .depend
