# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGEPREFIX=pa_ppx_

DESTDIR=

TESTRUNNER=$(TOP)/tests-inline/test-runner
PACKAGES := $(PACKAGES),ounit2,fmt,bos,pcre2,rresult,compiler-libs.common,sexplib0,str,threads.posix
OCAMLCFLAGS += -thread

ORIGINAL_TESTS= \
	test_doc_comments_official_original.TEST \
	test_doc_comments_pa_dock_original.TEST \
	bigass_doc_comments_official_original.TEST \
	bigass_doc_comments_pa_dock_original.TEST \

REVISED_TESTS= \
	bigass_doc_comments_revised.TEST \
	test_doc_comments_revised.TEST \

TEST_EXECUTABLES = $(ORIGINAL_TESTS:.TEST=.pa.byte) $(REVISED_TESTS:.TEST=.pa.byte)

all: $(CMO) test-executables

test-executables: $(TEST_EXECUTABLES)

all-test: test
test: $(ORIGINAL_TESTS) $(REVISED_TESTS)

PAPACKAGES:=$(PACKAGEPREFIX)import
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.std
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.sexp
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)here
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)assert
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)dock.link
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)testutils

PAINLINEPACKAGES:=$(PAPACKAGES)
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),$(PACKAGEPREFIX)inline_test
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),$(PACKAGEPREFIX)expect_test
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),expect_test_helpers
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),ppx_expect.matcher
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),ppx_expect.collector
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),ppx_expect.evaluator
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),ppx_inline_test.runtime-lib,ppx_inline_test.runner.lib

PAINLINEOPT=-ppopt -pa_inline_test-lib -ppopt foo

test_doc_comments_official_original.ml: test_doc_comments_original.ML
	cppo -n -D OFFICIAL $< > $@

test_doc_comments_pa_dock_original.ml: test_doc_comments_original.ML
	cppo -n -D PA_DOCK $< > $@

bigass_doc_comments_official_original.ml: bigass_doc_comments_original.ML
	cppo -n -D OFFICIAL $< > $@

bigass_doc_comments_pa_dock_original.ml: bigass_doc_comments_original.ML
	cppo -n -D PA_DOCK $< > $@

%_original.pa.byte: %_original.ml dock_test_utils.ml
	$(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.pa_o.link,$(PAINLINEPACKAGES) -syntax camlp5o $(PAINLINEOPT) $< | $(OCAMLFIND) not-ocamlfind/papr_official.exe -binary-input -impl > $<.pappx.ppo
	$(OCAMLFIND) ocamlc $(DEBUG) -custom -thread $(WARNERR) -package $(PACKAGES),camlp5.pa_o.link,$(PAINLINEPACKAGES) -syntax camlp5o $(PAINLINEOPT) -I ../tests-ounit2 -linkpkg dock_test_utils.ml $< ppx_inline_test_epilogue.ml -o $@

%_revised.pa.byte: %_revised.ml dock_test_utils.ml
	$(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.pa_r.link,$(PAINLINEPACKAGES) -syntax camlp5o $(PAINLINEOPT) $< | $(OCAMLFIND) not-ocamlfind/papr_official.exe -binary-input -impl > $<.pappx.ppo
	$(OCAMLFIND) ocamlc $(DEBUG) -custom -thread $(WARNERR) -package $(PACKAGES),camlp5.pa_r.link,$(PAINLINEPACKAGES) -syntax camlp5o $(PAINLINEOPT) -I ../tests-ounit2 -linkpkg dock_test_utils.ml $< ppx_inline_test_epilogue.ml -o $@

%.TEST: %.pa.byte
	$(TESTRUNNER) $^ foo $(VERBOSE)

.SUFFIXES: .TEST

clean::
	rm -f *.corrected *.ppo *_doc_comments_official.ml *_doc_comments_pa_dock.ml

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES),$(PAINLINEPACKAGES) | grep local-install)
$(TEST_EXECUTABLES): $(EXTERNAL)

depend::
	echo "$(TEST_EXECUTABLES): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5o test_*.ml >> .depend.NEW
	mv .depend.NEW .depend

-include .depend
