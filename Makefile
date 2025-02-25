# Makefile,v
# Copyright (c) INRIA 2007-2017

TOP=.
include $(TOP)/config/Makefile.top

WD=$(shell pwd)
DESTDIR=
RM=rm

PRESYSDIRS= util-lib runtime runtime_fat testutils base pa_unmatched_vala \
	pa_dock pa_fail pa_here pa_here_original pa_undo_deriving pa_assert \
	pa_inline_test pa_expect_test pa_hashrecons \
	pa_deriving pa_deriving.plugins pa_import

SYSDIRS= $(PRESYSDIRS) \
	located_sexp params_runtime

TESTDIRS= tests-ounit2 tests-mdx

EXTRATESTDIRS = our-tests-inline tests-inline tests-expect

PACKAGES := pa_ppx_utils
PACKAGES := $(PACKAGES),pa_ppx_base
PACKAGES := $(PACKAGES),pa_ppx_unmatched_vala
PACKAGES := $(PACKAGES),pa_ppx_here
PACKAGES := $(PACKAGES),pa_ppx_assert
PACKAGES := $(PACKAGES),pa_ppx_inline_test
PACKAGES := $(PACKAGES),pa_ppx_expect_test
PACKAGES := $(PACKAGES),pa_ppx_deriving
PACKAGES := $(PACKAGES),pa_ppx_deriving_plugins.std
PACKAGES := $(PACKAGES),pa_ppx_deriving_plugins.yojson
PACKAGES := $(PACKAGES),pa_ppx_hashrecons
PACKAGES := $(PACKAGES),pa_ppx_import

BATCHTOP = camlp5o.pa_ppx camlp5o.pa_ppx.opt \
	camlp5r.pa_ppx camlp5r.pa_ppx.opt

setup: get-generated

all: sys
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) all; cd ..; done

sys: plugins $(BATCHTOP)

$(BATCHTOP): plugins

plugins:
	set -e; for i in $(SYSDIRS); do cd $$i; $(MAKE) all; cd ..; done

presys:
	set -e; for i in $(PRESYSDIRS); do cd $$i; $(MAKE) all; cd ..; done

doc: all
	set -e; for i in $(SYSDIRS); do cd $$i; $(MAKE) doc; cd ..; done
	rm -rf docs
	tools/make-docs pa_ppx docs
	make -C doc html

test-everything: all
	set -e; for i in $(TESTDIRS) $(EXTRATESTDIRS); do cd $$i; $(MAKE) test; cd ..; done

test: all
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) test; cd ..; done

make full-bootstrap:
	$(MAKE) clean
	$(MAKE) initialize
	$(MAKE) presys
	$(MAKE) bootstrap
	$(MAKE) save-generated

bootstrap:
	$(MAKE) -C runtime bootstrap-exn bootstrap-exn-i
	$(MAKE) -C runtime_fat bootstrap-exn bootstrap-exn-i
	$(MAKE) -C base bootstrap-pp-MLast bootstrap-pp-MLast-i bootstrap-pp-parsetree bootstrap-pp-parsetree-i

initialize:
	$(MAKE) -C runtime initialize-exn initialize-exn-i
	$(MAKE) -C runtime_fat initialize-exn initialize-exn-i
	$(MAKE) -C base initialize-pp-MLast initialize-pp-MLast-i initialize-pp-parsetree initialize-pp-parsetree-i

ocamlGENERATED=base/pp_parsetree.ml base/pp_parsetree.mli \
	runtime/exceptions.ml runtime/exceptions.mli \
	runtime_fat/exceptions.ml runtime_fat/exceptions.mli

camlp5GENERATED=base/pp_MLast.ml base/pp_MLast.mli

ocamlVERSION=$(shell ocamlc --version)
camlp5VERSION=$(shell camlp5 -version)

genocamlVERSION=$(shell tools/find_gensrc.pl ocaml $(ocamlVERSION))
gencamlp5VERSION=$(shell tools/find_gensrc.pl camlp5 $(camlp5VERSION))

save-generated:
	mkdir -p generated_src/$(ocamlVERSION)
	tar -cf - $(ocamlGENERATED) | tar -C generated_src/$(ocamlVERSION) -xvBf -
	mkdir -p generated_src/$(camlp5VERSION)
	tar -cf - $(camlp5GENERATED) | tar -C generated_src/$(camlp5VERSION) -xvBf -

get-generated: generated_src/$(genocamlVERSION) generated_src/$(gencamlp5VERSION)
	tar -C generated_src/$(genocamlVERSION) -cf - . | tar -xvBf -
	tar -C generated_src/$(gencamlp5VERSION) -cf - . | tar -xvBf -

camlp5r.pa_ppx:
	$(LAUNCH) $(MKCAMLP5) -package camlp5.pa_r,camlp5.pr_r,$(PACKAGES) -o $@

camlp5r.pa_ppx.opt:
	$(LAUNCH) $(MKCAMLP5OPT) -package camlp5.pa_r,camlp5.pr_r,$(PACKAGES) -o $@

camlp5o.pa_ppx:
	$(LAUNCH) $(MKCAMLP5) -package camlp5.pa_o,camlp5.pr_o,$(PACKAGES) -o $@

camlp5o.pa_ppx.opt:
	$(LAUNCH) $(MKCAMLP5OPT) -package camlp5.pa_o,camlp5.pr_o,$(PACKAGES) -o $@

META.OLD: META.pl META
	./META.pl > META.OLD
	diff -Bwiu META.OLD META

META: sys
	echo 'version = "0.09"' > META
	echo 'description = "pa_ppx: camlp5-based PPX rewriters"' >> META
	$(JOINMETA) \
		-rewrite pa_ppx_params_runtime:pa_ppx.params_runtime \
		-wrap-subdir params_runtime:params_runtime \
		-rewrite pa_ppx_expect_test:pa_ppx.expect_test \
		-wrap-subdir expect_test:pa_expect_test \
		-rewrite pa_ppx_inline_test:pa_ppx.inline_test \
		-wrap-subdir inline_test:pa_inline_test \
		-rewrite pa_ppx_assert:pa_ppx.assert \
		-wrap-subdir assert:pa_assert \
		-rewrite pa_ppx_here_original:pa_ppx.here_original \
		-wrap-subdir here_original:pa_here_original \
		-rewrite pa_ppx_here:pa_ppx.here \
		-wrap-subdir here:pa_here \
		-rewrite pa_ppx_import:pa_ppx.import \
		-wrap-subdir import:pa_import \
		-rewrite pa_ppx_dock:pa_ppx.dock \
		-wrap-subdir dock:pa_dock \
		-rewrite pa_ppx_deriving_plugins:pa_ppx.deriving_plugins \
		-wrap-subdir deriving_plugins:pa_deriving.plugins \
		-rewrite pa_ppx_deriving:pa_ppx.deriving \
		-wrap-subdir deriving:pa_deriving \
		-rewrite pa_ppx_hashrecons:pa_ppx.hashrecons \
		-wrap-subdir hashrecons:pa_hashrecons \
		-rewrite pa_ppx_unmatched_vala:pa_ppx.unmatched_vala \
		-wrap-subdir unmatched_vala:pa_unmatched_vala \
		-rewrite pa_ppx_base:pa_ppx.base \
		-wrap-subdir base:base \
		-rewrite pa_ppx_runtime_fat:pa_ppx.runtime_fat \
		-wrap-subdir runtime_fat:runtime_fat \
		-rewrite pa_ppx_runtime:pa_ppx.runtime \
		-wrap-subdir runtime:runtime \
		-rewrite pa_ppx_testutils:pa_ppx.testutils \
		-wrap-subdir testutils:testutils \
		-rewrite pa_ppx_utils:pa_ppx.utils \
		-wrap-subdir utils:util-lib \
		-rewrite pa_ppx_located_sexp:pa_ppx.located_sexp \
		-wrap-subdir located_sexp:located_sexp \
		>> META

install: sys META
	$(OCAMLFIND) remove pa_ppx || true
	$(OCAMLFIND) install pa_ppx META local-install/lib/*/*.*

uninstall:
	$(OCAMLFIND) remove pa_ppx || true

clean::
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) clean; cd ..; done
	rm -rf docs local-install $(BATCHTOP) META

depend::
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) depend; cd ..; done
