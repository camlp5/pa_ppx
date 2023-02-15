# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),pa_ppx_deriving,pa_ppx_base,pa_ppx_unmatched_vala,camlp5.parser_quotations.link

ML=\
	pa_deriving_params.ml \
	pa_deriving_show.ml \
	pa_deriving_eq.ml \
	pa_deriving_ord.ml \
	pa_deriving_enum.ml \
	pa_deriving_iter.ml \
	pa_deriving_map.ml \
	pa_deriving_fold.ml \
	pa_deriving_make.ml \
	pa_deriving_yojson.ml \
	pa_deriving_sexp.ml \

ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)
BATCHTOP=camlp5o.pa_ppx_deriving_plugins camlp5o.pa_ppx_deriving_plugins.opt \
	camlp5r.pa_ppx_deriving_plugins camlp5r.pa_ppx_deriving_plugins.opt

all: $(CMO) $(BATCHTOP)
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

KITS=$(CMO)

camlp5r.pa_ppx_deriving_plugins: $(KITS)
	$(LAUNCH) $(MKCAMLP5) -package camlp5.pa_r,camlp5.pr_r,pa_ppx_deriving $(KITS) -o $@

camlp5r.pa_ppx_deriving_plugins.opt: $(KITS:.cmo=.cmx)
	$(LAUNCH) $(MKCAMLP5OPT) -package camlp5.pa_r,camlp5.pr_r,pa_ppx_deriving $(KITS:.cmo=.cmx) -o $@

camlp5o.pa_ppx_deriving_plugins: $(KITS)
	$(LAUNCH) $(MKCAMLP5) -package camlp5.pa_o,camlp5.pr_o,pa_ppx_deriving $(KITS) -o $@

camlp5o.pa_ppx_deriving_plugins.opt: $(KITS:.cmo=.cmx)
	$(LAUNCH) $(MKCAMLP5OPT) -package camlp5.pa_o,camlp5.pr_o,pa_ppx_deriving $(KITS:.cmo=.cmx) -o $@

install:: $(BATCHTOP) META
	mkdir -p $(DESTDIR)/lib
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_deriving_plugins -destdir $(DESTDIR)/lib META $(CMO) $(CMX) $(MLI:.mli=.cmi) $(KITS:.cmo=.o) $(wildcard *.cmt*) $(BATCHTOP)

clean::
	rm -rf META camlp5*.pa_ppx_deriving_plugins*
