# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

TARGET_PACKAGE=pa_ppx_deriving_plugins
PACKAGES := $(PACKAGES),pa_ppx_deriving,pa_ppx_base,pa_ppx_unmatched_vala

ML=\
	pa_deriving_fail.ml \
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
	pa_deriving_located_sexp.ml \
	pa_deriving_sexp.ml \
	pa_deriving_std.ml \

ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

all: $(CMO) $(CMX)
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

TARGET_INSTALL_FILES= $(FILES_TARGET_INSTALL_FILES)
install:: default_install

clean::
	rm -rf META camlp5*.pa_ppx_deriving_plugins*
