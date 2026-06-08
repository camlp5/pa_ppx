WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub
include $(TOP)/config/Makefile.mdx

all test:: mdx-test

mdx-test:: toplevel-error-handling.asciidoc.TEST shell-error-handling.asciidoc.TEST

shell-error-handling.asciidoc.TEST: | fixtures/*.ml
