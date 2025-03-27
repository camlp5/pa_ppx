ifeq ($(OS),Windows_NT)
WD=$(shell cygpath --absolute --windows .)
else
WD=$(shell pwd)
endif
TOP=..
include $(TOP)/config/Makefile.sub

all:

test:: mdx-test

mdx-test:: error-handling.asciidoc.TEST
