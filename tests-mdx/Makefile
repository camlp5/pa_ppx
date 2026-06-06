WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

all:

test:: mdx-test

mdx-test:: error-handling.asciidoc.TEST

%.asciidoc.TEST: %.asciidoc.corrected %.asciidoc
	cp $< $<.UNMODIFIED
	perl -n -i -e 'print unless (s,.*: added to search path.*,,)' $<
	perl -n -i -e 'print unless (s,.*: loaded.*,,)' $<
	perl -p -i -e 's,#<\|[^\|]+\|>,,' $<
	diff -Bwiu $^

error-handling.asciidoc.TEST: | stuff.cmo

clean::
	rm -f *.UNMODIFIED
