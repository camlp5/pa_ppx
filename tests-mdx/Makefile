WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

all:

test:: mdx-test

mdx-test:: toplevel-error-handling.asciidoc.TEST shell-error-handling.asciidoc.TEST

%.asciidoc.TEST: %.asciidoc.corrected %.asciidoc
	cp $< $<.UNMODIFIED
	perl -n -i -e 'print unless (s,.*: added to search path.*,,)' $<
	perl -n -i -e 'print unless (s,.*: loaded.*,,)' $<
	perl -n -i -e 'print unless (m,Command line,)' $<
	perl -n -i -e 'print unless (m,Cannot read directory,)' $<
	perl -p -i -e 's,#<\|[^\|]+\|>,,' $<
	diff -Bwiu $^

shell-error-handling.asciidoc.TEST: | *.ml

clean::
	rm -f *.UNMODIFIED
