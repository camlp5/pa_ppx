WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

all:

test:: mdx-test

mdx-test:: toplevel-error-handling.asciidoc.TEST shell-error-handling.asciidoc.TEST

shell-error-handling.asciidoc.TEST: | fixtures/*.ml

%.asciidoc.TEST: %.asciidoc.corrected %.asciidoc
	cp $< $<.UNMODIFIED
	./clean-ocaml-mdx-output $< > $<.NEW && mv $<.NEW $<
	diff -Bwiu $^

clean::
	$(RM) -f fixtures/*.cm* *.cm* *.byte *.log *.cache *.ppo tools/*.byte tools/*.opt link* *.NEW *.corrected *.corrected.UNMODIFIED
