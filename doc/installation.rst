.. _installation:

============
Installation
============

The Quick Way
-------------

The rest of this document explains how to install ``pa_ppx`` the hard
way.  Here, we'll explain the easy way, which is to use opam:

  opam install camlp5 pa_ppx

That's it.  You'll want to get the sources, so you can look at the tests (as samples).

For those who wish to do things "manually" or hack on camlp5/pa_ppx,
there are somewhat strict version-constraints that limit which
versions of ``camlp5`` and ``pa_ppx`` are compatible with each version
of OCaml (due to the fact that OCaml's AST changes from
version-to-version).  This is why it is easiest to just install via
opam, but we describe below the steps of manual installation.  Consult
the opam files to see what the version-requirements are for
prerequisites.

Prerequisites
-------------

1. Ocaml

To install ``pa_ppx`` you'll need Ocaml (version >= 4.10.0) and Camlp5
(version >= 8.00).

You can install both Ocaml and Camlp5 in the usual way, typically via
``opam``.

2. ``ocamlfind`` and ``not-ocamlfind``

``ocamlfind`` is unavoidable, and I've written a little wrapper, `not-ocamlfind <https://github.com/chetmurthy/not-ocamlfind>`_ that provides some useful extra commands, which I'll refer to throughout this documentation::

  opam install ocamlfind not-ocamlfind

3n. Various other ocaml packages::

``pa_ppx`` has tests to verify that things work with both ``pa_ppx``
and the standard PPX rewriters, so there are a ton of extra
packages needed::

  opam install rresult fmt ounit2 pcre core_kernel ppx_deriving \
       ppx_deriving_yojson ppx_import ppx_here sexplib0 sexplib bos \
       expect_test_helpers ppx_expect

NOTE WELL that some of these won't install
(e.g. ``expect_test_helpers`` b/c they're often not in-sync with other
PPX rewriters).

Building and Installing
-----------------------

Once having installed camlp5, you can build and install ``pa_ppx`` via::

  make && make install

and you can run (copious) tests (some of which fail -- I'm working on that!)::

  make -k test

.. container:: trailer
