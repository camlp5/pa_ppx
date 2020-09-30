========================================================================
Incompatibilities Between `pa_ppx` PPX Rewriters and the "standard" ones
========================================================================

As much as possible, we've tried to ensure that `pa_ppx`
implementations of PPX rewriters stay faithful to the behaviour of the
ones we've reverse-engineered.  There are some places where this was
either not easy, or where deviation was valuable, and I'll try to
document them here.

`pa_ppx.import`
===============

`pa_ppx.import` has a few incompatibilites with `ppx_import` (as we
understand its behaviour) and a bunch of extra features.

Incompatibilities
-----------------

Automatic renaming during import of recursive types
###################################################

When `ppx_import` imports a type, viz.::

   type longident = [%import: Longident.t]

it automatically renames the instances of `t` in the body of the
type-definition.  But if the *intent* of the import was to reuse that
type-definition and modify it somehow (for instance, to introduce an
intermediary type for the purpose of hash-consing) then this is
defeated by this automatic renaming.  Furthermore, there is an
*extension* in `pa_ppx.import` to import entire typedecl-groups, that
can be used to get this same behaviour::

  [%%import: Longident.t]

though admittedly, it doesn't rename the members of the group.

Here's an example of using this behaviour::

  type lam_node = [%import: Lam.lam]
  and lam = lam_node Hashcons.hash_consed

Specifying Search-path for Import
#################################

When importing a type, `ppx_import` has some sort of interesting
search-path behaviour, that I glanced at and .... thought was "not
sufficiently obvious" as well as being impenetrable.  Instead,
`pa_ppx.import` specifies the search-path with command-line arguments.
There are three:

`-pa_import-package`: this specifies a (comma-separated) list of
findlib packages, which are processed to determine the directories
that get added to the search-path.

`-pa_import-predicates`: this specifies a (comma-separated) list of
findlib predicates, which are added to the set of predicated evaluated
with the packages, to determine the directories that get added to the
search-path.

`-pa_import-I`: this specifies a directory that is added to the search-path

A typical usage for specifying these is::

  ocamlfind ocamlc  -ppopt -pa_import-I -ppopt . \
                    -package camlp5,pa_ppx.import \
		    -syntax camlp5o -c lam.ml

which adds the current directory to the search-path.  Or::
  

  ocamlfind ocamlc  -ppopt -pa_import-package -ppopt compiler-libs.common,camlp5 \
                    -package camlp5,compiler-libs.common,pa_ppx.import \
		    -syntax camlp5o -c lam.ml

which adds the directories where the packages `compiler-libs.common`
and `camlp5` are stored, to the search-path.  Note well that the list
of packages specified to the compiler (the `-package`) option and the
list specified to `pa_ppx.import` (the `-pa_import-package` option)
are entirely distinct.

.. container:: trailer
