#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx" preprocessor:
requires = "camlp5,fmt,pa_ppx_base"
version = "$Version::version"
description = "pa_ppx pa_here support"

# For linking
package "link" (
requires = "camlp5,fmt,pa_ppx_base.link"
archive(byte) = "pa_ppx_here.cma"
archive(native) = "pa_ppx_here.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_ppx_here.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,pa_ppx_base"
  archive(syntax,preprocessor,-native) = "pa_ppx_here.cma"
  archive(syntax,preprocessor,native) = "pa_ppx_here.cmxa"

EOF
