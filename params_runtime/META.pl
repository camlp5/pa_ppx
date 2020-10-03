#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx" preprocessor:
requires = "rresult,fmt,ppx_deriving.runtime,pa_ppx_runtime,uint"
version = "$Version::version"
description = "pa_ppx params runtime support"

# For linking
archive(byte) = "pa_ppx_params.cma"
archive(native) = "pa_ppx_params.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_ppx_params.cma"

EOF
