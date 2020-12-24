#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# test utilities for "pa_ppx"
requires = "unix,camlp5,oUnit,fmt,pcre,rresult,compiler-libs.common,yojson,sexplib0"
version = "$Version::version"
description = "pa_ppx test utilities"

archive(byte) = "pa_ppx_testutils.cma"
archive(native) = "pa_ppx_testutils.cmxa"

EOF
