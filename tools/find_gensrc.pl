#!/usr/bin/env perl

use strict;
use Data::Dumper ;

{
  my $kind = $ARGV[0] ;
  my $oversion = $ARGV[1] ;
  die "mal-formatted $kind version detected: please report to maintainer with this output: ".Dumper(\@ARGV)
    unless $oversion =~ s,^(\d+(?:\.\d+)+)(?:[+~-].*)?$,$1, ;

  if (-d "generated_src/$oversion") {
    print "$oversion\n";
    exit ;
  }

  print STDERR "WARNING: missing directory generated_src/$oversion\n" if (! -d "generated_src/$oversion") ;

  my $oroot = $oversion ; $oroot =~ s,\.?[0-9]+$,, ;
  my @versions = <generated_src/${oroot}*> ;
  @versions = sort @versions ;
  $oversion = $versions[-1] ;
  $oversion =~ s,.*/,,;
  if (-d "generated_src/$oversion") {
    print STDERR "WARNING: FALLING BACK to saved info for $kind version $oversion; please report to maintainer\n" ;
    print "$oversion\n";
    exit ;
  }


  # let the configure script fail in the usual way
  print "$oversion\n";
}
