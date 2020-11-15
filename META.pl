#!/usr/bin/env perl

use strict ;
use IPC::System::Simple qw(systemx runx capturex $EXITVAL);
use String::ShellQuote ;
use File::Basename;

use Version ;

our %pkgmap = (
  'pa_ppx_utils' => 'pa_ppx.utils',
  'pa_ppx_testutils' => 'pa_ppx.testutils',
  'pa_ppx_runtime' => 'pa_ppx.runtime',
  'pa_ppx_runtime_fat' => 'pa_ppx.runtime_fat',
  'pa_ppx_deriving' => 'pa_ppx.deriving',
  'pa_ppx_deriving_plugins' => 'pa_ppx.deriving_plugins',
  'pa_ppx_dock' => 'pa_ppx.dock',
  'pa_ppx_base' => 'pa_ppx.base',
  'pa_ppx_import' => 'pa_ppx.import',
  'pa_ppx_unmatched_vala' => 'pa_ppx.unmatched_vala',
  'pa_ppx_hasrecons' => 'pa_ppx.hashrecons',
  'pa_ppx_here' => 'pa_ppx.here',
  'pa_ppx_here_original' => 'pa_ppx.here_original',
  'pa_ppx_assert' => 'pa_ppx.assert',
  'pa_ppx_inline_test' => 'pa_ppx.inline_test',
  'pa_ppx_expect_test' => 'pa_ppx.expect_test',
  'pa_ppx_params_runtime' => 'pa_ppx.params_runtime',
  
    );

{
  my $utilsmeta = indent(2, fixdeps(capturex("./util-lib/META.pl"))) ;
  my $testutilsmeta = indent(2, fixdeps(capturex("./testutils/META.pl"))) ;
  my $rtmeta = indent(2, fixdeps(capturex("./runtime/META.pl"))) ;
  my $rtfmeta = indent(2, fixdeps(capturex("./runtime_fat/META.pl"))) ;
  my $basemeta = indent(2, fixdeps(capturex("./base/META.pl"))) ;
  my $impmeta = indent(2, fixdeps(capturex("./pa_import/META.pl"))) ;
  my $uvmeta = indent(2, fixdeps(capturex("./pa_unmatched_vala/META.pl"))) ;
  my $hrmeta = indent(2, fixdeps(capturex("./pa_hashrecons/META.pl"))) ;
  my $dermeta = indent(2, fixdeps(capturex("./pa_deriving/META.pl"))) ;
  my $derpmeta = indent(2, fixdeps(capturex("./pa_deriving.plugins/META.pl"))) ;
  my $dockmeta = indent(2, fixdeps(capturex("./pa_dock/META.pl"))) ;
  my $heremeta = indent(2, fixdeps(capturex("./pa_here/META.pl"))) ;
  my $hereorigmeta = indent(2, fixdeps(capturex("./pa_here_original/META.pl"))) ;
  my $assertmeta = indent(2, fixdeps(capturex("./pa_assert/META.pl"))) ;
  my $inlmeta = indent(2, fixdeps(capturex("./pa_inline_test/META.pl"))) ;
  my $expmeta = indent(2, fixdeps(capturex("./pa_expect_test/META.pl"))) ;
  my $parmeta = indent(2, fixdeps(capturex("./params_runtime/META.pl"))) ;

  print <<"EOF";
version = "$Version::version"
description = "pa_ppx: camlp5-based PPX rewriters"

package "utils" (
$utilsmeta
)

package "testutils" (
$testutilsmeta
)

package "runtime" (
$rtmeta
)
package "runtime_fat" (
$rtfmeta
)
package "base" (
$basemeta
)
package "unmatched_vala" (
$uvmeta
)
package "hashrecons" (
$hrmeta
)
package "deriving" (
$dermeta
)
package "deriving_plugins" (
$derpmeta
)
package "dock" (
$dockmeta
)
package "import" (
$impmeta
)
package "here" (
$heremeta
)
package "here_original" (
$hereorigmeta
)
package "assert" (
$assertmeta
)
package "inline_test" (
$inlmeta
)
package "expect_test" (
$expmeta
)
package "params_runtime" (
$parmeta
)

EOF
}

sub fixdeps {
  my $txt = join('', @_) ;
  $txt =~ s,^(.*require.*)$, fix0($1) ,mge;
  return $txt ; 
}

sub fix0 {
  my $txt = shift ;
  $txt =~ s,"([^"]+)", '"'. fix($1) .'"' ,e;
  return $txt ;
}

sub fix {
  my $txt = shift ;
  my @l = split(/,/,$txt);
  my @ol = () ;
  foreach my $p (@l) {
    $p =~ s,^([^.]+), $pkgmap{$1} || $1 ,e ;
    push(@ol, $p);
  }
  $txt = join(',', @ol) ;
  return $txt ;
}

sub indent {
  my $n = shift ;
  my $txt = shift ;
  my $pfx = ' ' x $n ;
  $txt =~ s,^,$pfx,gm;
  return $txt ;
}
