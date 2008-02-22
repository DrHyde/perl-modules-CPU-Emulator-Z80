# $Id: ALU.pm,v 1.6 2008/02/22 20:29:21 drhyde Exp $

package CPU::Emulator::Z80::ALU;

use strict;
use warnings;

use base qw(Exporter);
use vars qw(@EXPORT);

{   # find and export ALU_*
    no strict 'refs';
    while(my($k, $v) = each(%{__PACKAGE__.'::'})) {
        push @EXPORT, $k if(defined(&{$v}) && $k =~ /^ALU_/);
    }
}

=head1 NAME

CPU::Emulator::Z80::ALU

=head1 DESCRIPTION

This mix-in provides functions for addition and subtraction on a
Z80, settings flags and doing twos-complement jibber-jabber.  It
provides both 8- and 16-bit versions of all its functions.

=head1 FUNCTIONS

All functions are
exported.  They all take a
reference to the Flags register as the first parameter in addition
to the parameters listed:
listed:

=head2 ALU_add8/ALU_add16

Takes two 8/16-bit values and returns their 8/16-bit sum.

=cut

sub ALU_add8 {
    my($flags, $op1, $op2) = @_;
    my $lownybble = ($op1 & 0x0F) + ($op2 & 0x0F);
    # my $result = ($lownybble + ($op1 & 0xF0) + ($op2 & 0xF0)) & 0xFF;
    my $result = $op1 + $op2;
    $flags->setC($result & 0x100);
    $result &= 0xFF;
    $flags->resetN();
    $flags->setZ($result == 0);
    $flags->set3($result & 0b1000);
    $flags->setH($lownybble & 0b10000);
    $flags->set5($result & 0b100000);
    $flags->setS($result & 0b10000000);
    return $result;
}
sub ALU_add16 {
    my($flags, $op1, $op2) = @_;
    my $low11 = ($op1 & 0x0FFF) + ($op2 & 0x0FFF);
    my $result = $op1 + $op2;
    $flags->setC($result & 0x10000);
    $result &= 0xFFFF;
    $flags->resetN();
    $flags->set3($result & 0x800);
    $flags->setH($low11 & 0x1000);
    $flags->set5($result & 0x2000);
    return $result;
}

=head2 ALU_sub8/ALU_sub16

Takes two 8/16-bit values and subtracts the second from the first,
returning the result.

=cut

sub ALU_sub8 {
    my($flags, $op1, $op2) = @_;
    my $result = ($op1 - $op2) & 0xFF;
    $flags->setN();
    $flags->setZ($result == 0);
    $flags->setC($op2 > $op1);
    $flags->set3($result & 0b1000);
    $flags->setH(($op2 & 0x0F) > ($op1 & 0x0F));
    $flags->set5($result & 0b100000);
    $flags->setS($result & 0b10000000);
    return $result;
}

=head2 ALU_inc8/ALU/dec8

Take a single 8-bit value and incremement/decrement it, returning
the result.  They're just wrappers around add8/sub8 that preserve
the C flag.

=cut

sub ALU_dec8 {
    my($flags, $op) = @_;
    my $c = $flags->getC();
    my $s = $op & 0x80;
    my $result = ALU_sub8($flags, $op, 1);
    $flags->setC($c);
    $flags->setP($s && !($result & 0x80));
    return $result;
}
sub ALU_inc8 {
    my($flags, $op) = @_;
    my $c = $flags->getC();
    my $s = $op & 0x80;
    my $result = ALU_add8($flags, $op, 1);
    $flags->setC($c);
    $flags->setP(!$s && ($result & 0x80));
    return $result;
}

=head1 BUGS/WARNINGS/LIMITATIONS

None known

=head1 FEEDBACK

I welcome feedback about my code, including constructive criticism and bug reports. The best bug reports include files that I can add to the test suite, which fail with the current code in CVS and will pass once I've fixed the bug.

Feature requests are far more likely to get implemented if you submit a patch yourself.

=head1 CVS

L<http://drhyde.cvs.sourceforge.net/drhyde/perlmodules/CPU-Emulator-Z80/>

=head1 AUTHOR, LICENCE and COPYRIGHT

Copyright 2008 David Cantrell E<lt>david@cantrell.org.ukE<gt>

This module is free-as-in-speech software and may be used, distributed and modified under the same terms as Perl itself.

=head1 CONSPIRACY

This module is also free-as-in-mason software.

=cut

1;
