# $Id: ALU.pm,v 1.3 2008/02/19 17:28:51 drhyde Exp $

package CPU::Emulator::Z80::ALU;

use strict;
use warnings;

=head1 NAME

CPU::Emulator::Z80::ALU

=head1 DESCRIPTION

This mix-in provides methods for addition and subtraction on a
Z80, settings flags and doing twos-complement jibber-jabber.  It
provides both 8- and 16-bit versions of all its methods.

=head1 METHODS

All methods are available in 8- and 16-bit versions by appending
the appropriate number:

=head2 add8/add16

Takes two 8/16-bit values and returns their 8/16-bit sum.

=cut

sub add8 {
    my($self, $op1, $op2) = @_;
    my $lownybble = ($op1 & 0x0F) + ($op2 & 0x0F);
    $self->register('F')->setH($lownybble & 0x10); # half-carry
    my $result = $lownybble + ($op1 & 0xF0) + ($op2 & 0xF0);
    $self->register('F')->setC($result & 0x100);
    return $result & 0xFF
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
