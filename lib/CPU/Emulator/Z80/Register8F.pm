# $Id: Register8F.pm,v 1.1 2008/02/16 13:05:48 drhyde Exp $

package CPU::Emulator::Z80::Register8F;

use strict;
use warnings;

use vars qw($VERSION);

use base qw(CPU::Emulator::Z80::Register8);

$VERSION = '1.0';

=head1 NAME

CPU::Emulator::Z80::Register8F - the flags register for a Z80

=head1 DESCRIPTION

This class is a ...::Register8 with additional methods for
getting at the individual flag bits

=head1 METHODS

It has the same methods as its parent, with the following additions:

=head2 getX/setX/resetX

where X can be any of S Z H P N C 5 or 3.

getX takes no parameters and returns a true or false value depending
on the flag's status.

setX if called with no parameters sets the flag true.  If called with
a parameter it sets the flag true or false depending on the param's
value.

resetX takes no parameters and sets the flag false.

=cut

sub AUTOLOAD {
}

=head1 BUGS/WARNINGS/LIMITATIONS

None known.

=head1 AUTHOR, LICENCE and COPYRIGHT

Copyright 2008 David Cantrell E<lt>F<david@cantrell.org.uk>E<gt>

This module is free-as-in-speech software, and may be used,
distributed, and modified under the same terms as Perl itself.

=head1 CONSPIRACY

This module is also free-as-in-mason software.

=cut

1;
