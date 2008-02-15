# $Id: Register.pm,v 1.1 2008/02/15 00:06:40 drhyde Exp $

package CPU::Emulator::Z80::Register;

use vars qw($VERSION);

$VERSION = '1.0';

sub get {}
sub set {}

=head1 NAME

CPU::Emulator::Z80::Register - a register for a Z80

=head1 DESCRIPTION

This is a virtual class which merely defines empty methods so
that the can() method is useful for both the ...::Register8 and
...::Register16 classes.

=head1 METHODS

=head2 get, set

Stubs that do nothing.  They are over-ridden in the sub-classes.

=head1 AUTHOR, LICENCE and COPYRIGHT

Copyright 2008 David Cantrell E<lt>F<david@cantrell.org.uk>E<gt>

This module is free-as-in-speech software, and may be used,
distributed, and modified under the same terms as Perl itself.

=head1 CONSPIRACY

This module is also free-as-in-mason software.

=cut

1;
