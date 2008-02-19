# $Id: Register.pm,v 1.3 2008/02/19 23:51:08 drhyde Exp $

package CPU::Emulator::Z80::Register;

use vars qw($VERSION);

$VERSION = '1.0';

sub get {}
sub set {}

=head1 NAME

CPU::Emulator::Z80::Register - a register for a Z80

=head1 DESCRIPTION

This is a base class that defines some useful routines for
registers of any size.

=head1 METHODS

=head2 get, set

These do nothing in the base class.  They must be over-ridden in
sub-classes such
that setting stores a value, truncated to the right length, and
getting retrieves a value, truncated to the right length.

The set() method must accept -ve values and store them in
2s-complement.  Its behaviour is undefined if the user is foolish
enough to store too large a -ve value.

The get() method must return the value assuming it to be unsigned.

=head2 getsigned

Decodes the register 2s-complement-ly and return a signed value.

=cut

sub getsigned {
    my $self = shift;
    my $value = $self->get();
    # if MSB == 0, just return the value
    return $value unless($value & (2 ** ($self->{bits} - 1)));
    # algorithm is:
    #   flip all bits
    #   add 1
    #   negate
    return -1 * (($value ^ (2 ** $self->{bits} - 1)) + 1);
}

=head1 FIELDS

All subclasses must have the following fields:

=head2 bits

The number of bits in the register

=head1 AUTHOR, LICENCE and COPYRIGHT

Copyright 2008 David Cantrell E<lt>F<david@cantrell.org.uk>E<gt>

This module is free-as-in-speech software, and may be used,
distributed, and modified under the same terms as Perl itself.

=head1 CONSPIRACY

This module is also free-as-in-mason software.

=cut

1;
