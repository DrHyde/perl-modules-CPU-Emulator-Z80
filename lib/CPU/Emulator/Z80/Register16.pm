# $Id: Register16.pm,v 1.2 2008/02/19 21:19:29 drhyde Exp $

package CPU::Emulator::Z80::Register16;

use strict;
use warnings;

use vars qw($VERSION);

use base qw(CPU::Emulator::Z80::Register);

$VERSION = '1.0';

=head1 NAME

CPU::Emulator::Z80::Register16 - a 16 bit register for a Z80

=head1 DESCRIPTION

This class implements a 16-bit register for a Z80.

=head1 METHODS

=head2 new

Returns an object.  Takes either a single argument, the value to
initialise the register to, or two named parameters:

=over

=item get, set

Subroutines to call when getting/setting the register instead of
the default get/set methods.  The 'get' subroutine will be passed
no parameters, the 'set' subroutine will be passed the new value.
Consequently, they are expected to be closures if they are to be
of any use.

=back

=cut

sub new {
    my $class = shift;
    my $self = {};
    if(@_ == 4) { $self = { @_ }; }
     else { $self->{value} = shift; }
    $self->{bits} = 16;
    bless $self, $class;
}

=head2 get

Get the register's current value.

=cut

sub get {
    my $self = shift;
    if(exists($self->{get})) {
        return $self->{get}->();
    } else { return $self->{value} & 0xFFFF }
}

=head2 set

Set the register's value to whatever is passed in as a parameter.

=cut

sub set {
    my $self = shift;
    if(exists($self->{set})) {
        return $self->{set}->(shift);
    } else { $self->{value} = shift() & 0xFFFF }
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
