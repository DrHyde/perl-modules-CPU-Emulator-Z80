# $Id: Z80.pm,v 1.2 2008/02/14 18:25:59 drhyde Exp $

package CPU::Emulator::Z80;

use strict;
use warnings;

use vars qw($VERSION);

$VERSION = '1.0';

local $SIG{__DIE__} = sub {
    die(__PACKAGE__.": $_[0]\n");
};

use Scalar::Util qw(blessed);
use CPU::Emulator::Memory::Banked;

use constant REGISTERS16 => [qw(PC SP IX IY HL)];
use constant REGISTERS8  => [qw(A B C D E)];
use constant REGISTERS   => [@{REGISTERS16}, @{REGISTERS8}];

=head1 NAME

CPU::Emulator::Z80 - a Z80 emulator

=head1 SYNOPSIS

    # create a CPU with 64K of RAM
    my $cpu = CPU::Emulator::Z80->new();

    # add an OS ROM
    $cpu->memory()->bank(
        address => 0,
        size    => 0x4000,
        type    => 'ROM',
        file    => 'OS.rom'
    );

    # HALT instruction
    $cpu->memory()->poke(0x4000, 0x76);

    # run until we hit a HALT
    $cpu->run();

=head1 DESCRIPTION

This class provides a virtual Z80 micro-processor written in pure perl.
You can program it in Z80 machine code.  Machine code is fast!  This will
make your code faster!

=head1 METHODS

=head2 new

The constructor returns an object representing a Z80 CPU.  It takes
several optional parameters:

=over

=item memory

can be either an object inheriting from CPU::Emulator::Memory, or a string
of data with which to initialise RAM.  If a string is passed, then a
CPU::Emulator::Memory::Banked is created of the appropriate size.  If not
specified at all, then a CPU::Emulator::Memory::Banked is created with
64K of zeroes.

=item init_PC, init_A, init_B ...

For each of A B C D E HL IX IY PC SP, an integer, the starting value for
that register, defaulting to 0.

=back

=cut

sub new {
    my($class, %args) = @_;
    if(exists($args{memory})) {
        if(blessed($args{memory})) {
            die("memory must be a CPU::Emulator::Memory")
                unless($args{memory}->isa('CPU::Emulator::Memory');
        } elsif(!ref($args{memory})) {
            $args{memory} = CPU::Emulator::Memory::Banked->new(
                bytes => $args{memory},
                size  => length($args{memory})
            );
        } else {
            die("memory must be a string or an object\n");
        }
    } else {
        $args{memory} = CPU::Emulator::Memory::Banked->new();
    }

    foreach my $register (@{REGISTERS}) {
        $args{"init_$register"} = 0
            if(!exists($args{"init_$register"}));
    }

    # bless early so we can close over it ...
    my $self = bless {
        memory      => $args{memory},
        hw_registers => {
            (map {
                $_ => CPU::Emulator::Z80::Register8->new($args{"init_$_"})
            } REGISTERS8),
            (map {
                $_ => CPU::Emulator::Z80::Register16->new($args{"init_$_"})
            } REGISTERS16),
        }
    }, $class;

    $self->{derived_registers} = {
        BC   => $self->_derive_register16(qw(B C)),
        DE   => $self->_derive_register16(qw(D E)),
        H    => $self->_derive_register8(qw(HL high));
        L    => $self->_derive_register8(qw(HL low));
        HIX  => $self->_derive_register8(qw(IX high));
        LIX  => $self->_derive_register8(qw(IX low));
        HIY  => $self->_derive_register8(qw(IY high));
        LIY  => $self->_derive_register8(qw(IY low));
    };
    $self->{registers} = { %{$self->{hw_registers}}, %{$self->{derived_registers}} };
}

# create a 16-bit register-pair from two real 8-bit registers
sub _derive_register16 {
    my($self, $high, $low) = @_;
    return CPU::Emulator::Z80::Register16->new(
        get => sub {
                   return 256 * $self->register($high)->get() +
                                $self->register($low)->get()
               },
        set => sub {
                   my(undef, $value) = @_;
                   $self->register($high)->set($value >>8);
                   $self->register($low)->set($value & 0xFF);
               }
    );
}
# create an 8-bit pseudo-register from a 16-bit register
sub _derive_register8 {
    my($self, $pair, $half) = @_;
    return CPU::Emulator::Z80::Register8->new(
        get => sub {
                   my $r = $self->register($pair)->get();
                   return ($half eq 'high')
                       ? $r >> 8
                       : $r & 0xFF
               },
        set => sub {
                   my(undef, $value) = @_;
                   $self->register($pair)->set(
                       ($half eq 'high')
                           ? ($self->register($pair)->get() & 0x00FF) |
                             ($value << 8)
                           : ($self->register($pair)->get() & 0xFF00) | $value
                   );
               }
    );
}

=head2 memory

Return a reference to the object that represent's the system's memory.

=head2 run

Start the CPU running from whatever the Program Counter (PC) is set to.
This will run until either a HALT instruction is hit, or you can tell it
to run for a certain number of instructions by passing a number to the
method.  Note that when this method returns, the PC is set to the address
of the next instruction.

=head1 PROGRAMMING THE Z80

It is not the place of this documentation to teach you how to program a
Z80.  I recommend "Programming the Z80" by Rodnay Zaks.  This excellent
book is unfortunately out of print, but may be available through
abebooks.com
L<http://www.abebooks.com/servlet/SearchResults?an=zaks&tn=programming+the+z80>.

=head1 BUGS/WARNINGS/LIMITATIONS

Claims about making your code faster may not be true in all realities.

=head1 FEEDBACK

I welcome feedback about my code, including constructive criticism
and bug reports.  The best bug reports include files that I can add
to the test suite, which fail with the current code in CVS and will
pass once I've fixed the bug.

Feature requests are far more likely to get implemented if you submit
a patch yourself.

=head1 CVS

L<http://drhyde.cvs.sourceforge.net/drhyde/perlmodules/CPU-Emulator-Z80/>

=head1 AUTHOR, LICENCE and COPYRIGHT

Copyright 2008 David Cantrell E<lt>F<david@cantrell.org.uk>E<gt>

This module is free-as-in-speech software, and may be used,
distributed, and modified under the same terms as Perl itself.

=head1 CONSPIRACY

This module is also free-as-in-mason software.

=cut

1;
