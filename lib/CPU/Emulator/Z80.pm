# $Id: Z80.pm,v 1.15 2008/02/21 20:02:31 drhyde Exp $

package CPU::Emulator::Z80;

use strict;
use warnings;

use vars qw($VERSION %INSTR_LENGTHS %INSTR_DISPATCH);

$VERSION = '1.0';

local $SIG{__DIE__} = sub {
    die(__PACKAGE__.": $_[0]\n");
};

use Scalar::Util qw(blessed reftype);
use Tie::Hash::Vivify;
use Carp qw(confess);
use Data::Dumper;

use CPU::Emulator::Memory::Banked;
use CPU::Emulator::Z80::Register8;
use CPU::Emulator::Z80::Register8F;
use CPU::Emulator::Z80::Register16;
use CPU::Emulator::Z80::ALU; # import add/subtract methods

my @REGISTERS16 = qw(PC SP IX IY HL);          # 16 bit registers
my @REGISTERS8  = qw(A B C D E F R);           # 8 bit registers
my @ALTREGISTERS = qw(A B C D E F HL);         # those which have alt.s
my @REGISTERS   = (@REGISTERS16, @REGISTERS8); # all registers

=head1 NAME

CPU::Emulator::Z80 - a Z80 emulator

=head1 SYNOPSIS

    # create a CPU with 64K of zeroes in RAM
    my $cpu = CPU::Emulator::Z80->new();

    # set a breakpoint
    $cpu->memory()->bank(
        address => 8, # RST 1
        size    => 1,
        type    => 'dynamic',
        function_read  => sub { die("Breakpoint reached"); },
        function_write => sub { }
    );

    $cpu->memory()->poke(0, 0xC3);     # JP 0xC000
    $cpu->memory()->poke16(1, 0xC000);

    # run until we hit a breakpoint ie RST 1
    eval { $cpu->run(); }
    print Dumper($cpu->dump_registers());

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

For each of A B C D E F R HL IX IY PC SP, an integer, the starting
value for that register, defaulting to 0.

=item init_A_, init_B_, ...

For each of A B C D E F HL, an integer for the starting value for that
register in the alternate set, defaulting to 0.  Note that contrary to
normal Z80 custom these are named as X_ instead of X'.  This is just for
quoting convenience.

=back

=cut

sub new {
    my($class, %args) = @_;
    if(exists($args{memory})) {
        if(blessed($args{memory})) {
            die("memory must be a CPU::Emulator::Memory")
                unless($args{memory}->isa('CPU::Emulator::Memory'));
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

    foreach my $register (@REGISTERS, map { "${_}_" } @ALTREGISTERS) {
        $args{"init_$register"} = 0
            if(!exists($args{"init_$register"}));
    }

    # bless early so we can close over it ...
    my $self;
    $self = bless {
        memory      => $args{memory},
        registers => Tie::Hash::Vivify->new(sub { confess("No auto-vivifying registers!\n".Dumper(\@_)) }),
        hw_registers => Tie::Hash::Vivify->new(sub { confess("No auto-vivifying hw_registers!\n".Dumper(\@_)) }),
        derived_registers => Tie::Hash::Vivify->new(sub { confess("No auto-vivifying derived_registers!\n".Dumper(\@_)) }),
    }, $class;

    $self->{hw_registers}->{$_} = CPU::Emulator::Z80::Register8->new($args{"init_$_"})
        foreach(@REGISTERS8);

    $self->{hw_registers}->{$_} = CPU::Emulator::Z80::Register16->new($args{"init_$_"})
        foreach(@REGISTERS16);

    $self->{hw_registers}->{$_.'_'} = blessed($self->{hw_registers}->{$_})->new($args{"init_${_}_"})
        foreach(@ALTREGISTERS);

    bless $self->{hw_registers}->{$_}, 'CPU::Emulator::Z80::Register8F'
        foreach(qw(F F_));

    $self->{derived_registers}->{AF}  = $self->_derive_register16(qw(A F));
    $self->{derived_registers}->{AF_} = $self->_derive_register16(qw(A_ F_));
    $self->{derived_registers}->{BC}  = $self->_derive_register16(qw(B C));
    $self->{derived_registers}->{BC_}  = $self->_derive_register16(qw(B_ C_));
    $self->{derived_registers}->{DE}  = $self->_derive_register16(qw(D E));
    $self->{derived_registers}->{DE_}  = $self->_derive_register16(qw(D_ E_));
    $self->{derived_registers}->{H}   = $self->_derive_register8(qw(HL high));
    $self->{derived_registers}->{H_}   = $self->_derive_register8(qw(HL_ high));
    $self->{derived_registers}->{L}   = $self->_derive_register8(qw(HL low));
    $self->{derived_registers}->{L_}   = $self->_derive_register8(qw(HL_ low));
    $self->{derived_registers}->{HIX} = $self->_derive_register8(qw(IX high));
    $self->{derived_registers}->{LIX} = $self->_derive_register8(qw(IX low));
    $self->{derived_registers}->{HIY} = $self->_derive_register8(qw(IY high));
    $self->{derived_registers}->{LIY} = $self->_derive_register8(qw(IY low));
    
    $self->{registers}->{$_} = $self->{hw_registers}->{$_}
        foreach(keys %{$self->{hw_registers}});
    $self->{registers}->{$_} = $self->{derived_registers}->{$_}
        foreach(keys %{$self->{derived_registers}});

    return $self;
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
                   my $value = shift;
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
                   my $value = shift;
                   $self->register($pair)->set(
                       ($half eq 'high')
                           ? ($self->register($pair)->get() & 0xFF) |
                             ($value << 8)
                           : ($self->register($pair)->get() & 0xFF00) | $value
                   );
               }
    );
}

=head2 memory

Return a reference to the object that represent's the system's memory.

=cut

sub memory {
    my $self = shift;
    return $self->{memory};
}

=head2 register

Return the object representing a specified register.  This can be any
of the real registers (eg D or D_) or a derived register (eg DE or L).

=cut

sub register {
    my $self = shift;
    return $self->{registers}->{shift()};
}

=head2 status

Return a scalar representing the entire state of the CPU or, if passed
a scalar, attempt to initialise the CPU to the status it represents.

=cut

sub status {
    my $self = shift;
    if(@_) { $self->_status_load(@_); }
    return
        join('', map {
            chr($self->register($_)->get())
        } qw(A B C D E F A_ B_ C_ D_ E_ F_ R))
       .join('', map {
            chr($self->register($_)->get() >> 8),
            chr($self->register($_)->get() & 0xFF),
        } qw(SP PC IX IY HL HL_));
}
sub _status_load {
    my($self, $status) = @_;
    my @regs = split(//, $status);
    $self->register($_)->set(ord(shift(@regs)))
        foreach(qw(A B C D E F A_ B_ C_ D_ E_ F_ R));
    $self->register($_)->set(256 * ord(shift(@regs)) + ord(shift(@regs)))
        foreach(qw(SP PC IX IY HL HL_));
}

=head2 registers

Return a hashref of all the real registers and their values.

=head2 print_registers

A convenient method for printing out all the registers in a sane format.

=cut

sub registers {
    my $self = shift;
    return {
        map {
            $_ => $self->register($_)->get()
        } keys %{$self->{hw_registers}}
    }
}

sub print_registers {
    my $self = shift;
    printf("
             SZ5H3PNC                             SZ5H3PNC
A:  0x%02X F:  %08b HL:  0x%04X    A_: 0x%02X F_: %08b HL_: 0x%04X
B:  0x%02X C:  0x%02X                    B_: 0x%02X C_: 0x%02X
D:  0x%02X E:  0x%02X                    D_: 0x%02X E_: 0x%02X

R:  0x%02X IX: 0x%04X IY: 0x%04X SP: 0x%04X PC: 0x%04X
", map { $self->register($_)->get(); } qw(A F HL A_ F_ HL_ B C B_ C_ D E D_ E_ R IX IY SP PC));
}

=head2 run

Start the CPU running from whatever the Program Counter (PC) is set to.
This will run until either a HALT instruction is hit, or you can tell it
to run for a certain number of instructions by passing a number to the
method.  Note that when this method returns, the PC is set to the address
of the next instruction.

=cut

# SEE http://www.z80.info/decoding.htm
# NB when decoding, x == first 2 bits, y == next 3, z == last 3
#                   p == first 2 bits of y, q == last bit of y
my @TABLE_R   = (qw(B C D E H L (HL) A));
my @TABLE_RP  = (qw(BC DE HL SP));
my @TABLE_RP2 = (qw(BC DE HL AF));
my @TABLE_CC  = (qw(NZ Z NC C PO PE P M));
my @TABLE_ALU = ("ADD A", "ADC A", "SUB", "SBC A", qw(AND XOR OR CP));
my @TABLE_ROT = (qw(RLC RRC RL RR SLA SRA SLL SRL));
%INSTR_LENGTHS = (
    (map { $_ => 'UNDEFINED' } (0 .. 255)),
    # un-prefixed instructions
    # x=0, z=0
    (map { ($_ << 3) => 1 } (0, 1, 2)), # NOP; EX AF, AF'; DJNZ
    (map { ($_ << 3) => 2 } (3 .. 7)), # JR X, d
    # x=0, z=1
    (map { 0b00000001 | ($_ << 4 ) => 3 } (0 .. 3)), # LD rp[p], nn
    (map { 0b00001001 | ($_ << 4 ) => 1 } (0 .. 3)), # ADD HL, rp[p]
    # x=0, z=2
    # LD (BC/DE), A; LD A, (BC/DE)
    (map { 0b00000010 | ($_ << 3) => 1 } (0b000, 0b010, 0b001, 0b011)),
    # LD (nn), HL/A, LD HL/A, (nn)
    (map { 0b00000010 | ($_ << 3) => 3 } (0b100, 0b110, 0b101, 0b111)),
    # x=0, z=3
    # q=0: INC rp[p]
    # q=1: DEC rp[p]
    (map { 0b00000011 | ($_ << 3) => 1 } (0 .. 7)),
    # x=0, z=4: INC r[y]
    (map { 0b00000100 | ($_ << 3) => 1 } (0 .. 7)),
    # x=0, z=5: DEC r[y]
    (map { 0b00000101 | ($_ << 3) => 1 } (0 .. 7)),
    # x=0, z=6: LD r[y], n
    (map { 0b00000110 | ($_ << 3) => 2 } (0 .. 7)),
    # x=0, z=7: RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, CCF
    (map { 0b00000111 | ($_ << 3) => 2 } (0 .. 7)),

    # x=1: LD r[y], r[z] (exception: y=6, z=6 is HALT
    (map { 0b01000000 + $_ => 1 } (0 .. 0b111111)),

    # x=2: alu[y] on A and r[z]
    (map { 0b10000000 + $_ => 1 } (0 .. 0b111111)),


    0xC3 => 3, # JP
    # and finally ...
    # length tables for prefixes ...
    0xDD, {
            # NB lengths in here do *not* include the prefix
            (map { $_ => sub { $INSTR_LENGTHS{$_} } } ( 0 .. 255)),
            0xCB => {
                # NB lengths in here do *not* include either prefix byte
                (map { $_ => 'UNDEFINED' } (0 .. 255)),
            },
            0xDD => 1, # NOP
            0xED => 1, # NOP
            0xFD => 1, # NOP
          },
    # synthesise a copy of 0xDD's table
    0xFD, {
            (map { $_ => sub { $INSTR_LENGTHS{0xDD}->{$_} } } ( 0 .. 255)),
          },
    0xCB, {
            (map { $_ => 'UNDEFINED' } (0 .. 255)),
            # Roll, BIT, RES and SET go here
          },
    0xED, {
            (map { $_ => 'UNDEFINED' } (0 .. 255)),
            # invalid instr, equiv to NOP (actually a NONI)
            (map { $_ => 1 } ( 0b00000000 .. 0b00111111,
                               0b11000000 .. 0b11111111)),
          },
);

# these are all passed a list of parameter bytes
%INSTR_DISPATCH = (
    # un-prefixed instructions
    0          => \&_NOP,
    0b00001000 => \&_EX_AF_AF,
    0b00010000 => \&_DJNZ,
    0b00011000 => \&_JR_unconditonal,
    (map { ($_ << 3) => sub {
        _check_cond($_[0], TABLE_CC()->[$_ - 4]) &&
        _JR_unconditonal(@_);
    } } (4 .. 7)),
    (map { 0b00000001 | ($_ << 4 ) => 3 } (0 .. 3)), # LD rp[p], nn
    (map { 0b00001001 | ($_ << 4 ) => 1 } (0 .. 3)), # ADD HL, rp[p]
    # LD (BC/DE), A; LD A, (BC/DE)
    (map { 0b00000010 | ($_ << 3) => 1 } (0b000, 0b010, 0b001, 0b011)),
    # LD (nn), HL/A, LD HL/A, (nn)
    (map { 0b00000010 | ($_ << 3) => 3 } (0b100, 0b110, 0b101, 0b111)),
    # q=0: INC rp[p]
    # q=1: DEC rp[p]
    (map { 0b00000011 | ($_ << 3) => 1 } (0 .. 7)),
    # INC r[y]
    (map { 0b00000100 | ($_ << 3) => 1 } (0 .. 7)),
    # DEC r[y]
    (map { 0b00000101 | ($_ << 3) => 1 } (0 .. 7)),
    # LD r[y], n
    (map { 0b00000110 | ($_ << 3) => 2 } (0 .. 7)),
    # RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, CCF
    (map { 0b00000111 | ($_ << 3) => 2 } (0 .. 7)),
    # LD r[y], r[z] (exception: y=6, z=6 is HALT
    (map { 0b01000000 + $_ => 1 } (0 .. 0b111111)),
    # alu[y] on A and r[z]
    (map { 0b10000000 + $_ => 1 } (0 .. 0b111111)),

    0x76 => \&_HALT,
    0xC3 => \&_JP_unconditional,
    # and finally,  prefixed instructions
    0xCB, {
          },
    0xED, {
            (map { $_ => \&_NOP } ( 0b00000000 .. 0b00111111,
                                    0b11000000 .. 0b11111111)),
          },
    0xDD, {
            0xDD => \&_NOP,
            0xED => \&_NOP,
            0xFD => \&_NOP,
          },
    0xFD, {
            0xDD => \&_NOP,
            0xED => \&_NOP,
            0xFD => \&_NOP,
          },
);

{ my $instrs_to_execute;
sub run {
    my $self = shift;
    if(@_) { $instrs_to_execute = shift(); }
     else { $instrs_to_execute = -1; }

    while($instrs_to_execute) {
        $instrs_to_execute--;
        $self->{instr_length_table} = \%INSTR_LENGTHS;
        $self->{instr_dispatch_table} = \%INSTR_DISPATCH;
        $self->{prefix_bytes} = [];
        $self->_execute($self->_fetch());
        delete $self->{prefix_bytes};
        delete $self->{instr_lengths_table};
        delete $self->{instr_dispatch_table};
    }
}
}

# fetch all the bytes for an instruction and return them
sub _fetch {
    my $self = shift;
    my @bytes = ();
    my $pc = $self->register('PC')->get();
    
    # R register increments weirdly ...
    my $r = $self->register('R')->get();
    $self->register('R')->set(
        ($r & 0b10000000) | (($r + 1) & 0b01111111)
    );
    my $byte = $self->memory()->peek($pc);
    push @bytes, $byte;

    # prefix byte
    if(ref($byte) && reftype($byte) eq 'HASH') {
        $self->{instr_dispatch_table} = $self->{instr_dispatch_table}->{$byte};
        $self->{instr_length_table} = $self->{instr_length_table}->{$byte};
        push @{$self->{prefix_bytes}}, $byte;
        $self->register('PC')->set($pc + 1);
        return $self->_fetch();
    }

    my $bytes_to_fetch = $self->{instr_length_table}->{$byte};
    $bytes_to_fetch = $bytes_to_fetch->()
        if(ref($bytes_to_fetch) && reftype($bytes_to_fetch) eq 'CODE');
    
    die(sprintf(
        "_fetch: Unknown instruction 0x%02X at 0x%04X with prefix bytes "
          .join(' ', map { "0x%02X" } @{$self->{prefix_bytes}})
          ."\n", $byte, $pc, @{$self->{prefix_bytes}}
    )) if($bytes_to_fetch eq 'UNDEFINED');

    push @bytes, map { $self->memory()->peek($pc + $_) } (1 .. $bytes_to_fetch - 1);
    $self->register('PC')->set($pc + $bytes_to_fetch);
    return @bytes;
}

# execute an instruction. NB, the PC already points at the next instr
sub _execute {
    my($self, $instr) = (shift(), shift());
    # printf("_execute: 0x%02X (PC=0x%04X)\n", $instr, $self->register('PC')->get());
    die(sprintf(
        "_execute: No entry in dispatch table for instr "
          .join(' ', map { "0x%02x" } (@{$self->{prefix_bytes}}, $instr))
          ." of known length, near addr 0x%04x\n",
        @{$self->{prefix_bytes}}, $instr, $self->register('PC')->get()
    )) unless(
        exists($self->{instr_dispatch_table}->{$instr}) &&
        ref($self->{instr_dispatch_table}->{$instr}) &&
        reftype($self->{instr_dispatch_table}->{$instr}) eq 'CODE'
    );
    $self->{instr_dispatch_table}->{$instr}->($self, @_);
}

sub _check_cond {
    my($self, $cond) = @_;
    die("_check_cond NYI\n");
}
sub _NOP { }
sub _HALT { while(sleep(10)) {} }
sub _JR_unconditional {
    my $self = shift;
    die("_JR_unconditional NYI\n");
}
sub _JP_unconditional {
    my $self = shift;
    $self->register('PC')->set(shift() + 256 * shift());
}

sub _EX_AF_AF {
    shift()->_swap_regs(qw(AF AF_));
}
sub _swap_regs {
    my($self, $r1, $r2) = @_;
    my $temp = $self->register($r1)->get();
    $self->register($r1)->set($self->register($r2)->get());
    $self->register($r2)->set($temp);
}

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
