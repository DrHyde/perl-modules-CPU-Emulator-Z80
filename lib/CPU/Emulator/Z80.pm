# $Id: Z80.pm,v 1.31 2008/02/24 02:15:46 drhyde Exp $

package CPU::Emulator::Z80;

use strict;
use warnings;

use vars qw($VERSION %INSTR_LENGTHS %INSTR_DISPATCH);

$VERSION = '1.0';

$SIG{__WARN__} = sub {
    warn(__PACKAGE__.": $_[0]\n");
};

use Scalar::Util qw(blessed reftype);
use Tie::Hash::Vivify;
use Carp qw(confess);
use Data::Dumper;

use CPU::Emulator::Memory::Banked;
use CPU::Emulator::Z80::Register8;
use CPU::Emulator::Z80::Register8F;
use CPU::Emulator::Z80::Register8R;
use CPU::Emulator::Z80::Register16;
use CPU::Emulator::Z80::Register16SP;
use CPU::Emulator::Z80::ALU; # import add/subtract methods

my @REGISTERS16 = qw(PC SP IX IY HL);          # 16 bit registers
# NB W and Z aren't programmer-accesible, for internal use only!
my @REGISTERS8  = qw(A B C D E F R W Z);       # 8 bit registers
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
    print $cpu->format_registers();

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

    $self->{hw_registers}->{$_} = CPU::Emulator::Z80::Register8->new(
        cpu => $self, value => $args{"init_$_"}
    ) foreach(@REGISTERS8);

    $self->{hw_registers}->{$_} = CPU::Emulator::Z80::Register16->new(
        cpu => $self, value => $args{"init_$_"}
    ) foreach(@REGISTERS16);

    $self->{hw_registers}->{$_.'_'} = blessed($self->{hw_registers}->{$_})->new(
        cpu => $self, value => $args{"init_$_"}
    ) foreach(@ALTREGISTERS);

    bless $self->{hw_registers}->{$_}, 'CPU::Emulator::Z80::Register8F'
        foreach(qw(F F_));
    bless $self->{hw_registers}->{R}, 'CPU::Emulator::Z80::Register8R';
    bless $self->{hw_registers}->{SP}, 'CPU::Emulator::Z80::Register16SP';


    $self->{derived_registers}->{AF}  = $self->_derive_register16(qw(A F));
    $self->{derived_registers}->{AF_} = $self->_derive_register16(qw(A_ F_));
    $self->{derived_registers}->{BC}  = $self->_derive_register16(qw(B C));
    $self->{derived_registers}->{BC_}  = $self->_derive_register16(qw(B_ C_));
    $self->{derived_registers}->{DE}  = $self->_derive_register16(qw(D E));
    $self->{derived_registers}->{DE_}  = $self->_derive_register16(qw(D_ E_));
    $self->{derived_registers}->{WZ}  = $self->_derive_register16(qw(W Z));
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
               },
        cpu => $self
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
               },
        cpu => $self
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
For (HL) it returns the private internal DON'T TOUCH THIS 'W' register,
for evil twisty internals reasons.

=cut

sub register {
    my($self, $r) = @_;
    return $self->{registers}->{($r eq '(HL)') ? 'W' : $r};
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

=head2 format_registers

A convenient method for getting all the registers in a nice
printable format.  It mostly exists to help me with debuggering,
but if you promise to be good you can use it too.  Just don't
rely on the format remaining unchanged.

=cut

sub registers {
    my $self = shift;
    return {
        map {
            $_ => $self->register($_)->get()
        } grep { $_ !~ /^(W|Z)$/ } keys %{$self->{hw_registers}}
    }
}

sub format_registers {
    my $self = shift;
    sprintf("#
#              SZ5H3PNC                             SZ5H3PNC
# A:  0x%02X F:  %08b HL:  0x%04X    A_: 0x%02X F_: %08b HL_: 0x%04X
# B:  0x%02X C:  0x%02X                    B_: 0x%02X C_: 0x%02X
# D:  0x%02X E:  0x%02X                    D_: 0x%02X E_: 0x%02X
# 
# R:  0x%02X IX: 0x%04X IY: 0x%04X SP: 0x%04X PC: 0x%04X
# W:  0x%02X Z:  0x%02X (internal use only)
", map { $self->register($_)->get(); } qw(A F HL A_ F_ HL_ B C B_ C_ D E D_ E_ R IX IY SP PC W Z));
}

=head2 run

Start the CPU running from whatever the Program Counter (PC) is set to.
This will by default run for ever.  However, it takes an optional
parameter telling the CPU to run
that number of instructions.

=cut

# SEE http://www.z80.info/decoding.htm
# NB when decoding, x == first 2 bits, y == next 3, z == last 3
#                   p == first 2 bits of y, q == last bit of y
my @TABLE_R   = (qw(B C D E H L (HL) A));
my @TABLE_RP  = (qw(BC DE HL SP));
my @TABLE_RP2 = (qw(BC DE HL AF));
my @TABLE_CC  = (qw(NZ Z NC C PO PE P M));
my @TABLE_ALU = (
    \&_ADD_r8_r8, \&_ADC_r8_r8, \&_SUB_r8_r8, \&_SBC_r8_r8,
    \&_AND_r8_r8, \&_XOR_r8_r8, \&_OR_r8_r8, \&_CP_r8_r8
);
my @TABLE_ROT = (
    \&_RLC, \&_RRC, \&_RL, \&_RR, \&_SLA, \&_SRA, \&_SLL, \&_SRL
);

# NB order is important in these tables
%INSTR_LENGTHS = (
    (map { $_ => 'UNDEFINED' } (0 .. 255)),
    # un-prefixed instructions
    # x=0, z=0
    (map { ($_ << 3) => 1 } (0, 1)), # NOP; EX AF, AF'
    (map { ($_ << 3) => 2 } (2 .. 7)), # DJNZ d; JR d; JR X, d
    # x=0, z=1
    (map { 0b00000001 | ($_ << 4 ) => 3 } (0 .. 3)), # LD rp[p], nn
    (map { 0b00001001 | ($_ << 4 ) => 1 } (0 .. 3)), # ADD HL, rp[p]
    # x=0, z=2
    (map { 0b00000010 | ($_ << 3) => 1 } (0 .. 3)), # LD (BC/DE), A; LD A, (BC/DE)
    (map { 0b00000010 | ($_ << 3) => 3 } (4 .. 7)), #  LD (nn), HL/A, LD HL/A, (nn)
    # x=0, z=3
    (map { 0b00000011 | ($_ << 3) => 1 } (0 .. 7)), # INC/DEC rp
    # x=0, z=4
    (map { 0b00000100 | ($_ << 3) => 1 } (0 .. 7)), # INC r[y]
    # x=0, z=5
    (map { 0b00000101 | ($_ << 3) => 1 } (0 .. 7)), # DEC r[y]
    # x=0, z=6
    (map { 0b00000110 | ($_ << 3) => 2 } (0 .. 7)), # LD r[y], n
    # x=0, z=7: RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, CCF
    (map { 0b00000111 | ($_ << 3) => 1 } (0 .. 7)),
    # x=1
    (map { 0b01000000 + $_ => 1 } (0 .. 63)), # LD r[y], r[z], HALT
    # x=2
    (map { 0b10000000 + $_ => 1 } (0 .. 63)), # alu[y] on A and r[z]
    # x=3, z=0
    (map { 0b11000000 | ($_ << 3) => 1 } (0 .. 7)), # RET cc[y]
    (map { 0b11000001 | ($_ << 3) => 1 } (0 .. 7)), # POP rp2[p]/RET/EXX/JP HL/LD SP, HL
    (map { 0b11000010 | ($_ << 3) => 3 } (0 .. 7)), # JP cc[y], nn
    (map { 0b11000100 | ($_ << 3) => 3 } (0 .. 7)), # CALL cc[y], nn
    (map { 0b11000101 | ($_ << 4) => 1 } (0 .. 3)), # PUSH rp2[p]
    (map { 0b11000110 | ($_ << 3) => 2 } (0 .. 7)), # ALU[y] A, n
    (map { 0b11000111 | ($_ << 3) => 1 } (0 .. 7)), # RST y*8
    (map { 0b11000011 | ($_ << 4) => 1 } (4 ..7)), # EX(SP), HL/EX DE, HL/DI/EI
    0b11010011 => 2, # OUT (n), A
    0b11011011 => 2, # IN A, (n)
    0xC3 => 3, # JP nn
    0xCD => 3, # CALL nn

    0xCB, { (map { $_ => 1 } (0 .. 255)) }, # roll/shift/bit/res/set
    0xED, {
            (map { $_ => 'UNDEFINED' } (0 .. 255)),
            # invalid instr, equiv to NOP (actually a NONI)
            (map { $_ => 1 } ( 0b00000000 .. 0b00111111,
                               0b11000000 .. 0b11111111)),
          },
);
$INSTR_LENGTHS{0xDD} = $INSTR_LENGTHS{0xFD} = {
    # NB lengths in here do *not* include the prefix
    (map { $_ => $INSTR_LENGTHS{$_} } (0 .. 255)),
    0x34 => 2, # INC (IX + d)
    0x35 => 2, # DEC (IX + d)
    0xCB => {
        # NB lengths in here do *not* include either prefix byte
        (map { $_ => 'UNDEFINED' } (0 .. 255)),
    },
    0xDD => 1, # NOP
    0xED => 1, # NOP
    0xFD => 1, # NOP
};

# these are all passed a list of parameter bytes
%INSTR_DISPATCH = (
    # un-prefixed instructions
    0          => \&_NOP,
    0b00001000 => sub { _swap_regs(shift(), qw(AF AF_)); },
    0b00010000 => \&_DJNZ,
    0b00011000 => \&_JR_unconditional,
    (map { my $y = $_; ($_ << 3) => sub {
        _check_cond($_[0], $TABLE_CC[$y - 4]) &&
        _JR_unconditional(@_);
    } } (4 .. 7)),
    (map { my $p = $_; 0b00000001 | ($p << 4 ) => sub {
        _LD_r16_imm(shift(), $TABLE_RP[$p], @_) # LD rp[p], nn
    } } (0 .. 3)),
    (map { my $p = $_; 0b00001001 | ($_ << 4 ) => sub {
        _ADD_r16_r16(shift(), 'HL', $TABLE_RP[$p]) # ADD HL, rp[p]
    } } (0 .. 3)),
    0b00000010 => sub { _LD_indr16_r8($_[0], 'BC', 'A'); }, # LD (BC), A
    0b00001010 => sub { _LD_r8_indr16($_[0], 'A', 'BC'); }, # LD A, (BC)
    0b00010010 => sub { _LD_indr16_r8($_[0], 'DE', 'A'); }, # LD (DE), A
    0b00011010 => sub { _LD_r8_indr16($_[0], 'A', 'DE'); }, # LD A, (DE)
    0b00100010 => sub { _LD_ind_r16(shift(), 'HL', @_); }, # LD (nn), HL
    0b00101010 => sub { _LD_r16_ind(shift(), 'HL', @_); }, #LD HL, (nn)
    0b00110010 => sub { _LD_ind_r8(shift(), 'A', @_); }, # LD (nn), A
    0b00111010 => sub { _LD_r8_ind(shift(), 'A', @_); }, #LD A, (nn)
    (map {
        my($p, $q) = (($_ & 0b110) >> 1, $_ & 0b1);
        0b00000011 | ($_ << 3) => sub {
            $q ? _DEC($_[0], $TABLE_RP[$p]) # DEC rp[p]
               : _INC($_[0], $TABLE_RP[$p]) # INC rp[p]
        }
    } (0 .. 7)),
    (map { my $y = $_; 0b00000100 | ($_ << 3) => sub {
        _INC($_[0], $TABLE_R[$y], $_[1]) # INC r[y] or INC(IX/Y + d)
    } } (0 .. 7)),
    (map { my $y = $_; 0b00000101 | ($_ << 3) => sub {
        _DEC($_[0], $TABLE_R[$y], $_[1]) # DEC r[y] or DEC(IX/Y + d)
    } } (0 .. 7)),
    (map { my $y = $_; 0b00000110 | ($_ << 3) => sub {
        _LD_r8_imm(shift(), $TABLE_R[$y], @_) # LD r[y], n
    } } (0 .. 7)),
    0b00000111 => \&_RLCA,
    0b00001111 => \&_RRCA,
    0b00010111 => \&_RLA,
    0b00011111 => \&_RRA,
    0b00100111 => \&_DAA,
    0b00101111 => \&_CPL,
    0b00110111 => \&_SCF,
    0b00111111 => \&_CCF,
    (map { my $y = $_ >> 3; my $z = $_ & 0b111; 0b01000000 + $_ => sub {
        _LD_r8_r8(shift(), $TABLE_R[$y], $TABLE_R[$z]); # LD r[y], r[z]
    } } (0 .. 0b111111)),
    0b01110110 => \&_HALT,
    (map { my $y = $_ >> 3; my $z = $_ & 0b111; 0b10000000 + $_ => sub {
        $TABLE_ALU[$y]->(shift(), 'A', $TABLE_R[$z]); # alu[y] A, r[z]
    } } (0 .. 0b111111)),
    (map { my $y = $_; 0b11000110 | ($_ << 3) => sub {
        _LD_r8_imm($_[0], 'W', $_[1]);       # alu[y] A, n
        $TABLE_ALU[$y]->(shift(), 'A', 'W');
    } } (0 .. 7)),
    (map { my $y = $_; 0b11000000 | ($_ << 3) => sub {
        _check_cond($_[0], $TABLE_CC[$y]) && # RET cc[y]
        _POP(shift(), 'PC');
    } } (0 .. 7)),
    (map { my $p = $_; 0b11000001 | ($_ << 4) => sub {
        _POP(shift(), $TABLE_RP2[$p]); # POP rp2[p]
    } } (0 .. 3)),
    (map { my $y = $_; 0b11000010 | ($_ << 3) => sub {
        _check_cond($_[0], $TABLE_CC[$y]) && # JP cc[y], nn
        _JP_unconditional(@_);
    } } (0 .. 7)),
    (map { my $y = $_; 0b11000100 | ($_ << 3) => sub {
        _check_cond($_[0], $TABLE_CC[$y]) && # CALL cc[y], nn
        _CALL_unconditional(@_);
    } } (0 .. 7)),
    (map { my $y = $_; 0b11000111 | ($_ << 3) => sub {
        _CALL_unconditional(shift(), $y * 8, 0); # RST y*8
    } } (0 .. 7)),
    0xC3 => \&_JP_unconditional,
    0b11010011 => \&_OUT_n_A, # OUT (n), A
    0b11011011 => \&_IN_A_n, # IN A, (n)
    0b11100011 => sub { # EX (SP), HL
        my $self = shift;
        _POP($self, 'WX'); _PUSH($self, 'HL');
        _LD_r16_r16($self, 'HL', 'WX');
    },
    0b11101011 => sub { _swap_regs(shift(), qw(DE HL)); },
    0b11110011 => \&_DI,
    0b11111011 => \&_EI,
    (map { my $p = $_; 0b11000101 | ($_ << 4) => sub {
        _PUSH(shift(), $TABLE_RP2[$p]); # PUSH rp2[p]
    } } (0 .. 3)),
    0xCD => \&_CALL_unconditional,
    0b11001001 => sub { _POP(shift(), 'PC'); }, # RET
    0b11011001 => \&_EXX,
    0b11101001 => \&_JP_HL,
    0b11111001 => \&_LD_SP_HL,

    # and finally,  prefixed instructions
    0xCB, {
            (map { my $y = $_ >> 3; my $z = $_ & 7; 0b00000000 | $_ => sub {
                $TABLE_ROT[$y]->(shift(), $TABLE_R[$z]);
            } } (0 .. 63)),
            (map { my $y = $_ >> 3; my $z = $_ & 7; 0b01000000 | $_ => sub {
                _BIT(shift(), $y, $TABLE_R[$z]);
            } } (0 .. 63)),
            (map { my $y = $_ >> 3; my $z = $_ & 7; 0b10000000 | $_ => sub {
                _RES(shift(), $y, $TABLE_R[$z]);
            } } (0 .. 63)),
            (map { my $y = $_ >> 3; my $z = $_ & 7; 0b11000000 | $_ => sub {
                _SET(shift(), $y, $TABLE_R[$z]);
            } } (0 .. 63)),
          },
    0xDD, {
               map { my $i = $_; $_ => sub {
                   _swap_regs($_[0], qw(HL IX));
                   $INSTR_DISPATCH{$i}->(@_);
                   _swap_regs($_[0], qw(HL IX));
               } } (0 .. 255)
          },
    0xED, {
            (map { $_ => \&_NOP } ( 0b00000000 .. 0b00111111,
                                    0b11000000 .. 0b11111111)),
          },
    0xFD, {
               map { my $i = $_; $_ => sub {
                   _swap_regs($_[0], qw(HL IY));
                   $INSTR_DISPATCH{$i}->(@_);
                   _swap_regs($_[0], qw(HL IY));
               } } (0 .. 255)
          },
);

sub run {
    my $self = shift;
    my $instrs_to_execute = -1;
    $instrs_to_execute = shift() if(@_);

    while($instrs_to_execute) {
        $instrs_to_execute--;
        $self->{instr_length_table} = \%INSTR_LENGTHS;
        $self->{instr_dispatch_table} = \%INSTR_DISPATCH;
        $self->{prefix_bytes} = [];
        $self->_execute($self->_fetch());
        delete $self->{instr_length_table};
        delete $self->{instr_dispatch_table};
        delete $self->{prefix_bytes};
    }
}

# fetch all the bytes for an instruction and return them
sub _fetch {
    my $self = shift;
    my $pc = $self->register('PC')->get();
    
    $self->register('R')->inc();
    my $byte = $self->memory()->peek($pc);
    my @bytes = ($byte);

    # prefix byte
    if(ref($self->{instr_length_table}->{$byte})) {
        # printf("Found prefix byte %#04x\n", $byte);
        $self->{instr_dispatch_table} = $self->{instr_dispatch_table}->{$byte};
        $self->{instr_length_table} = $self->{instr_length_table}->{$byte};
        push @{$self->{prefix_bytes}}, $byte;
        $self->register('PC')->inc(); # set($pc + 1);
        return $self->_fetch();
    }

    my $bytes_to_fetch = $self->{instr_length_table}->{$byte};
    
    die(sprintf(
        "_fetch: Unknown instruction 0x%02X at 0x%04X with prefix bytes ["
          .join(' ', map { "0x%02X" } @{$self->{prefix_bytes}})
          ."]\n", $byte, $pc, @{$self->{prefix_bytes}}
    )) if($bytes_to_fetch eq 'UNDEFINED');

    push @bytes, map { $self->memory()->peek($pc + $_) } (1 .. $bytes_to_fetch - 1);
    $self->register('PC')->set($pc + $bytes_to_fetch);
    return @bytes;
}

# execute an instruction. NB, the PC already points at the next instr
sub _execute {
    my($self, $instr) = (shift(), shift());
    # printf("_execute: 0x%02X (PC=0x%04X)\n", $instr, $self->register('PC')->get());
    if(
        exists($self->{instr_dispatch_table}->{$instr}) &&
        ref($self->{instr_dispatch_table}->{$instr}) &&
        reftype($self->{instr_dispatch_table}->{$instr}) eq 'CODE'
    ) {
        $self->{instr_dispatch_table}->{$instr}->($self, @_);
    } else {
        warn(sprintf(
            "_execute: No entry in dispatch table for instr "
              .join(' ', map { "0x%02x" } (@{$self->{prefix_bytes}}, $instr))
              ." of known length, near addr 0x%04x\n",
            @{$self->{prefix_bytes}}, $instr, $self->register('PC')->get()
        ));
    }
}

sub _check_cond {
    my($self, $cond) = @_;
    my $f = $self->register('F');
    return
           $cond eq 'NC' ? !$f->getC() :
           $cond eq 'C'  ?  $f->getC() :
           $cond eq 'NZ' ? !$f->getZ() :
           $cond eq 'Z'  ?  $f->getZ() :
           die("_check_cond: condition $cond NYI");
}

sub _ADD_r16_r16 {
    my($self, $r1, $r2) = @_;
    $self->register($r1)->add($self->register($r2)->get());
}
sub _ADC_r8_r8 {
    my($self, $r1, $r2) = @_;
    _ADD_r8_r8($self, $r1, $r2, $self->register('F')->getC());
    # $self->register('F')->setP(ALU_parity($self->register($r1)->get()));
}
sub _ADD_r8_r8 {
    my($self, $r1, $r2, $c) = @_;
    # $r1 is always A, $r2 is A/B/C/D/EH/L/(HL)/W/Z
    # $c is defined if this is really ADC
    $c ||= 0;
    if($r2 eq '(HL)') {
        my @addr = map { $self->register($_)->get()} qw(L H);
        _LD_r8_ind($self, 'W', @addr);
        $r2 = 'W';
    }
    $self->register($r1)->add($self->register($r2)->get() + $c);
}
sub _BIT {
    my($self, $bit, $r) = @_;
    if($r eq '(HL)') {
        my @addr = map { $self->register($_)->get()} qw(L H);
        _LD_r8_ind($self, 'W', @addr);
        $r = 'W';
    }
    $self->register('F')->setZ(!($self->register($r)->get() & 2**$bit));
    $self->register('F')->setH();
    $self->register('F')->resetN();
    $self->register('F')->set5($self->register($r)->get() & 0b100000);
    $self->register('F')->set3($self->register($r)->get() & 0b1000);
    $self->register('F')->setS(
        $bit == 7 && $self->register($r)->get() & 0x80
    );
    $self->register('F')->setP($self->register('F')->getZ());
}
sub _RES {
    my($self, $bit, $r) = @_;
    my @addr;
    if($r eq '(HL)') {
        @addr = map { $self->register($_)->get()} qw(L H);
        _LD_r8_ind($self, 'W', @addr);
        $r = 'W';
    }
    $self->register($r)->set($self->register($r)->get() & (255 - 2**$bit));
    _LD_ind_r8($self, 'W', @addr) if($r eq 'W');
}
sub _SET {
    my($self, $bit, $r) = @_;
    my @addr;
    if($r eq '(HL)') {
        @addr = map { $self->register($_)->get()} qw(L H);
        _LD_r8_ind($self, 'W', @addr);
        $r = 'W';
    }
    $self->register($r)->set($self->register($r)->get() | (2**$bit));
    _LD_ind_r8($self, 'W', @addr) if($r eq 'W');
}

sub _binop {
    my($self, $r1, $r2, $op) = @_;
    # $r1 is always A, $r2 is A/B/C/D/EH/L/(HL)/W/Z
    if($r2 eq '(HL)') {
        my @addr = map { $self->register($_)->get()} qw(L H);
        _LD_r8_ind($self, 'W', @addr);
        $r2 = 'W';
    }
    $self->register($r1)->set(eval
        '$self->register($r1)->get()'.$op.'$self->register($r2)->get()'
    );
    die($@) if($@);
    $self->register('F')->setS($self->register($r1)->get() & 0x80);
    $self->register('F')->setZ($self->register($r1)->get() == 0);
    $self->register('F')->set5($self->register($r1)->get() & 0b100000);
    $self->register('F')->setH($op eq '&');
    $self->register('F')->set3($self->register($r1)->get() & 0b1000);
    $self->register('F')->setP(ALU_parity($self->register($r1)->get()));
    $self->register('F')->resetN();
    $self->register('F')->resetC();
}
sub _AND_r8_r8 { _binop(@_, '&'); }
sub _OR_r8_r8  { _binop(@_, '|'); }
sub _XOR_r8_r8 { _binop(@_, '^'); }
sub _SBC_r8_r8 { _SUB_r8_r8(@_, shift()->register('F')->getC()); }
sub _SUB_r8_r8 {
    my($self, $r1, $r2, $c) = @_;
    # $r1 is always A, $r2 is A/B/C/D/EH/L/(HL)/W
    # $c is defined if this is really SBC
    die("Can't SUB with Z reg") if($r2 eq 'Z');
    $c ||= 0;
    if($r2 eq '(HL)') {
        my @addr = map { $self->register($_)->get()} qw(L H);
        _LD_r8_ind($self, 'W', @addr);
        $r2 = 'W';
    }
    $self->register($r1)->sub($self->register($r2)->get() + $c);
}
sub _CP_r8_r8 {
    # $r1 is always A, $r2 is A/B/C/D/EH/L/(HL)/W
    my($self, $r1, $r2) = @_;

    # bleh, CP uses the *operand* to set flags 3 and 5, instead of
    # the result, so wrap SUB and correct afterwards
    if($r2 eq '(HL)') {
        my @addr = map { $self->register($_)->get()} qw(L H);
        _LD_r8_ind($self, 'W', @addr);
        $r2 = 'W';
    }
    # and this is why we can't SUB with the Z reg
    _LD_r8_r8($self, 'Z', $r1); # preserve r1
    _SUB_r8_r8($self, $r1, $r2);
    _LD_r8_r8($self, $r1, 'Z'); # restore r1
    $self->register('F')->set5($self->register($r2)->get() & 0b100000);
    $self->register('F')->set3($self->register($r2)->get() & 0b1000);
}
sub _DEC {
    my($self, $r, $d) = @_;
    _LD_r8_indHL($self, 'W', $d) if($r eq '(HL)');
    $self->register($r)->dec();
    _LD_indHL_r8($self, 'W', $d) if($r eq '(HL)');
    # my($self, $r) = @_;
    # if($r eq '(HL)') {
    #     my @addr = map { $self->register($_)->get()} qw(L H);
    #     _LD_r8_ind($self, 'W', @addr);
    #     $self->register('W')->dec();
    #     _LD_ind_r8($self, 'W', @addr);
    # } else {
    #     $self->register($r)->dec();
   #  }
}
sub _EXX {
    my $self = shift;
    _swap_regs($self, qw(BC BC_));
    _swap_regs($self, qw(DE DE_));
    _swap_regs($self, qw(HL HL_));

}
sub _DJNZ {
    my($self, $offset) = @_;

    _LD_r8_r8($self, 'W', 'F');          # preserve flags

    $self->register('B')->dec();         # decrement B and ...
    if($self->register('B')->get()) {    # jump if not zero
        $self->register('PC')->set(
            $self->register('PC')->get() +
            ALU_getsigned($offset, 8)
        );
    }
    _LD_r8_r8($self, 'F', 'W');          # restore flags
}
sub _HALT { shift()->register('PC')->dec(); sleep(1) }
sub _INC {
    my($self, $r, $d) = @_;
    _LD_r8_indHL($self, 'W', $d) if($r eq '(HL)');
    $self->register($r)->inc();
    _LD_indHL_r8($self, 'W', $d) if($r eq '(HL)');
}
sub _JR_unconditional {
    my($self, $offset) = @_;
    $self->register('PC')->set(
        $self->register('PC')->get() +
        ALU_getsigned($offset, 8)
    );
}
sub _JP_unconditional {
    _LD_r16_imm(shift(), 'PC', @_);
}
sub _CALL_unconditional {
    _PUSH($_[0], 'PC');
    _JP_unconditional(@_);
}
sub _LD_ind_r16 {
    my($self, $r16, @bytes) = @_;
    $self->memory()->poke16($bytes[0] + 256 * $bytes[1], $self->register($r16)->get())
}
sub _LD_ind_r8 {
    my($self, $r8, @bytes) = @_;
    $self->memory()->poke($bytes[0] + 256 * $bytes[1], $self->register($r8)->get())
}
sub _LD_indHL_r8 {
    my($self, $r8, $d) = @_;
    $d = ALU_getsigned($d || 0, 8);
    $self->memory()->poke($d + $self->register('HL')->get(), $self->register($r8)->get())
}
sub _LD_indr16_r8 {
    my($self, $r16, $r8) = @_;
    $self->memory()->poke($self->register($r16)->get(), $self->register($r8)->get());
}
sub _LD_r16_imm {
    # self, register, lo, hi
    my $self = shift;
    $self->register(shift())->set(shift() + 256 * shift());
}
sub _LD_r8_imm {
    # self, register, data
    my($self, $r8, $byte) = @_;
    $r8 eq '(HL)'
        ? $self->memory()->poke($self->register('HL')->get(), $byte)
        : $self->register($r8)->set($byte)
}
sub _LD_r16_ind {
    my($self, $r16, @bytes) = @_;
    $self->register($r16)->set($self->memory()->peek16($bytes[0] + 256 * $bytes[1]));
}
sub _LD_r8_indr16 {
    my($self, $r8, $r16) = @_;
    $self->register($r8)->set($self->memory()->peek($self->register($r16)->get()));
}
sub _LD_r8_ind {
    my($self, $r8, @bytes) = @_;
    $self->register($r8)->set($self->memory()->peek($bytes[0] + 256 * $bytes[1]));
}
sub _LD_r8_indHL {
    my($self, $r8, $d) = @_;
    $d = ALU_getsigned($d || 0, 8);
    $self->register($r8)->set($self->memory()->peek($d + $self->register('HL')->get()));
}
sub _LD_r16_r16 {
    my($self, $r1, $r2) = @_;
    $self->register($r1)->set($self->register($r2)->get());
}
sub _LD_r8_r8 {
    my($self, $r1, $r2) = @_;
    if($r2 eq '(HL)') {
        my @addr = map { $self->register($_)->get()} qw(L H);
        _LD_r8_ind($self, 'W', @addr);
        $r2 = 'W';
    }
    if($r1 eq '(HL)') {
        my @addr = map { $self->register($_)->get()} qw(L H);
        _LD_ind_r8($self, $r2, @addr);
    } else {
        $self->register($r1)->set($self->register($r2)->get());
    }
}
sub _NOP { }
sub _RLCA {
    my $self = shift;
    $self->register('A')->set(
        (($self->register('A')->get() & 0b01111111) << 1) |
        (($self->register('A')->get() & 0b10000000) >> 7)
    );
    $self->register('F')->resetH();
    $self->register('F')->resetN();
    $self->register('F')->set5($self->register('A')->get() & 0b100000);
    $self->register('F')->set3($self->register('A')->get() & 0b1000);
    $self->register('F')->setC($self->register('A')->get() & 1);
}
sub _RRCA {
    my $self = shift;
    $self->register('A')->set(
        (($self->register('A')->get() & 0b11111110) >> 1) |
        (($self->register('A')->get() & 1) << 7)
    );
    $self->register('F')->resetH();
    $self->register('F')->resetN();
    $self->register('F')->set5($self->register('A')->get() & 0b100000);
    $self->register('F')->set3($self->register('A')->get() & 0b1000);
    $self->register('F')->setC($self->register('A')->get() & 0x80);
}
sub _RLA {
    my $self = shift;
    my $msb = $self->register('A')->get() & 0b10000000;
    $self->register('A')->set(
        (($self->register('A')->get() & 0b01111111) << 1) |
        $self->register('F')->getC()
    );
    $self->register('F')->setC($msb);
    $self->register('F')->resetH();
    $self->register('F')->resetN();
}
sub _RRA {
    my $self = shift;
    my $lsb = $self->register('A')->get() & 1;
    my $c = $self->register('F')->getC();
    $self->register('A')->set(
        (($self->register('A')->get() & 0b11111110) >> 1) |
        ($c << 7)
    );
    $self->register('F')->setC($lsb);
    $self->register('F')->resetH();
    $self->register('F')->resetN();
}
# generic wrapper for CB prefixed ROTs - wrap around A-reg version
# and also diddle P/S/Z flags
sub _cb_rot {
    my($self, $fn, $r) = @_;
    my @addr;
    if($r eq '(HL)') {
        @addr = map { $self->register($_)->get()} qw(L H);
        _LD_r8_ind($self, 'W', @addr);
        $r = 'W';
    }
    _swap_regs($self, $r, 'A') if($r ne 'A'); # preserve A, mv r to A
    $fn->($self);
    _swap_regs($self, $r, 'A') if($r ne 'A'); # swap back again
    _LD_ind_r8($self, 'W', @addr) if($r eq 'W');
    # now frob extra flags
    $self->register('F')->setP(ALU_parity($self->register($r)->get()));
    $self->register('F')->setS($self->register($r)->get() & 0x80);
    $self->register('F')->setZ($self->register($r)->get() == 0);
}
sub _RRC { my($self, $r) = @_; _cb_rot($self, \&_RRCA, $r); }
sub _RLC { my($self, $r) = @_; _cb_rot($self, \&_RLCA, $r); }
sub _RL {
    my($self, $r) = @_;
    _cb_rot($self, \&_RLA, $r); # also loads W with (HL) if needed
    $r = 'W' if($r eq '(HL)');
    # these two aren't done by _cb_rot
    $self->register('F')->set5($self->register($r)->get() &0b100000);
    $self->register('F')->set3($self->register($r)->get() &0b1000);
}
sub _RR {
    my($self, $r) = @_;
    _cb_rot($self, \&_RRA, $r); # also loads W with (HL) if needed
    $r = 'W' if($r eq '(HL)');
    $self->register('F')->set5($self->register($r)->get() &0b100000);
    $self->register('F')->set3($self->register($r)->get() &0b1000);
}
sub _SLA {
    my($self, $r) = @_;
    my @addr;
    if($r eq '(HL)') {
        @addr = map { $self->register($_)->get()} qw(L H);
        _LD_r8_ind($self, 'W', @addr);
        $r = 'W';
    }

    $self->register('F')->setC($self->register($r)->get() & 0x80);
    $self->register($r)->set($self->register($r)->get() << 1);
    $self->register('F')->setZ($self->register($r)->get() == 0);
    $self->register('F')->set5($self->register($r)->get() & 0b100000);
    $self->register('F')->set3($self->register($r)->get() & 0b1000);
    $self->register('F')->setP(ALU_parity($self->register($r)->get()));
    $self->register('F')->setS($self->register($r)->get() & 0x80);
    $self->register('F')->resetH();
    $self->register('F')->resetN();
    _LD_ind_r8($self, 'W', @addr) if($r eq 'W');
}
sub _SLL {
    my($self, $r) = @_;
    my @addr;
    if($r eq '(HL)') {
        @addr = map { $self->register($_)->get()} qw(L H);
        _LD_r8_ind($self, 'W', @addr);
        $r = 'W';
    }
    _SLA(@_);
    $self->register($r)->set($self->register($r)->get() | 1);
    $self->register('F')->setP(ALU_parity($self->register($r)->get()));
    _LD_ind_r8($self, 'W', @addr) if($r eq 'W');
}
sub _SRA {
    my($self, $r) = @_;
    my @addr;
    if($r eq '(HL)') {
        @addr = map { $self->register($_)->get()} qw(L H);
        _LD_r8_ind($self, 'W', @addr);
        $r = 'W';
    }

    $self->register('F')->setC($self->register($r)->get() & 1);
    $self->register($r)->set(
        ($self->register($r)->get() & 0x80) |
        ($self->register($r)->get() >> 1)
    );
    $self->register('F')->setZ($self->register($r)->get() == 0);
    $self->register('F')->set5($self->register($r)->get() & 0b100000);
    $self->register('F')->set3($self->register($r)->get() & 0b1000);
    $self->register('F')->setP(ALU_parity($self->register($r)->get()));
    $self->register('F')->setS($self->register($r)->get() & 0x80);
    $self->register('F')->resetH();
    $self->register('F')->resetN();
    _LD_ind_r8($self, 'W', @addr) if($r eq 'W');
}
sub _SRL {
    my($self, $r) = @_;
    my @addr;
    if($r eq '(HL)') {
        @addr = map { $self->register($_)->get()} qw(L H);
        _LD_r8_ind($self, 'W', @addr);
        $r = 'W';
    }
    _SRA(@_);
    $self->register($r)->set($self->register($r)->get() & 0x7F);
    $self->register('F')->setS($self->register($r)->get() & 0x80);
    $self->register('F')->setP(ALU_parity($self->register($r)->get()));
    _LD_ind_r8($self, 'W', @addr) if($r eq 'W');
}
sub _DAA {
    my $self = shift;
    my $a = $self->register('A');
    my $f = $self->register('F');
    my($n, $h, $lo, $hi) =
        ($f->getN(), $f->getH(),
         $a->get() & 0x0F, ($a->get() >> 4) & 0x0F);
    my $table = [
        # NB this table comes from Sean Young's "The Undocumented
        # Z80 Documented".  Zaks is wrong.
        # http://www.z80.info/zip/z80-documented.pdf
        #   C high   H low add Cafter
        [qw(0 0-9    0 0-9 0   0)],
        [qw(0 0-9    1 0-9 6   0)],
        [qw(0 0-8    . a-f 6   0)],
        [qw(0 a-f    0 0-9 60  1)],
        [qw(1 0-9a-f 0 0-9 60  1)],
        [qw(1 0-9a-f 1 0-9 66  1)],
        [qw(1 0-9a-f . a-f 66  1)],
        [qw(0 9a-f   . a-f 66  1)],
        [qw(0 a-f    1 0-9 66  1)],
    ];
    foreach my $row (@{$table}) {
        my @row = @{$row};
        if(
            $f->getC() == $row[0] &&
            ($row[2] eq '.' || $f->getH() == $row[2]) &&
            sprintf('%x', ($a->get() >> 4) & 0x0F) =~ /^[$row[1]]$/ &&
            sprintf('%x', $a->get() & 0x0F) =~ /^[$row[3]]$/
        ) {
            $f->getN() ? $a->set(ALU_sub8($f, $a->get(), hex($row[4])))
                       : $a->set(ALU_add8($f, $a->get(), hex($row[4])));
            $f->setC($row[5]);
            last;
        }
    }
    $f->setH($lo > 9) if(!$n);
    $f->resetH()      if($n && !$h);
    $f->setH($lo < 6) if($n && $h);

    $f->set3($a->get() & 0b1000);
    $f->set5($a->get() & 0b100000);
}
sub _CPL {
    my $self = shift;
    $self->register('A')->set(~ $self->register('A')->get());
    $self->register('F')->setH();
    $self->register('F')->setN();
    $self->register('F')->set3($self->register('A')->get() & 0b1000);
    $self->register('F')->set5($self->register('A')->get() & 0b100000);
}
sub _SCF {
    my $self = shift();
    my $f = $self->register('F');
    my $a = $self->register('A');
    $f->setC();
    $f->resetH();
    $f->resetN();
    $f->set5($a->get() & 0b100000);
    $f->set3($a->get() & 0b1000);
}
sub _CCF {
    my $self = shift;
    my $f = $self->register('F');
    my $a = $self->register('A');
    $f->setH($f->getC());
    $f->setC(!$f->getC());
    $f->resetN();
    $f->set5($a->get() & 0b100000);
    $f->set3($a->get() & 0b1000);
}
sub _POP {
    my($self, $r) = @_;
    $self->register($r)->set(
        $self->memory()->peek16($self->register('SP')->get())
    );
    $self->register('SP')->add(2);
}
sub _PUSH {
    my($self, $r) = @_;
    $self->register('SP')->sub(2);
    $self->memory()->poke16(
        $self->register('SP')->get(),
        $self->register($r)->get()
    );
}
sub _OUT_n_A {}
sub _IN_A_n {}
sub _DI {}
sub _EI {}
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
