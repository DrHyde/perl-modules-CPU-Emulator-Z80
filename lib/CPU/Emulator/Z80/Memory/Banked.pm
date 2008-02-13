# $Id: Banked.pm,v 1.8 2008/02/13 22:12:10 drhyde Exp $

package CPU::Emulator::Z80::Memory::Banked;

use strict;
use warnings;

use vars qw($VERSION);

$VERSION = '1.0';

local $SIG{__DIE__} = sub {
    die(__PACKAGE__.": $_[0]\n");
};

=head1 NAME

CPU::Emulator::Z80::Memory::Banked - banked memory for the Z80 emulator

=head1 SYNOPSIS

    my $memory = CPU::Emulator::Z80::Memory::Banked->new();
    $memory->poke(0xBEEF, ord('s'));
    
    my $value = $memory->peek(0xBEEF); # 115 == ord('s')

    $memory->bank(
        address      => 0x8000,
        size         => 0x4000,
        type         => 'ROM',
        file         => '.../somerom.rom',
        writethrough => 1
    );

    my $value = $memory->peek(0xBEEF); # read from ROM instead
    $memory->poke(0xBEEF, 0);          # write to underlying RAM

=head1 DESCRIPTION

This class provides a flat 64K array of values.  You can then
temporarily replace chunks of that array with other arrays, to
simulate bank-switching.  Those chunks can be of arbitrary size,
and can be either RAM, ROM, or 'dynamic', meaning that instead
of being just dumb storage, when you read or write them perl code
gets run.

=head1 METHODS

=head2 new

The constructor returns an object representing a flat 64K memory
space addressable by byte.  It takes two optional named parameters:

=over

=item file

if provided, will provide a disc-based backup of the
RAM represented.  This file will be read when the object is created
(if it exists) and written whenever anything is altered.  If no
file exists or no filename is provided, then memory is initialised
to all zeroes.  If the file exists it must be writeable and of the
correct size.

=item endianness

defaults to LITTLE, can be set to BIG.  This matters for the peek16
and poke16 methods.

=back

=cut

sub new {
    my($class, %params) = @_;
    my $bytes = chr(0) x 0x10000;
    if(exists($params{file})) {
        if(-e $params{file}) {
            $bytes = _readRAM($params{file}, 0x10000);
        } else {
            _writeRAM($params{file}, $bytes)
        }
    }
    return bless(
        {
            contents => $bytes,
            overlays => [],
            ($params{file} ? (file => $params{file}) : ()),
            endianness => $params{endianness} || 'LITTLE'
        },
        __PACKAGE__
    );
}

=head2 bank

This method performs a bank switch.  This changes your view of
the memory, mapping another block of memory in place of part of the
main RAM.  The main RAM's contents are preserved (although see
below for an exception).  It takes several named parameters, three
of which are compulsory:

=over

=item address

The base address at which to swap in the extra bank of memory.

=item size

The size of the bank to swap.  This means that you'll be swapping
addresses $base_address to $base_address + $size - 1.

=item type

Either 'ROM' (for read-only memory), or 'RAM' to swap in a block of
RAM.  Support will be added in the future for type 'dynamic' which
will let you run arbitrary perl code for reads and writes to/from
the memory.

=back

When you change memory banks, any banks already loaded which would
overlap are unloaded.

The following optional parameters are also supported:

=over

=item file

A file which backs the memory.  For ROM memory this is compulsory,
for RAM it is optional.

=item writethrough

This is only meaningful for ROM.  If set, then any writes to the
addresses affected will be directed through to the underlying main
RAM.  Otherwise writes will be ignored.

=item function_read and function_write

Coderefs which will be called when 'dynamic' memory is read/written.
Both are compulsory for 'dynamic' memory.  They are called with a
reference to the memory object, the address being accessed, and
(for function_write) the byte to be written.  function_read should
return a byte.  function_write's return value is ignored.

=back

=cut

sub bank {
    my($self, %params) = @_;
    my($address, $size, $type) = @params{qw(address size type)};
    foreach (qw(address size type)) {
        die("bank: No $_ specified\n")
            if(!exists($params{$_}));
    }

    my $contents ='';
    if($type eq 'ROM') {
        die("For ROM banks you need to specify a file\n")
            unless(exists($params{file}));
        $contents = _readROM($params{file}, $size);
    } elsif($type eq 'RAM') {
        $contents = (exists($params{file}))
            ? _readRAM($params{file}, $size)
            : chr(0) x $size;
    } elsif($type eq 'dynamic') {
        die("For dynamic banks you need to specify function_read and function_write\n")
            unless(exists($params{function_read}) && exists($params{function_write}));
    }
    foreach my $bank (@{$self->{overlays}}) {
        if(
            (      # does an older bank start in the middle of this one?
                $bank->{address} >= $address &&
                $bank->{address} < $address + $size
            ) || ( # does this start in the middle of an older bank?
                $address >=  $bank->{address} &&
                $address < $bank->{address} + $bank->{size}
            )
        ) { $self->unbank(address => $bank->{address}) }
    }
    push @{$self->{overlays}}, {
        address  => $address,
        size     => $size,
        type     => $type,
        (length($contents) ? (contents => $contents) : ()),
        (exists($params{file}) ? (file => $params{file}) : ()),
        (exists($params{writethrough}) ? (writethrough => $params{writethrough}) : ()),
        (exists($params{function_read}) ? (function_read => $params{function_read}) : ()),
        (exists($params{function_write}) ? (function_write => $params{function_write}) : ())
    };
}

=head2 unbank

This method unloads a bank of memory, making the main RAM visible
again at the affected addresses.  It takes a single named parameter
'address' to tell which bank to switch.

=cut

sub unbank {
    my($self, %params) = @_;
    die("unbank: No address specified\n") unless(exists($params{address}));
    $self->{overlays} = [
        grep { $_->{address} != $params{address} }
        @{$self->{overlays}}
    ];
}

=head2 peek, peek8

This method takes a single parameter, an address from 0 to 0xFFFF.
It returns the value stored at that address, taking account of what
secondary memory banks are active.  'peek8' is simply another name
for the same function, the suffix indicating that it returns an 8
bit (ie one byte) value.

=head2 peek16

As peek and peek8, except it returns a 16 bit value.  This is where
endianness matters.

=cut

sub peek { ord(_peek(@_)); }
sub peek8 { peek(@_); }
sub peek16 {
    my($self, $address) = @_;
    # assume little-endian
    my $r = $self->peek($address) + 256 * $self->peek($address + 1);
    # swap bytes if necessary
    if($self->{endianness} eq 'BIG') {
        $r = (($r & 0xFF) << 8) + int($r / 256);
    }
    return $r;
}
sub _peek {
    my($self, $addr) = @_;
    die("Address $addr out of range") if($addr< 0 || $addr > 0xFFFF);
    foreach my $bank (@{$self->{overlays}}) {
        if(
            $bank->{address} <= $addr &&
            $bank->{address} + $bank->{size} > $addr
        ) {
            if($bank->{type} eq 'dynamic') {
                return chr($bank->{function_read}->($self, $addr));
            } else {
                return substr($bank->{contents}, $addr - $bank->{address}, 1);
            }
        }
    }
    return substr($self->{contents}, $addr, 1);
}

=head2 poke, poke8

This method takes two parameters, an address and a byte value.
The value is written to the address, into whichever bank is currently
selected at that address.  If that address is ROM and writethrough is
enabled for that bank, then the value is written to the main memory.

It returns 1 if something was written, or 0 if nothing was written.

=head2 poke16

This method takes two parameters, an address and a 16-bit value.
The value is written to memory as two bytes at the address specified
and the following one.  This is where endianness matters.
If either
byte is ROM and writethrough is enabled for that bank, then
that address's value is written to the main memory.

Return values are undefined.

=cut

sub poke8 { poke(@_); }
sub poke16 {
    my($self, $addr, $value) = @_;
    # if BIGendian, swap bytes, ...
    if($self->{endianness} eq 'BIG') {
        $value = (($value & 0xFF) << 8) + int($value / 256);
    }
    # write in little-endian order
    $self->poke($addr, $value & 0xFF);
    $self->poke($addr + 1, ($value >> 8));
}
sub poke {
    my($self, $addr, $value) = @_;
    die("Value $value out of range") if($value < 0 || $value > 255);
    die("Address $addr out of range") if($addr< 0 || $addr > 0xFFFF);
    $value = chr($value);
    foreach my $bank (@{$self->{overlays}}) {
        if(
            $bank->{address} <= $addr &&
            $bank->{address} + $bank->{size} > $addr
        ) {
            if($bank->{type} eq 'RAM') {
                substr($bank->{contents}, $addr - $bank->{address}, 1) = $value;
                _writeRAM($bank->{file}, $bank->{contents})
                    if(exists($bank->{file}));
                return 1;
            } elsif($bank->{type} eq 'ROM' && $bank->{writethrough}) {
                substr($self->{contents}, $addr, 1) = $value;
                _writeRAM($self->{file}, $self->{contents})
                    if(exists($self->{file}));
                return 1;
            } elsif($bank->{type} eq 'ROM') {
                return 0;
            } elsif($bank->{type} eq 'dynamic') {
                return $bank->{function_write}->($self, $addr, ord($value));
            } else {
                die("Type ".$bank->{type}." NYI");
            }
        }
    }
    substr($self->{contents}, $addr, 1) = $value;
    _writeRAM($self->{file}, $self->{contents})
        if(exists($self->{file}));
    return 1;
}

# input: filename, required size
# output: file contents, or fatal error
sub _read_file { 
    my($file, $size) = @_;
    local $/ = undef;
    open(my $fh, $file) || die("Couldn't read $file\n");
    my $contents = <$fh>;
    die("$file is wrong size\n") unless(length($contents) == $size);
    close($fh);
    return $contents;
}

sub _readROM { _read_file(@_); }

# input: filename, required size
# output: file contents, or fatal error
sub _readRAM {
    my($file, $size) = @_;
    my $contents = _read_file($file, $size);
    _writeRAM($file, $contents);
    return $contents;
}

# input: filename, data
# output: none, fatal on error
sub _writeRAM {
    my($file, $contents) = @_;
    open(my $fh, '>', $file) || die("Can't write $file\n");
    print $fh $contents || die("Can't write $file\n");
    close($fh);
}
