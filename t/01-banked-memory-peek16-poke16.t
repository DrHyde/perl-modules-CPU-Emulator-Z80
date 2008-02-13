use strict;
$^W = 1;

use Test::More tests => 3;

use CPU::Emulator::Z80::Memory::Banked;

my $memory = CPU::Emulator::Z80::Memory::Banked->new();
ok($memory->poke16(0x1000, 258) && $memory->peek16(0x1000) == 258,
    "Can peek and poke 16 bit values");
ok($memory->peek(0x1000) == 2 && $memory->peek(0x1001) == 1,
    "Little-endian works");

$memory = CPU::Emulator::Z80::Memory::Banked->new(
    endianness => 'BIG'
);
$memory->poke16(0x1000, 258);
ok($memory->peek(0x1000) == 1 && $memory->peek(0x1001) == 2,
    "Big-endian works");
