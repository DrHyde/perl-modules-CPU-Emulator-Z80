use strict;
$^W = 1;

use Test::More tests => 2;

use CPU::Emulator::Z80::Memory::Banked;

my $memory = CPU::Emulator::Z80::Memory::Banked->new();

ok($memory->poke16(0x1000, 256) && $memory->peek16(0x1000) == 256,
    "Can peek and poke 16 bit values");
ok($memory->peek(0x1000) == 0 && $memory->peek(0x1001) == 1,
    "I can has correct endianness!");

print $memory->peek16(0x1000)."\n";
