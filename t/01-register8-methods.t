use strict;
$^W = 1;

use Test::More tests => 8;

use CPU::Emulator::Z80::Register8;

my $f = CPU::Emulator::Z80::Register8->new();
ok($f->isa('CPU::Emulator::Z80::Register'),
   "Inheritance tree is hunky-dory");

$f->set(0b10101010);
ok($f->get() == 0b10101010, "get() and set() work");

$f->set(-100);
ok($f->get() == ((100 - 1) ^ 0xFF), sprintf("-ve is twos-complement shiny: %d == %d == %#08b", $f->get(), $f->getneg(), $f->get()));
ok($f->getneg() == -100, "getneg works");

$f->set(-128);
ok($f->get() == 128 && $f->getneg() == -128, "can set a maximally -ve value");

$f->set(300); # out of range +ve
ok($f->get() == (300 & 0xFF), "out-of-range set gets truncated at 8 bits using the API");

ok($f->{value} == (300 & 0xFF), "it's even truncated internally when set()ing ...");
$f->{value} = 300;
ok($f->get() == (300 & 0xFF), "... and when get()ing");
