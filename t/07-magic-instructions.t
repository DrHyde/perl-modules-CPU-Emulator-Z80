use strict;
$^W = 1;

use Test::More tests => 8;

use CPU::Emulator::Z80;

my $cpu = CPU::Emulator::Z80->new();

foreach (0xDDDD, 0xDDFD, 0xFDDD, 0xFDFD) {
    $cpu->register('W')->set(0b01010101);
    $cpu->register('PC')->set(0);
    $cpu->memory()->poke16(0, 0);  # NOP; NOP
    $cpu->memory()->poke16(2, $_); # magic prefix
    $cpu->memory()->poke(4, 0xFF); #              LD W, 1
    $cpu->run(3);
    ok($cpu->register('W')->get() == 1, sprintf("Dispatch table works for 0x%04X prefix", $_));
}

foreach (0xDDDD, 0xDDFD, 0xFDDD, 0xFDFD) {
    $cpu->register('PC')->set(0);
    $cpu->memory()->poke16(2, $_); # magic prefix
    $cpu->memory()->poke(4, 0);    # STOP
    $cpu->memory()->poke(5, 0); # NOP; NOP
    $cpu->memory()->poke(6, 0); # NOP; NOP
    $cpu->run(7);
    ok($cpu->register('PC')->get() == 5, sprintf("0x%04X00 STOP instruction works", $_));
}
