use strict;
$^W = 1;

use Test::More tests => 7;

use CPU::Emulator::Z80;

my $cpu = CPU::Emulator::Z80->new();
my $m = $cpu->memory();

$cpu->interrupt();
ok(!$cpu->{INTERRUPT}, "Interrupts are ignored when not enabled");

$cpu->run(1); # set PC to 1
$cpu->_EI();
$cpu->interrupt();
ok($cpu->{INTERRUPT}, "Interrupt flag raised on an int when they're enabled");

$cpu->run(1);
ok($cpu->register('PC')->get() == 0x0038, "soft interrupts vector OK");
ok($cpu->register('SP')->get() == 0xFFFE, "SP get diddled right");
ok($m->peek16($cpu->register('SP')->get()) == 1, "PC pushed correctly");

$cpu->_DI();
$cpu->interrupt();
ok(!$cpu->{INTERRUPT}, "Interrupts can be disabled");
$cpu->run(1);
ok($cpu->register('PC')->get() == 0x0039, "execution continues at the right place");

