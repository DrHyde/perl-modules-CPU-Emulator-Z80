use strict;
$^W = 1;

use Test::More tests => 1;
use CPU::Emulator::Z80;

my $cpu = CPU::Emulator::Z80->new();
my $m = $cpu->memory();

# ORG 0x200
# .table = 0x300
#
# LD HL, table
# LD B, 0
# .loop
#   BIT B, 0
#   ...

$cpu->run(10000);
my $z80parity = '';
$z80parity .= chr($m->peek($_)) foreach(0x300 .. 0x300 + 271);

# 16 per line, X=even, .=odd, 272 chars total
my $parity_table = 'X..X.XX..XX.X..X
.XX.X..XX..X.XX.
.XX.X..XX..X.XX.
X..X.XX..XX.X..X
.XX.X..XX..X.XX.
X..X.XX..XX.X..X
X..X.XX..XX.X..X
.XX.X..XX..X.XX.
.XX.X..XX..X.XX.
X..X.XX..XX.X..X
X..X.XX..XX.X..X
.XX.X..XX..X.XX.
X..X.XX..XX.X..X
.XX.X..XX..X.XX.
.XX.X..XX..X.XX.
X..X.XX..XX.X..X
';
ok($cpu->stopped() && $z80parity eq $parity_table, "can run a little program with no I/O");
