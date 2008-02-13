use strict;
$^W = 1;

use Test::More tests => 5;

use CPU::Emulator::Z80::Memory::Banked;

unlink 'newfile.ram';
my $memory = CPU::Emulator::Z80::Memory::Banked->new(file => 'newfile.ram');

# NB using {0xHEXSTUFF} in regexes doesn't work.
# and the repeated {30000}...{30000} is cos there's a 2^15 - 2 limit

$/ = undef;
open(my $fh, 'newfile.ram') || die("Couldn't open newfile.ram\n");
ok(<$fh> =~ /^\000{30000}\000{30000}\000{5536}$/, "New file created as all zeroes");
close($fh);
ok($memory->peek(0) == 0, "Peek confirms a zero");
ok($memory->poke(0, 1) && $memory->peek(0) == 1, "Poke works ...");
open($fh, 'newfile.ram');
ok(<$fh> =~ /^\001\000{30000}\000{30000}\000{5535}$/, "... and is reflected in the file");
close($fh);

undef $memory;

my $newmemory = CPU::Emulator::Z80::Memory::Banked->new(file => 'newfile.ram');
ok($newmemory->peek(0) == 1, "RAM can be initialised correctly from a file");

unlink 'newfile.ram';
