use strict;
$^W = 1;

use Test::More tests => 14;

undef $/;

use CPU::Emulator::Z80::Memory::Banked;

unlink 'ramfile.ram', 'romfile.rom';
my $memory = CPU::Emulator::Z80::Memory::Banked->new(file => 'ramfile.ram');

# NB using {0xHEXSTUFF} in regexes doesn't work.
# and the repeated {30000}...{30000} is cos there's a 2^15 - 2 limit

open(my $fh, '>', 'romfile.rom') || die("Can't create ROM file for testing\n");
print $fh 'This is a ROM';
close($fh);

eval { $memory->bank(
    address => 0,
    size    => length('This is a ROM'), 
    type    => 'ROM'
) };
ok($@, "Can't map ROM without a filename");

$memory->bank(
    address => 0,
    size    => length('This is a ROM'), 
    type    => 'ROM',
    file    => 'romfile.rom'
);
ok($memory->peek(0) == ord('T'), "peek returns data from the ROM");
ok($memory->poke(0, 1) == 0, "poke returns 0 when we try to write ...");
ok($memory->peek(0) == ord('T'), "... and really didn't change anything");
ok($memory->poke(0xFFFF, 1) && $memory->peek(0xFFFF) == 1, "We can still write elsewhere in RAM");

$memory->unbank(address => 0);
ok($memory->peek(0) == 0, "poking to ROM is ignored when writethrough isn't enabled");

$memory->bank(
    address => 1,
    size    => length('This is a ROM'), 
    type    => 'ROM',
    file    => 'romfile.rom',
    writethrough => 1
);
ok($memory->peek(0) == 0 && $memory->peek(1) == ord('T'), "Loading a ROM at a random address puts it at the right place");
$memory->poke(1, 1);
$memory->unbank(address => 1);
ok($memory->peek(1) == 1, "With writethrough, RAM gets updated");

open($fh, 'romfile.rom') || die("Can't read romfile.rom\n");
ok(<$fh> eq 'This is a ROM', "ROM files don't get altered");
close($fh);
open($fh, 'ramfile.ram') || die("Can't read ramfile.ram\n");
ok(<$fh> =~ /^\000\001\000{30000}\000{30000}\000{5533}\001$/, "With writethrough, RAM file gets updated correctly");

$memory->bank(
    address => 0,
    size    => length('This is a ROM'), 
    type    => 'ROM',
    file    => 'romfile.rom'
);
$memory->bank(
    address => 6,
    size    => length('This is a ROM'), 
    type    => 'ROM',
    file    => 'romfile.rom'
);
ok($memory->peek(0) == 0, "Loading a new overlay starting in an older one unloads the old one ...");
ok($memory->peek(6) == ord('T'), "... and loads the new one");

$memory->bank(
    address => 6,
    size    => length('This is a ROM'), 
    type    => 'ROM',
    file    => 'romfile.rom'
);
$memory->bank(
    address => 0,
    size    => length('This is a ROM'), 
    type    => 'ROM',
    file    => 'romfile.rom'
);
ok($memory->peek(5 + length('This is a ROM')) == 0, "Loading a new overlay finishing in an older one unloads the old one ...");
ok($memory->peek(0) == ord('T'), "... and loads the new one");

unlink 'ramfile.ram', 'romfile.rom';
