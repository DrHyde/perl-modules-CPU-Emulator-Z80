# $Id: 04-FUSE-tests.t,v 1.7 2008/02/23 00:23:10 drhyde Exp $
# FUSE tester is at http://fuse-emulator.svn.sourceforge.net/viewvc/fuse-emulator/trunk/fuse/z80/coretest.c?revision=3414&view=markup

use strict;
$^W = 1;

opendir(my $dir, 't/fuse-tests') || die("Can't read t/fuse-tests/\n");
my @tests = map { s/\.in\.yml$//; "t/fuse-tests/$_"; }
            grep { -f "t/fuse-tests/$_" && /\.in\.yml$/ }
            readdir($dir);
closedir($dir);

print "1..".scalar(@tests)."\n";

use CPU::Emulator::Z80;
use YAML::Tiny;

my(%testinstrs, %testnames) = ();
my $fh;
open($fh, 't/fuse-tests/testinstrs') && do {
    foreach(<$fh>) {
        chomp;
        my($name, $instrs) = split(/;/, $_);
        $testinstrs{$name} = $instrs;
    }
    close($fh);
};
open($fh, 't/fuse-tests/testnames') && do {
    foreach(<$fh>) {
        chomp;
        my($opcodes, $desc) = split(/;/, $_);
        $opcodes =~ s/[^0-9a-f]//gi;
        $testnames{lc($opcodes)} = $desc;
    }
    close($fh);
};

my $test = 0;
foreach my $yamlfile (@tests) {
    $test++;
    my $y = YAML::Tiny->read("$yamlfile.in.yml");
    my $cpu = CPU::Emulator::Z80->new(
        memory => CPU::Emulator::Memory->new(
            bytes => "\xDE\xAD\xBE\xEF" x (65536 / 4)
        )
    );
    foreach my $r (grep { $_ ne 'I' } keys %{$y->[0]->{registers}}) {
        $cpu->register($r)->set($y->[0]->{registers}->{$r});
    }
    foreach my $addr (keys %{$y->[0]->{mem}}) {
        foreach(@{$y->[0]->{mem}->{$addr}}) {
            $cpu->memory()->poke($addr, $_);
            $addr++;
        }
    }

    my $beforememory = $cpu->memory()->{contents}; # FIXME - internals
    my $beforeregs = $cpu->format_registers();
    $cpu->run($testinstrs{$y->[0]->{name}} || 1); # execute this many instructions

    $y = YAML::Tiny->read("$yamlfile.expected.yml");
    my $errors = "";
    foreach my $r (grep { $_ ne 'I' } keys %{$y->[0]->{registers}}) {
        if($cpu->register($r)->get() != $y->[0]->{registers}->{$r}) {
            $errors .=
              "# Register $r differs.".
              (($r eq 'AF') ? '        SZ5H3PNC' : '')."\n".
              "#   should be ".sprintf('0x%04X', $y->[0]->{registers}->{$r}).
                  (($r eq 'AF') ? sprintf(" flags: 0b%08b\n", $y->[0]->{registers}->{$r} & 0xFF) : "\n").
              "#   but is    ".sprintf('0x%04X', $cpu->register($r)->get()).
                  (($r eq 'AF') ? sprintf(" flags: 0b%08b\n", $cpu->register('F')->get()) : "\n");
        }
    }
    if($errors) {
        $errors .= "#\n# started with\n".$beforeregs.
                   "#\n# finished with\n".$cpu->format_registers()
    }

    foreach my $addr (keys %{$y->[0]->{mem}}) {
        foreach(@{$y->[0]->{mem}->{$addr}}) {
            if($cpu->memory()->peek($addr) != $_) {
                $errors .=
                  "# Memory location ".sprintf('0x%04X', $addr)." differs\n".
                  "#   should be ".sprintf('0x%02X', $_)."\n".
                  "#   but is    ".sprintf('0x%02X', $cpu->memory()->peek($addr)).
                  (($cpu->memory()->peek($addr) == ord(substr($beforememory, $addr, 1))) ? ' (unchanged)' : '').
                  "\n";
            }
            $addr++;
        }
    }
    print ''.($errors ? 'not ' : '')."ok $test -\t\t".uc($y->[0]->{name}).": $testnames{$y->[0]->{name}}\n";
    print $errors;
    last if($errors);
}
