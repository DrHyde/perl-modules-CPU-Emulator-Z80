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

my $test = 0;
foreach my $yamlfile (@tests) {
    $test++;
    my $y = YAML::Tiny->read("$yamlfile.in.yml");
    my $cpu = CPU::Emulator::Z80->new();
    foreach my $r (grep { $_ ne 'I' } keys %{$y->[0]->{registers}}) {
        $cpu->register($r)->set($y->[0]->{registers}->{$r});
    }
    my $pokes = 0;
    foreach my $addr (keys %{$y->[0]->{mem}}) {
        foreach(@{$y->[0]->{mem}->{$addr}}) {
            $cpu->memory()->poke($addr++, $_);
            $pokes++;
        }
    }
    $cpu->run($pokes); # run however many values we poked

    $y = YAML::Tiny->read("$yamlfile.expected.yml");
    my $errors = "";
    foreach my $r (grep { $_ ne 'I' } keys %{$y->[0]->{registers}}) {
        if($cpu->register($r)->get() != $y->[0]->{registers}->{$r}) {
            $errors .=
              "# Register $r differs.\n".
              "#   should be ".sprintf('0x%04X', $y->[0]->{registers}->{$r})."\n".
              "#   but is    ".sprintf('0x%04X', $cpu->register($r)->get())."\n";
        }
    }
    foreach my $addr (keys %{$y->[0]->{mem}}) {
        foreach(@{$y->[0]->{mem}->{$addr}}) {
            if($cpu->memory()->peek($addr) != $_) {
                $errors .=
                  "# Memory location ".sprintf('0x%04X', $addr)." differs\n".
                  "#   should be ".sprintf('0x%02X', $_)."\n".
                  "#   but is    ".sprintf('0x%02X', $cpu->memory()->peek($addr))."\n";
            }
        }
    }
    print ''.($errors ? 'not ' : '')."ok $test $y->[0]->{name}\n";
    print $errors;
}
