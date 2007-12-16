package CPU::Emulator::Z80;

use warnings;
use strict;

use Scalar::Util qw(blessed);

sub new {
    my($class, %args) = @_;

    $args{memory} ||= (0) x 65536;

    if(length($args{memory}) != 65536) {
        if(blessed($args{memory}) eq 'Emulator::Z80::Memory' ...
    }
}
