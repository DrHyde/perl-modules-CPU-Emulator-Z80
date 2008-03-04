# $Id: Assembler.pm,v 1.3 2008/03/04 23:06:27 drhyde Exp $

package CPU::Emulator::Z80::Assembler;

use strict;
use warnings;

use vars qw($VERSION @EXPORT);

$VERSION = '1.0';

use Data::Dumper;
use base qw(Exporter);

@EXPORT = qw(z80asm);

=head1 NAME

CPU::Emulator::Z80::Assembler - a Z80 assembler

=head1 SYNOPSIS

    z80asm(
        in  => 'blah.z80',
        out => 'blah.rom'
    );

    my $binary = z80asm(in => 'blah.z80');

    my $binary = z80asm(q{
        ORG 0x1000
        LD A, 1
        ...
    });

=head1 DESCRIPTION

This module provides a single subroutine which implements a Z80
assembler.

=head1 EXPORTS

By default the 'z80asm' subroutine is exported.  To disable that, do:

    use CPU::Emulator::Z80::Assembler ();

=head1 FUNCTIONS

=head2 z80asm

This takes one parameter, a string of Z80 assembler source.  It
returns the assembled version as a string, with any gaps padded
with NULLs if necessary.

=head1 SYNTAX

=cut

sub z80asm {
    my $source = shift();
    local $/ = "\n";

    my %mapping = ();
    while(my $line = <DATA>) {
        $line =~ s/^\s+|\s$//g;
        my $localmapping = \%mapping;
        my($mnemonic, $bytes) = split(/\s+;\s+/, $line);
        my @words = split(/[\s,]/, $mnemonic);
        my @bytes = split(/\s+/, $bytes);
        while(@words) {
            my $word = shift(@words);
            $localmapping->{$word} = {} unless($localmapping->{$word});
            if(@words) {
                $localmapping = $localmapping->{$word};
            } else {
                # NB RET vs RET C etc
                $localmapping->{$word}->{BYTES} = \@bytes;
            }
        }
    }
    print Dumper(\%mapping);
}


# careful - LD (IX+XX),XX
#         - JR, DJNZ
__DATA__
        LD BC,XXXX                ; 01 XX XX
        LD (BC),A               ; 02
        DEC B                   ; 05
        RLCA                    ; 07
        EX AF,AF'               ; 08
        ADD HL,BC               ; 09
        LD A,(BC)               ; 0A
        DEC BC                  ; 0B
        DEC C                   ; 0D
        RRCA                    ; 0F
        LD DE,XXXX                ; 11 XX XX
        LD (DE),A               ; 12
        DEC D                   ; 15
        RLA                     ; 17
        JR XX                  ; 18 XX
        ADD HL,DE               ; 19
        LD A,(DE)               ; 1A
        DEC DE                  ; 1B
        DEC E                   ; 1D
        RRA                     ; 1F
        LD HL,XXXX                ; 21 XX XX
        LD (XXXX),HL              ; 22 XX XX
        DEC H                   ; 25
        DAA                     ; 27
        JR Z,XX                ; 28 XX
        ADD HL,HL               ; 29
        LD HL,(XXXX)              ; 2A XX XX
        DEC HL                  ; 2B
        DEC L                   ; 2D
        CPL                     ; 2F
        LD SP,XXXX                ; 31 XX XX
        LD (XXXX),A               ; 32 XX XX
        DEC (HL)                ; 35
        SCF                     ; 37
        JR C,XX                ; 38 XX
        ADD HL,SP               ; 39
        LD A,(XXXX)               ; 3A XX XX
        DEC SP                  ; 3B
        DEC A                   ; 3D
        CCF                     ; 3F
        LD B,B                  ; 40
        LD B,C                  ; 41
        LD B,D                  ; 42
        LD B,E                  ; 43
        LD B,H                  ; 44
        LD B,L                  ; 45
        LD B,(HL)               ; 46
        LD B,A                  ; 47
        LD C,B                  ; 48
        LD C,C                  ; 49
        LD C,D                  ; 4A
        LD C,E                  ; 4B
        LD C,H                  ; 4C
        LD C,L                  ; 4D
        LD C,(HL)               ; 4E
        LD C,A                  ; 4F
        LD D,B                  ; 50
        LD D,C                  ; 51
        LD D,D                  ; 52
        LD D,E                  ; 53
        LD D,H                  ; 54
        LD D,L                  ; 55
        LD D,(HL)               ; 56
        LD D,A                  ; 57
        LD E,B                  ; 58
        LD E,C                  ; 59
        LD E,D                  ; 5A
        LD E,E                  ; 5B
        LD E,H                  ; 5C
        LD E,L                  ; 5D
        LD E,(HL)               ; 5E
        LD E,A                  ; 5F
        LD H,B                  ; 60
        LD H,C                  ; 61
        LD H,D                  ; 62
        LD H,E                  ; 63
        LD H,H                  ; 64
        LD H,L                  ; 65
        LD H,(HL)               ; 66
        LD H,A                  ; 67
        LD L,B                  ; 68
        LD L,C                  ; 69
        LD L,D                  ; 6A
        LD L,E                  ; 6B
        LD L,H                  ; 6C
        LD L,L                  ; 6D
        LD L,(HL)               ; 6E
        LD L,A                  ; 6F
        LD (HL),B               ; 70
        LD (HL),C               ; 71
        LD (HL),D               ; 72
        LD (HL),E               ; 73
        LD (HL),H               ; 74
        LD (HL),L               ; 75
        HALT                    ; 76
        LD (HL),A               ; 77
        LD A,B                  ; 78
        LD A,C                  ; 79
        LD A,D                  ; 7A
        LD A,E                  ; 7B
        LD A,H                  ; 7C
        LD A,L                  ; 7D
        LD A,(HL)               ; 7E
        LD A,A                  ; 7F
        ADD A,B                 ; 80
        ADD A,C                 ; 81
        ADD A,D                 ; 82
        ADD A,E                 ; 83
        ADD A,H                 ; 84
        ADD A,L                 ; 85
        ADD A,(HL)              ; 86
        ADD A,A                 ; 87
        ADC A,B                 ; 88
        ADC A,C                 ; 89
        ADC A,D                 ; 8A
        ADC A,E                 ; 8B
        ADC A,H                 ; 8C
        ADC A,L                 ; 8D
        ADC A,(HL)              ; 8E
        ADC A,A                 ; 8F
        SUB B                   ; 90
        SUB C                   ; 91
        SUB D                   ; 92
        SUB E                   ; 93
        SUB H                   ; 94
        SUB L                   ; 95
        SUB (HL)                ; 96
        SUB A                   ; 97
        SBC B                   ; 98
        SBC C                   ; 99
        SBC D                   ; 9A
        SBC E                   ; 9B
        SBC H                   ; 9C
        SBC L                   ; 9D
        SBC (HL)                ; 9E
        SBC A                   ; 9F
        XOR B                   ; A8
        XOR C                   ; A9
        XOR D                   ; AA
        XOR E                   ; AB
        XOR H                   ; AC
        XOR L                   ; AD
        XOR (HL)                ; AE
        XOR A                   ; AF
        OR B                    ; B0
        OR C                    ; B1
        OR D                    ; B2
        OR E                    ; B3
        OR H                    ; B4
        OR L                    ; B5
        OR (HL)                 ; B6
        OR A                    ; B7
        CP B                    ; B8
        CP C                    ; B9
        CP D                    ; BA
        CP E                    ; BB
        CP H                    ; BC
        CP L                    ; BD
        CP (HL)                 ; BE
        CP A                    ; BF
        POP BC                  ; C1
        JP XXXX                  ; C3 XX XX
        PUSH BC                 ; C5
        RST 0                   ; C7
        RET Z                   ; C8
        RET                     ; C9
        JP Z,XXXX                ; CA XX XX
        RLC B                   ; CB 00
        RLC C                   ; CB 01
        RLC D                   ; CB 02
        RLC E                   ; CB 03
        RLC H                   ; CB 04
        RLC L                   ; CB 05
        RLC (HL)                ; CB 06
        RLC A                   ; CB 07
        RRC B                   ; CB 08
        RRC C                   ; CB 09
        RRC D                   ; CB 0A
        RRC E                   ; CB 0B
        RRC H                   ; CB 0C
        RRC L                   ; CB 0D
        RRC (HL)                ; CB 0E
        RRC A                   ; CB 0F
        RL  B                   ; CB 10
        RL  C                   ; CB 11
        RL  D                   ; CB 12
        RL  E                   ; CB 13
        RL  H                   ; CB 14
        RL  L                   ; CB 15
        RL  (HL)                ; CB 16
        RL  A                   ; CB 17
        RR  B                   ; CB 18
        RR  C                   ; CB 19
        RR  D                   ; CB 1A
        RR  E                   ; CB 1B
        RR  H                   ; CB 1C
        RR  L                   ; CB 1D
        RR  (HL)                ; CB 1E
        RR  A                   ; CB 1F
        SLA B                   ; CB 20
        SLA C                   ; CB 21
        SLA D                   ; CB 22
        SLA E                   ; CB 23
        SLA H                   ; CB 24
        SLA L                   ; CB 25
        SLA (HL)                ; CB 26
        SLA A                   ; CB 27
        SRA B                   ; CB 28
        SRA C                   ; CB 29
        SRA D                   ; CB 2A
        SRA E                   ; CB 2B
        SRA H                   ; CB 2C
        SRA L                   ; CB 2D
        SRA (HL)                ; CB 2E
        SRA A                   ; CB 2F
        SRL B                   ; CB 38
        SRL C                   ; CB 39
        SRL D                   ; CB 3A
        SRL E                   ; CB 3B
        SRL H                   ; CB 3C
        SRL L                   ; CB 3D
        SRL (HL)                ; CB 3E
        SRL A                   ; CB 3F
        BIT 0,B                 ; CB 40
        BIT 0,C                 ; CB 41
        BIT 0,D                 ; CB 42
        BIT 0,E                 ; CB 43
        BIT 0,H                 ; CB 44
        BIT 0,L                 ; CB 45
        BIT 0,(HL)              ; CB 46
        BIT 0,A                 ; CB 47
        BIT 1,B                 ; CB 48
        BIT 1,C                 ; CB 49
        BIT 1,D                 ; CB 4A
        BIT 1,E                 ; CB 4B
        BIT 1,H                 ; CB 4C
        BIT 1,L                 ; CB 4D
        BIT 1,(HL)              ; CB 4E
        BIT 1,A                 ; CB 4F
        BIT 2,B                 ; CB 50
        BIT 2,C                 ; CB 51
        BIT 2,D                 ; CB 52
        BIT 2,E                 ; CB 53
        BIT 2,H                 ; CB 54
        BIT 2,L                 ; CB 55
        BIT 2,(HL)              ; CB 56
        BIT 2,A                 ; CB 57
        BIT 3,B                 ; CB 58
        BIT 3,C                 ; CB 59
        BIT 3,D                 ; CB 5A
        BIT 3,E                 ; CB 5B
        BIT 3,H                 ; CB 5C
        BIT 3,L                 ; CB 5D
        BIT 3,(HL)              ; CB 5E
        BIT 3,A                 ; CB 5F
        BIT 4,B                 ; CB 60
        BIT 4,C                 ; CB 61
        BIT 4,D                 ; CB 62
        BIT 4,E                 ; CB 63
        BIT 4,H                 ; CB 64
        BIT 4,L                 ; CB 65
        BIT 4,(HL)              ; CB 66
        BIT 4,A                 ; CB 67
        BIT 5,B                 ; CB 68
        BIT 5,C                 ; CB 69
        BIT 5,D                 ; CB 6A
        BIT 5,E                 ; CB 6B
        BIT 5,H                 ; CB 6C
        BIT 5,L                 ; CB 6D
        BIT 5,(HL)              ; CB 6E
        BIT 5,A                 ; CB 6F
        BIT 6,B                 ; CB 70
        BIT 6,C                 ; CB 71
        BIT 6,D                 ; CB 72
        BIT 6,E                 ; CB 73
        BIT 6,H                 ; CB 74
        BIT 6,L                 ; CB 75
        BIT 6,(HL)              ; CB 76
        BIT 6,A                 ; CB 77
        BIT 7,B                 ; CB 78
        BIT 7,C                 ; CB 79
        BIT 7,D                 ; CB 7A
        BIT 7,E                 ; CB 7B
        BIT 7,H                 ; CB 7C
        BIT 7,L                 ; CB 7D
        BIT 7,(HL)              ; CB 7E
        BIT 7,A                 ; CB 7F
        RES 0,B                 ; CB 80
        RES 0,C                 ; CB 81
        RES 0,D                 ; CB 82
        RES 0,E                 ; CB 83
        RES 0,H                 ; CB 84
        RES 0,L                 ; CB 85
        RES 0,(HL)              ; CB 86
        RES 0,A                 ; CB 87
        RES 1,B                 ; CB 88
        RES 1,C                 ; CB 89
        RES 1,D                 ; CB 8A
        RES 1,E                 ; CB 8B
        RES 1,H                 ; CB 8C
        RES 1,L                 ; CB 8D
        RES 1,(HL)              ; CB 8E
        RES 1,A                 ; CB 8F
        RES 2,B                 ; CB 90
        RES 2,C                 ; CB 91
        RES 2,D                 ; CB 92
        RES 2,E                 ; CB 93
        RES 2,H                 ; CB 94
        RES 2,L                 ; CB 95
        RES 2,(HL)              ; CB 96
        RES 2,A                 ; CB 97
        RES 3,B                 ; CB 98
        RES 3,C                 ; CB 99
        RES 3,D                 ; CB 9A
        RES 3,E                 ; CB 9B
        RES 3,H                 ; CB 9C
        RES 3,L                 ; CB 9D
        RES 3,(HL)              ; CB 9E
        RES 3,A                 ; CB 9F
        RES 4,B                 ; CB A0
        RES 4,C                 ; CB A1
        RES 4,D                 ; CB A2
        RES 4,E                 ; CB A3
        RES 4,H                 ; CB A4
        RES 4,L                 ; CB A5
        RES 4,(HL)              ; CB A6
        RES 4,A                 ; CB A7
        RES 5,B                 ; CB A8
        RES 5,C                 ; CB A9
        RES 5,D                 ; CB AA
        RES 5,E                 ; CB AB
        RES 5,H                 ; CB AC
        RES 5,L                 ; CB AD
        RES 5,(HL)              ; CB AE
        RES 5,A                 ; CB AF
        RES 6,B                 ; CB B0
        RES 6,C                 ; CB B1
        RES 6,D                 ; CB B2
        RES 6,E                 ; CB B3
        RES 6,H                 ; CB B4
        RES 6,L                 ; CB B5
        RES 6,(HL)              ; CB B6
        RES 6,A                 ; CB B7
        RES 7,B                 ; CB B8
        RES 7,C                 ; CB B9
        RES 7,D                 ; CB BA
        RES 7,E                 ; CB BB
        RES 7,H                 ; CB BC
        RES 7,L                 ; CB BD
        RES 7,(HL)              ; CB BE
        RES 7,A                 ; CB BF
        SET 0,B                 ; CB C0
        SET 0,C                 ; CB C1
        SET 0,D                 ; CB C2
        SET 0,E                 ; CB C3
        SET 0,H                 ; CB C4
        SET 0,L                 ; CB C5
        SET 0,(HL)              ; CB C6
        SET 0,A                 ; CB C7
        SET 1,B                 ; CB C8
        SET 1,C                 ; CB C9
        SET 1,D                 ; CB CA
        SET 1,E                 ; CB CB
        SET 1,H                 ; CB CC
        SET 1,L                 ; CB CD
        SET 1,(HL)              ; CB CE
        SET 1,A                 ; CB CF
        SET 2,B                 ; CB D0
        SET 2,C                 ; CB D1
        SET 2,D                 ; CB D2
        SET 2,E                 ; CB D3
        SET 2,H                 ; CB D4
        SET 2,L                 ; CB D5
        SET 2,(HL)              ; CB D6
        SET 2,A                 ; CB D7
        SET 3,B                 ; CB D8
        SET 3,C                 ; CB D9
        SET 3,D                 ; CB DA
        SET 3,E                 ; CB DB
        SET 3,H                 ; CB DC
        SET 3,L                 ; CB DD
        SET 3,(HL)              ; CB DE
        SET 3,A                 ; CB DF
        SET 4,B                 ; CB E0
        SET 4,C                 ; CB E1
        SET 4,D                 ; CB E2
        SET 4,E                 ; CB E3
        SET 4,H                 ; CB E4
        SET 4,L                 ; CB E5
        SET 4,(HL)              ; CB E6
        SET 4,A                 ; CB E7
        SET 5,B                 ; CB E8
        SET 5,C                 ; CB E9
        SET 5,D                 ; CB EA
        SET 5,E                 ; CB EB
        SET 5,H                 ; CB EC
        SET 5,L                 ; CB ED
        SET 5,(HL)              ; CB EE
        SET 5,A                 ; CB EF
        SET 6,B                 ; CB F0
        SET 6,C                 ; CB F1
        SET 6,D                 ; CB F2
        SET 6,E                 ; CB F3
        SET 6,H                 ; CB F4
        SET 6,L                 ; CB F5
        SET 6,(HL)              ; CB F6
        SET 6,A                 ; CB F7
        SET 7,B                 ; CB F8
        SET 7,C                 ; CB F9
        SET 7,D                 ; CB FA
        SET 7,E                 ; CB FB
        SET 7,H                 ; CB FC
        SET 7,L                 ; CB FD
        SET 7,(HL)              ; CB FE
        SET 7,A                 ; CB FF
        CALL Z,XXXX               ; CC XX XX
        CALL XXXX                 ; CD XX XX
        RST 8H                  ; CF 
        POP DE                  ; D1
        PUSH DE                 ; D5
        RST 10H                 ; D7
        RET C                   ; D8
        EXX                     ; D9
        JP C,XXXX                 ; DA XX XX
        CALL C,XXXX               ; DC XX XX
        ADD IX,BC               ; DD 09
        ADD IX,DE               ; DD 19
        LD IX,XXXX                ; DD 21 XX XX
        LD (XXXX),IX              ; DD 22 XX XX
        ADD IX,IX               ; DD 29
        LD IX,(XXXX)              ; DD 2A XX XX
        DEC IX                  ; DD 2B
        ADD IX,SP               ; DD 39
        POP IX                  ; DD E1
        EX (SP),IX              ; DD E3
        PUSH IX                 ; DD E5
        JP (IX)                 ; DD E9
        LD SP,IX                ; DD F9
        RST 18H                 ; DF
        RET PO                  ; E0
        POP HL                  ; E1
        JP PO,XXXX               ; E2 XX XX
        EX (SP),HL              ; E3
        CALL PO,XXXX              ; E4 XX XX
        PUSH HL                 ; E5
        RST 20H                 ; E7
        RET PE                  ; E8
        JP (HL)                 ; E9
        JP PE,XXXX               ; EA XX XX
        EX DE,HL                ; EB
        CALL PE,XXXX              ; EC XX XX
        OUT (C),B               ; ED 41
        SBC HL,BC               ; ED 42
        LD (XXXX),BC              ; ED 43 XX XX
        IM 0                    ; ED 46
        LD I,A                  ; ED 47
        OUT (C),C               ; ED 49
        ADC HL,BC               ; ED 4A
        LD BC,(XXXX)              ; ED 4B XX XX
        RETI                    ; ED 4D
        OUT (C),D               ; ED 51
        SBC HL,DE               ; ED 52
        LD (XXXX),DE              ; ED 53 XX XX
        IM 1                    ; ED 56
        LD A,I                  ; ED 57
        OUT (C),E               ; ED 59
        ADC HL,DE               ; ED 5A
        LD DE,(XXXX)              ; ED 5B XX XX
        IM 2                    ; ED 5E
        OUT (C),H               ; ED 61
        SBC HL,HL               ; ED 62
        RRD                     ; ED 67
        OUT (C),L               ; ED 69
        ADC HL,HL               ; ED 6A
        RLD                     ; ED 6F
        SBC HL,SP               ; ED 72
        LD (XXXX),SP              ; ED 73 XX XX
        OUT (C),A               ; ED 79
        ADC HL,SP               ; ED 7A
        LD SP,(XXXX)              ; ED 7B XX XX
        LDI                     ; ED A0
        CPI                     ; ED A1
        OUTI                    ; ED A3
        LDD                     ; ED A8
        CPD                     ; ED A9
        OUTD                    ; ED AB
        LDIR                    ; ED B0
        CPIR                    ; ED B1
        OTIR                    ; ED B3
        LDDR                    ; ED B8
        CPDR                    ; ED B9
        OTDR                    ; ED BB
        RST 28H                 ; EF
        RET P                   ; F0
        POP AF                  ; F1
        JP P,XXXX                ; F2 XX XX
        DI                      ; F3
        CALL P,XXXX               ; F4 XX XX
        PUSH AF                 ; F5
        RST 30H                 ; F7
        RET M                   ; F8
        LD SP,HL                ; F9
        JP M,XXXX                ; FA XX XX
        EI                      ; FB
        CALL M,XXXX               ; FC XX XX
        ADD IY,BC               ; FD 09
        ADD IY,DE               ; FD 19
        LD IY,XXXX                ; FD 21 XX XX
        LD (XXXX),IY              ; FD 22 XX XX
        ADD IY,IY               ; FD 29
        LD IY,(XXXX)              ; FD 2A XX XX
        DEC IY                  ; FD 2B
        ADD IY,SP               ; FD 39
        POP IY                  ; FD E1
        EX (SP),IY              ; FD E3
        PUSH IY                 ; FD E5
        JP (IY)                 ; FD E9
        LD SP,IY                ; FD F9
        RST 38H                 ; FF
        NOP                     ; 00
        INC BC                  ; 03
        INC B                   ; 04
        LD B,XX                  ; 06 XX
        INC C                   ; 0C
        LD C,XX                  ; 0E XX
        DJNZ XX                ; 10 XX
        INC DE                  ; 13
        INC D                   ; 14
        LD D,XX                  ; 16 XX
        INC E                   ; 1C
        LD E,XX                  ; 1E XX
        JR NZ,XX               ; 20 XX
        INC HL                  ; 23
        INC H                   ; 24
        LD H,XX                  ; 26 XX
        INC L                   ; 2C
        LD L,XX                  ; 2E XX
        JR NC,XX               ; 30 XX
        INC SP                  ; 33
        INC (HL)                ; 34
        LD (HL),XX               ; 36 XX
        INC A                   ; 3C
        LD A,XX                  ; 3E XX
        AND B                   ; A0
        AND C                   ; A1
        AND D                   ; A2
        AND E                   ; A3
        AND H                   ; A4
        AND L                   ; A5
        AND (HL)                ; A6
        AND A                   ; A7
        RET NZ                  ; C0
        JP NZ,XXXX               ; C2 XX XX
        CALL NZ,XXXX              ; C4 XX XX
        ADD A,XX                 ; C6 XX
        ADC A,XX                 ; CE XX
        RET NC                  ; D0
        JP NC,XXXX               ; D2 XX XX
        OUT (XX),A               ; D3 XX
        CALL NC,XXXX              ; D4 XX XX
        CALL NC,XXXX              ; D4 XX XX
        SUB XX                   ; D6 XX
        IN A,(XX)                ; DB XX
        INC IX                  ; DD 23
        INC (IX+XX)              ; DD 34 XX
        DEC (IX+XX)              ; DD 35 XX
        LD (IX+XX),XX             ; DD 36 XX XX
        LD B,(IX+XX)             ; DD 46 XX
        LD C,(IX+XX)             ; DD 4E XX
        LD D,(IX+XX)             ; DD 56 XX
        LD E,(IX+XX)             ; DD 5E XX
        LD H,(IX+XX)             ; DD 66 XX
        LD L,(IX+XX)             ; DD 6E XX
        LD (IX+XX),B             ; DD 70 XX
        LD (IX+XX),C             ; DD 71 XX
        LD (IX+XX),D             ; DD 72 XX
        LD (IX+XX),E             ; DD 73 XX
        LD (IX+XX),H             ; DD 74 XX
        LD (IX+XX),L             ; DD 75 XX
        LD (IX+XX),A             ; DD 77 XX
        LD A,(IX+XX)             ; DD 7E XX
        ADD A,(IX+XX)            ; DD 86 XX
        ADC A,(IX+XX)            ; DD 8E XX
        SUB (IX+XX)              ; DD 96 XX
        SBC A,(IX+XX)            ; DD 9E XX
        AND (IX+XX)              ; DD A6 XX
        XOR (IX+XX)              ; DD AE XX
        OR (IX+XX)               ; DD B6 XX
        CP (IX+XX)               ; DD BE XX
        RLC (IX+XX)              ; DD CB XX 06
        RRC (IX+XX)              ; DD CB XX 0E
        RL (IX+XX)               ; DD CB XX 16
        RR (IX+XX)               ; DD CB XX 1E
        SLA (IX+XX)              ; DD CB XX 26
        SRA (IX+XX)              ; DD CB XX 2E
        BIT 0,(IX+XX)            ; DD CB XX 46
        BIT 1,(IX+XX)            ; DD CB XX 4E
        BIT 2,(IX+XX)            ; DD CB XX 56
        BIT 3,(IX+XX)            ; DD CB XX 5E
        BIT 4,(IX+XX)            ; DD CB XX 66
        BIT 5,(IX+XX)            ; DD CB XX 6E
        BIT 6,(IX+XX)            ; DD CB XX 76
        BIT 7,(IX+XX)            ; DD CB XX 7E
        RES 0,(IX+XX)            ; DD CB XX 86
        RES 1,(IX+XX)            ; DD CB XX 8E
        RES 2,(IX+XX)            ; DD CB XX 96
        RES 3,(IX+XX)            ; DD CB XX 9E
        RES 4,(IX+XX)            ; DD CB XX A6
        RES 5,(IX+XX)            ; DD CB XX AE
        RES 6,(IX+XX)            ; DD CB XX B6
        RES 7,(IX+XX)            ; DD CB XX BE
        SET 0,(IX+XX)            ; DD CB XX C6
        SET 1,(IX+XX)            ; DD CB XX CE
        SET 2,(IX+XX)            ; DD CB XX D6
        SET 3,(IX+XX)            ; DD CB XX DE
        SET 4,(IX+XX)            ; DD CB XX E6
        SET 5,(IX+XX)            ; DD CB XX EE
        SET 6,(IX+XX)            ; DD CB XX F6
        SET 7,(IX+XX)            ; DD CB XX FE
        SBC A,XX                 ; DE XX
        AND XX                   ; E6 XX
        IN B,(C)                ; ED 40
        NEG                     ; ED 44
        RETN                    ; ED 45
        IN C,(C)                ; ED 48
        IN D,(C)                ; ED 50
        IN E,(C)                ; ED 58
        IN H,(C)                ; ED 60
        IN L,(C)                ; ED 68
        IN A,(C)                ; ED 78
        INI                     ; ED A2
        IND                     ; ED AA
        INIR                    ; ED B2
        INDR                    ; ED BA
        XOR XX                   ; EE XX
        OR XX                    ; F6 XX
        INC IY                  ; FD 23
        INC (IY+XX)              ; FD 34 XX
        DEC (IY+XX)              ; FD 35 XX
        LD (IY+XX),XX             ; FD 36 XX XX
        LD B,(IY+XX)             ; FD 46 XX
        LD C,(IY+XX)             ; FD 4E XX
        LD D,(IY+XX)             ; FD 56 XX
        LD E,(IY+XX)             ; FD 5E XX
        LD H,(IY+XX)             ; FD 66 XX
        LD L,(IY+XX)             ; FD 6E XX
        LD (IY+XX),B             ; FD 70 XX
        LD (IY+XX),C             ; FD 71 XX
        LD (IY+XX),D             ; FD 72 XX
        LD (IY+XX),E             ; FD 73 XX
        LD (IY+XX),H             ; FD 74 XX
        LD (IY+XX),L             ; FD 75 XX
        LD (IY+XX),A             ; FD 77 XX
        LD A,(IY+XX)             ; FD 7E XX
        ADD A,(IY+XX)            ; FD 86 XX
        ADC A,(IY+XX)            ; FD 8E XX
        SUB (IY+XX)              ; FD 96 XX
        SBC A,(IY+XX)            ; FD 9E XX
        AND (IY+XX)              ; FD A6 XX
        XOR (IY+XX)              ; FD AE XX
        OR (IY+XX)               ; FD B6 XX
        CP (IY+XX)               ; FD BE XX
        RLC (IY+XX)              ; FD CB XX 06
        RRC (IY+XX)              ; FD CB XX 0E
        RL (IY+XX)               ; FD CB XX 16
        RR (IY+XX)               ; FD CB XX 1E
        SLA (IY+XX)              ; FD CB XX 26
        SRA (IY+XX)              ; FD CB XX 2E
        BIT 0,(IY+XX)            ; FD CB XX 46
        BIT 1,(IY+XX)            ; FD CB XX 4E
        BIT 2,(IY+XX)            ; FD CB XX 56
        BIT 3,(IY+XX)            ; FD CB XX 5E
        BIT 4,(IY+XX)            ; FD CB XX 66
        BIT 5,(IY+XX)            ; FD CB XX 6E
        BIT 6,(IY+XX)            ; FD CB XX 76
        BIT 7,(IY+XX)            ; FD CB XX 7E
        RES 0,(IY+XX)            ; FD CB XX 86
        RES 1,(IY+XX)            ; FD CB XX 8E
        RES 2,(IY+XX)            ; FD CB XX 96
        RES 3,(IY+XX)            ; FD CB XX 9E
        RES 4,(IY+XX)            ; FD CB XX A6
        RES 5,(IY+XX)            ; FD CB XX AE
        RES 6,(IY+XX)            ; FD CB XX B6
        RES 7,(IY+XX)            ; FD CB XX BE
        SET 0,(IY+XX)            ; FD CB XX C6
        SET 1,(IY+XX)            ; FD CB XX CE
        SET 2,(IY+XX)            ; FD CB XX D6
        SET 3,(IY+XX)            ; FD CB XX DE
        SET 4,(IY+XX)            ; FD CB XX E6
        SET 5,(IY+XX)            ; FD CB XX EE
        SET 6,(IY+XX)            ; FD CB XX F6
        SET 7,(IY+XX)            ; FD CB XX FE
        CP XX                    ; FE XX
