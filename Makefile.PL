use ExtUtils::MakeMaker;
WriteMakefile(
		NAME	=> 'CPU::Emulator::Z80',
		VERSION => '1.0',
		PREREQ_PM => {
		    'CPU::Emulator::Memory::Banked' => 0
		}
	     );
