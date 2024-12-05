#include "olc6502.h"
#include "Bus.h"

olc6502::olc6502()
{
	// Below is a 16x16 matrix that has all the 256 instructions
	// It is arranged in such a way that the bottom 4bits represent the
	// Column and the top 4 bits represent the rows 
	// This is made according to that r650x and r651x pdf 
	using a = olc6502;
	lookup =
	{
		{ "BRK", &a::BRK, &a::IMM, 7 },{ "ORA", &a::ORA, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::ZP0, 3 },{ "ASL", &a::ASL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHP", &a::PHP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::IMM, 2 },{ "ASL", &a::ASL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABS, 4 },{ "ASL", &a::ASL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BPL", &a::BPL, &a::REL, 2 },{ "ORA", &a::ORA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ZPX, 4 },{ "ASL", &a::ASL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLC", &a::CLC, &a::IMP, 2 },{ "ORA", &a::ORA, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABX, 4 },{ "ASL", &a::ASL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "JSR", &a::JSR, &a::ABS, 6 },{ "AND", &a::AND, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "BIT", &a::BIT, &a::ZP0, 3 },{ "AND", &a::AND, &a::ZP0, 3 },{ "ROL", &a::ROL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLP", &a::PLP, &a::IMP, 4 },{ "AND", &a::AND, &a::IMM, 2 },{ "ROL", &a::ROL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "BIT", &a::BIT, &a::ABS, 4 },{ "AND", &a::AND, &a::ABS, 4 },{ "ROL", &a::ROL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BMI", &a::BMI, &a::REL, 2 },{ "AND", &a::AND, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ZPX, 4 },{ "ROL", &a::ROL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEC", &a::SEC, &a::IMP, 2 },{ "AND", &a::AND, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ABX, 4 },{ "ROL", &a::ROL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "RTI", &a::RTI, &a::IMP, 6 },{ "EOR", &a::EOR, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "EOR", &a::EOR, &a::ZP0, 3 },{ "LSR", &a::LSR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHA", &a::PHA, &a::IMP, 3 },{ "EOR", &a::EOR, &a::IMM, 2 },{ "LSR", &a::LSR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::ABS, 3 },{ "EOR", &a::EOR, &a::ABS, 4 },{ "LSR", &a::LSR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BVC", &a::BVC, &a::REL, 2 },{ "EOR", &a::EOR, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ZPX, 4 },{ "LSR", &a::LSR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLI", &a::CLI, &a::IMP, 2 },{ "EOR", &a::EOR, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ABX, 4 },{ "LSR", &a::LSR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "RTS", &a::RTS, &a::IMP, 6 },{ "ADC", &a::ADC, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ADC", &a::ADC, &a::ZP0, 3 },{ "ROR", &a::ROR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLA", &a::PLA, &a::IMP, 4 },{ "ADC", &a::ADC, &a::IMM, 2 },{ "ROR", &a::ROR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::IND, 5 },{ "ADC", &a::ADC, &a::ABS, 4 },{ "ROR", &a::ROR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BVS", &a::BVS, &a::REL, 2 },{ "ADC", &a::ADC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ZPX, 4 },{ "ROR", &a::ROR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEI", &a::SEI, &a::IMP, 2 },{ "ADC", &a::ADC, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ABX, 4 },{ "ROR", &a::ROR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "???", &a::NOP, &a::IMP, 2 },{ "STA", &a::STA, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZP0, 3 },{ "STA", &a::STA, &a::ZP0, 3 },{ "STX", &a::STX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "DEY", &a::DEY, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 2 },{ "TXA", &a::TXA, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "STY", &a::STY, &a::ABS, 4 },{ "STA", &a::STA, &a::ABS, 4 },{ "STX", &a::STX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "BCC", &a::BCC, &a::REL, 2 },{ "STA", &a::STA, &a::IZY, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZPX, 4 },{ "STA", &a::STA, &a::ZPX, 4 },{ "STX", &a::STX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "TYA", &a::TYA, &a::IMP, 2 },{ "STA", &a::STA, &a::ABY, 5 },{ "TXS", &a::TXS, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::NOP, &a::IMP, 5 },{ "STA", &a::STA, &a::ABX, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::XXX, &a::IMP, 5 },
		{ "LDY", &a::LDY, &a::IMM, 2 },{ "LDA", &a::LDA, &a::IZX, 6 },{ "LDX", &a::LDX, &a::IMM, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "LDY", &a::LDY, &a::ZP0, 3 },{ "LDA", &a::LDA, &a::ZP0, 3 },{ "LDX", &a::LDX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "TAY", &a::TAY, &a::IMP, 2 },{ "LDA", &a::LDA, &a::IMM, 2 },{ "TAX", &a::TAX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "LDY", &a::LDY, &a::ABS, 4 },{ "LDA", &a::LDA, &a::ABS, 4 },{ "LDX", &a::LDX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "BCS", &a::BCS, &a::REL, 2 },{ "LDA", &a::LDA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "LDY", &a::LDY, &a::ZPX, 4 },{ "LDA", &a::LDA, &a::ZPX, 4 },{ "LDX", &a::LDX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "CLV", &a::CLV, &a::IMP, 2 },{ "LDA", &a::LDA, &a::ABY, 4 },{ "TSX", &a::TSX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 4 },{ "LDY", &a::LDY, &a::ABX, 4 },{ "LDA", &a::LDA, &a::ABX, 4 },{ "LDX", &a::LDX, &a::ABY, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "CPY", &a::CPY, &a::IMM, 2 },{ "CMP", &a::CMP, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPY", &a::CPY, &a::ZP0, 3 },{ "CMP", &a::CMP, &a::ZP0, 3 },{ "DEC", &a::DEC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INY", &a::INY, &a::IMP, 2 },{ "CMP", &a::CMP, &a::IMM, 2 },{ "DEX", &a::DEX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "CPY", &a::CPY, &a::ABS, 4 },{ "CMP", &a::CMP, &a::ABS, 4 },{ "DEC", &a::DEC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BNE", &a::BNE, &a::REL, 2 },{ "CMP", &a::CMP, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ZPX, 4 },{ "DEC", &a::DEC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLD", &a::CLD, &a::IMP, 2 },{ "CMP", &a::CMP, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ABX, 4 },{ "DEC", &a::DEC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "CPX", &a::CPX, &a::IMM, 2 },{ "SBC", &a::SBC, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPX", &a::CPX, &a::ZP0, 3 },{ "SBC", &a::SBC, &a::ZP0, 3 },{ "INC", &a::INC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INX", &a::INX, &a::IMP, 2 },{ "SBC", &a::SBC, &a::IMM, 2 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::SBC, &a::IMP, 2 },{ "CPX", &a::CPX, &a::ABS, 4 },{ "SBC", &a::SBC, &a::ABS, 4 },{ "INC", &a::INC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BEQ", &a::BEQ, &a::REL, 2 },{ "SBC", &a::SBC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ZPX, 4 },{ "INC", &a::INC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SED", &a::SED, &a::IMP, 2 },{ "SBC", &a::SBC, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ABX, 4 },{ "INC", &a::INC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
	};
}
olc6502::~olc6502() {}
// Code for connecting bus will be here // 






// READ & WRITE FOR OLC6502 // 

// Implementing the olc6502 functions // 
// This is set default to false because some devices on bus tend to change the state
// When they are read from but the disassembler will want want to read the data
// Without changing the state of the device on the bus 
uint8_t olc6502::read(uint16_t a) {
	return bus->read(a, false);
}
void olc6502::write(uint16_t a, uint8_t d) {
	bus->write(a, d);
}

// RESET FUNCTION FOR OLC6502 // 

// This forces the 6502 CPU into a known and safe state
// The registers are set to 0x00, the status register is cleared 
// The unused bit is not cleared and it remains at 1 
// An absolute address is read from 0xFFFC which contains a second 
// address that the program counter is set to, this allows the programmer
// to jump to a known and programmable location for execution of statements 

void olc6502::reset()
{
	addr_abs = 0xFFFC;
	uint16_t lo = read(addr_abs + 0);
	uint16_t hi = read(addr_abs + 1);

	// Setting pc // 
	pc = (hi << 8) | lo;

	// Reset the internal registers // 
	a = 0;
	x = 0;
	y = 0;
	stkp = 0xFD;
	status = 0x00 | U;

	// Clear internal helper variables // 
	addr_rel = 0x0000;
	addr_abs = 0x0000;
	fetched = 0x00;

	// Reset the cycles // 
	cycles = 8;
}

// IRQ FUNCTION //

// Irq's happen only when the disable interrupt flag is set to 0
// They can happen at any time according to the above condition but we 
// don't want them to be destructive therefore the current instruction is 
// allowed to finish and then the current pc is stored on the stack
// after that the current status register is store on the stack and as soon 
// as the routine that services the interrupt is finished the pc and status 
// registers are again loaded 
// This works similar to reset() in some way because as soon as the IRQ has 
// finished the programmable address is read from 0xFFFE which is set to the pc

void olc6502::irq() {
	if (GetFlag(I) == 0) {

		// Pushing the pc to the stack //
		// That shit is 16bit so we need to make two pushes of 8bit //
		write(0x0100 + stkp, (pc >> 8) & 0x00FF); // Writing hi bytes // 
		// 0x00FF is 0000 0000 1111 1111 // So use the last 8 bytes for the access // 
		stkp--;
		write(0x0100 + stkp, pc & 0x00FF); // Writing lo bytes // 
		stkp--;

		// Pushing status register to the stack //
		SetFlag(B, 0);
		SetFlag(U, 1);
		SetFlag(I, 1);
		write(0x0100 + stkp, status);
		stkp--;

		// Reading the new pc location from fixed address //
		addr_abs = 0xFFFE;
		uint16_t lo = read(addr_abs + 0);
		uint16_t hi = read(addr_abs + 1);
		pc = (hi << 8) | lo;

		// Time taken for IRQs // 
		cycles = 7;
	}
}

// NON-MASKABLE INTERRUPTS // 

// This can't be ignored.
// Behaves just like the regular IRQ but rather this reads the pc address 
// from 0xFFFA

void olc6502::nmi()
{
	write(0x0100 + stkp, (pc >> 8) & 0x00FF);
	stkp--;
	write(0x0100 + stkp, pc & 0x00FF);
	stkp--;

	SetFlag(B, 0);
	SetFlag(U, 1);
	SetFlag(I, 1);
	write(0x0100 + stkp, status);
	stkp--;

	addr_abs = 0xFFFA;
	uint16_t lo = read(addr_abs + 0);
	uint16_t hi = read(addr_abs + 1);
	pc = (hi << 8) | lo;

	cycles = 8;
}

//	CLOCK FUNCTION // 
// To perform one clock cycle worth of emulation // 
// Each instruction requires a variable number of cycles to execute 
// But here I'm performing the entire calculation in one hit 
// I'm maintaining a cycle counter in order to stay compliant with the
// connected deviced, this sets up a delay 
// In short the counter reaches 0 then it means that the instruction is complete
// And the next one is ready to execute 

void olc6502::clock() {

	// Read the next instruction's bytes. The 8bit value is used to get the 
	// necessary information from the translation table on how to perform 
	// the instruction.
	if (cycles == 0) {
		opcode = read(pc);

#ifdef LOGMODE
		uint16_t log_pc = pc;
#endif
		// don't forget to set the unused status flag bit to 1 
		SetFlag(U, true);

		// increment the pc, we read the opcode byte 
		pc++;

		// Get the number of cycles 
		cycles = lookup[opcode].cycles;

		// fetch the intermediate data // 
		uint8_t additional_cycle1 = (this->*lookup[opcode].addrmode)();

		// Perform the operation // 
		uint8_t additional_cycle2 = (this->*lookup[opcode].operate)();

		// Also this addr_mode and opcode must have altered the number of cycle
		// Before the instruction has completed 

		cycles += (additional_cycle1 & additional_cycle2);

		// Again set unused to 1;
		SetFlag(U, true);

		// logger to dump every cycle the entire processer state for the ananlysis
#ifdef LOGMODE 
		if (logfile == nullptr) {
			logfile = fopen("olc6502.txt", "wt");
		}
		if (logfile != nullptr) {
			fprintf(logfile, "%10d:%02d PC:%04X %s A:%02X X:%02X Y:%02X %s%s%s%s%s%s%s%s STKP:%02X\n",
				clock_count = 0, log_pc, "XXX", a, x, y,
				GetFlag(N) ? "N" : ".",
				GetFlag(V) ? "V" : ".",
				GetFlag(U) ? "U" : ".",
				GetFlag(B) ? "B" : ".",
				GetFlag(D) ? "D" : ".",
				GetFlag(I) ? "I" : ".",
				GetFlag(Z) ? "Z" : ".",
				GetFlag(C) ? "C" : ".",
				stkp
			);
		}
#endif // LOGMODE 
	}
	// increment the global_clock_count // 
	clock_count++;
	// Decrement the number of cycles for the current instruction // 
	cycles--;
}



// FLAG FUNCTIONS //

// To return the value of a specific bit in the status register 
uint8_t olc6502::GetFlag(FLAGS6502 f)
{
	return ((status & f) > 0) ? 1 : 0;
}

// Set or clear the specific bit of a status register 
void olc6502::SetFlag(FLAGS6502 f, bool v) {
	if (v) { // Set a bit // 
		status |= f;
	}
	else { // Clear a bit, Set a bit to 0 // 
		status &= ~f;
	}
}

// ADDRESSING MODES //
// Addresses in 6502 are between 0x0000 and 0xFFFF.
// The high byte is mostly the page and the low byte is mostly the offset 
// If both the instruction and address function return 1 then it means 
// that an additional clock cycle is required 

// IMPLIED //
// No additional data is required
// We will target the accumulator with this for instructions like PHA 

uint8_t olc6502::IMP()
{
	fetched = a;
	return 0;
}

// IMMEDIATE // 
// The instruction expects the next byte to be used as the value
// I'll use the read address to point to the next byte 

uint8_t olc6502::IMM() {
	addr_abs = pc++; // next byte // 
	return 0;
}

// ZERO PAGE // 
// Allows to absolutely address a location in the first 0xFF bytes of address
// range. So this requires only one byte instead of two 

uint8_t olc6502::ZP0() {
	addr_abs = read(pc);
	pc++;
	addr_abs &= 0x00FF;
	return 0;
}

// ZERO PAGE, X OFFSET // 
// Same as the ZP0 but the contents of the X register are added to the supplied
// single byte address 

uint8_t olc6502::ZPX() {
	addr_abs = (read(pc) + x);
	pc++;
	addr_abs &= 0x00FF;
	return 0;
}

// ZERP PAGE, Y OFFSET // 
// Same as ZP0 but the contents of Y register are added to the supplied to the 
// single byte address

uint8_t olc6502::ZPY() {
	addr_abs = (read(pc) + y);
	pc++;
	addr_abs &= 0x00FF;
	return;
}

// RELATIVE //
// This is exclusive to branch instructions. The address range must reside b/w
// -128 to 127 of the branch instruction
// You can't directly branch to any address in tbe addressable range 

uint8_t olc6502::REL() {
	addr_abs = read(pc);
	pc++;
	// We are checking the MSB here if its 1, then the number is negative
	// So to represent it as 16bit signed integer we do its OR with FF00
	// If the MSB is 0 then the number if positive so there's no need for extension 
	if (addr_rel & 0x80){ // If the value here is 0 then it means that the number is positive and the 0x80 is not set,
		// if the value is something then the 0x80 is set and number is negative so we perform extension 
		addr_rel |= 0xFF00;
	}
	return 0;
}

// ABSOLUTE // 
// A full 16bit address is loaded and used // 
uint8_t olc6502::ABS() {
	// Just like reading pc address from a fixed address //
	uint16_t lo = read(pc); // read the first byte // 
	pc++;
	uint16_t hi = read(pc); // read the next byte after increment // 
	pc++;
	addr_abs = (hi << 8) | lo; // combines hi and lo to form a 16 bit address 
	return 0;
	/*
Working example:
0x5600 | 0x0078 = 0x5678
Binary:
01010110 00000000  (0x5600)
OR
00000000 01111000  (0x0078)
-----------------------
01010110 01111000  (0x5678)

	*/
}

// ABSOLUTE, OFFSET X // 
uint8_t olc6502::ABX() {
	uint16_t lo = read(pc);
	pc++;
	uint16_t hi = read(pc);
	pc++;

	addr_abs = (hi << 8) | lo;
	addr_abs += x;

	if ((addr_abs & 0xFF00) != (hi << 8)) {
		return 1; // means that page boundary is crosses // 
		// If the high bytes of addr are different than that of the hi bytes, then it
		// means that the page is crossed 
	}
	else {
		return 0;
	}
}

// ABSOLUTE, OFFSET Y // 
uint8_t olc6502::ABY() {
	uint16_t lo = read(pc);
	pc++;
	uint16_t hi = read(pc);
	pc++;
	addr_abs = (hi << 8) | lo;

	if ((addr_abs & 0xFF00) != (hi << 8)) {
		return 1;
	}
	else {
		return 0;
	}
}

// INDIRECT // 
// The provided address is read to get the actual 16-bit address 
// What shit here is that:
// This instruction has a bug in the hardware and to emulate this, we also
// need to emulate the bug. If the low byte of instruction if 0xFF then 
// to read the high byte of the actual address we need to cross a page
// This doesn't work on a page so we need to wrap around the same page to
// form an actual email address 

uint8_t olc6502::IND() {
	uint16_t ptr_lo = read(pc);
	pc++;
	uint16_t ptr_hi = read(pc);
	pc++;
	uint16_t ptr = (ptr_hi << 8) | ptr_lo;

	// To simulate the page boundary bug //
	if (ptr_lo == 0x00FF) {
		addr_abs = (read(ptr & 0XFF00) << 8) | read(ptr + 0);
		// First reading the high byte 0xFF00 << 8 nad read the low bytes by ptr+
	}
	// If no bug then behave normally // 
	else {
		addr_abs = (read(ptr + 1) << 8) | read(ptr + 0);
		// First reading the high bytes from ptr+1 and the the low bytes from ptr+0 
	}
	return 0;
}

// INDIRECT X // 
// The given 8bit address is offset by X register to index a location in page
// 0x00. The original 16bit address is read from this location 
uint8_t olc6502::IZX() {
	uint16_t t = read(pc);
	pc++;
	uint16_t lo = read((uint16_t)(t + (uint16_t)x) & 0x00FF);
	uint16_t hi = read((uint16_t)(t + (uint16_t)x + 1) & 0x00FF);

	addr_abs = (hi << 8) | lo;
	return 0;
}

// INDIRECT Y // 
// The given 8bit address indexes a location in page 0x00. From there the 
// original 16bit address is read and the Y register is added to offset it
// If the offset leads to a change in page then addiional clock cycle is needed 

uint8_t olc6502::IZY() {
	uint16_t t = read(pc);
	pc++;
	uint16_t lo = read(t & 0x00FF);
	uint16_t hi = read((t + 1) & 0x00FF);

	addr_abs = (hi << 8) | lo;
	addr_abs += y;

	if ((addr_abs & 0x00FF) != (hi << 8)) {
		return 1;
	}
	else {
		return 0;
	}
}

// FETCHED //
// Its job is to retrive data from the memeory if the instruction needs it
// and store it in the fetched variable 
// If the addressing mode is implied then there's no need to extra address
// The data is implied by the instruction itself
// If the instruction is other than IMPLIED then we need to read the absolute
// address 

uint8_t olc6502::fetch() {
	if (!(lookup[opcode].addrmode == &olc6502::IMP)){
		fetched = read(addr_abs);
	}
	return fetched;
}






