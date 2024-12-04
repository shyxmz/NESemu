// Implements the bus class // 

#include "Bus.h"

Bus::Bus() {
	cpu.ConnectBus(this); // Connect CPU to communication bus // 
	for (auto& i : ram) {
		i = 0x00; // Clearing ram just in case // 
	}
}
Bus::~Bus() {}

void Bus::write(uint16_t addr, uint8_t data){
	if (addr >= 0x0000 && addr <= 0xFFFF) {
		ram[addr] = data;
	}
}

uint8_t Bus::read(uint16_t addr, bool bReadOnly) {
	if (addr >= 0x0000 && addr <= 0xFFFF) {
		return ram[addr];
	}
	else return 0x00;
}