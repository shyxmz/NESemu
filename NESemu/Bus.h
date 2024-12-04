// bus.h simulates the communication bus in the NES emulator, connecting the CPU and RAM // 
#pragma once // States that the file is included only once during compilation to avoid redefination errors // 

#include<cstdint> // To include fixed sized integer types such as uint8_t or uint16_t 
#include<array>
#include "olc6502.h"  // Header for the CPU class that represnets the 6502 CPU // 
using namespace std;

class Bus
{
public:
	Bus();
	~Bus();

public:
	olc6502 cpu;
	array<uint8_t, 64 * 1024>ram; // Original ram size if 2KB for NES but this 64KB represents the whole NES addressable range // 

public: // Bus read and write functions // 
	void write(uint16_t addr, uint8_t data); // Writes a 8bit data to a 16bit address // 
	uint8_t read(uint16_t addr, bool bReadOnly = false); // Reads 8bit data from a 16bit address // 
};

