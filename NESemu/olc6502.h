#pragma once

// vector for translation table and disassembler 
#include <vector>
#include <string>
#include <map>
using namespace std;

#ifdef LOGMODE
#include <stdio.h>
#endif // LOGMODE

class Bus;

class olc6502 {
public:
    olc6502();
    ~olc6502();

public:
    // CPU's core registers, this is all the 6502 has 
    uint8_t a = 0x00; // Accumulator register 
    uint8_t x = 0x00; // X register 
    uint8_t y = 0x00; // Y register 
    uint8_t stkp = 0x00; // Stack pointer
    uint16_t pc = 0x0000; // Program counter
    uint8_t status = 0x00; // Status register

    // External event functions 
    void reset(); // Reset Interrupt - Forces CPU into a safe known state
    void irq(); // Interrupt request (can be disabled)
    void nmi(); // Non-maskable interrupt(cannot be disabled)
    void clock(); // Simulates one clock cycle

    // To implement the step-by-step execution we include the complete() function 
    bool complete(); // Tells if the current instruction has finished execution

    // Linking CPU to the communication bus 
    void ConnectBus(Bus* n) { bus = n; }

    // To get human-readable code form machine code 
    map<uint16_t, string> disassemble(uint16_t nStart, uint16_t nStop);

    // Flags of the status register 
    enum FLAGS6502 {
        C = (1 << 0), // CARRY
        Z = (1 << 1), // ZERO
        I = (1 << 2), // DISABLE INTERRUPTS
        D = (1 << 3), // DECIMAL MODE
        B = (1 << 4), // BREAK
        U = (1 << 5), // UNUSED
        V = (1 << 6), // OVERFLOW
        N = (1 << 7), // NEGATIVE
    };

    // Functions to access the status registers 
    uint8_t GetFlag(FLAGS6502 f);
    void SetFlag(FLAGS6502 f, bool v);

    // Assistive variables 
    uint8_t fetched = 0x00; // fetched data for ALU instructions 
    uint16_t temp = 0x0000; // temporary storage for computations 
    uint16_t addr_abs = 0x0000; // Absolute memory addr 
    uint16_t addr_rel = 0x00; // Relative memory addr (branches)
    uint8_t opcode = 0x00; // Current instruction opcode 
    uint8_t cycles = 0; // Remaining clock cycles for the instruction 
    uint32_t clock_count = 0; // Global clock count

    // Linkage to communication bus 
    Bus* bus = nullptr;
    uint8_t read(uint16_t a); // Reads 8bit data from 16 bit address 
    void write(uint16_t a, uint8_t d); // Write 8bit data from a 16 bit address 

    uint8_t fetch(); // Fetch the current instruction

    // Structure for the compilation and storage of the opcode translation table
    struct INSTRUCTION {
        string name;
        uint8_t(olc6502::*operate)(void) = nullptr;
        uint8_t(olc6502::*addrmode)(void) = nullptr;
        uint8_t cycles = 0;
    };
    vector<INSTRUCTION> lookup;

private:
    // Setting up the addressing modes
    uint8_t IMP(); uint8_t IMM();
    uint8_t ZP0(); uint8_t ZPX();
    uint8_t ZPY(); uint8_t REL();
    uint8_t ABS(); uint8_t ABX();
    uint8_t ABY(); uint8_t IND();
    uint8_t IZX(); uint8_t IZY();

    // Setting up the opcodes
    uint8_t ADC(); uint8_t AND(); uint8_t ASL(); uint8_t BCC();
    uint8_t BCS(); uint8_t BEQ(); uint8_t BIT(); uint8_t BMI();
    uint8_t BNE(); uint8_t BPL(); uint8_t BRK(); uint8_t BVC();
    uint8_t BVS(); uint8_t CLC(); uint8_t CLD(); uint8_t CLI();
    uint8_t CLV(); uint8_t CMP(); uint8_t CPX(); uint8_t CPY();
    uint8_t DEC(); uint8_t DEX(); uint8_t DEY(); uint8_t EOR();
    uint8_t INC(); uint8_t INX(); uint8_t INY(); uint8_t JMP();
    uint8_t JSR(); uint8_t LDA(); uint8_t LDX(); uint8_t LDY();
    uint8_t LSR(); uint8_t NOP(); uint8_t ORA(); uint8_t PHA();
    uint8_t PHP(); uint8_t PLA(); uint8_t PLP(); uint8_t ROL();
    uint8_t ROR(); uint8_t RTI(); uint8_t RTS(); uint8_t SBC();
    uint8_t SEC(); uint8_t SED(); uint8_t SEI(); uint8_t STA();
    uint8_t STX(); uint8_t STY(); uint8_t TAX(); uint8_t TAY();
    uint8_t TSX(); uint8_t TXA(); uint8_t TXS(); uint8_t TYA();

    // Unofficial opcodes handler 
    uint8_t XXX();

#ifdef LOGMODE
private:
    FILE* logfile = nullptr;
#endif // LOGMODE
};
