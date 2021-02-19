#ifndef _CPU_H_
#define _CPU_H_

#include <stdint.h>


#define FLAG_SIGN_BIT   7
#define FLAG_ZERO_BIT   6
// FLAG_BIT 5 is unused
#define FLAG_HCARRY_BIT 4
// FLAG_BIT 3 is unused
#define FLAG_PARITY_BIT 2
#define FLAG_ADDSUB_BIT 1
#define FLAG_CARRY_BIT  0

#define FLAG_SIGN 	(1 << FLAG_SIGN_BIT)   // S
#define FLAG_ZERO 	(1 << FLAG_ZERO_BIT)   // Z
#define FLAG_HCARRY (1 << FLAG_HCARRY_BIT) // H
#define FLAG_PARITY (1 << FLAG_PARITY_BIT) // P/V
#define FLAG_ADDSUB (1 << FLAG_ADDSUB_BIT) // N
#define FLAG_CARRY 	(1 << FLAG_CARRY_BIT)  // C


extern OpTbl opTbl[];


struct {
    uint32_t cycles;
    uint8_t  halt;

    // Main register set.
    union {
        struct {
            uint8_t C;
            uint8_t B;
            uint8_t E;
            uint8_t D;
            uint8_t L;
            uint8_t H;
            uint8_t F;
            uint8_t A;
        };
        struct {
            uint16_t BC;
            uint16_t DE;
            uint16_t HL;
            uint16_t AF;
        };
    };

    // Alternate register set.
    union {
        struct {
            uint8_t Cr;
            uint8_t Br;
            uint8_t Er;
            uint8_t Dr;
            uint8_t Lr;
            uint8_t Hr;
            uint8_t Fr;
            uint8_t Ar;
        };
        struct {
            uint16_t BrCr;
            uint16_t DrEr;
            uint16_t HrLr;
            uint16_t ArFr;
        };
    };

    // Special purpose registers.
    struct {
        uint8_t  I;    // Interrupt vector.
        uint8_t  R;    // Memory refresh.
        uint16_t IX;
        uint16_t IY;
        uint16_t PC;
        uint16_t SP;
    };

    RamBank *ram;

    // Interrupt.
    uint8_t IFF1;
    uint8_t IFF2;
    uint8_t IM;
    /*
      Mode 0: similar to 8080. The interrupting device can place any instruction
        on the data bus and the cpu executes it. Only a single-byte instruction
        can be actually placed on the bus (usually RST). Two additional clock
        cycles are needed to complete the restarting instruction.
      Mode 1: the cpu executes a restart at address 0x38. Two additional clock
        cycles are needed to complete the restarting instruction.
      Mode 2: the cpu executes a restart at address
        || z80.I | 7-bit from interrupting device | 0 ||
        19 clock cycles are required (7 to fetch the lower bits, 6 to save the PC
        and 6 to obtain the jump address).
    */
    uint8_t pendingInterrupt;
    // TODO: NMI is not implemented yet.

    // Input/Output
    uint8_t (*portIn) (int32_t port);
    void (*portOut) (int32_t port, uint8_t value);
} cpu_t;

void cpu_reset(void);
int32_t cpu_init(char rom[]);


void dumpRegisters();
void emulateZ80(int instrToExec);
void causeMaskblInt();

#endif // _CPU_H_
