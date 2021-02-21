#ifndef _CPU_H_
#define _CPU_H_

#include <stdint.h>


///////////////////////////////////////////////////////////
// FLAG REGISTER BITS
// Sign flag is set if the sign of a result after an operation is negative,
// reset if it is zero or positive.
#define FLAG_SIGN_BIT   7
// Zero flag is set if the result of an operation is zero.
#define FLAG_ZERO_BIT   6
// BCD half-carry flag (bit 4 in BCD operations).
#define FLAG_HCARRY_BIT 4
// Parity/overflow flag.
#define FLAG_PARITY_BIT 2
// Subtraction flag used in BCD subtract operations.
#define FLAG_ADDSUB_BIT 1
// Carry flag indicates a carry from the high-order bit of the accumulator (B7).
#define FLAG_CARRY_BIT  0

// #define FLAG_SIGN 	(1 << FLAG_SIGN_BIT)   // S
// #define FLAG_ZERO 	(1 << FLAG_ZERO_BIT)   // Z
// #define FLAG_HCARRY (1 << FLAG_HCARRY_BIT) // H
// #define FLAG_PARITY (1 << FLAG_PARITY_BIT) // P/V
// #define FLAG_ADDSUB (1 << FLAG_ADDSUB_BIT) // N
// #define FLAG_CARRY 	(1 << FLAG_CARRY_BIT)  // C

#define GET_FLAG_SIGN(F) 	((F >> FLAG_SIGN_BIT) & 0x1)    // S
#define GET_FLAG_ZERO(F) 	((F >> FLAG_ZERO_BIT) & 0x1)    // Z
#define GET_FLAG_HCARRY(F)  ((F >> FLAG_HCARRY_BIT) & 0x1)  // H
#define GET_FLAG_PARITY(F)  ((F >> FLAG_PARITY_BIT) & 0x1)  // P/V
#define GET_FLAG_ADDSUB(F)  ((F >> FLAG_ADDSUB_BIT) & 0x1)  // N
#define GET_FLAG_CARRY(F) 	((F >> FLAG_CARRY_BIT) & 0x1)   // C

// Interrupt mode codes.
#define INT_MODE_0      0
#define INT_MODE_1      1
#define INT_MODE_2      2

// Chunk types.
#define CHUNK_UNUSED     0
#define CHUNK_READONLY   1
#define CHUNK_READWRITE  2

#define WHITIN(x, y, z) ((x >= y) && (x <= z))


// Memory bank description.
typedef struct mem_chunk_t {
    char *label;
    uint8_t type;
    uint16_t start;
    uint16_t size;
    uint8_t *buff;
    struct mem_chunk_t *next;
} mem_chunk_t;


typedef struct {
    uint32_t cycles;
    uint32_t instr;
    // If a software HALT instruction is encountered, the cpu will sit there
    // executing NOPs until a non maskable interrupt is received or a maskable
    // interrupt is received and interrupts are globally enabled.
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

    // Attached memory banks.
    mem_chunk_t *memory;

    // Interrupt enable flag. IFF1 disables interrupts from being accepted.
    // IFF2 is a temporary storage location for IFF1.
    uint8_t IFF1;
    uint8_t IFF2;
    // Interrupt mode.
    uint8_t IM;

    // MODE 0: similar to 8080. The interrupting device can place any instruction
    //  on the data bus and the cpu executes it. Only a single-byte instruction
    //  can be actually placed on the bus (usually RST). Two additional clock
    //  cycles are needed to complete the restarting instruction.
    // MODE 1: the cpu executes a restart at address 0x38. Two additional clock
    //  cycles are needed to complete the restarting instruction.
    // MODE 2: the cpu executes a restart at address
    //  { z80.I | 7-bit from interrupting device | 0 }
    //  19 clock cycles are required (7 to fetch the lower bits, 6 to save
    //  the PC and 6 to obtain the jump address).

    // is_pendingInterrupt is set when any kind of interrupt is pending
    // is_pendingNMI is set when the pending interrupt is a NMI.
    bool is_pendingInterrupt;
    bool is_pendingNMI;

    // Input/Output
    uint8_t (*portIn) (int32_t port);
    void (*portOut) (int32_t port, uint8_t value);
} cpu_t;


int32_t cpu_init(cpu_t *cpu, mem_chunk_t *mem_list);
int32_t cpu_destroy(cpu_t *cpu);
void cpu_reset(cpu_t *cpu);
uint8_t cpu_read(cpu_t *cpu, const uint16_t addr);
void cpu_write(cpu_t *cpu, const uint8_t data, const uint16_t addr);
void cpu_stackPush(cpu_t *cpu, uint16_t data);
uint16_t cpu_stackPop(cpu_t *cpu);
void cpu_emulate(cpu_t *cpu, int32_t instr_limit);

void cpu_printChunk(mem_chunk_t *chunk);
void cpu_dumpRegisters(cpu_t *cpu);

#endif // _CPU_H_
