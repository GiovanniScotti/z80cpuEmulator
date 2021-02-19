#ifndef Z80_H
#define Z80_H

#include "logger.h"
#include "mc6850.h"
#include "memory.h"
#include "opcodes.h"
#include "roms.h"


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
  u32 cycles;
  u8  halt;
    // Main register set
    union {
      struct {
        u8 C;
        u8 B;
        u8 E;
        u8 D;
        u8 L;
        u8 H;
        u8 F;
        u8 A;
      };
      struct {
        u16 BC;
        u16 DE;
        u16 HL;
        u16 AF;
      };
    };

    // Alternate register set
    union {
      struct {
        u8 Cr;
        u8 Br;
        u8 Er;
        u8 Dr;
        u8 Lr;
        u8 Hr;
        u8 Fr;
        u8 Ar;
      };
      struct {
        u16 BrCr;
        u16 DrEr;
        u16 HrLr;
        u16 ArFr;
      };
    };

    // Special purpose registers
    struct {
      u8  I;    // Interrupt vector
      u8  R;    // Memory refresh
      u16 IX;
      u16 IY;
      u16 PC;
      u16 SP;
    };

  RamBank *ram;

  // Interrupt
  u8 IFF1;
  u8 IFF2;
  u8 IM;
  /*
    Mode 0: similar to 8080. The interrupting device can place any instruction
      on the data bus and the cpu executes it. Only a single-byte instrudiediediediediediediection
      can be actually placed on the bus (usually RST). Two additional clock
      cycles are needed to complete the restarting instruction.
    Mode 1: the cpu executes a restart at address 0x38. Two additional clock
      cycles are needed to complete the restarting instruction.
    Mode 2: the cpu executes a restart at address
      || z80.I | 7-bit from interrupting device | 0 ||
      19 clock cycles are required (7 to fetch the lower bits, 6 to save the PC
      and 6 to obtain the jump address).
  */
  u8 pendingInterrupt;
  // TODO: NMI is not implemented yet

  // Input/Output
  u8   (*portIn)  (int port);
  void (*portOut) (int port, u8 value);
} z80;

void resetZ80();
int  initZ80(char rom[]);
void dumpRegisters();
void emulateZ80(int instrToExec);
void die(char *errMsg);
void causeMaskblInt();

#endif
