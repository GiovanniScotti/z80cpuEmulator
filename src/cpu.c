#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "cpu.h"


// Resets the CPU.
void cpu_reset(void) {
    LOG_INFO("Reset Z80 CPU.\n");

    // TODO: this must be a real hard reset!
  memset(&z80, 0xFF, sizeof(z80));
  z80.cycles = 0;
  z80.halt = 0;
  z80.I = 0;
  z80.R = 0;
  z80.PC = 0;
  z80.IFF1 = 0;
  z80.IFF2 = 0;
  z80.IM = 0;
  z80.pendingInterrupt = 0;

  z80.ram = (RamBank *)&ramBanks;

  for (int i = 0; i < RAM_BANKS_N; i++) {
    if (z80.ram[i].flag == FLAG_UNUSED)
      continue;
    if (!z80.ram[i].ptr) {
      z80.ram[i].ptr = malloc(z80.ram[i].size);
      memset(z80.ram[i].ptr, 0, z80.ram[i].size);
    }
  }
}


// Initializes the CPU data structures.
int32_t cpu_init(char rom[]) {
  // Reset the CPU
  resetZ80();

  u8 *bankPtr = NULL;

  for (int i = 0; i < RAM_BANKS_N; i++) {
    if(z80.ram[i].flag == FLAG_ROM) {
      fprintf(fpLog, "[INFO] ROM starts at %04X, size: %04X\n", z80.ram[i].start, z80.ram[i].size);
      bankPtr = z80.ram[i].ptr;
      continue;
    }
    if(z80.ram[i].flag == FLAG_USED) {
      fprintf(fpLog, "[INFO] RAM starts at %04X, size: %04X\n", z80.ram[i].start, z80.ram[i].size);
      continue;
    }
  }

  if (!bankPtr) {
    writeLog("[WARNING] No ROM banks defined.\n");
    return 0; // Return error
  }

  // Initialize ROM bank
  memcpy(bankPtr, rom, ROM_SIZE);
  writeLog("[INFO] ROM code loaded!\n");

  return 1; // Return successfully
}


void dumpRegisters() {
  writeLog("\nRegisters:\n");
  fprintf(fpLog, "A: %02X F: %02X\t A': %02X F': %02X\n"
                 "B: %02X C: %02X\t B': %02X C': %02X\n"
                 "D: %02X E: %02X\t D': %02X E': %02X\n"
                 "H: %02X L: %02X\t H': %02X L': %02X\n\n"
                 "IX: %04X IY: %04X\n"
                 "SP: %04X PC: %04X\n\n"
                 "IFF1: %01X IFF2: %01X IM: %01X I: %02X\n"
                 "SF: %01X ZF: %01X HF: %01X PF: %01X NF: %01X CF: %01X\n\n",
    z80.A, z80.F, z80.Ar, z80.Fr, z80.B, z80.C, z80.Br, z80.Cr, z80.D, z80.E,
    z80.Dr, z80.Er, z80.H, z80.L, z80.Hr, z80.Lr, z80.IX, z80.IY, z80.SP, z80.PC,
    z80.IFF1, z80.IFF2, z80.IM, z80.I,
    (z80.F & FLAG_SIGN)   >> FLAG_SIGN_BIT,   (z80.F & FLAG_ZERO)   >> FLAG_ZERO_BIT,
    (z80.F & FLAG_HCARRY) >> FLAG_HCARRY_BIT, (z80.F & FLAG_PARITY) >> FLAG_PARITY_BIT,
    (z80.F & FLAG_ADDSUB) >> FLAG_ADDSUB_BIT, (z80.F & FLAG_CARRY)  >> FLAG_CARRY_BIT);
}


void emulateZ80(int instrToExec) {
  z80.cycles = 0;
  int instrDone = 0;
  int exitAtHalt = 0;

  if(instrToExec == -1) {
    exitAtHalt = 1;
  }

  while((instrDone < instrToExec) || exitAtHalt) {

    // FOR DEBUGGING ONLY
    //sleep(0.3);

    if(z80.halt) {
      writeLog("[INFO] Z80 halted.\n");
      printw("***** Z80 HALTED *****\n");
      refresh();
      break;
    }

    // Fetch instruction
    u8 opcode = fetch8();

    // Execute instruction
    opTbl[opcode].execute (opcode);
    z80.cycles += opTbl[opcode].TStates;
    instrDone++;

    /*
      After the execution of the current instruction, check for
      key pressed with kbhit(). If one key was pressed, then put it
      into RDR, set RX_FULL and pendingInterrupt.
    */

    if(kbhit() && !(m6850.status & RX_FULL)) {
      char ch = getch();
      if(ch == 0x0A) {
        m6850.RDR = 0x0D; // Carriage return
      }
      else m6850.RDR = ch;
      m6850.status |= RX_FULL;
      z80.pendingInterrupt = 1;
    }

    /*
      After checking incoming characters, check the ACIA status register
      and determine if a byte is ready to be transmitted.
    */

    if(!(m6850.status & TX_EMPTY)) {
      char charToPrint = m6850.TDR;
      if(charToPrint == 0x0D) { // Carriage return
        printw("\n");
      }
      else if(charToPrint == 0x0C) { // New page - Form Feed
        //clear();
      }
      else if(charToPrint == 0x0A) {
        // Do nothing
      }
      else {
        printw("%c", charToPrint);
      }
      m6850.status |= TX_EMPTY;
      refresh();
    }

    // Detect interrupt at the end of the instruction's execution
    if(z80.pendingInterrupt) {
      // If the previous instruction is not an EI
      if(readByte(z80.PC - 1) != 0xFB)
        causeMaskblInt();
    }
  }

  fprintf(fpLog, "[INFO] %d executed instructions.\n", instrDone);
}


void causeMaskblInt() {
  // Check IFF1 status
  if(z80.IFF1) { // If the interrupt is not masked
    // Accept the interrupt - IFF1 and IFF2 to 0
    z80.IFF1 = 0;
    z80.IFF2 = 0;
    z80.pendingInterrupt = 0; // Reset pending interrupt flag
    // Interrupt mode 1
    if(z80.IM == 1) {
      stackPush(z80.PC);
      z80.PC = 0x0038;
      z80.cycles += 2; // Two additional cycles required for restarting
      writeLog("[INFO] Caught mode 1 interrupt.\n");
    }
    // TODO: IM 0 and IM 2 are still missing
    else die("[ERROR] Interrupt mode not supported.\n");
  }
}
