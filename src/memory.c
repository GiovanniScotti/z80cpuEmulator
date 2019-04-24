#include <ncurses.h>
#include <stdio.h>

#include "Z80.h"
#include "logger.h"
#include "memory.h"


void printMemory(RamBank *ram, u16 size) {
  writeLog("\n");
  writeLog("Addr.\t0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F\n");

  for(int byte = 0; byte < size; byte++) {
    int col = byte/16;

    // Right-hand side of the table
    if(byte%16 == 0) {
      writeLog("\n");
      fprintf(fpLog, "%03X", col);
      writeLog("0\t");
    }

    fprintf(fpLog, "%02X ", *(ram[0].ptr+byte));
  }
  writeLog("\n\n");
}


u8 readByte(u16 offset) {
  int c;

  for (c = 0; c < 4; c++) {
    if (z80.ram[c].flag == FLAG_UNUSED)
      continue;
    if (!WHITIN(offset, z80.ram[c].start, z80.ram[c].start + z80.ram[c].size - 1))
      continue;

    return z80.ram[c].ptr[offset - z80.ram[c].start];
  }

  die("[ERROR] Memory read out of bounds.\n");

  return 0xFF; // Memory location is unused and empty
}


void writeByte(u8 value, u16 offset) {
  int c;

  for (c = 0; c < 4; c++) {
    if (z80.ram[c].flag == FLAG_UNUSED)
      continue;
    if (!WHITIN(offset, z80.ram[c].start, z80.ram[c].start + z80.ram[c].size - 1))
      continue;
    if (z80.ram[c].flag == FLAG_ROM) {
      die("[ERROR] Unable to write to ROM region.\n");
      return;
    }

    z80.ram[c].ptr[offset - z80.ram[c].start] = value;
    return;
  }

  die("[ERROR] Memory write out of bounds.\n");
}


void stackPush(u16 value) {
  writeByte((value >> 8) & 0xFF, --z80.SP);  // (SP-1) <- valueH
  writeByte(value & 0xFF, --z80.SP);         // (SP-2) <- valueL
}


u16 stackPop() {
  u16 value  =  readByte(z80.SP++);
  value |= (readByte(z80.SP++) << 8);

  return value;
}
