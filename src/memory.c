#include "memory.h"
#include "logger.h"


void memory_printChunk(mem_chunk_t *chunk) {
    LOG_DEBUG("Memory chunk: %s\n", chunk->label);
    LOG_DEBUG("Addr.\t0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F\n");

    for (int32_t byte = 0; byte < chunk->size; byte++) {
        int32_t col = byte >> 4;

        // Right-hand side of the table.
        if (byte % 16 == 0) {
            LOG_DEBUG("\n");
            LOG_DEBUG("%03X0\t", col);
        }

        LOG_DEBUG("%02X ", *((chunk->ptr) + byte));
    }
    LOG_DEBUG("\n\n");
    return;
}

/*
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
*/