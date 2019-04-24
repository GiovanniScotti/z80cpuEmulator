#ifndef MEMORY_H
#define MEMORY_H

#include <stdint.h>

// Set bit at position x
#define SET(x)          (1 << x)

#define FLAG_ROM        0x00
#define FLAG_UNUSED     SET(0)
#define FLAG_USED       SET(1)

#define RAM_BANKS_N			2
#define ROM_SIZE				0x8000  // 32KB
#define RAM_SIZE 				0x8000  // 32KB

#define WHITIN(x,y,z) ((x >= y) && (x <= z))

// Support types
typedef uint8_t   u8;   // Unsigned 8-bit data
typedef uint16_t  u16;  // Unsigned 16-bit data
typedef uint32_t  u32;  // Unsigned 32-bit data


// Memory bank
typedef struct {
  u16 start;
  u16 size;
  u8 flag;
  u8 *ptr;
} RamBank;


// Use ramBanks (actually declared into main.c)
extern RamBank ramBanks[RAM_BANKS_N];

void printMemory(RamBank *ram, u16 size);
u8   readByte(u16 offset);
void writeByte(u8 value, u16 offset);
void stackPush(u16 value);
u16  stackPop();

#endif
