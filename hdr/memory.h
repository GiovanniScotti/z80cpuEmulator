#ifndef _MEMORY_H_
#define _MEMORY_H_

#include <stdint.h>

// Sets bit at position x.
#define SET(x)          (1 << x)

#define FLAG_ROM        0x00
#define FLAG_UNUSED     SET(0)
#define FLAG_USED       SET(1)

#define RAM_BANKS_N			2
#define ROM_SIZE			0x8000  // 32KB
#define RAM_SIZE 			0x8000  // 32KB

#define WHITIN(x,y,z) ((x >= y) && (x <= z))

// Memory bank type.
typedef struct {
    char *label;
    uint16_t start;
    uint16_t size;
    uint8_t flag;
    uint8_t *ptr;
} mem_chunk_t;


void memory_printChunk(mem_chunk_t *chunk);

/*
u8   readByte(u16 offset);
void writeByte(u8 value, u16 offset);
void stackPush(u16 value);
u16  stackPop();
*/

#endif // _MEMORY_H_
