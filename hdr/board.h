#ifndef _BOARD_H_
#define _BOARD_H_

#include <stdint.h>

#include "cpu.h"
#include "mc6850.h"


// This is used to fix the circular dependency between board and cpu.
typedef struct cpu_t cpu_t;


// A board is made of a cpu with its memory and a simple uart.
typedef struct board_t {
    cpu_t *cpu;
    mc6850_t *acia;
} board_t;


int32_t board_init(board_t *board, char *rom_file);
void board_emulate(board_t *board, int32_t instr_limit, bool is_terminal);
int32_t board_destroy(board_t *board);

#endif // _BOARD_H_