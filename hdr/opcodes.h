#ifndef _OPCODES_H_
#define _OPCODES_H_

#include <stdint.h>
#include "cpu.h"


typedef struct {
    void (*execute) (cpu_t *cpu, uint8_t opcode);
    int32_t TStates;
} opc_t;


extern opc_t opc_tbl[0x100];


uint8_t opc_fetch8(cpu_t *cpu);
uint16_t opc_fetch16(cpu_t *cpu);

#endif // _OPCODES_H_
