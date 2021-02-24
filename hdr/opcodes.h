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

// Z80 instruction set architecture.
// void opc_LDrn(cpu_t *cpu, uint8_t opcode);
// void opc_LDrr(cpu_t *cpu, uint8_t opcode);
// void opc_LDrHL(cpu_t *cpu, uint8_t opcode);
// void opc_LDIX(cpu_t *cpu, uint8_t opcode);
// void opc_LDIY(cpu_t *cpu, uint8_t opcode);
// void opc_LDHLr(cpu_t *cpu, uint8_t opcode);
// void opc_LDHLn(cpu_t *cpu, uint8_t opcode);
// void opc_LDABC(cpu_t *cpu, uint8_t opcode);
// void opc_LDADE(cpu_t *cpu, uint8_t opcode);
// void opc_LDAnn(cpu_t *cpu, uint8_t opcode);
// void opc_LDBCA(cpu_t *cpu, uint8_t opcode);
// void opc_LDDEA(cpu_t *cpu, uint8_t opcode);
// void opc_LDnnA(cpu_t *cpu, uint8_t opcode);
// void opc_LDRIddnn(cpu_t *cpu, uint8_t opcode);
// void opc_LDddnn(cpu_t *cpu, uint8_t opcode);
// void opc_LDHLnn(cpu_t *cpu, uint8_t opcode);
// void opc_LDnnHL(cpu_t *cpu, uint8_t opcode);
// void opc_LDSPHL(cpu_t *cpu, uint8_t opcode);
// void opc_EXDEHL(cpu_t *cpu, uint8_t opcode);
// void opc_EXAFAFr(cpu_t *cpu, uint8_t opcode);
// void opc_EXX(cpu_t *cpu, uint8_t opcode);
// void opc_EXSPHL(cpu_t *cpu, uint8_t opcode);
// void opc_ADDAr(cpu_t *cpu, uint8_t opcode);
// void opc_ADDAn(cpu_t *cpu, uint8_t opcode);
// void opc_ADDAHL(cpu_t *cpu, uint8_t opcode);
// void opc_ADCAr(cpu_t *cpu, uint8_t opcode);
// void opc_ADCAn(cpu_t *cpu, uint8_t opcode);
// void opc_ADCAHL(cpu_t *cpu, uint8_t opcode);
// void opc_SUBAr(cpu_t *cpu, uint8_t opcode);
// void opc_SUBAn(cpu_t *cpu, uint8_t opcode);
// void opc_SUBAHL(cpu_t *cpu, uint8_t opcode);
// void opc_SBCAr(cpu_t *cpu, uint8_t opcode);
// void opc_SBCAn(cpu_t *cpu, uint8_t opcode);
// void opc_SBCAHL(cpu_t *cpu, uint8_t opcode);
// void opc_ANDr(cpu_t *cpu, uint8_t opcode);
// void opc_ANDn(cpu_t *cpu, uint8_t opcode);
// void opc_ANDHL(cpu_t *cpu, uint8_t opcode);
// void opc_ORr(cpu_t *cpu, uint8_t opcode);
// void opc_ORn(cpu_t *cpu, uint8_t opcode);
// void opc_ORHL(cpu_t *cpu, uint8_t opcode);
// void opc_XORr(cpu_t *cpu, uint8_t opcode);
// void opc_XORn(cpu_t *cpu, uint8_t opcode);
// void opc_XORHL(cpu_t *cpu, uint8_t opcode);
// void opc_CPr(cpu_t *cpu, uint8_t opcode);
// void opc_CPn(cpu_t *cpu, uint8_t opcode);
// void opc_CPHL(cpu_t *cpu, uint8_t opcode);
// void opc_INCr(cpu_t *cpu, uint8_t opcode);
// void opc_INCHL(cpu_t *cpu, uint8_t opcode);
// void opc_DECr(cpu_t *cpu, uint8_t opcode);
// void opc_DECHL(cpu_t *cpu, uint8_t opcode);
// void opc_DAA(cpu_t *cpu, uint8_t opcode);
// void opc_CPL(cpu_t *cpu, uint8_t opcode);
// void opc_CCF(cpu_t *cpu, uint8_t opcode);
// void opc_SCF(cpu_t *cpu, uint8_t opcode);
// void opc_NOP(cpu_t *cpu, uint8_t opcode);
// void opc_HALT(cpu_t *cpu, uint8_t opcode);
// void opc_DI(cpu_t *cpu, uint8_t opcode);
// void opc_EI(cpu_t *cpu, uint8_t opcode);
// void opc_ADDHLss(cpu_t *cpu, uint8_t opcode);
// void opc_INCss(cpu_t *cpu, uint8_t opcode);
// void opc_DECss(cpu_t *cpu, uint8_t opcode);
// void opc_RLCA(cpu_t *cpu, uint8_t opcode);
// void opc_RLA(cpu_t *cpu, uint8_t opcode);
// void opc_RRCA(cpu_t *cpu, uint8_t opcode);
// void opc_RRA(cpu_t *cpu, uint8_t opcode);
// void opc_RLC(cpu_t *cpu, uint8_t opcode);
// void opc_JPnn(cpu_t *cpu, uint8_t opcode);
// void opc_JPccnn(cpu_t *cpu, uint8_t opcode);
// void opc_JRe(cpu_t *cpu, uint8_t opcode);
// void opc_JRCe(cpu_t *cpu, uint8_t opcode);
// void opc_JRNCe(cpu_t *cpu, uint8_t opcode);
// void opc_JRZe(cpu_t *cpu, uint8_t opcode);
// void opc_JRNZe(cpu_t *cpu, uint8_t opcode);
// void opc_JPHL(cpu_t *cpu, uint8_t opcode);
// void opc_DJNZe(cpu_t *cpu, uint8_t opcode);
// void opc_CALLnn(cpu_t *cpu, uint8_t opcode);
// void opc_CALLccnn(cpu_t *cpu, uint8_t opcode);
// void opc_RET(cpu_t *cpu, uint8_t opcode);
// void opc_RETcc(cpu_t *cpu, uint8_t opcode);
// void opc_RSTp(cpu_t *cpu, uint8_t opcode);
// void opc_INAn(cpu_t *cpu, uint8_t opcode);
// void opc_OUTnA(cpu_t *cpu, uint8_t opcode);

#endif // _OPCODES_H_
