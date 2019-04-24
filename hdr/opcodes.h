#ifndef OPCODES_H
#define OPCODES_H

#include "memory.h"

#define FASTSWAP(X1,X2) X1 ^= X2; X2 ^= X1; X1 ^= X2

u8   fetch8();
u16  fetch16();

// Status register support functions
int  isNegative(u8 val);
void rstAddSub();
void setAddSub();

void testZero_8(u8 val);
void testZero_16(u16 val);

void testSign_8(u8 val);
void testSign_16(u16 val);

void testHalfCarry_8(u8 val1, u8 val2, u8 carry);

void testCarry_8(u8 val1, u8 val2, u8 carry);
void testCarry_16(u16 val1, u16 val2, u16 carry);

void testOverflow_8(u8 val1, u8 val2, u8 res);
void testOverflow_16(u16 val1, u16 val2, u16 res);

void testParity_8(u8 val);
void invertHC();

// Register access functions
void writeReg(u8 value, u8 index);
u8   readReg(u8 index);
void logReg8(u8 index);
void logReg16(u8 index, int af_flag);

// Z80 instruction set architecture
void LDrr(u8 opcode);
void LDrn(u8 opcode);
void LDrHL(u8 opcode);
void LDIX(u8 opcode);
void LDIY(u8 opcode);
void LDHLr(u8 opcode);
void LDHLn(u8 opcode);
void LDABC(u8 opcode);
void LDADE(u8 opcode);
void LDAnn(u8 opcode);
void LDBCA(u8 opcode);
void LDDEA(u8 opcode);
void LDnnA(u8 opcode);
void LDRIddnn(u8 opcode);
void LDddnn(u8 opcode);
void LDHLnn(u8 opcode);
void LDnnHL(u8 opcode);
void LDSPHL(u8 opcode);
void EXDEHL(u8 opcode);
void EXAFAFr(u8 opcode);
void EXX(u8 opcode);
void EXSPHL(u8 opcode);
void ADDAr(u8 opcode);
void ADDAn(u8 opcode);
void ADDAHL(u8 opcode);
void ADCAr(u8 opcode);
void ADCAn(u8 opcode);
void ADCAHL(u8 opcode);
void SUBAr(u8 opcode);
void SUBAn(u8 opcode);
void SUBAHL(u8 opcode);
void SBCAr(u8 opcode);
void SBCAn(u8 opcode);
void SBCAHL(u8 opcode);
void ANDr(u8 opcode);
void ANDn(u8 opcode);
void ANDHL(u8 opcode);
void ORr(u8 opcode);
void ORn(u8 opcode);
void ORHL(u8 opcode);
void XORr(u8 opcode);
void XORn(u8 opcode);
void XORHL(u8 opcode);
void CPr(u8 opcode);
void CPn(u8 opcode);
void CPHL(u8 opcode);
void INCr(u8 opcode);
void INCHL(u8 opcode);
void DECr(u8 opcode);
void DECHL(u8 opcode);
void DAA(u8 opcode);
void CPL(u8 opcode);
void CCF(u8 opcode);
void SCF(u8 opcode);
void NOP(u8 opcode);
void HALT(u8 opcode);
void DI(u8 opcode);
void EI(u8 opcode);
void ADDHLss(u8 opcode);
void INCss(u8 opcode);
void DECss(u8 opcode);
void RLCA(u8 opcode);
void RLA(u8 opcode);
void RRCA(u8 opcode);
void RRA(u8 opcode);
void RLC(u8 opcode);
void JPnn(u8 opcode);
void JPccnn(u8 opcode);
void JRe(u8 opcode);
void JRCe(u8 opcode);
void JRNCe(u8 opcode);
void JRZe(u8 opcode);
void JRNZe(u8 opcode);
void JPHL(u8 opcode);
void DJNZe(u8 opcode);
void CALLnn(u8 opcode);
void CALLccnn(u8 opcode);
void RET(u8 opcode);
void RETcc(u8 opcode);
void RSTp(u8 opcode);
void INAn(u8 opcode);
void OUTnA(u8 opcode);

typedef struct {
  void (*execute) (u8 opcode);
  int TStates;
} OpTbl;

#endif
