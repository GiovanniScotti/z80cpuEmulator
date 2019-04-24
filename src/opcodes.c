#include <ncurses.h>

#include "opcodes.h"
#include "Z80.h"
#include "logger.h"

/*
  TODO: Interrupts must be recognized during LDIR instruction iterations.
  TODO: Check 16-bit arithmetic instructions
  TODO: DAA is missing, pag. 173.
  TODO: Complete IN and OUT, pag. 298.
*/


// Log or not executed instructions
int logInstr = 0;


u8 fetch8() {
  return readByte(z80.PC++);
}


u16 fetch16() {
  u16 res = fetch8() | (fetch8() << 8);
  return res;
}


// *****************************************************
// *****    STATUS REGISTER - SUPPORT FUNCTIONS    *****
// *****************************************************

// Test if the two's complemented passed value is negative
int isNegative(u8 val) {
  if(val & 0x80)
    return 1;
  else
    return 0;
}


void rstAddSub() {
  z80.F &= ~(FLAG_ADDSUB);
}


void setAddSub() {
  z80.F |= FLAG_ADDSUB;
}


void testZero_8(u8 val) {
  if(val == 0) z80.F |= FLAG_ZERO;
  else         z80.F &= ~(FLAG_ZERO);
}


void testZero_16(u16 val) {
  if(val == 0) z80.F |= FLAG_ZERO;
  else         z80.F &= ~(FLAG_ZERO);
}


void testSign_8(u8 val) {
  if(isNegative(val)) z80.F |= FLAG_SIGN;
  else                z80.F &= ~(FLAG_SIGN);
}


void testSign_16(u16 val) {
  if(val & 0x8000) z80.F |= FLAG_SIGN;
  else             z80.F &= ~(FLAG_SIGN);
}


void testHalfCarry_8(u8 val1, u8 val2, u8 carry) {
  if((((val1 & 0x0F) + (val2 & 0x0F) + (carry & 0x0F)) & 0x10) == 0x10) {
    z80.F |= FLAG_HCARRY;
  }
  else {
    z80.F &= ~(FLAG_HCARRY);
  }
}


void testCarry_8(u8 val1, u8 val2, u8 carry) {
  // Avoid overflow in C and test carry condition
  if(val1 > 0xFF - val2 - carry) z80.F |= FLAG_CARRY;
  else                           z80.F &= ~(FLAG_CARRY);
}


void testCarry_16(u16 val1, u16 val2, u16 carry) {
  // Avoid overflow in C and test carry condition
  if(val1 > 0xFFFF - val2 - carry) z80.F |= FLAG_CARRY;
  else                             z80.F &= ~(FLAG_CARRY);
}


void testOverflow_8(u8 val1, u8 val2, u8 res) {
  if(((val1 ^ val2) ^ 0x80) & 0x80) {
    if((res ^ val1) & 0x80) {
      z80.F |= FLAG_PARITY;
    }
    else {
      z80.F &= ~(FLAG_PARITY);
    }
  } else {
    z80.F &= ~(FLAG_PARITY);
  }
}


void testOverflow_16(u16 val1, u16 val2, u16 res) {
  if(((val1 ^ val2) ^ 0x8000) & 0x8000) {
    if((res ^ val1) & 0x8000) {
      z80.F |= FLAG_PARITY;
    }
    else {
      z80.F &= ~(FLAG_PARITY);
    }
  } else {
    z80.F &= ~(FLAG_PARITY);
  }
}


void testParity_8(u8 val) {
  int set_bits = 0;
  while(val > 0) {
    if((val & 1) == 1)
      set_bits++;
    val = val >> 1;
  }

  if(!(set_bits % 2))
    z80.F |= FLAG_PARITY;
  else
    z80.F &= ~(FLAG_PARITY);
}


// Used in SUB instructions
void invertHC() {
  u8 mask = (FLAG_CARRY | FLAG_HCARRY);
  z80.F ^= mask;
}


// *****************************************************
// *****         REGISTER ACCESS FUNCTIONS         *****
// *****************************************************

// Write data into a register
void writeReg(u8 value, u8 index) {
  switch(index) {
    case 0x00:
      z80.B = value; break;
    case 0x01:
      z80.C = value; break;
    case 0x02:
      z80.D = value; break;
    case 0x03:
      z80.E = value; break;
    case 0x04:
      z80.H = value; break;
    case 0x05:
      z80.L = value; break;
    case 0x07:
      z80.A = value; break;
    default:
      die("[ERROR] Unable to write a non-existing register.\n");
  }
}


// Read data from a register
u8 readReg(u8 index) {
  switch(index) {
    case 0x00:
      return z80.B; break;
    case 0x01:
      return z80.C; break;
    case 0x02:
      return z80.D; break;
    case 0x03:
      return z80.E; break;
    case 0x04:
      return z80.H; break;
    case 0x05:
      return z80.L; break;
    case 0x07:
      return z80.A; break;
    default:
      die("[ERROR] Unable to read a non-existing register.\n");
  }
  return 0; // Should never get there
}


// Log accessed 8-bit register for debugging
void logReg8(u8 index) {
  switch(index) {
    case 0x00:
      writeLog("B"); break;
    case 0x01:
      writeLog("C"); break;
    case 0x02:
      writeLog("D"); break;
    case 0x03:
      writeLog("E"); break;
    case 0x04:
      writeLog("H"); break;
    case 0x05:
      writeLog("L"); break;
    case 0x07:
      writeLog("A"); break;
    default:
      die("[ERROR] Unable to log a non-existing register.\n");
  }
}


// Log accessed 16-bit register for debugging
void logReg16(u8 index, int af_flag) {
  switch(index) {
    case 0x00:
      writeLog("BC"); break;
    case 0x01:
      writeLog("DE"); break;
    case 0x02:
      writeLog("HL"); break;
    case 0x03:
      if(af_flag)
        writeLog("AF");
      else writeLog("SP");
      break;
    default:
      die("[ERROR] Unable to log a non-existing pair of registers.\n");
  }
}


// ******************************************************
// *****        INSTRUCTION SET ARCHITECTURE        *****
// ******************************************************

// This is LD r, r' instruction
void LDrr(u8 opcode) {
  u8 dst = ((opcode >> 3) & 0x07);
  u8 src = (opcode & 0x07);
  u8 srcVal = readReg(src);
  writeReg(srcVal, dst);
  if(logInstr) {
    writeLog("LD "); logReg8(dst); writeLog(", "); logReg8(src); writeLog("\n");
  }
}


// This is LD r, n instruction
void LDrn(u8 opcode) {
  u8 dst = ((opcode >> 3) & 0x07);
  u8 n = fetch8();
  writeReg(n, dst);
  if(logInstr) {
    writeLog("LD "); logReg8(dst); fprintf(fpLog, ", %02X\n", n);
  }
}


// This is LD r, (HL) instruction
void LDrHL(u8 opcode) {
  u8 dst = ((opcode >> 3) & 0x07);
  u8 val = readByte(z80.HL);
  writeReg(val, dst);
  if(logInstr) {
    writeLog("LD "); logReg8(dst); fprintf(fpLog, ", (HL)\t\tHL = %04X\n", z80.HL);
  }
}


void LDIX(u8 opcode) {
  opTbl[0xDD].TStates = 19;
  u8 follByte = fetch8();

  // This is LD r, (IX+d) instruction
  if((follByte & 0xC7) == 0x46) {
    u8 dst = ((follByte >> 3) & 0x07);
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u16 offset = z80.IX + extended_d;
    u8 val = readByte(offset);
    writeReg(val, dst);
    if(logInstr) {
      writeLog("LD "); logReg8(dst); fprintf(fpLog, ", (IX+d)\t\tIX+d = %04X\n", offset);
    }
  }

  // This is LD (IX+d), r instruction
  else if((follByte & 0xF8) == 0x70) {
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u16 offset = z80.IX + extended_d;
    u8 src = (follByte & 0x07);
    u8 val = readReg(src);
    writeByte(val, offset);
    if(logInstr) {
      writeLog("LD (IX+d), "); logReg8(src); fprintf(fpLog, "\t\tIX+d = %04X\n", offset);
    }
  }

  // This is LD (IX+d), n instruction
  else if(follByte == 0x36) {
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 n = fetch8();
    u16 offset = z80.IX + extended_d;
    writeByte(n, offset);
    if(logInstr) {
      fprintf(fpLog, "LD (IX+d), n\t\tIX+d = %04X, n = %02X\n", offset, n);
    }
  }

  // This is LD IX, nn instruction
  else if(follByte == 0x21) {
    opTbl[0xDD].TStates = 14;
    u16 nn = fetch16();
    z80.IX = nn;
    if(logInstr) {
      fprintf(fpLog, "LD IX, nn\t\tnn = %04X\n", nn);
    }
  }

  // This is LD IX, (nn) instruction
  else if(follByte == 0x2A) {
    opTbl[0xDD].TStates = 20;
    u16 offset = fetch16();
    z80.IX = (readByte(offset) | (readByte(offset + 1) << 8));
    if(logInstr) {
      fprintf(fpLog, "LD IX, (nn)\t\tnn = %04X\n", offset);
    }
  }

  // This is LD (nn), IX instruction
  else if(follByte == 0x22) {
    opTbl[0xDD].TStates = 20;
    u16 offset = fetch16();
    writeByte((z80.IX >> 8) & 0x00FF, offset + 1);  // (nn+1) <- IXh
    writeByte(z80.IX & 0x00FF, offset);             // (nn) <- IXl
    if(logInstr) {
      fprintf(fpLog, "LD (nn), IX\t\tnn = %04X\n", offset);
    }
  }

  // This is LD SP, IX instruction
  else if(follByte == 0xF9) {
    opTbl[0xDD].TStates = 10;
    z80.SP = z80.IX;
    if(logInstr) {
      fprintf(fpLog, "LD SP, IX\t\tIX = %04X\n", z80.IX);
    }
  }

  // This is PUSH IX instruction
  else if(follByte == 0xE5) {
    opTbl[0xDD].TStates = 15;
    stackPush(z80.IX);
    if(logInstr) {
      writeLog("PUSH IX\n");
    }
  }

  // This is POP IX instruction
  else if(follByte == 0xE1) {
    opTbl[0xDD].TStates = 14;
    z80.IX = stackPop();
    if(logInstr) {
      writeLog("POP IX\n");
    }
  }

  // This is EX (SP), IX instruction
  else if(follByte == 0xE3) {
    opTbl[0xDD].TStates = 23;
    u8 valSP  = readByte(z80.SP);
    u8 valSPH = readByte(z80.SP + 1);
    writeByte(z80.IX & 0x00FF, z80.SP);
    writeByte((z80.IX >> 8) & 0x00FF, z80.SP + 1);
    z80.IX = (valSP | (valSPH << 8));
    if(logInstr) {
      writeLog("EX (SP), IX"); fprintf(fpLog, "\t\tSP = %04X\n", z80.SP);
    }
  }

  // This is ADD A, (IX+d) instruction
  else if(follByte == 0x86) {
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IX + extended_d);
    u8 res = z80.A + value;

    testZero_8(res);
    testSign_8(res);
    testHalfCarry_8(z80.A, value, 0);
    rstAddSub();
    testCarry_8(z80.A, value, 0);
    testOverflow_8(z80.A, value, res);

    z80.A = res;
    if(logInstr) {
      fprintf(fpLog, "ADD A, (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
    }
  }

  // This is ADC A, (IX+d) instruction
  else if(follByte == 0x8E) {
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IX + extended_d);
    u8 carry = (z80.F & FLAG_CARRY);
    u8 res = z80.A + value + carry;

    testZero_8(res);
    testSign_8(res);
    testHalfCarry_8(z80.A, value, carry);
    rstAddSub();
    testCarry_8(z80.A, value, carry);
    testOverflow_8(z80.A, value, res);

    z80.A = res;
    if(logInstr) {
      fprintf(fpLog, "ADC A, (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
    }
  }

  // This is SUB A, (IX+d) instruction
  else if(follByte == 0x96){
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IX + extended_d);
    u8 complVal = ~value + 1;
    u8 res = z80.A + complVal;

    testZero_8(res);
    testSign_8(res);
    setAddSub();
    testOverflow_8(z80.A, complVal, res);
    testHalfCarry_8(z80.A, complVal, 0);
    testCarry_8(z80.A, complVal, 0);
    invertHC();

    z80.A = res;
    if(logInstr) {
      fprintf(fpLog, "SUB A, (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
    }
  }

  // This is SBC A, (IX+d) instruction
  else if(follByte == 0x9E){
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IX + extended_d);
    u8 not_carry = (z80.F & FLAG_CARRY) ^ FLAG_CARRY;
    // a - b - c = a + ~b + 1 - c = a + ~b + !c
    u8 res = z80.A + ~value + not_carry;

    testZero_8(res);
    testSign_8(res);
    setAddSub();
    testOverflow_8(z80.A, ~value, res);
    testHalfCarry_8(z80.A, ~value, not_carry);
    testCarry_8(z80.A, ~value, not_carry);
    invertHC();

    z80.A = res;
    if(logInstr) {
      fprintf(fpLog, "SBC A, (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
    }
  }

  // This is AND (IX+d) instruction
  else if(follByte == 0xA6) {
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IX + extended_d);
    u8 res = z80.A & value;

    testZero_8(res);
    testSign_8(res);
    rstAddSub();
    testParity_8(res);
    z80.F &= ~(FLAG_CARRY);
    z80.F |= (FLAG_HCARRY);

    z80.A = res;
    if(logInstr) {
      fprintf(fpLog, "AND (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
    }
  }

  // This is OR (IX+d) instruction
  else if(follByte == 0xB6) {
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IX + extended_d);
    u8 res = z80.A | value;

    testZero_8(res);
    testSign_8(res);
    rstAddSub();
    testParity_8(res);
    z80.F &= ~(FLAG_CARRY | FLAG_HCARRY);

    z80.A = res;
    if(logInstr) {
      fprintf(fpLog, "OR (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
    }
  }

  // This is XOR (IX+d) instruction
  else if(follByte == 0xAE) {
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IX + extended_d);
    u8 res = z80.A ^ value;

    testZero_8(res);
    testSign_8(res);
    rstAddSub();
    testParity_8(res);
    z80.F &= ~(FLAG_CARRY | FLAG_HCARRY);

    z80.A = res;
    if(logInstr) {
      fprintf(fpLog, "XOR (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
    }
  }

  // This is CP (IX+d) instruction
  else if(follByte == 0xBE) {
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IX + extended_d);
    u8 complVal = ~value + 1;
    u8 res = z80.A + complVal;

    testSign_8(res);
    testZero_8(res);
    setAddSub();
    testOverflow_8(z80.A, complVal, res);
    testHalfCarry_8(z80.A, complVal, 0);
    testCarry_8(z80.A, complVal, 0);
    invertHC();

    if(logInstr) {
      fprintf(fpLog, "CP (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
    }
  }

  // This is INC (IX+d) instruction
  else if(follByte == 0x34) {
    opTbl[0xDD].TStates = 23;
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IX + extended_d);
    u8 res = value + 1;

    testSign_8(res);
    testZero_8(res);
    testHalfCarry_8(value, 1, 0);
    testOverflow_8(value, 1, res);
    rstAddSub();

    writeByte(res, z80.IX + extended_d);
    if(logInstr) {
      fprintf(fpLog, "INC (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
    }
  }

  // This is DEC (IX+d) instruction
  else if(follByte == 0x35) {
    opTbl[0xDD].TStates = 23;
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IX + extended_d);
    u8 complDec = 0xFF;	// -1
    u8 res = value + complDec;
    
    testSign_8(res);
    testZero_8(res);
    testHalfCarry_8(value, complDec, 0);
    testOverflow_8(value, complDec, res);
    setAddSub();
    invertHC();
    z80.F ^= (FLAG_CARRY);	// De-invert carry flag

    writeByte(res, z80.IX + extended_d);
    if(logInstr) {
      fprintf(fpLog, "DEC (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
    }
  }

  // This is ADD IX, pp instruction
  else if((follByte & 0xCF) == 0x09) {
    opTbl[0xDD].TStates = 15;
    u16 val1 = z80.IX;
    u16 val2;
    u8 src = ((follByte >> 4) & 0x03);

    switch(src) {
      case 0x00:
        val2 = z80.BC;
        break;
      case 0x01:
        val2 = z80.DE;
        break;
      case 0x02:
        val2 = z80.IX;
        break;
      case 0x03:
        val2 = z80.SP;
        break;
      default:
        die("[ERROR] Invalid pp in ADD IX, pp instruction.");
    }

    u16 res = val1 + val2;

    // TODO: half carry?
    rstAddSub();
    testCarry_16(val1, val2, 0);

    z80.IX = res;
    if(logInstr) {
      writeLog("ADD IX, "); logReg16(src, 0); writeLog("\n");
    }
  }

  // This is INC IX instruction
  else if(follByte == 0x23) {
    opTbl[0xDD].TStates = 10;
    z80.IX++;
    if(logInstr) {
      writeLog("INC IX\n");
    }
  }

  // This is DEC IX instruction
  else if(follByte == 0x2B) {
    opTbl[0xDD].TStates = 10;
    z80.IX--;
    if(logInstr) {
      writeLog("DEC IX\n");
    }
  }

  // This is JP (IX) instruction
  else if(follByte == 0xE9) {
    opTbl[0xDD].TStates = 8;
    z80.PC = z80.IX;
    if(logInstr) {
      fprintf(fpLog, "JP (IX)\t\tIX = %04X\n", z80.IX);
    }
  }

  else if(follByte == 0xCB) {
    u8 d = fetch8();  // 3rd instruction byte
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 controlByte = fetch8(); // 4th instruction byte

    // This is RLC (IX+d) instruction
    if(controlByte == 0x06) {
      opTbl[0xDD].TStates = 23;
      u8 value = readByte(z80.IX + extended_d);
      u8 v_msb = (value & 0x80) >> 7;

      // Shift left by one
      u8 res = ((value << 1) | v_msb);

      // Update carry flag with old msb
      if(v_msb)
        z80.F |= FLAG_CARRY;
      else
        z80.F &= ~(FLAG_CARRY);

      testSign_8(res);
      testZero_8(res);
      z80.F &= ~(FLAG_HCARRY);
      testParity_8(res);
      rstAddSub();

      writeByte(res, z80.IX + extended_d);
      if(logInstr) {
        fprintf(fpLog, "RLC (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
      }
    }

    // This is BIT b, (IX+d) instruction
    else if((controlByte & 0xC7) == 0x46) {
      opTbl[0xDD].TStates = 20;
      u8 bit = ((controlByte >> 3) & 0x07); // Bit to test
      u8 value = readByte(z80.IX + extended_d);

      value = ((value >> bit) & 0x01);

      testZero_8(value);
      z80.F |= FLAG_HCARRY;
      rstAddSub();

      if(logInstr) {
        fprintf(fpLog, "BIT %01d, (IX+d)\t\tIX+d = %04X\n", bit, z80.IX + extended_d);
      }
    }

    // This is SET b, (IX+d) instruction
    else if((controlByte & 0xC7) == 0xC6) {
      opTbl[0xDD].TStates = 23;
      u8 bit = ((controlByte >> 3) & 0x07); // Bit to test
      u8 value = readByte(z80.IX + extended_d);

      value |= (1 << bit);

      writeByte(value, z80.IX + extended_d);
      if(logInstr) {
        fprintf(fpLog, "SET %01d, (IX+d)\t\tIX+d = %04X\n", bit, z80.IX + extended_d);
      }
    }

    // This is RES b, (IX+d) instruction
    else if((controlByte & 0xC7) == 0x86) {
      opTbl[0xDD].TStates = 23;
      u8 bit = ((controlByte >> 3) & 0x07); // Bit to test
      u8 value = readByte(z80.IX + extended_d);

      value &= ~(1 << bit);

      writeByte(value, z80.IX + extended_d);
      if(logInstr) {
        fprintf(fpLog, "RES %01d, (IX+d)\t\tIX+d = %04X\n", bit, z80.IX + extended_d);
      }
    }

    // This is RL (IX+d) instruction
    else if(controlByte == 0x16) {
      opTbl[0xDD].TStates = 23;
      u8 oldCarry = (z80.F & FLAG_CARRY);
      u8 value = readByte(z80.IX + extended_d);

      // Update carry flag with value msb
      if(value & 0x80)
        z80.F |= FLAG_CARRY;
      else
        z80.F &= ~(FLAG_CARRY);

      u8 res = ((value << 1) | oldCarry);

      testSign_8(res);
      testZero_8(res);
      z80.F &= ~(FLAG_HCARRY);
      testParity_8(res);
      rstAddSub();

      writeByte(res, z80.IX + extended_d);
      if(logInstr) {
        fprintf(fpLog, "RL (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
      }
    }

    // This is RRC (IX+d) instruction
    else if(controlByte == 0x0E) {
      opTbl[0xDD].TStates = 23;
      u8 value = readByte(z80.IX + extended_d);
      u8 v_lsb = (value & 0x01);

      // Copy r LSB to carry flag
      if(v_lsb)
        z80.F |= FLAG_CARRY;
      else
        z80.F &= ~(FLAG_CARRY);

      u8 res = ((value >> 1) | (v_lsb << 7));

      testSign_8(res);
      testZero_8(res);
      z80.F &= ~(FLAG_HCARRY);
      testParity_8(res);
      rstAddSub();

      writeByte(value, z80.IX + extended_d);
      if(logInstr) {
        fprintf(fpLog, "RRC (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
      }
    }

    // This is RR (IX+d) instruction
    else if(controlByte == 0x1E) {
      opTbl[0xDD].TStates = 23;
      u8 oldCarry = (z80.F & FLAG_CARRY);
      u8 value = readByte(z80.IX + extended_d);

      // Copy bit 0 to carry bit
      if(value & 0x01)
        z80.F |= FLAG_CARRY;
      else
        z80.F &= ~(FLAG_CARRY);

      u8 res = ((value >> 1) | (oldCarry << 7));

      testSign_8(res);
      testZero_8(res);
      z80.F &= ~(FLAG_HCARRY);
      testParity_8(res);
      rstAddSub();

      writeByte(res, z80.IX + extended_d);
      if(logInstr) {
        fprintf(fpLog, "RR (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
      }
    }

    // This is SLA (IX+d) instruction
    else if(controlByte == 0x26) {
      opTbl[0xDD].TStates = 23;
      u8 value = readByte(z80.IX + extended_d);

      // Copy bit 7 to carry bit
      if(value & 0x80)
        z80.F |= FLAG_CARRY;
      else
        z80.F &= ~(FLAG_CARRY);

      u8 res = (value << 1); // Shift left by one

      testSign_8(res);
      testZero_8(res);
      z80.F &= ~(FLAG_HCARRY);
      testParity_8(res);
      rstAddSub();

      writeByte(res, z80.IX + extended_d);
      if(logInstr) {
        fprintf(fpLog, "SLA (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
      }
    }

    // This is SRA (IX+d) instruction
    else if(controlByte == 0x2E) {
      opTbl[0xDD].TStates = 23;
      u8 value = readByte(z80.IX + extended_d);
      u8 v_msb = (value & 0x80);

      // Copy bit 0 to carry flag
      if(value & 0x01)
        z80.F |= FLAG_CARRY;
      else
        z80.F &= ~(FLAG_CARRY);

      u8 res = ((value >> 1) | v_msb); // Shift right by one

      testSign_8(res);
      testZero_8(res);
      z80.F &= ~(FLAG_HCARRY);
      testParity_8(res);
      rstAddSub();

      writeByte(res, z80.IX + extended_d);
      if(logInstr) {
        fprintf(fpLog, "SRA (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
      }
    }

    // This is SRL (IX+d) instruction
    else if(controlByte == 0x3E) {
      opTbl[0xDD].TStates = 23;
      u8 value = readByte(z80.IX + extended_d);

      // Copy bit 0 to carry bit
      if(value & 0x01)
        z80.F |= FLAG_CARRY;
      else
        z80.F &= ~(FLAG_CARRY);

      u8 res = (value >> 1); // Shift right by one

      z80.F &= ~(FLAG_SIGN | FLAG_HCARRY);
      testZero_8(res);
      testParity_8(res);
      rstAddSub();

      writeByte(res, z80.IX + extended_d);
      if(logInstr) {
        fprintf(fpLog, "SRL (IX+d)\t\tIX+d = %04X\n", z80.IX + extended_d);
      }
    }
    else die("[ERROR] Invalid instruction in IX BIT, SET, RESET group or in Rotate and Shift group.");
  }
  else die("[ERROR] Invalid operation in 0xDD instruction group.");
}


void LDIY(u8 opcode) {
  opTbl[0xFD].TStates = 19;
  u8 follByte = fetch8();

  // This is the LD r, (IY+d) instruction
  if((follByte & 0xC7) == 0x46) {
    u8 dst = ((follByte >> 3) & 0x07);
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u16 offset = z80.IY + extended_d;
    u8 val = readByte(offset);
    writeReg(val, dst);
    if(logInstr) {
      writeLog("LD "); logReg8(dst); fprintf(fpLog, ", (IY+d)\t\tIY+d = %04X\n", offset);
    }
  }

  // This is LD (IY+d), r instruction
  else if((follByte & 0xF8) == 0x70) {
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u16 offset = z80.IY + extended_d;
    u8 src = (follByte & 0x07);
    u8 val = readReg(src);
    writeByte(val, offset);
    if(logInstr) {
      writeLog("LD (IY+d), "); logReg8(src); fprintf(fpLog, "\t\tIY+d = %04X\n", offset);
    }
  }

  // This is LD (IY+d), n instruction
  else if(follByte == 0x36) {
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 n = fetch8();
    u16 offset = z80.IY + extended_d;
    writeByte(n, offset);
    if(logInstr) {
      fprintf(fpLog, "LD (IY+d), n\t\tIY+d = %04X, n = %02X\n", offset, n);
    }
  }

  // This is LD IY, nn instruction
  else if(follByte == 0x21) {
    opTbl[0xFD].TStates = 14;
    u16 nn = fetch16();
    z80.IY = nn;
    if(logInstr) {
      fprintf(fpLog, "LD IY, nn\t\tnn = %04X\n", nn);
    }
  }

  // This is LD IY, (nn) instruction
  else if(follByte == 0x2A) {
    opTbl[0xFD].TStates = 20;
    u16 offset = fetch16();
    z80.IY = (readByte(offset) | (readByte(offset + 1) << 8));
    if(logInstr) {
      fprintf(fpLog, "LD IY, (nn)\t\tnn = %04X\n", offset);
    }
  }

  // This is LD (nn), IY instruction
  else if(follByte == 0x22) {
    opTbl[0xFD].TStates = 20;
    u16 offset = fetch16();
    writeByte((z80.IY >> 8) & 0x00FF, offset + 1);  // (nn+1) <- IYh
    writeByte(z80.IY & 0x00FF, offset);             // (nn) <- IYl
    if(logInstr) {
      fprintf(fpLog, "LD (nn), IY\t\tnn = %04X\n", offset);
    }
  }

  // This is LD SP, IY instruction
  else if(follByte == 0xF9) {
    opTbl[0xFD].TStates = 10;
    z80.SP = z80.IY;
    if(logInstr) {
      fprintf(fpLog, "LD SP, IY\t\tIY = %04X\n", z80.IY);
    }
  }

  // This is PUSH IY instruction
  else if(follByte == 0xE5) {
    opTbl[0xFD].TStates = 15;
    stackPush(z80.IY);
    if(logInstr) {
      writeLog("PUSH IY\n");
    }
  }

  // This is POP IY instruction
  else if(follByte == 0xE1) {
    opTbl[0xFD].TStates = 14;
    z80.IY = stackPop();
    if(logInstr) {
      writeLog("POP IY\n");
    }
  }

  // This is EX (SP), IY instruction
  else if(follByte == 0xE3) {
    opTbl[0xFD].TStates = 23;
    u8 valSP  = readByte(z80.SP);
    u8 valSPH = readByte(z80.SP + 1);
    writeByte(z80.IY & 0x00FF, z80.SP);
    writeByte((z80.IY >> 8) & 0x00FF, z80.SP + 1);
    z80.IY = (valSP | (valSPH << 8));
    if(logInstr) {
      writeLog("EX (SP), IY"); fprintf(fpLog, "\t\tSP = %04X\n", z80.SP);
    }
  }

  // This is ADD A, (IY+d) instruction
  else if(follByte == 0x86){
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IY + extended_d);
    u8 res = z80.A + value;

    testZero_8(res);
    testSign_8(res);
    testHalfCarry_8(z80.A, value, 0);
    rstAddSub();
    testCarry_8(z80.A, value, 0);
    testOverflow_8(z80.A, value, res);

    z80.A = res;
    if(logInstr) {
      fprintf(fpLog, "ADD A, (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
    }
  }

  // This is ADC A, (IY+d) instruction
  else if(follByte == 0x8E){
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IY + extended_d);
    u8 carry = (z80.F & FLAG_CARRY);
    u8 res = z80.A + value;

    testZero_8(res);
    testSign_8(res);
    testHalfCarry_8(z80.A, value, carry);
    rstAddSub();
    testCarry_8(z80.A, value, carry);
    testOverflow_8(z80.A, value, res);

    z80.A = res;
    if(logInstr) {
      fprintf(fpLog, "ADC A, (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
    }
  }

  // This is SUB A, (IY+d) instruction
  else if(follByte == 0x96){
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IY + extended_d);
    u8 complVal = ~value + 1;
    u8 res = z80.A + complVal;

    testZero_8(res);
    testSign_8(res);
    setAddSub();
    testOverflow_8(z80.A, complVal, res);
    testHalfCarry_8(z80.A, complVal, 0);
    testCarry_8(z80.A, complVal, 0);
    invertHC();

    z80.A = res;
    if(logInstr) {
      fprintf(fpLog, "SUB A, (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
    }
  }

  // This is SBC A, (IY+d) instruction
  else if(follByte == 0x9E){
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IY + extended_d);
    u8 not_carry = (z80.F & FLAG_CARRY) ^ FLAG_CARRY;
    // a - b - c = a + ~b + 1 - c = a + ~b + !c
    u8 res = z80.A + ~value + not_carry;

    testZero_8(res);
    testSign_8(res);
    setAddSub();
    testOverflow_8(z80.A, ~value, res);
    testHalfCarry_8(z80.A, ~value, not_carry);
    testCarry_8(z80.A, ~value, not_carry);
    invertHC();

    z80.A = res;
    if(logInstr) {
      fprintf(fpLog, "SBC A, (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
    }
  }

  // This is AND (IY+d) instruction
  else if(follByte == 0xA6) {
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IY + extended_d);
    u8 res = z80.A & value;

    testZero_8(res);
    testSign_8(res);
    rstAddSub();
    testParity_8(res);
    z80.F &= ~(FLAG_CARRY);
    z80.F |= (FLAG_HCARRY);

    z80.A = res;
    if(logInstr) {
      fprintf(fpLog, "AND (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
    }
  }

  // This is OR (IY+d) instruction
  else if(follByte == 0xB6) {
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IY + extended_d);
    u8 res = z80.A | value;

    testZero_8(res);
    testSign_8(res);
    rstAddSub();
    testParity_8(res);
    z80.F &= ~(FLAG_CARRY | FLAG_HCARRY);

    z80.A = res;
    if(logInstr) {
      fprintf(fpLog, "OR (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
    }
  }

  // This is XOR (IY+d) instruction
  else if(follByte == 0xAE) {
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IY + extended_d);
    u8 res = z80.A ^ value;

    testZero_8(res);
    testSign_8(res);
    rstAddSub();
    testParity_8(res);
    z80.F &= ~(FLAG_CARRY | FLAG_HCARRY);

    z80.A = res;
    if(logInstr) {
      fprintf(fpLog, "XOR (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
    }
  }

  // This is CP (IY+d) instruction
  else if(follByte == 0xBE) {
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IY + extended_d);
    u8 complVal = ~value + 1;
    u8 res = z80.A + complVal;

    testSign_8(res);
    testZero_8(res);
    setAddSub();
    testOverflow_8(z80.A, complVal, res);
    testHalfCarry_8(z80.A, complVal, 0);
    testCarry_8(z80.A, complVal, 0);
    invertHC();

    if(logInstr) {
      fprintf(fpLog, "CP (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
    }
  }

  // This is INC (IY+d) instruction
  else if(follByte == 0x34) {
    opTbl[0xFD].TStates = 23;
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IY + extended_d);
    u8 res = value + 1;

    testSign_8(res);
    testZero_8(res);
    testHalfCarry_8(value, 1, 0);
    testOverflow_8(value, 1, res);
    rstAddSub();

    writeByte(res, z80.IY + extended_d);
    if(logInstr) {
      fprintf(fpLog, "INC (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
    }
  }

  // This is DEC (IY+d) instruction
  else if(follByte == 0x35) {
    opTbl[0xFD].TStates = 23;
    u8 d = fetch8();
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 value = readByte(z80.IY + extended_d);
    u8 complDec = 0xFF; // -1
    u8 res = value + complDec;

    testSign_8(res);
    testZero_8(res);
    testHalfCarry_8(value, complDec, 0);
    testOverflow_8(value, complDec, res);
    setAddSub();
    invertHC();
    z80.F ^= (FLAG_CARRY);	// De-invert carry flag

    writeByte(res, z80.IY + extended_d);
    if(logInstr) {
      fprintf(fpLog, "DEC (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
    }
  }

  // This is ADD IY, rr instruction
  else if((follByte & 0xCF) == 0x09) {
    opTbl[0xFD].TStates = 15;
    u16 val1 = z80.IY;
    u16 val2;
    u8 src = ((follByte >> 4) & 0x03);

    switch(src) {
      case 0x00:
        val2 = z80.BC;
        break;
      case 0x01:
        val2 = z80.DE;
        break;
      case 0x02:
        val2 = z80.IY;
        break;
      case 0x03:
        val2 = z80.SP;
        break;
      default:
        die("[ERROR] Invalid rr in ADD IY, rr instruction.");
    }

    u16 res = val1 + val2;

    // TODO: half carry?
    rstAddSub();
    testCarry_16(val1, val2, 0);

    z80.IY = res;
    if(logInstr) {
      writeLog("ADD IY, "); logReg16(src, 0); writeLog("\n");
    }
  }

  // This is INC IY instruction
  else if(follByte == 0x23) {
    opTbl[0xFD].TStates = 10;
    z80.IY++;
    if(logInstr) {
      writeLog("INC IY\n");
    }
  }

  // This is DEC IY instruction
  else if(follByte == 0x2B) {
    opTbl[0xFD].TStates = 10;
    z80.IY--;
    if(logInstr) {
      writeLog("DEC IY\n");
    }
  }

  // This is JP (IY) instruction
  else if(follByte == 0xE9) {
    opTbl[0xFD].TStates = 8;
    z80.PC = z80.IY;
    if(logInstr) {
      fprintf(fpLog, "JP (IY)\t\tIY = %04X\n", z80.IY);
    }
  }

  else if(follByte == 0xCB) {
    u8 d = fetch8();  // 3rd instruction byte
    u16 extended_d = d;
    if(isNegative(d))
      extended_d |= 0xFF00;
    u8 controlByte = fetch8(); // 4th instruction byte

    // This is RLC (IY+d) instruction
    if(controlByte == 0x06) {
      opTbl[0xFD].TStates = 23;
      u8 value = readByte(z80.IY + extended_d);
      u8 v_msb = (value & 0x80) >> 7;

      // Shift left by one
      u8 res = ((value << 1) | v_msb);

      // Update carry flag with old msb
      if(v_msb)
        z80.F |= FLAG_CARRY;
      else
        z80.F &= ~(FLAG_CARRY);

      testSign_8(res);
      testZero_8(res);
      z80.F &= ~(FLAG_HCARRY);
      testParity_8(res);
      rstAddSub();

      writeByte(res, z80.IY + extended_d);
      if(logInstr) {
        fprintf(fpLog, "RLC (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
      }
    }

    // This is BIT b, (IY+d) instruction
    else if((controlByte & 0xC7) == 0x46) {
      opTbl[0xFD].TStates = 20;
      u8 bit = ((controlByte >> 3) & 0x07); // Bit to test
      u8 value = readByte(z80.IY + extended_d);

      value = ((value >> bit) & 0x01);

      testZero_8(value);
      z80.F |= FLAG_HCARRY;
      rstAddSub();

      if(logInstr) {
        fprintf(fpLog, "BIT %01d, (IY+d)\t\tIY+d = %04X\n", bit, z80.IY + extended_d);
      }
    }

    // This is SET b, (IY+d) instruction
    else if((controlByte & 0xC7) == 0xC6) {
      opTbl[0xFD].TStates = 23;
      u8 bit = ((controlByte >> 3) & 0x07); // Bit to test
      u8 value = readByte(z80.IY + extended_d);

      value |= (1 << bit);

      writeByte(value, z80.IY + extended_d);
      if(logInstr) {
        fprintf(fpLog, "SET %01d, (IY+d)\t\tIY+d = %04X\n", bit, z80.IY + extended_d);
      }
    }

    // This is RES b, (IY+d) instruction
    else if((controlByte & 0xC7) == 0x86) {
      opTbl[0xFD].TStates = 23;
      u8 bit = ((controlByte >> 3) & 0x07); // Bit to test
      u8 value = readByte(z80.IY + extended_d);

      value &= ~(1 << bit);

      writeByte(value, z80.IY + extended_d);
      if(logInstr) {
        fprintf(fpLog, "RES %01d, (IY+d)\t\tIY+d = %04X\n", bit, z80.IY + extended_d);
      }
    }

    // This is RL (IY+d) instruction
    else if(controlByte == 0x16) {
      opTbl[0xFD].TStates = 23;
      u8 oldCarry = (z80.F & FLAG_CARRY);
      u8 value = readByte(z80.IY + extended_d);

      // Update carry flag with value msb
      if(value & 0x80)
        z80.F |= FLAG_CARRY;
      else
        z80.F &= ~(FLAG_CARRY);

      u8 res = ((value << 1) | oldCarry);

      testSign_8(res);
      testZero_8(res);
      z80.F &= ~(FLAG_HCARRY);
      testParity_8(res);
      rstAddSub();

      writeByte(res, z80.IY + extended_d);
      if(logInstr) {
        fprintf(fpLog, "RL (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
      }
    }

    // This is RRC (IY+d) instruction
    else if(controlByte == 0x0E) {
      opTbl[0xFD].TStates = 23;
      u8 value = readByte(z80.IY + extended_d);
      u8 v_lsb = (value & 0x01);

      // Copy r LSB to carry flag
      if(v_lsb)
        z80.F |= FLAG_CARRY;
      else
        z80.F &= ~(FLAG_CARRY);

      u8 res = ((value >> 1) | (v_lsb << 7));

      testSign_8(res);
      testZero_8(res);
      z80.F &= ~(FLAG_HCARRY);
      testParity_8(res);
      rstAddSub();

      writeByte(value, z80.IY + extended_d);
      if(logInstr) {
        fprintf(fpLog, "RRC (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
      }
    }

    // This is RR (IY+d) instruction
    else if(controlByte == 0x1E) {
      opTbl[0xFD].TStates = 23;
      u8 oldCarry = (z80.F & FLAG_CARRY);
      u8 value = readByte(z80.IY + extended_d);

      // Copy bit 0 to carry bit
      if(value & 0x01)
        z80.F |= FLAG_CARRY;
      else
        z80.F &= ~(FLAG_CARRY);

      u8 res = ((value >> 1) | (oldCarry << 7));

      testSign_8(res);
      testZero_8(res);
      z80.F &= ~(FLAG_HCARRY);
      testParity_8(res);
      rstAddSub();

      writeByte(res, z80.IY + extended_d);
      if(logInstr) {
        fprintf(fpLog, "RR (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
      }
    }

    // This is SLA (IY+d) instruction
    else if(controlByte == 0x26) {
      opTbl[0xFD].TStates = 23;
      u8 value = readByte(z80.IY + extended_d);

      // Copy bit 7 to carry bit
      if(value & 0x80)
        z80.F |= FLAG_CARRY;
      else
        z80.F &= ~(FLAG_CARRY);

      u8 res = (value << 1); // Shift left by one

      testSign_8(res);
      testZero_8(res);
      z80.F &= ~(FLAG_HCARRY);
      testParity_8(res);
      rstAddSub();

      writeByte(res, z80.IY + extended_d);
      if(logInstr) {
        fprintf(fpLog, "SLA (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
      }
    }

    // This is SRA (IY+d) instruction
    else if(controlByte == 0x2E) {
      opTbl[0xFD].TStates = 23;
      u8 value = readByte(z80.IY + extended_d);
      u8 v_msb = (value & 0x80);

      // Copy bit 0 to carry flag
      if(value & 0x01)
        z80.F |= FLAG_CARRY;
      else
        z80.F &= ~(FLAG_CARRY);

      u8 res = ((value >> 1) | v_msb); // Shift right by one

      testSign_8(res);
      testZero_8(res);
      z80.F &= ~(FLAG_HCARRY);
      testParity_8(res);
      rstAddSub();

      writeByte(res, z80.IY + extended_d);
      if(logInstr) {
        fprintf(fpLog, "SRA (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
      }
    }

    // This is SRL (IY+d) instruction
    else if(controlByte == 0x3E) {
      opTbl[0xFD].TStates = 23;
      u8 value = readByte(z80.IY + extended_d);

      // Copy bit 0 to carry bit
      if(value & 0x01)
        z80.F |= FLAG_CARRY;
      else
        z80.F &= ~(FLAG_CARRY);

      u8 res = (value >> 1); // Shift right by one

      z80.F &= ~(FLAG_SIGN | FLAG_HCARRY);
      testZero_8(res);
      testParity_8(res);
      rstAddSub();

      writeByte(res, z80.IY + extended_d);
      if(logInstr) {
        fprintf(fpLog, "SRL (IY+d)\t\tIY+d = %04X\n", z80.IY + extended_d);
      }
    }
    else die("[ERROR] Invalid instruction in IY BIT, SET, RESET group or in Rotate and Shift group.");
  }
  else die("[ERROR] Invalid operation in 0xFD instruction group.");
}


// This is LD (HL), r instruction
void LDHLr(u8 opcode) {
  u8 src = (opcode & 0x07);
  u8 srcVal = readReg(src);
  writeByte(srcVal, z80.HL);
  if(logInstr) {
    writeLog("LD (HL), "); logReg8(src); fprintf(fpLog, "\t\tHL = %04X\n", z80.HL);
  }
}


// This is LD (HL), n instruction
void LDHLn(u8 opcode) {
  u8 n = fetch8();
  writeByte(n, z80.HL);
  if(logInstr) {
    fprintf(fpLog, "LD (HL), n\t\tHL = %04X, n = %02X\n", z80.HL, n);
  }
}


// This is LD A, (BC) instruction
void LDABC(u8 opcode) {
  z80.A = readByte(z80.BC);
  if(logInstr) {
    fprintf(fpLog, "LD A, (BC)\t\tBC = %04X\n", z80.BC);
  }
}


// This is LD A, (DE) instruction
void LDADE(u8 opcode) {
  z80.A = readByte(z80.DE);
  if(logInstr) {
    fprintf(fpLog, "LD A, (DE)\t\tDE = %04X\n", z80.DE);
  }
}


// This is LD A, (nn) instruction
void LDAnn(u8 opcode) {
  u16 offset = fetch16();
  z80.A = readByte(offset);
  if(logInstr) {
    fprintf(fpLog, "LD A, (nn)\t\tnn = %04X\n", offset);
  }
}


// This is LD (BC), A
void LDBCA(u8 opcode) {
  writeByte(z80.A, z80.BC);
  if(logInstr) {
    fprintf(fpLog, "LD (BC), A\t\tBC = %04X\n", z80.BC);
  }
}


// This is LD (DE), A
void LDDEA(u8 opcode) {
  writeByte(z80.A, z80.DE);
  if(logInstr) {
    fprintf(fpLog, "LD (DE), A\t\tDE = %04X\n", z80.DE);
  }
}


// This is LD (nn), A
void LDnnA(u8 opcode) {
  u16 offset = fetch16();
  writeByte(z80.A, offset);
  if(logInstr) {
    fprintf(fpLog, "LD (nn), A\t\tnn = %04X\n", offset);
  }
}


void LDRIddnn(u8 opcode) {
  opTbl[0xED].TStates = 9;
  u8 follByte = fetch8();

  // This is LD A, I instruction
  if(follByte == 0x57) {
    z80.A = z80.I;
    // Condition bits
    if(isNegative(z80.I))
      z80.F |= FLAG_SIGN;
    else
      z80.F &= ~(FLAG_SIGN);

    if(z80.I == 0x00)
      z80.F |= FLAG_ZERO;
    else
      z80.F &= ~(FLAG_ZERO);

    z80.F &= ~(FLAG_ADDSUB | FLAG_HCARRY);
    if(z80.IFF2 & 0x01)
      z80.F |= FLAG_PARITY;
    else
      z80.F &= ~(FLAG_PARITY);
    if(logInstr) {
      writeLog("LD A, I");
    }
  }

  // This is LD A, R instruction
  else if(follByte == 0x5F) {
    z80.A = z80.R;

    // Condition bits
    if(isNegative(z80.R))
      z80.F |= FLAG_SIGN;
    else
      z80.F &= ~(FLAG_SIGN);

    if(z80.R == 0x00)
      z80.F |= FLAG_ZERO;
    else
      z80.F &= ~(FLAG_ZERO);

    z80.F &= ~(FLAG_ADDSUB | FLAG_HCARRY);
    if(z80.IFF2 & 0x01)
      z80.F |= FLAG_PARITY;
    else
      z80.F &= ~(FLAG_PARITY);
    if(logInstr) {
      writeLog("LD A, R");
    }
  }

  // This is LD I, A instruction
  else if(follByte == 0x47) {
    z80.I = z80.A;
    if(logInstr) {
      writeLog("LD I, A");
    }
  }

  // This is LD R, A instruction
  else if(follByte == 0x4F) {
    z80.R = z80.A;
    if(logInstr) {
      writeLog("LD R, A");
    }
  }

  // This is LD dd, (nn)
  else if((follByte & 0xCF) == 0x4B) {
    opTbl[0xED].TStates = 20;
    u8 dst = ((follByte >> 4) & 0x03);
    u16 offset = fetch16();
    u16 res = (readByte(offset) | (readByte(offset + 1) << 8));
    switch(dst) {
      case 0x00:
        z80.BC = res;
        break;
      case 0x01:
        z80.DE = res;
        break;
      case 0x02:
        z80.HL = res;
        break;
      case 0x03:
        z80.SP = res;
        break;
      default:
        die("[ERROR] Wrong destination register for LD dd, (nn) instruction.\n");
    }
    if(logInstr) {
      writeLog("LD "); logReg16(dst, 0); fprintf(fpLog, ", (nn)\t\tnn = %04X\n", offset);
    }
  }

  // This is LD (nn), dd
  else if((follByte & 0xCF) == 0x43) {
    opTbl[0xED].TStates = 20;
    u8 src = ((follByte >> 4) & 0x03);
    u16 offset = fetch16();
    switch(src) {
      case 0x00:
        writeByte(z80.B, offset + 1);
        writeByte(z80.C, offset);
        break;
      case 0x01:
        writeByte(z80.D, offset + 1);
        writeByte(z80.E, offset);
        break;
      case 0x02:
        writeByte(z80.H, offset + 1);
        writeByte(z80.L, offset);
        break;
      case 0x03:
        writeByte((z80.SP >> 8) & 0x00FF, offset + 1);
        writeByte(z80.SP & 0x00FF, offset);
        break;
      default:
        die("[ERROR] Wrong source register for LD (nn), dd instruction.\n");
    }
    if(logInstr) {
      writeLog("LD (nn), "); logReg16(src, 0); fprintf(fpLog, "\t\tnn = %04X\n", offset);
    }
  }

  // This is LDI instruction
  else if(follByte == 0xA0) {
    opTbl[0xED].TStates = 16;
    u8 valueHL = readByte(z80.HL);
    writeByte(valueHL, z80.DE);
    z80.DE++;
    z80.HL++;
    z80.BC--;
    z80.F &= ~(FLAG_HCARRY | FLAG_ADDSUB); // Reset H and N
    // P/V is set if BC-1 != 0
    if(z80.BC == 0) {
      z80.F &= ~(FLAG_PARITY); // Reset parity bit
    }
    else z80.F |= FLAG_PARITY;
    if(logInstr) {
      writeLog("LDI\n");
    }
  }

  // This is LDIR instruction
  else if(follByte == 0xB0) {
    u8 valueHL = readByte(z80.HL);
    writeByte(valueHL, z80.DE);
    z80.DE++;
    z80.HL++;
    z80.BC--;
    z80.F &= ~(FLAG_HCARRY | FLAG_ADDSUB | FLAG_PARITY); // Reset H, N and P/V

    if(z80.BC != 0) {
      z80.PC -= 2;
      opTbl[0xED].TStates = 21;
    }
    else {
      opTbl[0xED].TStates = 16;
    }
    if(logInstr) {
      writeLog("LDIR\n");
    }
  }

  // This is LDD instruction
  else if(follByte == 0xA8) {
    opTbl[0xED].TStates = 16;
    u8 valueHL = readByte(z80.HL);
    writeByte(valueHL, z80.DE);
    z80.DE--;
    z80.HL--;
    z80.BC--;
    z80.F &= ~(FLAG_HCARRY | FLAG_ADDSUB); // Reset H and N
    // P/V is set if BC-1 != 0
    if(z80.BC == 0) {
      z80.F &= ~(FLAG_PARITY); // Reset parity bit
    }
    else z80.F |= FLAG_PARITY;
    if(logInstr) {
      writeLog("LDD\n");
    }
  }

  // This is LDDR instruction
  else if(follByte == 0xB8) {
    u8 valueHL = readByte(z80.HL);
    writeByte(valueHL, z80.DE);
    z80.DE--;
    z80.HL--;
    z80.BC--;
    z80.F &= ~(FLAG_HCARRY | FLAG_ADDSUB | FLAG_PARITY); // Reset H, N and P/V

    if(z80.BC != 0) {
      z80.PC -= 2;
      opTbl[0xED].TStates = 21;
    }
    else {
      opTbl[0xED].TStates = 16;
    }
    if(logInstr) {
      writeLog("LDDR\n");
    }
  }

  // This is CPI instruction
  else if(follByte == 0xA1) {
    opTbl[0xED].TStates = 16;
    u8 valueHL = readByte(z80.HL);
    u8 complHL = ~valueHL + 1;
    u8 tmp = z80.A + complHL; // Perform subtraction
    z80.HL++;
    z80.BC--;

    testZero_8(tmp);
    testSign_8(tmp);
    testHalfCarry_8(z80.A, complHL, 0);
    setAddSub();

    if(z80.BC == 0) {
      z80.F &= ~(FLAG_PARITY);
    }
    else {
      z80.F |= FLAG_PARITY;
    }

    if(logInstr) {
      writeLog("CPI\n");
    }
  }

  // This is CPIR instruction
  else if(follByte == 0xB1) {
    u8 valueHL = readByte(z80.HL);
    u8 complHL = ~valueHL + 1;
    u8 tmp = z80.A + complHL; // Perform subtraction
    z80.HL++;
    z80.BC--;

    testZero_8(tmp);
    testSign_8(tmp);
    testHalfCarry_8(z80.A, complHL, 0);
    setAddSub();

    if(z80.BC == 0) {
      z80.F &= ~(FLAG_PARITY);
    }
    else {
      z80.F |= FLAG_PARITY;
      z80.PC -= 2;
    }

    if((z80.BC != 0) && (tmp != 0)) {
      opTbl[0xED].TStates = 21;
    }
    else {
      opTbl[0xED].TStates = 16;
    }

    if(logInstr) {
      writeLog("CPIR\n");
    }
  }

  // This is CPD instruction
  else if(follByte == 0xA9) {
    opTbl[0xED].TStates = 16;
    u8 valueHL = readByte(z80.HL);
    u8 complHL = ~valueHL + 1;
    u8 tmp = z80.A + complHL; // Perform subtraction
    z80.HL--;
    z80.BC--;

    testZero_8(tmp);
    testSign_8(tmp);
    testHalfCarry_8(z80.A, complHL, 0);
    setAddSub();

    if(z80.BC == 0) {
      z80.F &= ~(FLAG_PARITY);
    }
    else {
      z80.F |= FLAG_PARITY;
    }

    if(logInstr) {
      writeLog("CPD\n");
    }
  }

  // This is CPDR instruction
  else if(follByte == 0xB9) {
    u8 valueHL = readByte(z80.HL);
    u8 complHL = ~valueHL + 1;
    u8 tmp = z80.A - complHL;
    z80.HL--;
    z80.BC--;

    testZero_8(tmp);
    testSign_8(tmp);
    testHalfCarry_8(z80.A, complHL, 0);
    setAddSub();

    if(z80.BC == 0) {
      z80.F &= ~(FLAG_PARITY);
    }
    else {
      z80.F |= FLAG_PARITY;
      z80.PC -= 2;
    }

    if((z80.BC != 0) && (tmp != 0)) {
      opTbl[0xED].TStates = 21;
    }
    else {
      opTbl[0xED].TStates = 16;
    }

    if(logInstr) {
      writeLog("CPDR\n");
    }
  }

  // This is NEG instruction
  else if(follByte == 0x44) {
    opTbl[0xED].TStates = 8;
    u8 complVal = ~z80.A + 1;
    u8 res = 0 + complVal;

    testZero_8(res);
    testSign_8(res);
    setAddSub();
    testOverflow_8(0, complVal, res);
    testHalfCarry_8(0, complVal, 0);
    testCarry_8(0, complVal, 0);
    invertHC();

    z80.A = res;
    if(logInstr) {
      writeLog("NEG\n");
    }
  }

  // This is IM 0 instruction
  else if(follByte == 0x46) {
    opTbl[0xED].TStates = 8;
    z80.IM = 0;

    if(logInstr) {
      writeLog("IM 0\n");
    }
  }

  // This is IM 1 instruction
  else if(follByte == 0x56) {
    opTbl[0xED].TStates = 8;
    z80.IM = 1;

    if(logInstr) {
      writeLog("IM 1\n");
    }
  }

  // This is IM 2 instruction
  else if(follByte == 0x5E) {
    opTbl[0xED].TStates = 8;
    z80.IM = 2;
    if(logInstr) {
      writeLog("IM 2\n");
    }
  }

  // This is ADC HL, ss instruction
  else if((follByte & 0xCF) == 0x4A) {
    opTbl[0xED].TStates = 15;
    u16 val1 = z80.HL;
    u16 val2;
    u8 src = ((follByte >> 4) & 0x03);

    switch(src) {
      case 0x00:
        val2 = z80.BC;
        break;
      case 0x01:
        val2 = z80.DE;
        break;
      case 0x02:
        val2 = z80.HL;
        break;
      case 0x03:
        val2 = z80.SP;
        break;
      default:
        die("[ERROR] Invalid ss in ADC HL, ss instruction.");
    }

    u16 carry = (z80.F & FLAG_CARRY);
    u16 res = val1 + val2 + carry;

    testSign_16(res);
    testZero_16(res);
    // TODO: half carry?
    testOverflow_16(val1, val2, res);
    rstAddSub();
    testCarry_16(val1, val2, carry);

    z80.HL = res;
    if(logInstr) {
      writeLog("ADC HL, "); logReg16(src, 0); writeLog("\n");
    }
  }

  // This is SBC HL, ss instruction
  else if((follByte & 0xCF) == 0x42) {
    opTbl[0xED].TStates = 15;
    u16 val1 = z80.HL;
    u16 val2;
    u8 src = ((follByte >> 4) & 0x03);

    switch(src) {
      case 0x00:
        val2 = z80.BC;
        break;
      case 0x01:
        val2 = z80.DE;
        break;
      case 0x02:
        val2 = z80.HL;
        break;
      case 0x03:
        val2 = z80.SP;
        break;
      default:
        die("[ERROR] Invalid ss in SBC HL, ss instruction.");
    }

    u16 not_carry = (z80.F & FLAG_CARRY) ^ FLAG_CARRY;
    // a - b - c = a + ~b + 1 - c = a + ~b + !c
    u16 res = val1 + ~val2 + not_carry;

    testSign_16(res);
    testZero_16(res);
    // TODO: half carry?
    testOverflow_16(val1, ~val2, res);
    setAddSub();
    testCarry_16(val1, ~val2, not_carry);
    invertHC();

    z80.HL = res;
    if(logInstr) {
      writeLog("SBC HL, "); logReg16(src, 0); writeLog("\n");
    }
  }

  // This is RETI instruction
  else if(follByte == 0x4D) {
    opTbl[0xED].TStates = 14;
    z80.PC = stackPop();
    if(logInstr) {
      writeLog("RETI\n");
    }
  }

  // This is RETN instruction
  else if(follByte == 0x45) {
    opTbl[0xED].TStates = 14;
    z80.IFF1 = z80.IFF2;
    z80.PC = stackPop();
    if(logInstr) {
      writeLog("RETN\n");
    }
  }

  // This is RLD instruction
  else if(follByte == 0x6F) {
    opTbl[0xED].TStates = 18;
    u8 valueHL = readByte(z80.HL);
    u8 valueHLhigh = (valueHL & 0xF0);
    u8 valueHLlow = (valueHL & 0x0F);
    u8 aLow = (z80.A & 0x0F);

    z80.A &= 0xF0;
    z80.A |= (valueHLhigh >> 4);

    valueHL = ((valueHLlow << 4) | aLow);

    testSign_8(z80.A);
    testZero_8(z80.A);
    z80.F &= ~(FLAG_HCARRY);
    testParity_8(z80.A);
    rstAddSub();

    writeByte(valueHL, z80.HL);
    if(logInstr) {
      writeLog("RLD\n");
    }
  }

  // This is RRD instruction
  else if(follByte == 0x67) {
    opTbl[0xED].TStates = 18;
    u8 valueHL = readByte(z80.HL);
    u8 valueHLhigh = (valueHL & 0xF0);
    u8 valueHLlow = (valueHL & 0x0F);
    u8 aLow = (z80.A & 0x0F);

    z80.A &= 0xF0;
    z80.A |= (valueHLlow);

    valueHL = ((aLow << 4) | (valueHLhigh >> 4));

    testSign_8(z80.A);
    testZero_8(z80.A);
    z80.F &= ~(FLAG_HCARRY);
    testParity_8(z80.A);
    rstAddSub();

    writeByte(valueHL, z80.HL);
    if(logInstr) {
      writeLog("RRD\n");
    }
  }

  // TODO
  // This is IN r, (C) instruction
  else if((follByte & 0xC7) == 0x40) {
    opTbl[0xED].TStates = 12;
    u8 dst = ((follByte >> 3) & 0x07);
    writeReg(dst, z80.portIn(z80.C));

    u8 res = readReg(dst);

    testSign_8(res);
    testZero_8(res);
    z80.F &= ~(FLAG_HCARRY);
    testParity_8(res);
    rstAddSub();

    if(logInstr) {
      writeLog("IN "); logReg8(dst); writeLog(", (C)\n");
    }
  }

  // This is OUT (C), r instruction
  else if((follByte & 0xC7) == 0x41) {
    opTbl[0xED].TStates = 12;
    u8 src = ((follByte >> 3) & 0x07);
    z80.portOut(z80.C, readReg(src));
    if(logInstr) {
      writeLog("OUT (C), "); logReg8(src); writeLog("\n");
    }
  }
  else die("[ERROR] Invalid operation in 0xED instruction group.");
}


// This is LD dd, nn instruction
void LDddnn(u8 opcode) {
  u8 dst = ((opcode >> 4) & 0x03);
  u16 nn = fetch16();
  switch(dst) {
    case 0x00:
      z80.BC = nn;
      break;
    case 0x01:
      z80.DE = nn;
      break;
    case 0x02:
      z80.HL = nn;
      break;
    case 0x03:
      z80.SP = nn;
      break;
    default:
      die("[ERROR] Wrong destination register for LD dd, nn instruction.\n");
  }
  if(logInstr) {
    fprintf(fpLog, "LD "); logReg16(dst, 0); fprintf(fpLog, ", %04X\n", nn);
  }
}


// This is LD HL, (nn)
void LDHLnn(u8 opcode) {
  u16 offset = fetch16();
  z80.L = readByte(offset);
  z80.H = readByte(offset + 1);
  if(logInstr) {
    fprintf(fpLog, "LD HL, (nn)\t\tnn = %04X\n", offset);
  }
}


// This is LD (nn), HL
void LDnnHL(u8 opcode) {
  u16 offset = fetch16();
  writeByte(z80.L, offset);
  writeByte(z80.H, offset + 1);
  if(logInstr) {
    fprintf(fpLog, "LD (nn), HL\t\tnn = %04X\n", offset);
  }
}


// This is LD SP, HL instruction
void LDSPHL(u8 opcode) {
  z80.SP = z80.HL;
  if(logInstr) {
    writeLog("LD SP, HL\n");
  }
}


// This is PUSH qq instruction
void PUSHqq(u8 opcode) {
  u8 src = ((opcode >> 4) & 0x03); // Isolate qq
  switch(src) {
    case 0x00:
      stackPush(z80.BC);
      break;
    case 0x01:
      stackPush(z80.DE);
      break;
    case 0x02:
      stackPush(z80.HL);
      break;
    case 0x03:
      stackPush(z80.AF);
      break;
    default:
      die("[ERROR] Wrong operand for PUSH qq instruction.\n");
  }
  if(logInstr) {
    writeLog("PUSH "); logReg16(src, 1); writeLog("\n");
  }
}


// This is POP qq instruction
void POPqq(u8 opcode) {
  u8 dst = ((opcode >> 4) & 0x03); // Isolate qq
  u16 popVal = stackPop();
  switch(dst) {
    case 0x00:
      z80.BC = popVal;
      break;
    case 0x01:
      z80.DE = popVal;
      break;
    case 0x02:
      z80.HL = popVal;
      break;
    case 0x03:
      z80.AF = popVal;
      break;
    default:
      die("[ERROR] Wrong operand for POP qq instruction.\n");
  }
  if(logInstr) {
    writeLog("POP "); logReg16(dst, 1); writeLog("\n");
  }
}


// This is EX DE, HL instruction
void EXDEHL(u8 opcode) {
  FASTSWAP(z80.DE, z80.HL);
  if(logInstr) {
    writeLog("EX DE, HL\n");
  }
}


// This is EX AF, AF' instruction
void EXAFAFr(u8 opcode) {
  FASTSWAP(z80.AF, z80.ArFr);
  if(logInstr) {
    writeLog("EX AF, AF'\n");
  }
}


// This is EXX instruction
void EXX(u8 opcode) {
  FASTSWAP(z80.BC, z80.BrCr);
  FASTSWAP(z80.DE, z80.DrEr);
  FASTSWAP(z80.HL, z80.HrLr);
  if(logInstr) {
    writeLog("EXX\n");
  }
}


// This is EX (SP), HL instruction
void EXSPHL(u8 opcode) {
  u8 valSP  = readByte(z80.SP);      // Byte at SP
  u8 valSPH = readByte(z80.SP + 1);  // Byte at SP + 1
  writeByte(z80.H, z80.SP + 1);
  writeByte(z80.L, z80.SP);
  z80.H = valSPH;
  z80.L = valSP;
  if(logInstr) {
    fprintf(fpLog, "EX (SP), HL\t\tSP = %04X\n", z80.SP);
  }
}


// This is ADD A, r instruction
void ADDAr(u8 opcode) {
  u8 src = (opcode & 0x07);
  u8 value = readReg(src);
  u8 res = z80.A + value;

  testZero_8(res);
  testSign_8(res);
  testHalfCarry_8(z80.A, value, 0);
  z80.F &= ~(FLAG_ADDSUB);
  testCarry_8(z80.A, value, 0);
  testOverflow_8(z80.A, value, res);

  z80.A = res;
  if(logInstr) {
    writeLog("ADD A, "); logReg8(src); writeLog("\n");
  }
}


// This is SUB A, r instruction
void SUBAr(u8 opcode) {
  u8 src = (opcode & 0x07);
  u8 value = readReg(src);
  u8 complVal = ~value + 1;
  u8 res = z80.A + complVal;

  testZero_8(res);
  testSign_8(res);
  setAddSub();
  testOverflow_8(z80.A, complVal, res);
  testHalfCarry_8(z80.A, complVal, 0);
  testCarry_8(z80.A, complVal, 0);
  invertHC();

  z80.A = res;
  if(logInstr) {
    writeLog("SUB A, "); logReg8(src); writeLog("\n");
  }
}


// This is ADD A, n instruction
void ADDAn(u8 opcode) {
  u8 n = fetch8();
  u8 res = z80.A + n;

  testZero_8(res);
  testSign_8(res);
  testHalfCarry_8(z80.A, n, 0);
  z80.F &= ~(FLAG_ADDSUB);
  testCarry_8(z80.A, n, 0);
  testOverflow_8(z80.A, n, res);

  z80.A = res;
  if(logInstr) {
    fprintf(fpLog, "ADD A, %02X\n", n);
  }
}


// This is SUB A, n instruction
void SUBAn(u8 opcode) {
  u8 n = fetch8();
  u8 complVal = ~n + 1;
  u8 res = z80.A + complVal;

  testZero_8(res);
  testSign_8(res);
  setAddSub();
  testOverflow_8(z80.A, complVal, res);
  testHalfCarry_8(z80.A, complVal, 0);
  testCarry_8(z80.A, complVal, 0);
  invertHC();

  z80.A = res;
  if(logInstr) {
    fprintf(fpLog, "SUB A, %02X\n", n);
  }
}


// This is ADD A, (HL) instruction
void ADDAHL(u8 opcode) {
  u8 value = readByte(z80.HL);
  u8 res = z80.A + value;

  testZero_8(res);
  testSign_8(res);
  testHalfCarry_8(z80.A, value, 0);
  z80.F &= ~(FLAG_ADDSUB);
  testCarry_8(z80.A, value, 0);
  testOverflow_8(z80.A, value, res);

  z80.A = res;
  if(logInstr) {
    fprintf(fpLog, "ADD A, (HL)\t\tHL = %04X\n", z80.HL);
  }
}


// This is SUB A, (HL) instruction
void SUBAHL(u8 opcode) {
  u8 value = readByte(z80.HL);
  u8 complVal = ~value + 1;
  u8 res = z80.A + complVal;

  testZero_8(res);
  testSign_8(res);
  setAddSub();
  testOverflow_8(z80.A, complVal, res);
  testHalfCarry_8(z80.A, complVal, 0);
  testCarry_8(z80.A, complVal, 0);
  invertHC();

  z80.A = res;
  if(logInstr) {
    fprintf(fpLog, "SUB A, (HL)\t\tHL = %04X\n", z80.HL);
  }
}


// This is ADC A, r instruction
void ADCAr(u8 opcode) {
  u8 src = (opcode & 0x07);
  u8 value = readReg(src);
  u8 carry = (z80.F & FLAG_CARRY);
  u8 res = z80.A + value + carry;

  testZero_8(res);
  testSign_8(res);
  testHalfCarry_8(z80.A, value, carry);
  rstAddSub();
  testCarry_8(z80.A, value, carry);
  testOverflow_8(z80.A, value, res);

  z80.A = res;
  if(logInstr) {
    writeLog("ADC A, "); logReg8(src); writeLog("\n");
  }
}


// This is SBC A, r instruction
void SBCAr(u8 opcode) {
  u8 src = (opcode & 0x07);
  u8 value = readReg(src);
  u8 not_carry = (z80.F & FLAG_CARRY) ^ FLAG_CARRY;
  // a - b - c = a + ~b + 1 - c = a + ~b + !c
  u8 res = z80.A + ~value + not_carry;

  testZero_8(res);
  testSign_8(res);
  setAddSub();
  testOverflow_8(z80.A, ~value, res);
  testHalfCarry_8(z80.A, ~value, not_carry);
  testCarry_8(z80.A, ~value, not_carry);
  invertHC();

  z80.A = res;
  if(logInstr) {
    writeLog("SBC A, "); logReg8(src); writeLog("\n");
  }
}


// This is ADC A, n instruction
void ADCAn(u8 opcode) {
  u8 n = fetch8();
  u8 carry = (z80.F & FLAG_CARRY);
  u8 res = z80.A + n + carry;

  testZero_8(res);
  testSign_8(res);
  testHalfCarry_8(z80.A, n, carry);
  rstAddSub();
  testCarry_8(z80.A, n, carry);
  testOverflow_8(z80.A, n, res);

  z80.A = res;
  if(logInstr) {
    fprintf(fpLog, "ADC A, %02X\n", n);
  }
}


// This is SBC A, n instruction
void SBCAn(u8 opcode) {
  u8 n = fetch8();
  u8 not_carry = (z80.F & FLAG_CARRY) ^ FLAG_CARRY;
  // a - b - c = a + ~b + 1 - c = a + ~b + !c
  u8 res = z80.A + ~n + not_carry;

  testZero_8(res);
  testSign_8(res);
  setAddSub();
  testOverflow_8(z80.A, ~n, res);
  testHalfCarry_8(z80.A, ~n, not_carry);
  testCarry_8(z80.A, ~n, not_carry);
  invertHC();

  z80.A = res;
  if(logInstr) {
    fprintf(fpLog, "SBC A, %02X\n", n);
  }
}


// This is ADC A, (HL) instruction
void ADCAHL(u8 opcode) {
  u8 value = readByte(z80.HL);
  u8 carry = (z80.F & FLAG_CARRY);
  u8 res = z80.A + value + carry;

  testZero_8(res);
  testSign_8(res);
  testHalfCarry_8(z80.A, value, carry);
  rstAddSub();
  testCarry_8(z80.A, value, carry);
  testOverflow_8(z80.A, value, res);

  z80.A = res;
  if(logInstr) {
    fprintf(fpLog, "ADC A, (HL)\t\tHL = %04X\n", z80.HL);
  }
}


// This is SBC A, (HL) instruction
void SBCAHL(u8 opcode) {
  u8 value = readByte(z80.HL);
  u8 not_carry = (z80.F & FLAG_CARRY) ^ FLAG_CARRY;
  // a - b - c = a + ~b + 1 - c = a + ~b + !c
  u8 res = z80.A + ~value + not_carry;

  testZero_8(res);
  testSign_8(res);
  setAddSub();
  testOverflow_8(z80.A, ~value, res);
  testHalfCarry_8(z80.A, ~value, not_carry);
  testCarry_8(z80.A, ~value, not_carry);
  invertHC();

  z80.A = res;
  if(logInstr) {
    fprintf(fpLog, "SBC A, (HL)\t\tHL = %04X\n", z80.HL);
  }
}


// This is AND r instruction
void ANDr(u8 opcode) {
  u8 src = (opcode & 0x07);
  u8 value = readReg(src);
  u8 res = z80.A & value;

  testZero_8(res);
  testSign_8(res);
  rstAddSub();
  testParity_8(res);
  z80.F &= ~(FLAG_CARRY);
  z80.F |= (FLAG_HCARRY);

  z80.A = res;
  if(logInstr) {
    writeLog("AND "); logReg8(src); writeLog("\n");
  }
}


// This is AND n instruction
void ANDn(u8 opcode) {
  u8 n = fetch8();
  u8 res = z80.A & n;

  testZero_8(res);
  testSign_8(res);
  rstAddSub();
  testParity_8(res);
  z80.F &= ~(FLAG_CARRY);
  z80.F |= (FLAG_HCARRY);

  z80.A = res;
  if(logInstr) {
    fprintf(fpLog, "AND %02X\n", n);
  }
}


// This is AND (HL) instruction
void ANDHL(u8 opcode) {
  u8 value = readByte(z80.HL);
  u8 res = z80.A & value;

  testZero_8(res);
  testSign_8(res);
  rstAddSub();
  testParity_8(res);
  z80.F &= ~(FLAG_CARRY);
  z80.F |= (FLAG_HCARRY);

  z80.A = res;
  if(logInstr) {
    fprintf(fpLog, "AND (HL)\t\tHL = %04X\n", z80.HL);
  }
}


// This is OR r instruction
void ORr(u8 opcode) {
  u8 src = (opcode & 0x07);
  u8 value = readReg(src);
  u8 res = z80.A | value;

  testZero_8(res);
  testSign_8(res);
  rstAddSub();
  testParity_8(res);
  z80.F &= ~(FLAG_CARRY | FLAG_HCARRY);

  z80.A = res;
  if(logInstr) {
    writeLog("OR "); logReg8(src); writeLog("\n");
  }
}


// This is OR n instruction
void ORn(u8 opcode) {
  u8 n = fetch8();
  u8 res = z80.A | n;

  testZero_8(res);
  testSign_8(res);
  rstAddSub();
  testParity_8(res);
  z80.F &= ~(FLAG_CARRY | FLAG_HCARRY);

  z80.A = res;
  if(logInstr) {
    fprintf(fpLog, "OR %02X\n", n);
  }
}


// This is OR (HL) instruction
void ORHL(u8 opcode) {
  u8 value = readByte(z80.HL);
  u8 res = z80.A | value;

  testZero_8(res);
  testSign_8(res);
  rstAddSub();
  testParity_8(res);
  z80.F &= ~(FLAG_CARRY | FLAG_HCARRY);

  z80.A = res;
  if(logInstr) {
    fprintf(fpLog, "OR (HL)\t\tHL = %04X\n", z80.HL);
  }
}


// This is XOR r instruction
void XORr(u8 opcode) {
  u8 src = (opcode & 0x07);
  u8 value = readReg(src);
  u8 res = z80.A ^ value;

  testZero_8(res);
  testSign_8(res);
  rstAddSub();
  testParity_8(res);
  z80.F &= ~(FLAG_CARRY | FLAG_HCARRY);

  z80.A = res;
  if(logInstr) {
    writeLog("XOR "); logReg8(src); writeLog("\n");
  }
}


// This is XOR n instruction
void XORn(u8 opcode) {
  u8 n = fetch8();
  u8 res = z80.A ^ n;

  testZero_8(res);
  testSign_8(res);
  rstAddSub();
  testParity_8(res);
  z80.F &= ~(FLAG_CARRY | FLAG_HCARRY);

  z80.A = res;
  if(logInstr) {
    fprintf(fpLog, "XOR %02X\n", n);
  }
}


// This is XOR (HL) instruction
void XORHL(u8 opcode) {
  u8 value = readByte(z80.HL);
  u8 res = z80.A ^ value;

  testZero_8(res);
  testSign_8(res);
  rstAddSub();
  testParity_8(res);
  z80.F &= ~(FLAG_CARRY | FLAG_HCARRY);

  z80.A = res;
  if(logInstr) {
    fprintf(fpLog, "XOR (HL)\t\tHL = %04X\n", z80.HL);
  }
}


// This is CP r instruction
void CPr(u8 opcode) {
  u8 src = (opcode & 0x07);
  u8 value = readReg(src);
  u8 complVal = ~value + 1;
  u8 res = z80.A + complVal;

  testSign_8(res);
  testZero_8(res);
  setAddSub();
  testOverflow_8(z80.A, complVal, res);
  testHalfCarry_8(z80.A, complVal, 0);
  testCarry_8(z80.A, complVal, 0);
  invertHC();

  if(logInstr) {
    writeLog("CP "); logReg8(src); writeLog("\n");
  }
}


// This is CP n instruction
void CPn(u8 opcode) {
  u8 n = fetch8();
  u8 complVal = ~n + 1;
  u8 res = z80.A + complVal;

  testSign_8(res);
  testZero_8(res);
  setAddSub();
  testOverflow_8(z80.A, complVal, res);
  testHalfCarry_8(z80.A, complVal, 0);
  testCarry_8(z80.A, complVal, 0);
  invertHC();

  if(logInstr) {
    fprintf(fpLog, "CP %02X\n", n);
  }
}


// This is CP (HL) instruction
void CPHL(u8 opcode) {
  u8 value = readByte(z80.HL);
  u8 complVal = ~value + 1;
  u8 res = z80.A + complVal;

  testSign_8(res);
  testZero_8(res);
  setAddSub();
  testOverflow_8(z80.A, complVal, res);
  testHalfCarry_8(z80.A, complVal, 0);
  testCarry_8(z80.A, complVal, 0);
  invertHC();

  if(logInstr) {
    fprintf(fpLog, "CP (HL)\t\tHL = %04X\n", z80.HL);
  }
}


// This is INC r instruction
void INCr(u8 opcode) {
  u8 src = ((opcode >> 3) & 0x07);
  u8 value = readReg(src);
  u8 res = value + 1;

  testSign_8(res);
  testZero_8(res);
  testHalfCarry_8(value, 1, 0);
  testOverflow_8(value, 1, res);
  rstAddSub();

  writeReg(res, src);
  if(logInstr) {
    writeLog("INC "); logReg8(src); writeLog("\n");
  }
}


// This is INC (HL) instruction
void INCHL(u8 opcode) {
  u8 value = readByte(z80.HL);
  u8 res = value + 1;

  testSign_8(res);
  testZero_8(res);
  testHalfCarry_8(value, 1, 0);
  testOverflow_8(value, 1, res);
  rstAddSub();

  writeByte(res, z80.HL);
  if(logInstr) {
    fprintf(fpLog, "INC (HL)\t\tHL = %04X\n", z80.HL);
  }
}


// This is DEC r instruction
void DECr(u8 opcode) {
  u8 src = ((opcode >> 3) & 0x07);
  u8 value = readReg(src);
  u8 complDec = 0xFF; // -1
  u8 res = value + complDec;

  testSign_8(res);
  testZero_8(res);
  testHalfCarry_8(value, complDec, 0);
  testOverflow_8(value, complDec, res);
  setAddSub();
  invertHC();
  z80.F ^= (FLAG_CARRY);	// De-invert carry flag

  writeReg(res, src);
  if(logInstr) {
    writeLog("DEC "); logReg8(src); writeLog("\n");
  }
}


// This is DEC (HL) instruction
void DECHL(u8 opcode) {
  u8 value = readByte(z80.HL);
  u8 complDec = 0xFF; // -1
  u8 res = value + complDec;

  testSign_8(res);
  testZero_8(res);
  testHalfCarry_8(value, complDec, 0);
  testOverflow_8(value, complDec, res);
  setAddSub();
  invertHC();
  z80.F ^= (FLAG_CARRY);	// De-invert carry flag

  writeByte(res, z80.HL);
  if(logInstr) {
    fprintf(fpLog, "DEC (HL)\t\tHL = %04X\n", z80.HL);
  }
}


// TODO: DAA
void DAA(u8 opcode) {
  die("[FATAL] DAA instruction not implemented yet.");
/*
  int top4 = (e8080.A >> 4) & 0xF;
  int bot4 = (e8080.A & 0xF);

  if ((bot4 > 9) || (e8080.F & FLAG_ACARRY)) {
    setFlags(e8080.A + 6, FLAG_ZERO | FLAG_SIGN | FLAG_PARITY | FLAG_CARRY | FLAG_ACARRY);
    e8080.A += 6;
    top4 = (e8080.A >> 4) & 0xF;
    bot4 = (e8080.A & 0xF);
  }

  if ((top4 > 9) || (e8080.F & FLAG_CARRY)) {
    top4 += 6;
    e8080.A = (top4 << 4) | bot4;
  }
*/
}


// This is CPL instruction
void CPL(u8 opcode) {
  z80.A = ~(z80.A);

  setAddSub();
  z80.F |= (FLAG_HCARRY);

  if(logInstr) {
    writeLog("CPL\n");
  }
}


// This is CCF instruction
void CCF(u8 opcode) {
  rstAddSub();

  if(z80.F & FLAG_CARRY)
    z80.F |= (FLAG_HCARRY);
  else
    z80.F &= ~(FLAG_HCARRY);

  z80.F ^= (FLAG_CARRY);

  if(logInstr) {
    writeLog("CCF\n");
  }
}


// This is SCF instruction
void SCF(u8 opcode) {
  rstAddSub();
  z80.F &= ~(FLAG_HCARRY);
  z80.F |= (FLAG_CARRY);

  if(logInstr) {
    writeLog("SCF\n");
  }
}


// This is NOP instruction
void NOP(u8 opcode) {
  /* Do nothing */

  if(logInstr) {
    writeLog("NOP\n");
  }
}


// This is HALT instruction
void HALT(u8 opcode) {
  z80.halt = 1;

  if(logInstr) {
    writeLog("HALT\n");
  }
}


// This is DI instruction
void DI(u8 opcode) {
  z80.IFF1 = 0;
  z80.IFF2 = 0;

  if(logInstr) {
    writeLog("DI\n");
  }
}


// This is EI instruction
void EI(u8 opcode) {
  z80.IFF1 = 1;
  z80.IFF2 = 1;

  if(logInstr) {
    writeLog("EI\n");
  }
}


// This is ADD HL, ss instruction
void ADDHLss(u8 opcode) {
  u16 val1 = z80.HL;
  u16 val2;
  u8 src = ((opcode >> 4) & 0x03);

  switch(src) {
    case 0x00:
      val2 = z80.BC;
      break;
    case 0x01:
      val2 = z80.DE;
      break;
    case 0x02:
      val2 = z80.HL;
      break;
    case 0x03:
      val2 = z80.SP;
      break;
    default:
      die("[ERROR] Invalid ss in ADD HL, ss instruction.");
  }

  // TODO: half carry?
  rstAddSub();
  testCarry_16(z80.HL, val2, 0);

  z80.HL = val1 + val2;
  if(logInstr) {
    writeLog("ADD HL, "); logReg16(src, 0); writeLog("\n");
  }
}


// This is INC ss instruction
void INCss(u8 opcode) {
  u8 src = ((opcode >> 4) & 0x03);

  switch(src) {
    case 0x00:
      z80.BC++;
      break;
    case 0x01:
      z80.DE++;
      break;
    case 0x02:
      z80.HL++;
      break;
    case 0x03:
      z80.SP++;
      break;
    default:
      die("[ERROR] Invalid ss in INC ss instruction.");
  }

  if(logInstr) {
    writeLog("INC "); logReg16(src, 0); writeLog("\n");
  }
}


// This is DEC ss instruction
void DECss(u8 opcode) {
  u8 src = ((opcode >> 4) & 0x03);

  switch(src) {
    case 0x00:
      z80.BC--;
      break;
    case 0x01:
      z80.DE--;
      break;
    case 0x02:
      z80.HL--;
      break;
    case 0x03:
      z80.SP--;
      break;
    default:
      die("[ERROR] Invalid ss in DEC ss instruction.");
  }

  if(logInstr) {
    writeLog("DEC "); logReg16(src, 0); writeLog("\n");
  }
}


// This is RLCA instruction
void RLCA(u8 opcode) {
  u8 a_msb = (z80.A & 0x80) >> 7;
  // Shift left by one
  z80.A = ((z80.A << 1) | a_msb);

  // Update carry flag with old msb
  if(a_msb)
    z80.F |= FLAG_CARRY;
  else
    z80.F &= ~(FLAG_CARRY);

  rstAddSub();
  z80.F &= ~(FLAG_HCARRY);

  if(logInstr) {
    writeLog("RLCA\n");
  }
}


// This is RLA instruction
void RLA(u8 opcode) {
  u8 oldCarry = (z80.F & FLAG_CARRY);

  // Update carry flag with A msb
  if(z80.A & 0x80)
    z80.F |= FLAG_CARRY;
  else
    z80.F &= ~(FLAG_CARRY);

  z80.A = ((z80.A << 1) | oldCarry);

  rstAddSub();
  z80.F &= ~(FLAG_HCARRY);

  if(logInstr) {
    writeLog("RLA\n");
  }
}


// This is RRCA instruction
void RRCA(u8 opcode) {
  u8 a_lsb = (z80.A & 0x01);

  // Copy A LSB to carry flag
  if(a_lsb)
    z80.F |= FLAG_CARRY;
  else
    z80.F &= ~(FLAG_CARRY);

  z80.A = ((z80.A >> 1) | (a_lsb << 7));

  rstAddSub();
  z80.F &= ~(FLAG_HCARRY);

  if(logInstr) {
    writeLog("RRCA\n");
  }
}


// This is RRA instruction
void RRA(u8 opcode) {
  u8 oldCarry = (z80.F & FLAG_CARRY);

  // Copy bit 0 to carry bit
  if(z80.A & 0x01)
    z80.F |= FLAG_CARRY;
  else
    z80.F &= ~(FLAG_CARRY);

  z80.A = ((z80.A >> 1) | (oldCarry << 7));

  rstAddSub();
  z80.F &= ~(FLAG_HCARRY);

  if(logInstr) {
    writeLog("RRA\n");
  }
}


void RLC(u8 opcode) {
  opTbl[0xCB].TStates = 8;
  u8 follByte = fetch8();

  // This is RLCr instruction
  if((follByte >= 0x00 && follByte <= 0x07) && follByte != 0x06) {
    u8 src = (follByte & 0x07);
    u8 value = readReg(src);
    u8 r_msb = (value & 0x80) >> 7;

    // Shift left by one
    u8 res = ((value << 1) | r_msb);

    // Update carry flag with old msb
    if(r_msb)
      z80.F |= FLAG_CARRY;
    else
      z80.F &= ~(FLAG_CARRY);

    testSign_8(res);
    testZero_8(res);
    z80.F &= ~(FLAG_HCARRY);
    testParity_8(res);
    rstAddSub();

    writeReg(res, src);
    if(logInstr) {
      writeLog("RLC "); logReg8(src); writeLog("\n");
    }
  }

  // This is RLC (HL) instruction
  else if(follByte == 0x06) {
    opTbl[0xCB].TStates = 15;
    u8 value = readByte(z80.HL);
    u8 r_msb = (value & 0x80) >> 7;

    // Shift left by one
    u8 res = ((value << 1) | r_msb);

    // Update carry flag with old msb
    if(r_msb)
      z80.F |= FLAG_CARRY;
    else
      z80.F &= ~(FLAG_CARRY);

    testSign_8(res);
    testZero_8(res);
    z80.F &= ~(FLAG_HCARRY);
    testParity_8(res);
    rstAddSub();

    writeByte(res, z80.HL);
    if(logInstr) {
      fprintf(fpLog, "RLC (HL)\t\tHL = %04X\n", z80.HL);
    }
  }

  // This is BIT b, r instruction
  else if((follByte & 0xC0) == 0x40) {
    u8 bit = ((follByte >> 3) & 0x07); // Bit to test
    u8 src = (follByte & 0x07);
    u8 value = readReg(src);

    value = ((value >> bit) & 0x01);

    testZero_8(value);
    z80.F |= FLAG_HCARRY;
    rstAddSub();

    if(logInstr) {
      fprintf(fpLog, "BIT %01d, ", bit); logReg8(src); writeLog("\n");
    }
  }

  // This is BIT b, (HL) instruction
  else if((follByte & 0xC7) == 0x46) {
    opTbl[0xCB].TStates = 12;
    u8 bit = ((follByte >> 3) & 0x07); // Bit to test
    u8 value = readByte(z80.HL);

    value = ((value >> bit) & 0x01);

    testZero_8(value);
    z80.F |= FLAG_HCARRY;
    rstAddSub();

    if(logInstr) {
      fprintf(fpLog, "BIT %01d, (HL)\t\tHL = %04X\n", bit, z80.HL);
    }
  }

  // This is SET b, r instruction
  else if((follByte & 0xC0) == 0xC0) {
    u8 bit = ((follByte >> 3) & 0x07); // Bit to test
    u8 src = (follByte & 0x07);
    u8 value = readReg(src);

    value |= (1 << bit);

    writeReg(value, src);
    if(logInstr) {
      fprintf(fpLog, "SET %01d, ", bit); logReg8(src); writeLog("\n");
    }
  }

  // This is SET b, (HL) instruction
  else if((follByte & 0xC7) == 0xC6) {
    opTbl[0xCB].TStates = 15;
    u8 bit = ((follByte >> 3) & 0x07); // Bit to test
    u8 value = readByte(z80.HL);

    value |= (1 << bit);

    writeByte(value, z80.HL);
    if(logInstr) {
      fprintf(fpLog, "SET %01d, (HL)\t\tHL = %04X\n", bit, z80.HL);
    }
  }

  // This is RES b, r instruction
  else if((follByte & 0xC0) == 0x80) {
    u8 bit = ((follByte >> 3) & 0x07); // Bit to test
    u8 src = (follByte & 0x07);
    u8 value = readReg(src);

    value &= ~(1 << bit);

    writeReg(value, src);
    if(logInstr) {
      fprintf(fpLog, "RES %01d, ", bit); logReg8(src); writeLog("\n");
    }
  }

  // This is RES b, (HL) instruction
  else if((follByte & 0xC7) == 0x86) {
    opTbl[0xCB].TStates = 15;
    u8 bit = ((follByte >> 3) & 0x07); // Bit to test
    u8 value = readByte(z80.HL);

    value &= ~(1 << bit);

    writeByte(value, z80.HL);
    if(logInstr) {
      fprintf(fpLog, "RES %01d, (HL)\t\tHL = %04X\n", bit, z80.HL);
    }
  }

  // This is RL r instruction
  else if((follByte & 0xF8) == 0x10) {
    u8 oldCarry = (z80.F & FLAG_CARRY);
    u8 src = (follByte & 0x07);
    u8 value = readReg(src);

    // Update carry flag with value msb
    if(value & 0x80)
      z80.F |= FLAG_CARRY;
    else
      z80.F &= ~(FLAG_CARRY);

    u8 res = ((value << 1) | oldCarry);

    testSign_8(res);
    testZero_8(res);
    z80.F &= ~(FLAG_HCARRY);
    testParity_8(res);
    rstAddSub();

    writeReg(res, src);
    if(logInstr) {
      writeLog("RL "); logReg8(src); writeLog("\n");
    }
  }

  // This is RL (HL) instruction
  else if(follByte == 0x16) {
    opTbl[0xCB].TStates = 15;
    u8 oldCarry = (z80.F & FLAG_CARRY);
    u8 value = readByte(z80.HL);

    // Update carry flag with value msb
    if(value & 0x80)
      z80.F |= FLAG_CARRY;
    else
      z80.F &= ~(FLAG_CARRY);

    u8 res = ((value << 1) | oldCarry);

    testSign_8(res);
    testZero_8(res);
    z80.F &= ~(FLAG_HCARRY);
    testParity_8(res);
    rstAddSub();

    writeByte(res, z80.HL);
    if(logInstr) {
      fprintf(fpLog, "RL (HL)\t\tHL = %04X\n", z80.HL);
    }
  }

  // This is RRC r instruction
  else if((follByte & 0xF8) == 0x08) {
    u8 src = (follByte & 0x07);
    u8 value = readReg(src);
    u8 r_lsb = (value & 0x01);

    // Copy r LSB to carry flag
    if(r_lsb)
      z80.F |= FLAG_CARRY;
    else
      z80.F &= ~(FLAG_CARRY);

    u8 res = ((value >> 1) | (r_lsb << 7));

    testSign_8(res);
    testZero_8(res);
    z80.F &= ~(FLAG_HCARRY);
    testParity_8(res);
    rstAddSub();

    writeReg(res, src);
    if(logInstr) {
      writeLog("RRC "); logReg8(src); writeLog("\n");
    }
  }

  // This is RRC (HL) instruction
  else if(follByte == 0x0E) {
    opTbl[0xCB].TStates = 15;
    u8 value = readByte(z80.HL);
    u8 v_lsb = (value & 0x01);

    // Copy r LSB to carry flag
    if(v_lsb)
      z80.F |= FLAG_CARRY;
    else
      z80.F &= ~(FLAG_CARRY);

    u8 res = ((value >> 1) | (v_lsb << 7));

    testSign_8(res);
    testZero_8(res);
    z80.F &= ~(FLAG_HCARRY);
    testParity_8(res);
    rstAddSub();

    writeByte(res, z80.HL);
    if(logInstr) {
      fprintf(fpLog, "RRC (HL)\t\tHL = %04X\n", z80.HL);
    }
  }

  // This is RR r instruction
  else if((follByte & 0xF8) == 0x18) {
    u8 oldCarry = (z80.F & FLAG_CARRY);
    u8 src = (follByte & 0x07);
    u8 value = readReg(src);

    // Copy bit 0 to carry bit
    if(value & 0x01)
      z80.F |= FLAG_CARRY;
    else
      z80.F &= ~(FLAG_CARRY);

    u8 res = ((value >> 1) | (oldCarry << 7));

    testSign_8(res);
    testZero_8(res);
    z80.F &= ~(FLAG_HCARRY);
    testParity_8(res);
    rstAddSub();

    writeReg(res, src);
    if(logInstr) {
      writeLog("RR "); logReg8(src); writeLog("\n");
    }
  }

  // This is RR (HL) instruction
  else if(follByte == 0x1E) {
    opTbl[0xCB].TStates = 15;
    u8 oldCarry = (z80.F & FLAG_CARRY);
    u8 value = readByte(z80.HL);

    // Copy bit 0 to carry bit
    if(value & 0x01)
      z80.F |= FLAG_CARRY;
    else
      z80.F &= ~(FLAG_CARRY);

    u8 res = ((value >> 1) | (oldCarry << 7));

    testSign_8(res);
    testZero_8(res);
    z80.F &= ~(FLAG_HCARRY);
    testParity_8(res);
    rstAddSub();

    writeByte(res, z80.HL);
    if(logInstr) {
      fprintf(fpLog, "RR (HL)\t\tHL = %04X\n", z80.HL);
    }
  }

  // This is SLA r instruction
  else if((follByte & 0xF8) == 0x20) {
    u8 src = (follByte & 0x07);
    u8 value = readReg(src);

    // Copy bit 7 to carry bit
    if(value & 0x80)
      z80.F |= FLAG_CARRY;
    else
      z80.F &= ~(FLAG_CARRY);

    u8 res = (value << 1); // Shift left by one

    testSign_8(res);
    testZero_8(res);
    z80.F &= ~(FLAG_HCARRY);
    testParity_8(res);
    rstAddSub();

    writeReg(res, src);
    if(logInstr) {
      writeLog("SLA "); logReg8(src); writeLog("\n");
    }
  }

  // This is SLA (HL) instruction
  else if(follByte == 0x26) {
    opTbl[0xCB].TStates = 15;
    u8 value = readByte(z80.HL);

    // Copy bit 7 to carry bit
    if(value & 0x80)
      z80.F |= FLAG_CARRY;
    else
      z80.F &= ~(FLAG_CARRY);

    u8 res = (value << 1); // Shift left by one

    testSign_8(res);
    testZero_8(res);
    z80.F &= ~(FLAG_HCARRY);
    testParity_8(res);
    rstAddSub();

    writeByte(res, z80.HL);
    if(logInstr) {
      fprintf(fpLog, "SLA (HL)\t\tHL = %04X\n", z80.HL);
    }
  }

  // This is SRA r instruction
  else if((follByte & 0xF8) == 0x28) {
    u8 src = (follByte & 0x07);
    u8 value = readReg(src);
    u8 v_msb = (value & 0x80);

    // Copy bit 0 to carry flag
    if(value & 0x01)
      z80.F |= FLAG_CARRY;
    else
      z80.F &= ~(FLAG_CARRY);

    u8 res = ((value >> 1) | v_msb); // Shift right by one

    testSign_8(res);
    testZero_8(res);
    z80.F &= ~(FLAG_HCARRY);
    testParity_8(res);
    rstAddSub();

    writeReg(res, src);
    if(logInstr) {
      writeLog("SRA "); logReg8(src); writeLog("\n");
    }
  }

  // This is SRA (HL) instruction
  else if(follByte == 0x2E) {
    opTbl[0xCB].TStates = 15;
    u8 value = readByte(z80.HL);
    u8 v_msb = (value & 0x80);

    // Copy bit 0 to carry flag
    if(value & 0x01)
      z80.F |= FLAG_CARRY;
    else
      z80.F &= ~(FLAG_CARRY);

    u8 res = ((value >> 1) | v_msb); // Shift right by one

    testSign_8(res);
    testZero_8(res);
    z80.F &= ~(FLAG_HCARRY);
    testParity_8(res);
    rstAddSub();

    writeByte(res, z80.HL);
    if(logInstr) {
      fprintf(fpLog, "SRA (HL)\t\tHL = %04X\n", z80.HL);
    }
  }

  // This is SRL r instruction
  else if((follByte & 0xF8) == 0x38) {
    u8 src = (follByte & 0x07);
    u8 value = readReg(src);

    // Copy bit 0 to carry bit
    if(value & 0x01)
      z80.F |= FLAG_CARRY;
    else
      z80.F &= ~(FLAG_CARRY);

    u8 res = (value >> 1); // Shift right by one

    z80.F &= ~(FLAG_SIGN | FLAG_HCARRY);
    testZero_8(res);
    testParity_8(res);
    rstAddSub();

    writeReg(res, src);
    if(logInstr) {
      writeLog("SRL "); logReg8(src); writeLog("\n");
    }
  }

  // This is SRL (HL) instruction
  else if(follByte == 0x3E) {
    opTbl[0xCB].TStates = 15;
    u8 value = readByte(z80.HL);

    // Copy bit 0 to carry bit
    if(value & 0x01)
      z80.F |= FLAG_CARRY;
    else
      z80.F &= ~(FLAG_CARRY);

    u8 res = (value >> 1); // Shift right by one

    z80.F &= ~(FLAG_SIGN | FLAG_HCARRY);
    testZero_8(res);
    testParity_8(res);
    rstAddSub();

    writeByte(res, z80.HL);
    if(logInstr) {
      fprintf(fpLog, "SRL (HL)\t\tHL = %04X\n", z80.HL);
    }
  }
  else die("[ERROR] Invalid RLC instruction.");
}


// This is JP nn instruction
void JPnn(u8 opcode) {
  u16 offset = fetch16();
  z80.PC = offset;
  if(logInstr) {
    fprintf(fpLog, "JP %04X\n", offset);
  }
}


// This is JP cc, nn instruction
void JPccnn(u8 opcode) {
  u16 offset = fetch16();
  u8 cc = ((opcode >> 3) & 0x07);

  switch(cc) {
    case 0x00: // NZ non-zero
      if(!(z80.F & FLAG_ZERO))
        z80.PC = offset;
      if(logInstr)
        fprintf(fpLog, "JP NZ, %04X\n", offset);
      break;
    case 0x01: // Z zero
      if(z80.F & FLAG_ZERO)
        z80.PC = offset;
      if(logInstr)
        fprintf(fpLog, "JP Z, %04X\n", offset);
      break;
    case 0x02: // NC no carry
      if(!(z80.F & FLAG_CARRY))
        z80.PC = offset;
      if(logInstr)
        fprintf(fpLog, "JP NC, %04X\n", offset);
      break;
    case 0x03: // C carry
      if(z80.F & FLAG_CARRY)
        z80.PC = offset;
      if(logInstr)
        fprintf(fpLog, "JP C, %04X\n", offset);
      break;
    case 0x04: // P/V parity odd (P/V reset)
      if(!(z80.F & FLAG_PARITY))
        z80.PC = offset;
      if(logInstr)
        fprintf(fpLog, "JP PO, %04X\n", offset);
      break;
    case 0x05: // P/V parity even (P/V set)
      if(z80.F & FLAG_PARITY)
        z80.PC = offset;
      if(logInstr)
        fprintf(fpLog, "JP PE, %04X\n", offset);
      break;
    case 0x06: // S sign positive (S reset)
      if(!(z80.F & FLAG_SIGN))
        z80.PC = offset;
      if(logInstr)
        fprintf(fpLog, "JP P, %04X\n", offset);
      break;
    case 0x07: // S sign negative (S set)
      if(z80.F & FLAG_SIGN)
        z80.PC = offset;
      if(logInstr)
        fprintf(fpLog, "JP M, %04X\n", offset);
      break;
    default:
      die("[ERROR] Invalid 'cc' in JP cc, nn instruction.");
  }
}


// This is JR e instruction
void JRe(u8 opcode) {
  u8 offset = fetch8();
  u16 extended_o = offset;
  if(isNegative(offset))
    extended_o |= 0xFF00;

  z80.PC += extended_o;
  if(logInstr) {
    fprintf(fpLog, "JR %02X\n", offset);
  }
}


// This is JR C, e instruction
void JRCe(u8 opcode) {
  opTbl[0x38].TStates = 12;  // Condition is met
  u8 offset = fetch8();
  u16 extended_o = offset;
  if(isNegative(offset))
    extended_o |= 0xFF00;

  if(z80.F & FLAG_CARRY)
    z80.PC += extended_o;
  else opTbl[0x38].TStates = 7;  // Condition is not met

  if(logInstr) {
    fprintf(fpLog, "JR C, %02X\n", offset);
  }
}


// This is JR NC, e instruction
void JRNCe(u8 opcode) {
  opTbl[0x30].TStates = 12;  // Condition is met
  u8 offset = fetch8();
  u16 extended_o = offset;
  if(isNegative(offset))
    extended_o |= 0xFF00;

  if(!(z80.F & FLAG_CARRY))
    z80.PC += extended_o;
  else opTbl[0x30].TStates = 7;  // Condition is not met

  if(logInstr) {
    fprintf(fpLog, "JR NC, %02X\n", offset);
  }
}


// This is JR Z, e instruction
void JRZe(u8 opcode) {
  opTbl[0x28].TStates = 12;  // Condition is met
  u8 offset = fetch8();
  u16 extended_o = offset;
  if(isNegative(offset))
    extended_o |= 0xFF00;

  if(z80.F & FLAG_ZERO)
    z80.PC += extended_o;
  else opTbl[0x28].TStates = 7;  // Condition is not met

  if(logInstr) {
    fprintf(fpLog, "JR Z, %02X\n", offset);
  }
}


// This is JR NZ, e instruction
void JRNZe(u8 opcode) {
  opTbl[0x20].TStates = 12;  // Condition is met
  u8 offset = fetch8();
  u16 extended_o = offset;
  if(isNegative(offset))
    extended_o |= 0xFF00;

  if(!(z80.F & FLAG_ZERO))
    z80.PC += extended_o;
  else opTbl[0x20].TStates = 7;  // Condition is not met

  if(logInstr) {
    fprintf(fpLog, "JR NZ, %02X\n", offset);
  }
}


// This is JP (HL) instruction
void JPHL(u8 opcode) {
  z80.PC = z80.HL;
  if(logInstr) {
    writeLog("JP HL\n");
  }
}


// This is DJNZ, e instruction
void DJNZe(u8 opcode) {
  u8 offset = fetch8();
  u16 extended_o = offset;
  if(isNegative(offset))
    extended_o |= 0xFF00;

  z80.B--;

  if(z80.B != 0) {
    opTbl[0x10].TStates = 13;
    z80.PC += extended_o;
  }
  else opTbl[0x10].TStates = 8;

  if(logInstr) {
    fprintf(fpLog, "DJNZ %02X\n", offset);
  }
}


// This is CALL nn instruction
void CALLnn(u8 opcode) {
  u16 nn = fetch16();
  stackPush(z80.PC);
  z80.PC = nn;
  if(logInstr) {
    fprintf(fpLog, "CALL %04X\n", nn);
  }
}


// This is CALL cc, nn instruction
void CALLccnn(u8 opcode) {
  u16 nn = fetch16();
  u8 cc = ((opcode >> 3) & 0x07);

  switch(cc) {
    case 0x00: // NZ non-zero
      if(!(z80.F & FLAG_ZERO)) {
        stackPush(z80.PC);
        z80.PC = nn;
        opTbl[0xC4].TStates = 17; // Latency if cc is true
      }
      else opTbl[0xC4].TStates = 10; // Latency if cc is false
      if(logInstr) {
        fprintf(fpLog, "CALL NZ, %04X\n", nn);
      }
      break;
    case 0x01: // Z zero
      if(z80.F & FLAG_ZERO) {
        stackPush(z80.PC);
        z80.PC = nn;
        opTbl[0xCC].TStates = 17; // Latency if cc is true
      }
      else opTbl[0xCC].TStates = 10; // Latency if cc is false
      if(logInstr) {
        fprintf(fpLog, "CALL Z, %04X\n", nn);
      }
      break;
    case 0x02: // NC no carry
      if(!(z80.F & FLAG_CARRY)) {
        stackPush(z80.PC);
        z80.PC = nn;
        opTbl[0xD4].TStates = 17; // Latency if cc is true
      }
      else opTbl[0xD4].TStates = 10; // Latency if cc is false
      if(logInstr) {
        fprintf(fpLog, "CALL NC, %04X\n", nn);
      }
      break;
    case 0x03: // C carry
      if(z80.F & FLAG_CARRY) {
        stackPush(z80.PC);
        z80.PC = nn;
        opTbl[0xDC].TStates = 17; // Latency if cc is true
      }
      else opTbl[0xDC].TStates = 10; // Latency if cc is false
      if(logInstr) {
        fprintf(fpLog, "CALL C, %04X\n", nn);
      }
      break;
    case 0x04: // P/V parity odd (P/V reset)
      if(!(z80.F & FLAG_PARITY)) {
        stackPush(z80.PC);
        z80.PC = nn;
        opTbl[0xE4].TStates = 17; // Latency if cc is true
      }
      else opTbl[0xE4].TStates = 10; // Latency if cc is false
      if(logInstr) {
        fprintf(fpLog, "CALL PO, %04X\n", nn);
      }
      break;
    case 0x05: // P/V parity even (P/V set)
      if(z80.F & FLAG_PARITY) {
        stackPush(z80.PC);
        z80.PC = nn;
        opTbl[0xEC].TStates = 17; // Latency if cc is true
      }
      else opTbl[0xEC].TStates = 10; // Latency if cc is false
      if(logInstr) {
        fprintf(fpLog, "CALL PE, %04X\n", nn);
      }
      break;
    case 0x06: // S sign positive (S reset)
      if(!(z80.F & FLAG_SIGN)) {
        stackPush(z80.PC);
        z80.PC = nn;
        opTbl[0xF4].TStates = 17; // Latency if cc is true
      }
      else opTbl[0xF4].TStates = 10; // Latency if cc is false
      if(logInstr) {
        fprintf(fpLog, "CALL P, %04X\n", nn);
      }
      break;
    case 0x07: // S sign negative (S set)
      if(z80.F & FLAG_SIGN) {
        stackPush(z80.PC);
        z80.PC = nn;
        opTbl[0xFC].TStates = 17; // Latency if cc is true
      }
      else opTbl[0xFC].TStates = 10; // Latency if cc is false
      if(logInstr) {
        fprintf(fpLog, "CALL M, %04X\n", nn);
      }
      break;
    default:
      die("[ERROR] Invalid condition in CALL cc, nn instruction.");
  }
}


// This is RET instruction
void RET(u8 opcode) {
  z80.PC = stackPop();
  if(logInstr) {
    writeLog("RET\n");
  }
}


// This is RET cc instruction
void RETcc(u8 opcode) {
  u8 cc = ((opcode >> 3) & 0x07);

  switch(cc) {
    case 0x00: // NZ non-zero
      if(!(z80.F & FLAG_ZERO)) {
        z80.PC = stackPop();
        opTbl[0xC0].TStates = 11; // Latency if cc is true
      }
      else opTbl[0xC0].TStates = 5; // Latency if cc is false
      if(logInstr) {
        writeLog("RET NZ\n");
      }
      break;
    case 0x01: // Z zero
      if(z80.F & FLAG_ZERO) {
        z80.PC = stackPop();
        opTbl[0xC8].TStates = 11; // Latency if cc is true
      }
      else opTbl[0xC8].TStates = 5; // Latency if cc is false
      if(logInstr) {
        writeLog("RET Z\n");
      }
      break;
    case 0x02: // NC no carry
      if(!(z80.F & FLAG_CARRY)) {
        z80.PC = stackPop();
        opTbl[0xD0].TStates = 11; // Latency if cc is true
      }
      else opTbl[0xD0].TStates = 5; // Latency if cc is false
      if(logInstr) {
        writeLog("RET NC\n");
      }
      break;
    case 0x03: // C carry
      if(z80.F & FLAG_CARRY) {
        z80.PC = stackPop();
        opTbl[0xD8].TStates = 11; // Latency if cc is true
      }
      else opTbl[0xD8].TStates = 5; // Latency if cc is false
      if(logInstr) {
        writeLog("RET C\n");
      }
      break;
    case 0x04: // P/V parity odd (P/V reset)
      if(!(z80.F & FLAG_PARITY)) {
        z80.PC = stackPop();
        opTbl[0xE0].TStates = 11; // Latency if cc is true
      }
      else opTbl[0xE0].TStates = 5; // Latency if cc is false
      if(logInstr) {
        writeLog("RET PO\n");
      }
      break;
    case 0x05: // P/V parity even (P/V set)
      if(z80.F & FLAG_PARITY) {
        z80.PC = stackPop();
        opTbl[0xE8].TStates = 11; // Latency if cc is true
      }
      else opTbl[0xE8].TStates = 5; // Latency if cc is false
      if(logInstr) {
        writeLog("RET PE\n");
      }
      break;
    case 0x06: // S sign positive (S reset)
      if(!(z80.F & FLAG_SIGN)) {
        z80.PC = stackPop();
        opTbl[0xF0].TStates = 11; // Latency if cc is true
      }
      else opTbl[0xF0].TStates = 5; // Latency if cc is false
      if(logInstr) {
        writeLog("RET P\n");
      }
      break;
    case 0x07: // S sign negative (S set)
      if(z80.F & FLAG_SIGN) {
        z80.PC = stackPop();
        opTbl[0xF8].TStates = 11; // Latency if cc is true
      }
      else opTbl[0xF8].TStates = 5; // Latency if cc is false
      if(logInstr) {
        writeLog("RET M\n");
      }
      break;
    default:
      die("[ERROR] Invalid condition in RET cc instruction.");
  }
}


// This is RST p instruction
void RSTp(u8 opcode) {
  u8 t = ((opcode >> 3) & 0x07);
  stackPush(z80.PC);

  switch(t) {
    case 0x00:
      z80.PC = 0x0000;
      if(logInstr)
        writeLog("RST 00h\n");
      break;
    case 0x01:
      z80.PC = 0x0008;
      if(logInstr)
        writeLog("RST 08h\n");
      break;
    case 0x02:
      z80.PC = 0x0010;
      if(logInstr)
        writeLog("RST 10h\n");
      break;
    case 0x03:
      z80.PC = 0x0018;
      if(logInstr)
        writeLog("RST 18h\n");
      break;
    case 0x04:
      z80.PC = 0x0020;
      if(logInstr)
        writeLog("RST 20h\n");
      break;
    case 0x05:
      z80.PC = 0x0028;
      if(logInstr)
        writeLog("RST 28h\n");
      break;
    case 0x06:
      z80.PC = 0x0030;
      if(logInstr)
        writeLog("RST 30h\n");
      break;
    case 0x07:
      z80.PC = 0x0038;
      if(logInstr)
        writeLog("RST 38h\n");
      break;
    default:
      die("[ERROR] Invalid t in RST p instruction.");
  }
}


// This is IN A, (n) instruction
void INAn(u8 opcode) {
  u8 n = fetch8();
  z80.A = z80.portIn(n);

  if(logInstr) {
    fprintf(fpLog, "IN A, (n)\t\tn = %02X\n", n);
  }
}


// This is OUT (n), A
void OUTnA(u8 opcode) {
  u8 n = fetch8();
  z80.portOut(n, z80.A);

  if(logInstr) {
    fprintf(fpLog, "OUT (n), A\t\tn = %02X\n", n);
  }
}


OpTbl opTbl[0x100] = {
  {NOP, 4},
  {LDddnn, 10},
  {LDBCA, 7},
  {INCss, 6},
  {INCr, 4},
  {DECr, 4},
  {LDrn, 7},
  {RLCA, 4},
  {EXAFAFr, 4},
  {ADDHLss, 11},
  {LDABC, 7},
  {DECss, 6},
  {INCr, 4},
  {DECr, 4},
  {LDrn, 7},
  {RRCA, 4},
  {DJNZe, 13}, // 0x10
  {LDddnn, 10},
  {LDDEA, 7},
  {INCss, 6},
  {INCr, 4},
  {DECr, 4},
  {LDrn, 7},
  {RLA, 4},
  {JRe, 12},
  {ADDHLss, 11},
  {LDADE, 7},
  {DECss, 6},
  {INCr, 4},
  {DECr, 4},
  {LDrn, 7},
  {RRA, 4},
  {JRNZe, 12}, // 0x20
  {LDddnn, 10},
  {LDnnHL, 16},
  {INCss, 6},
  {INCr, 4},
  {DECr, 4},
  {LDrn, 7},
  {DAA, 4},
  {JRZe, 12},
  {ADDHLss, 11},
  {LDHLnn, 16},
  {DECss, 6},
  {INCr, 4},
  {DECr, 4},
  {LDrn, 7},
  {CPL, 4},
  {JRNCe, 12}, // 0x30
  {LDddnn, 10},
  {LDnnA, 13},
  {INCss, 6},
  {INCHL, 11},
  {DECHL, 11},
  {LDHLn, 10},
  {SCF, 4},
  {JRCe, 12},
  {ADDHLss, 11},
  {LDAnn, 13},
  {DECss, 6},
  {INCr, 4},
  {DECr, 4},
  {LDrn, 7},
  {CCF, 4},
  {LDrr, 4}, // 0X40
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrHL, 7},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrHL, 7},
  {LDrr, 4},
  {LDrr, 4}, // 0x50
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrHL, 7},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrHL, 7},
  {LDrr, 4},
  {LDrr, 4}, // 0x60
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrHL, 7},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrHL, 7},
  {LDrr, 4},
  {LDHLr, 7}, // 0x70
  {LDHLr, 7},
  {LDHLr, 7},
  {LDHLr, 7},
  {LDHLr, 7},
  {LDHLr, 7},
  {HALT, 4},
  {LDHLr, 7},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrr, 4},
  {LDrHL, 7},
  {LDrr, 4},
  {ADDAr, 4}, // 0x80
  {ADDAr, 4},
  {ADDAr, 4},
  {ADDAr, 4},
  {ADDAr, 4},
  {ADDAr, 4},
  {ADDAHL, 7},
  {ADDAr, 4},
  {ADCAr, 4},
  {ADCAr, 4},
  {ADCAr, 4},
  {ADCAr, 4},
  {ADCAr, 4},
  {ADCAr, 4},
  {ADCAHL, 7},
  {ADCAr, 4},
  {SUBAr, 4}, // 0x90
  {SUBAr, 4},
  {SUBAr, 4},
  {SUBAr, 4},
  {SUBAr, 4},
  {SUBAr, 4},
  {SUBAHL, 7},
  {SUBAr, 4},
  {SBCAr, 4},
  {SBCAr, 4},
  {SBCAr, 4},
  {SBCAr, 4},
  {SBCAr, 4},
  {SBCAr, 4},
  {SBCAHL, 7},
  {SBCAr, 4},
  {ANDr, 4}, // 0xA0
  {ANDr, 4},
  {ANDr, 4},
  {ANDr, 4},
  {ANDr, 4},
  {ANDr, 4},
  {ANDHL, 7},
  {ANDr, 4},
  {XORr, 4},
  {XORr, 4},
  {XORr, 4},
  {XORr, 4},
  {XORr, 4},
  {XORr, 4},
  {XORHL, 7},
  {XORr, 4},
  {ORr, 4}, // 0xB0
  {ORr, 4},
  {ORr, 4},
  {ORr, 4},
  {ORr, 4},
  {ORr, 4},
  {ORHL, 7},
  {ORr, 4},
  {CPr, 4},
  {CPr, 4},
  {CPr, 4},
  {CPr, 4},
  {CPr, 4},
  {CPr, 4},
  {CPHL, 7},
  {CPr, 4},
  {RETcc, 5}, // 0xC0
  {POPqq, 10},
  {JPccnn, 10},
  {JPnn, 10},
  {CALLccnn, 10},
  {PUSHqq, 11},
  {ADDAn, 7},
  {RSTp, 11},
  {RETcc, 5},
  {RET, 10},
  {JPccnn, 10},
  {RLC, 8},
  {CALLccnn, 10},
  {CALLnn, 17},
  {ADCAn, 7},
  {RSTp, 11},
  {RETcc, 5}, // 0xD0
  {POPqq, 10},
  {JPccnn, 10},
  {OUTnA, 11},
  {CALLccnn, 10},
  {PUSHqq, 11},
  {SUBAn, 7},
  {RSTp, 11},
  {RETcc, 5},
  {EXX, 4},
  {JPccnn, 10},
  {INAn, 11},
  {CALLccnn, 10},
  {LDIX, 19},
  {SBCAn, 7},
  {RSTp, 11},
  {RETcc, 5}, // 0xE0
  {POPqq, 10},
  {JPccnn, 10},
  {EXSPHL, 19},
  {CALLccnn, 10},
  {PUSHqq, 11},
  {ANDn, 7},
  {RSTp, 11},
  {RETcc, 5},
  {JPHL, 4},
  {JPccnn, 10},
  {EXDEHL, 4},
  {CALLccnn, 10},
  {LDRIddnn, 9},
  {XORn, 7},
  {RSTp, 11},
  {RETcc, 5}, // 0xF0
  {POPqq, 10},
  {JPccnn, 10},
  {DI, 4},
  {CALLccnn, 10},
  {PUSHqq, 11},
  {ORn, 7},
  {RSTp, 11},
  {RETcc, 5},
  {LDSPHL, 6},
  {JPccnn, 10},
  {EI, 4},
  {CALLccnn, 10},
  {LDIY, 19},
  {CPn, 7},
  {RSTp, 11}
};
