//#include <ncurses.h>

#include "opcodes.h"
#include "logger.h"

/*
  TODO: Interrupts must be recognized during LDIR instruction iterations.
  TODO: Check 16-bit arithmetic instructions
  TODO: DAA is missing, pag. 173.
  TODO: Complete IN and OUT, pag. 298.
*/

// Returns one byte from the current PC.
uint8_t opc_fetch8(cpu_t *cpu) {
    return cpu_read(cpu, cpu->PC++);
}


// Returns two bytes from the current PC.
uint16_t opc_fetch16(cpu_t *cpu) {
    return (opc_fetch8(cpu) | (opc_fetch8(cpu) << 8));
}


// *****************************************************
// *****    STATUS REGISTER - SUPPORT FUNCTIONS    *****
// *****************************************************
/*
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

*/



///////////////////////////////////////////////////////////
// REGISTER ACCESS FUNCTIONS
///////////////////////////////////////////////////////////

// Writes data into a register.
static void opc_writeReg(cpu_t *cpu, uint8_t reg, uint8_t value) {
    switch(reg) {
        case 0x00:
            cpu->B = value; break;
        case 0x01:
            cpu->C = value; break;
        case 0x02:
            cpu->D = value; break;
        case 0x03:
            cpu->E = value; break;
        case 0x04:
            cpu->H = value; break;
        case 0x05:
            cpu->L = value; break;
        case 0x07:
            cpu->A = value; break;
        default:
            LOG_FATAL("Cannot write to unknown register (0x%02X).\n", reg);
            exit(1);
    }
    return;
}


// Reads data from a register.
static uint8_t opc_readReg(cpu_t *cpu, uint8_t reg) {
    switch(reg) {
        case 0x00:
            return cpu->B;
        case 0x01:
            return cpu->C;
        case 0x02:
            return cpu->D;
        case 0x03:
            return cpu->E;
        case 0x04:
            return cpu->H;
        case 0x05:
            return cpu->L;
        case 0x07:
            return cpu->A;
        default:
            LOG_FATAL("Cannot read unknown register (0x%02X).\n", reg);
            exit(1);
    }
    return 0; // Never reached.
}


// Returns a string carrying the name of the given 8-bit register.
static char * opc_regName8(uint8_t reg) {
    switch(reg) {
        case 0x00:
            return "B";
        case 0x01:
            return "C";
        case 0x02:
            return "D";
        case 0x03:
            return "E";
        case 0x04:
            return "H";
        case 0x05:
            return "L";
        case 0x07:
            return "A";
        default:
            LOG_FATAL("Unknown register (0x%02X).\n", reg);
            exit(1);
    }
    return ""; // Never reached.
}

/*

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

*/



///////////////////////////////////////////////////////////
// INSTRUCTION SET ARCHITECTURE
///////////////////////////////////////////////////////////

// LD r,r' instruction.
static void opc_LDrr(cpu_t *cpu, uint8_t opcode) {
    uint8_t dst = ((opcode >> 3) & 0x07);
    uint8_t src = (opcode & 0x07);
    uint8_t data = opc_readReg(cpu, src);
    opc_writeReg(cpu, dst, data);
    LOG_DEBUG("Executed LD %s,%s\n", opc_regName8(dst), opc_regName8(src));
    return;
}


// LD r,n instruction.
static void opc_LDrn(cpu_t *cpu, uint8_t opcode) {
    uint8_t dst = ((opcode >> 3) & 0x07);
    uint8_t n = opc_fetch8(cpu);
    opc_writeReg(cpu, dst, n);
    LOG_DEBUG("Executed LD %s,0x%02X\n", opc_regName8(dst), n);
    return;
}


// LD r,(HL) instruction.
static void opc_LDrHL(cpu_t *cpu, uint8_t opcode) {
    uint8_t dst = ((opcode >> 3) & 0x07);
    uint8_t data = cpu_read(cpu, cpu->HL);
    opc_writeReg(cpu, dst, data);
    LOG_DEBUG("Executed LD %s,(HL) HL=0x%04X\n", opc_regName8(dst), cpu->HL);
    return;
}


static void opc_LDIX(cpu_t *cpu, uint8_t opcode) {
    opc_tbl[0xDD].TStates = 19;
    uint8_t next_opc = opc_fetch8(cpu);

    // LD r,(IX+d) instruction.
    if ((next_opc & 0xC7) == 0x46) {
        uint8_t dst = ((next_opc >> 3) & 0x07);
        int8_t d = (int8_t)opc_fetch8(cpu);
        uint16_t addr = cpu->IX + d;
        uint8_t data = cpu_read(cpu, addr);
        opc_writeReg(cpu, dst, data);
        LOG_DEBUG("Executed LD %s,(IX+d) IX+d=0x%04X\n", opc_regName8(dst), addr);
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


void LDIY(cpu_t *cpu, uint8_t opcode) {
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
void LDHLr(cpu_t *cpu, uint8_t opcode) {
  u8 src = (opcode & 0x07);
  u8 srcVal = readReg(src);
  writeByte(srcVal, z80.HL);
  if(logInstr) {
    writeLog("LD (HL), "); logReg8(src); fprintf(fpLog, "\t\tHL = %04X\n", z80.HL);
  }
}


// This is LD (HL), n instruction
void LDHLn(cpu_t *cpu, uint8_t opcode) {
  u8 n = fetch8();
  writeByte(n, z80.HL);
  if(logInstr) {
    fprintf(fpLog, "LD (HL), n\t\tHL = %04X, n = %02X\n", z80.HL, n);
  }
}


// This is LD A, (BC) instruction
void LDABC(cpu_t *cpu, uint8_t opcode) {
  z80.A = readByte(z80.BC);
  if(logInstr) {
    fprintf(fpLog, "LD A, (BC)\t\tBC = %04X\n", z80.BC);
  }
}


// This is LD A, (DE) instruction
void LDADE(cpu_t *cpu, uint8_t opcode) {
  z80.A = readByte(z80.DE);
  if(logInstr) {
    fprintf(fpLog, "LD A, (DE)\t\tDE = %04X\n", z80.DE);
  }
}


// This is LD A, (nn) instruction
void LDAnn(cpu_t *cpu, uint8_t opcode) {
  u16 offset = fetch16();
  z80.A = readByte(offset);
  if(logInstr) {
    fprintf(fpLog, "LD A, (nn)\t\tnn = %04X\n", offset);
  }
}


// This is LD (BC), A
void LDBCA(cpu_t *cpu, uint8_t opcode) {
  writeByte(z80.A, z80.BC);
  if(logInstr) {
    fprintf(fpLog, "LD (BC), A\t\tBC = %04X\n", z80.BC);
  }
}


// This is LD (DE), A
void LDDEA(cpu_t *cpu, uint8_t opcode) {
  writeByte(z80.A, z80.DE);
  if(logInstr) {
    fprintf(fpLog, "LD (DE), A\t\tDE = %04X\n", z80.DE);
  }
}


// This is LD (nn), A
void LDnnA(cpu_t *cpu, uint8_t opcode) {
  u16 offset = fetch16();
  writeByte(z80.A, offset);
  if(logInstr) {
    fprintf(fpLog, "LD (nn), A\t\tnn = %04X\n", offset);
  }
}


void LDRIddnn(cpu_t *cpu, uint8_t opcode) {
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
void LDddnn(cpu_t *cpu, uint8_t opcode) {
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
void LDHLnn(cpu_t *cpu, uint8_t opcode) {
  u16 offset = fetch16();
  z80.L = readByte(offset);
  z80.H = readByte(offset + 1);
  if(logInstr) {
    fprintf(fpLog, "LD HL, (nn)\t\tnn = %04X\n", offset);
  }
}


// This is LD (nn), HL
void LDnnHL(cpu_t *cpu, uint8_t opcode) {
  u16 offset = fetch16();
  writeByte(z80.L, offset);
  writeByte(z80.H, offset + 1);
  if(logInstr) {
    fprintf(fpLog, "LD (nn), HL\t\tnn = %04X\n", offset);
  }
}


// This is LD SP, HL instruction
void LDSPHL(cpu_t *cpu, uint8_t opcode) {
  z80.SP = z80.HL;
  if(logInstr) {
    writeLog("LD SP, HL\n");
  }
}


// This is PUSH qq instruction
void PUSHqq(cpu_t *cpu, uint8_t opcode) {
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
void POPqq(cpu_t *cpu, uint8_t opcode) {
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
void EXDEHL(cpu_t *cpu, uint8_t opcode) {
  FASTSWAP(z80.DE, z80.HL);
  if(logInstr) {
    writeLog("EX DE, HL\n");
  }
}


// This is EX AF, AF' instruction
void EXAFAFr(cpu_t *cpu, uint8_t opcode) {
  FASTSWAP(z80.AF, z80.ArFr);
  if(logInstr) {
    writeLog("EX AF, AF'\n");
  }
}


// This is EXX instruction
void EXX(cpu_t *cpu, uint8_t opcode) {
  FASTSWAP(z80.BC, z80.BrCr);
  FASTSWAP(z80.DE, z80.DrEr);
  FASTSWAP(z80.HL, z80.HrLr);
  if(logInstr) {
    writeLog("EXX\n");
  }
}


// This is EX (SP), HL instruction
void EXSPHL(cpu_t *cpu, uint8_t opcode) {
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
void ADDAr(cpu_t *cpu, uint8_t opcode) {
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
void SUBAr(cpu_t *cpu, uint8_t opcode) {
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
void ADDAn(cpu_t *cpu, uint8_t opcode) {
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
void SUBAn(cpu_t *cpu, uint8_t opcode) {
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
void ADDAHL(cpu_t *cpu, uint8_t opcode) {
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
void SUBAHL(cpu_t *cpu, uint8_t opcode) {
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
void ADCAr(cpu_t *cpu, uint8_t opcode) {
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
void SBCAr(cpu_t *cpu, uint8_t opcode) {
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
void ADCAn(cpu_t *cpu, uint8_t opcode) {
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
void SBCAn(cpu_t *cpu, uint8_t opcode) {
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
void ADCAHL(cpu_t *cpu, uint8_t opcode) {
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
void SBCAHL(cpu_t *cpu, uint8_t opcode) {
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
void ANDr(cpu_t *cpu, uint8_t opcode) {
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
void ANDn(cpu_t *cpu, uint8_t opcode) {
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
void ANDHL(cpu_t *cpu, uint8_t opcode) {
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
void ORr(cpu_t *cpu, uint8_t opcode) {
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
void ORn(cpu_t *cpu, uint8_t opcode) {
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
void ORHL(cpu_t *cpu, uint8_t opcode) {
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
void XORr(cpu_t *cpu, uint8_t opcode) {
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
void XORn(cpu_t *cpu, uint8_t opcode) {
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
void XORHL(cpu_t *cpu, uint8_t opcode) {
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
void CPr(cpu_t *cpu, uint8_t opcode) {
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
void CPn(cpu_t *cpu, uint8_t opcode) {
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
void CPHL(cpu_t *cpu, uint8_t opcode) {
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
void INCr(cpu_t *cpu, uint8_t opcode) {
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
void INCHL(cpu_t *cpu, uint8_t opcode) {
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
void DECr(cpu_t *cpu, uint8_t opcode) {
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
void DECHL(cpu_t *cpu, uint8_t opcode) {
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
void DAA(cpu_t *cpu, uint8_t opcode) {
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
void CPL(cpu_t *cpu, uint8_t opcode) {
  z80.A = ~(z80.A);

  setAddSub();
  z80.F |= (FLAG_HCARRY);

  if(logInstr) {
    writeLog("CPL\n");
  }
}


// This is CCF instruction
void CCF(cpu_t *cpu, uint8_t opcode) {
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
void SCF(cpu_t *cpu, uint8_t opcode) {
  rstAddSub();
  z80.F &= ~(FLAG_HCARRY);
  z80.F |= (FLAG_CARRY);

  if(logInstr) {
    writeLog("SCF\n");
  }
}


// This is NOP instruction
void NOP(cpu_t *cpu, uint8_t opcode) {
  /* Do nothing */

  if(logInstr) {
    writeLog("NOP\n");
  }
}


// This is HALT instruction
void HALT(cpu_t *cpu, uint8_t opcode) {
  z80.halt = 1;

  if(logInstr) {
    writeLog("HALT\n");
  }
}


// This is DI instruction
void DI(cpu_t *cpu, uint8_t opcode) {
  z80.IFF1 = 0;
  z80.IFF2 = 0;

  if(logInstr) {
    writeLog("DI\n");
  }
}


// This is EI instruction
void EI(cpu_t *cpu, uint8_t opcode) {
  z80.IFF1 = 1;
  z80.IFF2 = 1;

  if(logInstr) {
    writeLog("EI\n");
  }
}


// This is ADD HL, ss instruction
void ADDHLss(cpu_t *cpu, uint8_t opcode) {
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
void INCss(cpu_t *cpu, uint8_t opcode) {
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
void DECss(cpu_t *cpu, uint8_t opcode) {
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
void RLCA(cpu_t *cpu, uint8_t opcode) {
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
void RLA(cpu_t *cpu, uint8_t opcode) {
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
void RRCA(cpu_t *cpu, uint8_t opcode) {
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
void RRA(cpu_t *cpu, uint8_t opcode) {
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


void RLC(cpu_t *cpu, uint8_t opcode) {
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
void JPnn(cpu_t *cpu, uint8_t opcode) {
  u16 offset = fetch16();
  z80.PC = offset;
  if(logInstr) {
    fprintf(fpLog, "JP %04X\n", offset);
  }
}


// This is JP cc, nn instruction
void JPccnn(cpu_t *cpu, uint8_t opcode) {
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
void JRe(cpu_t *cpu, uint8_t opcode) {
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
void JRCe(cpu_t *cpu, uint8_t opcode) {
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
void JRNCe(cpu_t *cpu, uint8_t opcode) {
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
void JRZe(cpu_t *cpu, uint8_t opcode) {
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
void JRNZe(cpu_t *cpu, uint8_t opcode) {
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
void JPHL(cpu_t *cpu, uint8_t opcode) {
  z80.PC = z80.HL;
  if(logInstr) {
    writeLog("JP HL\n");
  }
}


// This is DJNZ, e instruction
void DJNZe(cpu_t *cpu, uint8_t opcode) {
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
void CALLnn(cpu_t *cpu, uint8_t opcode) {
  u16 nn = fetch16();
  stackPush(z80.PC);
  z80.PC = nn;
  if(logInstr) {
    fprintf(fpLog, "CALL %04X\n", nn);
  }
}


// This is CALL cc, nn instruction
void CALLccnn(cpu_t *cpu, uint8_t opcode) {
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
void RET(cpu_t *cpu, uint8_t opcode) {
  z80.PC = stackPop();
  if(logInstr) {
    writeLog("RET\n");
  }
}


// This is RET cc instruction
void RETcc(cpu_t *cpu, uint8_t opcode) {
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
void RSTp(cpu_t *cpu, uint8_t opcode) {
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
void INAn(cpu_t *cpu, uint8_t opcode) {
  u8 n = fetch8();
  z80.A = z80.portIn(n);

  if(logInstr) {
    fprintf(fpLog, "IN A, (n)\t\tn = %02X\n", n);
  }
}


// This is OUT (n), A
void OUTnA(cpu_t *cpu, uint8_t opcode) {
  u8 n = fetch8();
  z80.portOut(n, z80.A);

  if(logInstr) {
    fprintf(fpLog, "OUT (n), A\t\tn = %02X\n", n);
  }
}


opc_t opc_tbl[0x100] = {
    {opc_NOP, 4},
    {opc_LDddnn, 10},
    {opc_LDBCA, 7},
    {opc_INCss, 6},
    {opc_INCr, 4},
    {opc_DECr, 4},
    {opc_LDrn, 7},
    {opc_RLCA, 4},
    {opc_EXAFAFr, 4},
    {opc_ADDHLss, 11},
    {opc_LDABC, 7},
    {opc_DECss, 6},
    {opc_INCr, 4},
    {opc_DECr, 4},
    {opc_LDrn, 7},
    {opc_RRCA, 4},
    {opc_DJNZe, 13}, // 0x10
    {opc_LDddnn, 10},
    {opc_LDDEA, 7},
    {opc_INCss, 6},
    {opc_INCr, 4},
    {opc_DECr, 4},
    {opc_LDrn, 7},
    {opc_RLA, 4},
    {opc_JRe, 12},
    {opc_ADDHLss, 11},
    {opc_LDADE, 7},
    {opc_DECss, 6},
    {opc_INCr, 4},
    {opc_DECr, 4},
    {opc_LDrn, 7},
    {opc_RRA, 4},
    {opc_JRNZe, 12}, // 0x20
    {opc_LDddnn, 10},
    {opc_LDnnHL, 16},
    {opc_INCss, 6},
    {opc_INCr, 4},
    {opc_DECr, 4},
    {opc_LDrn, 7},
    {opc_DAA, 4},
    {opc_JRZe, 12},
    {opc_ADDHLss, 11},
    {opc_LDHLnn, 16},
    {opc_DECss, 6},
    {opc_INCr, 4},
    {opc_DECr, 4},
    {opc_LDrn, 7},
    {opc_CPL, 4},
    {opc_JRNCe, 12}, // 0x30
    {opc_LDddnn, 10},
    {opc_LDnnA, 13},
    {opc_INCss, 6},
    {opc_INCHL, 11},
    {opc_DECHL, 11},
    {opc_LDHLn, 10},
    {opc_SCF, 4},
    {opc_JRCe, 12},
    {opc_ADDHLss, 11},
    {opc_LDAnn, 13},
    {opc_DECss, 6},
    {opc_INCr, 4},
    {opc_DECr, 4},
    {opc_LDrn, 7},
    {opc_CCF, 4},
    {opc_LDrr, 4}, // 0X40
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrHL, 7},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrHL, 7},
    {opc_LDrr, 4},
    {opc_LDrr, 4}, // 0x50
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrHL, 7},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrHL, 7},
    {opc_LDrr, 4},
    {opc_LDrr, 4}, // 0x60
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrHL, 7},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrHL, 7},
    {opc_LDrr, 4},
    {opc_LDHLr, 7}, // 0x70
    {opc_LDHLr, 7},
    {opc_LDHLr, 7},
    {opc_LDHLr, 7},
    {opc_LDHLr, 7},
    {opc_LDHLr, 7},
    {opc_HALT, 4},
    {opc_LDHLr, 7},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrr, 4},
    {opc_LDrHL, 7},
    {opc_LDrr, 4},
    {opc_ADDAr, 4}, // 0x80
    {opc_ADDAr, 4},
    {opc_ADDAr, 4},
    {opc_ADDAr, 4},
    {opc_ADDAr, 4},
    {opc_ADDAr, 4},
    {opc_ADDAHL, 7},
    {opc_ADDAr, 4},
    {opc_ADCAr, 4},
    {opc_ADCAr, 4},
    {opc_ADCAr, 4},
    {opc_ADCAr, 4},
    {opc_ADCAr, 4},
    {opc_ADCAr, 4},
    {opc_ADCAHL, 7},
    {opc_ADCAr, 4},
    {opc_SUBAr, 4}, // 0x90
    {opc_SUBAr, 4},
    {opc_SUBAr, 4},
    {opc_SUBAr, 4},
    {opc_SUBAr, 4},
    {opc_SUBAr, 4},
    {opc_SUBAHL, 7},
    {opc_SUBAr, 4},
    {opc_SBCAr, 4},
    {opc_SBCAr, 4},
    {opc_SBCAr, 4},
    {opc_SBCAr, 4},
    {opc_SBCAr, 4},
    {opc_SBCAr, 4},
    {opc_SBCAHL, 7},
    {opc_SBCAr, 4},
    {opc_ANDr, 4}, // 0xA0
    {opc_ANDr, 4},
    {opc_ANDr, 4},
    {opc_ANDr, 4},
    {opc_ANDr, 4},
    {opc_ANDr, 4},
    {opc_ANDHL, 7},
    {opc_ANDr, 4},
    {opc_XORr, 4},
    {opc_XORr, 4},
    {opc_XORr, 4},
    {opc_XORr, 4},
    {opc_XORr, 4},
    {opc_XORr, 4},
    {opc_XORHL, 7},
    {opc_XORr, 4},
    {opc_ORr, 4}, // 0xB0
    {opc_ORr, 4},
    {opc_ORr, 4},
    {opc_ORr, 4},
    {opc_ORr, 4},
    {opc_ORr, 4},
    {opc_ORHL, 7},
    {opc_ORr, 4},
    {opc_CPr, 4},
    {opc_CPr, 4},
    {opc_CPr, 4},
    {opc_CPr, 4},
    {opc_CPr, 4},
    {opc_CPr, 4},
    {opc_CPHL, 7},
    {opc_CPr, 4},
    {opc_RETcc, 5}, // 0xC0
    {opc_POPqq, 10},
    {opc_JPccnn, 10},
    {opc_JPnn, 10},
    {opc_CALLccnn, 10},
    {opc_PUSHqq, 11},
    {opc_ADDAn, 7},
    {opc_RSTp, 11},
    {opc_RETcc, 5},
    {opc_RET, 10},
    {opc_JPccnn, 10},
    {opc_RLC, 8},
    {opc_CALLccnn, 10},
    {opc_CALLnn, 17},
    {opc_ADCAn, 7},
    {opc_RSTp, 11},
    {opc_RETcc, 5}, // 0xD0
    {opc_POPqq, 10},
    {opc_JPccnn, 10},
    {opc_OUTnA, 11},
    {opc_CALLccnn, 10},
    {opc_PUSHqq, 11},
    {opc_SUBAn, 7},
    {opc_RSTp, 11},
    {opc_RETcc, 5},
    {opc_EXX, 4},
    {opc_JPccnn, 10},
    {opc_INAn, 11},
    {opc_CALLccnn, 10},
    {opc_LDIX, 19},
    {opc_SBCAn, 7},
    {opc_RSTp, 11},
    {opc_RETcc, 5}, // 0xE0
    {opc_POPqq, 10},
    {opc_JPccnn, 10},
    {opc_EXSPHL, 19},
    {opc_CALLccnn, 10},
    {opc_PUSHqq, 11},
    {opc_ANDn, 7},
    {opc_RSTp, 11},
    {opc_RETcc, 5},
    {opc_JPHL, 4},
    {opc_JPccnn, 10},
    {opc_EXDEHL, 4},
    {opc_CALLccnn, 10},
    {opc_LDRIddnn, 9},
    {opc_XORn, 7},
    {opc_RSTp, 11},
    {opc_RETcc, 5}, // 0xF0
    {opc_POPqq, 10},
    {opc_JPccnn, 10},
    {opc_DI, 4},
    {opc_CALLccnn, 10},
    {opc_PUSHqq, 11},
    {opc_ORn, 7},
    {opc_RSTp, 11},
    {opc_RETcc, 5},
    {opc_LDSPHL, 6},
    {opc_JPccnn, 10},
    {opc_EI, 4},
    {opc_CALLccnn, 10},
    {opc_LDIY, 19},
    {opc_CPn, 7},
    {opc_RSTp, 11}
};
