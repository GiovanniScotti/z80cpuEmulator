#include <ncurses.h>

#include "debug.h"


// Implements simple test cases to verify the correctness of the implementation
DebugFunc testArray[DEBUG_SIZE] = {
  {LDrIXd_test},
  {ADDAr_test},
  {ADC_test},
  {SBC_test},
  {NEG_test},
  {JRe_test},
  {JRCe_test},
  {JRNCe_test},
  {JRZe_test}
};


void selfTest() {
  writeLog("\n##### START DEBUGGING #####\n\n");
  for(int i = 0; i < DEBUG_SIZE; i++)
    testArray[i].execute ();
  writeLog("\n##### END DEBUGGING #####\n\n");
}


int LDrIXd_test() {
  if(!initZ80(rom_LDrIXd)) {
    writeLog("ERROR debugging LD r, (IX+d)!\n");
    return 0;
  }
  /* result of the operation: 0x55 in register A.
     After the fourth instruction B contains 0x55.
     After the sixth instruction C contains 0x55.
     Condition bits are not affected.
  */
  emulateZ80(6);
  u8 flags = z80.F | ND_FLAGS;
  if(flags == 0xFF && z80.A == 0x55 && z80.B == 0x55 && z80.C == 0x55)
    writeLog("LD r, (IX+d)\t...OK\n");
  else {
    writeLog("LD r, (IX+d)\t...ERROR\n");
    dumpRegisters();
  }
  return 1;
}


int ADDAr_test() {
  if(!initZ80(rom_ADDAr)) {
    writeLog("ERROR debugging ADD A, r!\n");
    return 0;
  }
  /* result of the operation: 0xCC in register A
     S is set, Z is reset, H is set, N is reset
     P/V is not set, C is set
  */
  emulateZ80(100);
  u8 flags = z80.F | ND_FLAGS;
  if(flags == 0xB9 && z80.A == 0xCC && z80.B == 0xDD)
    writeLog("ADD A, r\t...OK\n");
  else {
    writeLog("ADD A, r\t...ERROR\n");
    dumpRegisters();
  }
  return 1;
}


int ADC_test() {
  if(!initZ80(rom_ADC)) {
    writeLog("ERROR debugging ADC group!\n");
    return 0;
  }
  /* ADC A, B is performed. A = 7Fh, B = 02h.
     Final A must be 82h (7F + 02 + carry).
     S is set, Z is reset, HF is set, P/V is set (overflow!),
     N and C are reset.
  */
  emulateZ80(3);
  z80.F |= ND_FLAGS;
  if(z80.F == 0xBC && z80.A == 0x80 && z80.B == 0x02) {
    writeLog("ADC A, r\t...OK\n");
  }
  else {
    writeLog("ADC A, r\t...ERROR\n");
    dumpRegisters();
  }
  /* ADC A, n is performed. A = FFh, n = 07h.
     Final A must be 06h (FF + 07 + carry (0)).
     S is reset, Z is reset, HF is set, P/V is reset,
     N is reset and C is set.
  */
  emulateZ80(2);
  if(z80.F == 0x39 && z80.A == 0x06 && z80.B == 0x02) {
    writeLog("ADC A, n\t...OK\n");
  }
  else {
    writeLog("ADC A, n\t...ERROR\n");
    dumpRegisters();
  }
  /* ADC A, (HL) is performed. A = 55h, (HL) = FBh.
     Final A must be 51h (55 + FB + carry (1)).
     S is reset, Z is reset, HF is set, P/V is reset,
     N is reset and C is set.
  */
  emulateZ80(4);
  if(z80.F == 0x39 && z80.A == 0x51 && z80.B == 0x02) {
    writeLog("ADC A, (HL)\t...OK\n");
  }
  else {
    writeLog("ADC A, (HL)\t...ERROR\n");
    dumpRegisters();
  }
  /* ADC A, (IX+d) is performed. A = 51h, (IX+d) = 03h.
     Final A must be 55h (51 + 03 + carry (1)).
     All the flags are reset.
  */
  emulateZ80(3);
  if(z80.F == 0x28 && z80.A == 0x55 && z80.B == 0x02 && readByte(0x8FFF) == 0x03) {
    writeLog("ADC A, (IX+d)\t...OK\n");
  }
  else {
    writeLog("ADC A, (IX+d)\t...ERROR\n");
    dumpRegisters();
  }
  return 1;
}


int SBC_test() {
  if(!initZ80(rom_SBC)) {
    writeLog("ERROR debugging SBC group!\n");
    return 0;
  }
  /* SBC A, B is performed. A = 02h, B = 7Dh.
     Final A must be 84h (02 - 7D - carry).
     S is set, Z is reset, HF is set, P/V is reset,
     N and C are set.
  */
  emulateZ80(3);
  z80.F |= ND_FLAGS;
  if(z80.F == 0xBB && z80.A == 0x84 && z80.B == 0x7D)
    writeLog("SBC A, r\t...OK\n");
  else {
    writeLog("SBC A, r\t...ERROR\n");
    dumpRegisters();
  }
  /* SBC A, n is performed. A = A8h, n = F0h.
     Final A must be B7h (A8 - F0 - carry (1)).
     S is set, Z is reset, HF is reset, P/V is reset,
     N and C are set.
  */
  emulateZ80(2);
  if(z80.F == 0xAB && z80.A == 0xB7 && z80.B == 0x7D)
    writeLog("SBC A, n\t...OK\n");
  else {
    writeLog("SBC A, n\t...ERROR\n");
    dumpRegisters();
  }
  /* SBC A, (HL) is performed. A = AAh, (HL) = 30h.
     Final A must be 79h (AA - 30 - carry (1)).
     S is reset, Z is reset, HF is reset, P/V is set,
     N is set and C is reset.
  */
  emulateZ80(4);
  if(z80.F == 0x2E && z80.A == 0x79 && z80.B == 0x7D && readByte(0x8123) == 0x30)
    writeLog("SBC A, (HL)\t...OK\n");
  else {
    writeLog("SBC A, (HL)\t...ERROR\n");
    dumpRegisters();
  }
  /* SBC A, (IX+d) is performed. A = 79h, (IX+d) = 30h.
     Final A must be 49h (79 - 30 - carry (0)).
     S is reset, Z is reset, HF is reset, P/V is reset,
     N is set and C is set.
  */
  emulateZ80(3);
  if(z80.F == 0x2A && z80.A == 0x49 && z80.B == 0x7D && readByte(0x8F88) == 0x30)
    writeLog("SBC A, (IX+d)\t...OK\n");
  else {
    writeLog("SBC A, (IX+d)\t...ERROR\n");
    dumpRegisters();
  }
  return 1;
}


int NEG_test() {
  if(!initZ80(rom_NEG)) {
    writeLog("ERROR debugging NEG!\n");
    return 0;
  }
  /* result of the operation: -05d 0xFB in register A
     S is set, Z is reset, H is set, N is set
     P/V is not set, C is set
  */
  emulateZ80(100);
  u8 flags = z80.F | ND_FLAGS;
  if(flags == 0xBB && z80.A == 0xFB)
    writeLog("NEG\t\t...OK\n");
  else {
    writeLog("NEG\t...ERROR\n");
    dumpRegisters();
  }
  return 1;
}

int JRe_test() {
  if(!initZ80(rom_JRe)) {
    writeLog("ERROR debugging JRe!\n");
    return 0;
  }
  /* Condition bits are not affected.
     The PC must be 0x0005 after the first instruction
     and 0x0000 at the end of the second
  */
  emulateZ80(1);
  if(z80.PC == 0x0005) {
    emulateZ80(1);
    if(z80.PC == 0x0000) {
      writeLog("JRe\t\t...OK\n");
    }
    else {
      writeLog("JRe\t...ERROR\n");
      dumpRegisters();
    }
  }
  else {
    writeLog("JRe\t...ERROR\n");
    dumpRegisters();
  }
  return 1;
}


int JRCe_test() {
  if(!initZ80(rom_JRCe)) {
    writeLog("ERROR debugging JRCe!\n");
    return 0;
  }
  /* Condition bits are not affected.
     The PC must be 0x0005 after the first instruction
     and 0x0000 at the end of the second
  */
  emulateZ80(1);
  if(z80.PC == 0x0005) {
    emulateZ80(1);
    if(z80.PC == 0x0000) {
      writeLog("JRCe\t\t...OK\n");
    }
    else {
      writeLog("JRCe\t...ERROR\n");
      dumpRegisters();
    }
  }
  else {
    writeLog("JRCe\t...ERROR\n");
    dumpRegisters();
  }
  return 1;
}


int JRNCe_test() {
  if(!initZ80(rom_JRNCe)) {
    writeLog("ERROR debugging JRNCe!\n");
    return 0;
  }
  /* Condition bits are not affected.
     The carry flag must be 0 after CCF and
     the PC must be 0x0000 after the fifth instruction
  */
  emulateZ80(4);
  if(!(z80.F & FLAG_CARRY)) {
    emulateZ80(1);
    if(z80.PC == 0x0000) {
      writeLog("JRNCe\t\t...OK\n");
    }
    else {
      writeLog("JRNCe\t...ERROR\n");
      dumpRegisters();
    }
  }
  else {
    writeLog("JRNCe\t...ERROR\n");
    dumpRegisters();
  }
  return 1;
}


int JRZe_test() {
  if(!initZ80(rom_JRZe)) {
    writeLog("ERROR debugging JRZe!\n");
    return 0;
  }
  /* Condition bits are not affected.
     The PC must be 0x0005 after the first instruction
     and 0x0000 at the end of the second
  */
  emulateZ80(1);
  if(z80.PC == 0x0005) {
    emulateZ80(1);
    if(z80.PC == 0x0000) {
      writeLog("JRZe\t\t...OK\n");
    }
    else {
      writeLog("JRZe\t...ERROR\n");
      dumpRegisters();
    }
  }
  else {
    writeLog("JRZe\t...ERROR\n");
    dumpRegisters();
  }
  return 1;
}
