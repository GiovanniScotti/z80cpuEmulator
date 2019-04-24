#ifndef DEBUG_H
#define DEBUG_H

#include "logger.h"
#include "memory.h"
#include "opcodes.h"
#include "roms.h"
#include "Z80.h"


#define DEBUG_SIZE 9
// Identify not used flag bits. They are set to 1 during debugging
#define ND_FLAGS 0x28


typedef struct {
  int (*execute) ();
} DebugFunc;


void selfTest();

int  LDrIXd_test();
int  ADDAr_test();
int  ADC_test();
int  SBC_test();
int  NEG_test();
int  JRe_test();
int  JRCe_test();
int  JRNCe_test();
int  JRZe_test();

#endif
