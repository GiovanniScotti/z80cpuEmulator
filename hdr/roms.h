#ifndef ROM_H
#define ROM_H

#include "memory.h"


char rom[ROM_SIZE]; // Contains code from parsed HEX file

char rom_LDrIXd[ROM_SIZE];
char rom_ADDAr[ROM_SIZE];
char rom_ADC[ROM_SIZE];
char rom_SBC[ROM_SIZE];
char rom_NEG[ROM_SIZE];
char rom_JRe[ROM_SIZE];
char rom_JRCe[ROM_SIZE];
char rom_JRNCe[ROM_SIZE];
char rom_JRZe[ROM_SIZE];

#endif
