#ifndef HEXTOARRAY_H
#define HEXTOARRAY_H

#include "logger.h"
#include "memory.h"
#include "roms.h"


int loadHEX(char *path);
int countHexFileLines(FILE *fp);
int hexFileToArray(FILE *fp, char *rom);
u8  ascii2Hex(char c);

#endif
