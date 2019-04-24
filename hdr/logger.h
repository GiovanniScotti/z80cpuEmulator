#ifndef LOGGER_H
#define LOGGER_H

#include <stdio.h>


FILE *fpLog;

void initLog(char *path);
void writeLog(char *message);
void closeLog();

#endif
