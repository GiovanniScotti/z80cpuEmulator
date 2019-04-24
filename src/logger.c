#include "logger.h"


// Initialize the logger
void initLog(char *path) {
  fpLog = fopen(path, "w");
}


// Write to the log file
void writeLog(char *message) {
    fprintf(fpLog, "%s", message);
}


// Close the file and stop the logger
void closeLog() {
  fclose(fpLog);
}
