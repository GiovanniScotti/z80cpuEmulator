#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include "logger.h"

// Logger verbosity level.
static int32_t logger_verbosity = 0;
// Logger file pointer.
static FILE *fplog = NULL;


// Initializes the logger by opening the log file.
// This function is called in case logging to file is enabled.
void logger_open(const char *logfile) {
    if (logfile != NULL) {
        fplog = fopen(logfile, "w");
        if (fplog == NULL)
            LOG_ERROR("Cannot open the given logfile (%s).\n", logfile);
        else
            LOG_INFO("Logging file successfully created (%s).\n", logfile);
    }
    return;
}


// Closes the file and stops the logger.
void logger_close(void) {
    if (fplog != NULL)
        if (fclose(fplog) != 0)
            LOG_ERROR("Cannot close the logfile.\n");
    return;
}


// Prints a log message. Writes to the log file, if used.
// Debug prints are output on the standard error.
void logger_write(const int32_t level, const char *format, ...) {
    va_list args;

    // Prints on the stderr.
    if (level <= logger_verbosity) {
        va_start(args, format);
        vfprintf(stderr, format, args);
        if (fplog != NULL) {
            va_start(args, format);
            vfprintf(fplog, format, args);
        }
    }

    va_end(args);
    return;
}


// Sets the logger verbosity level.
void logger_set_verbosity(int32_t level) {
    logger_verbosity = level;
    LOG_WARNING("Verbosity level set to %d.\n", logger_verbosity);
    return;
}