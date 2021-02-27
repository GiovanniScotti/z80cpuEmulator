#include <stdio.h>
#include <stdarg.h>
#include "logger.h"

// Logger verbosity level.
static int32_t logger_verbosity = 0;
// Logger file pointer.
static FILE *fplog = NULL;
// Enables/disabled prints on stderr/stdout.
static bool logger_no_prints = false;


// Initializes the logger by opening the log file.
// This function is called in case logging to file is enabled.
void logger_open(const char *logfile, bool is_terminal) {
    if (logfile != NULL) {
        fplog = fopen(logfile, "w");
        if (fplog == NULL)
            LOG_ERROR("Cannot open the given logfile (%s).\n", logfile);
        else
            LOG_INFO("Logging file successfully created (%s).\n", logfile);
    }

    // If the serial terminal is enabled, log messages are stored in
    // the log file only. Prints are disabled.
    logger_no_prints = is_terminal;

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

    if (level <= logger_verbosity) {
        // Prints on the stderr.
        if (!logger_no_prints) {
            va_start(args, format);
            vfprintf(stderr, format, args);
        }
        // Prints on log file.
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
    return;
}