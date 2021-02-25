#ifndef _LOGGER_H_
#define _LOGGER_H_

#include <stdint.h>
#include <stdbool.h>

// Verbosity levels. Messages with a verbosity less or equal to the current
// level are actually printed.
#define LOGGER_FATAL_LEVEL   1
#define LOGGER_ERROR_LEVEL   2
#define LOGGER_WARNING_LEVEL 3
#define LOGGER_INFO_LEVEL    4
#define LOGGER_DEBUG_LEVEL   10

#define LOG_FATAL(args...) \
    logger_write(LOGGER_FATAL_LEVEL, "[FATAL] " args)

#define LOG_ERROR(args...) \
    logger_write(LOGGER_ERROR_LEVEL, "[ERROR] " args)

#define LOG_WARNING(args...) \
    logger_write(LOGGER_WARNING_LEVEL, "[WARNING] " args)

#define LOG_INFO(args...) \
    logger_write(LOGGER_INFO_LEVEL, "[INFO] " args)

// Debug messages can be fully customized. No prefix is given.
#define LOG_DEBUG(args...) \
    logger_write(LOGGER_DEBUG_LEVEL, "[DEBUG] " args)


void logger_open(const char *logfile, bool is_terminal);
void logger_close(void);
void logger_write(const int32_t level, const char *format, ...);
void logger_set_verbosity(int32_t level);

#endif // _LOGGER_H_
