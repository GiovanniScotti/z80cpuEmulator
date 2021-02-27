#include <ncurses.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <signal.h>
#include <getopt.h>

#include "logger.h"
#include "board.h"

///////////////////////////////////////////////////////////
// Z80 CPU Emulator VERSION.
#define VERSION_STR "0.1"
///////////////////////////////////////////////////////////

#define ROM_PATH "./rom/ROM_32K.HEX"


static bool is_terminal = false;
static board_t z80_sys;


// Exit handler in case SIGINT is received.
static void exitHandler(int sigNumber) {
    logger_close();
    board_destroy(&z80_sys);
    if (is_terminal)
        endwin();
    exit(1);
}


// Prints usage information for this program and exits.
static void print_usage(FILE *stream, const char *this_program, int32_t exit_code) {
    fprintf(stream, "Usage: %s [OPTIONS...]\n", this_program);
    fprintf(stream, " -h --help        Display this help information.\n"
                    " -l --logfile     Output file path for logging messages.\n"
                    " -d --verb-level  Debug verbosity level.\n"
                    " -t --terminal    Enables serial terminal.\n"
                    " -v --version     Print current version.\n");
    exit(exit_code);
}


// Prints program version, license and exits.
static void print_version(FILE *stream, int32_t exit_code) {
    fprintf(stream, "Z80 CPU Emulator VER. %s\n", VERSION_STR);
    fprintf(stream, "License GPLv3+: GNU GPL version 3 or later "
                    "<http://gnu.org/licenses/gpl.html>.\n");
    fprintf(stream, "This is free software: you are free to change and "
                    "redistribute it.\n");
    fprintf(stream, "There is NO WARRANTY, to the extent permitted by law.\n");
    exit(exit_code);
}


///////////////////////////////////////////////////////////
// MAIN
///////////////////////////////////////////////////////////
int main(int argc, char **argv) {
    // Parses command line options.
    const char *this_program = argv[0];
    int32_t next_option;
    const char * const short_options = "hl:d:tv";
    const struct option long_options[] = {
        {"help",       0, NULL, 'h'},
        {"logfile",    1, NULL, 'l'},
        {"verb-level", 1, NULL, 'd'},
        {"terminal",   0, NULL, 't'},
        {"version",    0, NULL, 'v'},
        { NULL,        0, NULL,  0 }
    };

    // Default values for the program options.
    const char *logfile = NULL;
    int32_t debug_level = LOGGER_ERROR_LEVEL;

    do {
        next_option = getopt_long(argc, argv, short_options, long_options, NULL);
        switch (next_option) {
            case 'h': // Help.
                print_usage(stdout, this_program, 0);

            case 'l': // Logging file.
                logfile = optarg;
                break;

            case 'd': // Debug verbosity level.
                debug_level = atoi(optarg);
                break;

            case 't': // Serial terminal.
                is_terminal = true;
                break;

            case 'v': // Shows version.
                print_version(stdout, 0);

            case '?': // Invalid option.
                print_usage(stderr, this_program, 1);

            case -1:  // Done with options.
                break;

            default:  // Something unexpected.
                exit(1);
        }
    } while(next_option != -1);

    // NCURSES initialization.
    if (is_terminal) {
        initscr();              // Initialize terminal
        cbreak();               // Set per-character buffer
        noecho();               // Do not echo characters
        nodelay(stdscr, TRUE);  // No delay for getch function
        scrollok(stdscr, TRUE); // Set auto scrolling
    }

    // Initializes the logger and the verbosity level.
    logger_set_verbosity(debug_level);
    logger_open(logfile, is_terminal);

    // The user can use CTRL+C at any time to abort emulator execution.
    // The exitHandler takes care of gracefully close the program.
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = &exitHandler;
    sigaction(SIGINT, &sa, NULL);

    // Board initialization.
    if (board_init(&z80_sys, ROM_PATH)) {
        LOG_FATAL("Cannot initialize the system.\n");
        raise(SIGINT);
    }

    // System emulation.
    board_emulate(&z80_sys, -1, is_terminal);

    // Board destruction.
    board_destroy(&z80_sys);

    logger_close();
    if (is_terminal)
        endwin();

    return 0;
}
