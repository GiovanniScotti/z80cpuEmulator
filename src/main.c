#include <ncurses.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <getopt.h>

#include "logger.h"
#include "hex2array.h"
#include "cpu.h"
#include "mc6850.h"

///////////////////////////////////////////////////////////
// Z80 CPU Emulator VERSION.
#define VERSION_STR "0.1"
///////////////////////////////////////////////////////////

#define ROM_PATH "./rom/ROM_32K.HEX"


// Exit handler in case SIGINT is received.
static void exitHandler(int sigNumber) {
    // TODO: print memory content? Dump registers?
    logger_close();
    endwin();         // Gracefully close ncurses windows
    exit(1);
}


// Prints usage information for this program and exits.
static void print_usage(FILE *stream, const char *this_program, int32_t exit_code) {
    fprintf(stream, "Usage: %s [OPTIONS...]\n", this_program);
    fprintf(stream, " -h --help        Display this help information.\n"
                    " -l --logfile     Output file path for logging messages.\n"
                    " -d --verb-level  Debug verbosity level.\n"
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
    const char * const short_options = "hl:d:v";
    const struct option long_options[] = {
        {"help",       0, NULL, 'h'},
        {"logfile",    1, NULL, 'l'},
        {"verb-level", 1, NULL, 'd'},
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


    initscr();              // Initialize terminal
    cbreak();               // Set per-character buffer
    noecho();               // Do not echo characters
    nodelay(stdscr, TRUE);  // No delay for getch function
    scrollok(stdscr, TRUE); // Set auto scrolling


    // Initializes the logger and the verbosity level.
    logger_set_verbosity(debug_level);
    logger_open(logfile);

    // The user can use CTRL+C at any time to abort emulator execution.
    // The exitHandler takes care of gracefully close the program.
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = &exitHandler;
    sigaction(SIGINT, &sa, NULL);


    // TODO: call self_test() to perform self checking operations.


    ///////////////////////////////////////////////////////
    // MEMORY CONFIGURATION
    uint8_t *rom_buff = (uint8_t *)calloc(0x8000, sizeof(uint8_t));
    uint8_t *ram_buff = (uint8_t *)calloc(0x8000, sizeof(uint8_t));

    if (rom_buff != NULL && ram_buff != NULL) {
        // Loads the hex file into rom memory.
        if (hex2array(ROM_PATH, rom_buff, 0x8000)) {
            LOG_ERROR("Unable to load the hex file (%s).\n", ROM_PATH);
        }
    } else {
        LOG_FATAL("Cannot initialize memory structures.");
        exit(1);
    }


    // TODO: how can we deallocate memory? raise() instead of exit and
    // cpu_destroy in exit handler?

    // Memory configuration:
    // 0x0000 - 0x7FFF -> ROM
    // 0x8000 - 0xFFFF -> RAM
    mem_chunk_t ram = {"RAM", CHUNK_READWRITE, 0x8000, 0x8000, ram_buff, NULL};
    mem_chunk_t rom = {"ROM", CHUNK_READONLY, 0, 0x8000, rom_buff, &ram};
    cpu_t z80;

    // CPU initialization.
    if (cpu_init(&z80, &rom)) {
        LOG_FATAL("Cannot initialize the cpu.\n");
        cpu_destroy(&z80);
        exit(1);
    }

    // Hooks up the ACIA.
    mc6850_init();
    z80.portIO_in = mc6850_cpuOut;
    z80.portIO_out = mc6850_cpuIn;

    // Emulation.
    cpu_emulate(&z80, -1);

    // Termination.
    cpu_destroy(&z80);
    logger_close();
    endwin();

    return 0;
}
