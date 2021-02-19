#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <getopt.h>

#include "logger.h"
#include "hex2array.h"
#include "memory.h"

///////////////////////////////////////////////////////////
// Z80 CPU Emulator VERSION.
#define VERSION_STR "0.1"
///////////////////////////////////////////////////////////

#define ROM_PATH "./rom/ROM_32K.HEX"


/*
 * Default memory configuration:
 * 32KB ROM + 32KB RAM
 *
 * The Z80 CPU can directly address only up to 64KB of memory.
 */

// Memory initialization
/*
RamBank ramBanks[RAM_BANKS_N] = {
  { 0x0000,     // Starting address
    ROM_SIZE,   // Memory size
    FLAG_ROM,   // Flag
    NULL        // Pointer
  },
  { ROM_SIZE,
    RAM_SIZE,
    FLAG_USED,
    NULL
  }
};
*/


// Exit handler in case SIGINT is received.
static void exitHandler(int sigNumber) {
    //printMemory(z80.ram, 0xFFFF);
    //dumpRegisters();
    logger_close();
    //endwin();         // Gracefully close ncurses windows
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
    fprintf(stream, "Z80 CPU EMULATOR VER. %s\n", VERSION_STR);
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


    //initscr();              // Initialize terminal
    //cbreak();               // Set per-character buffer
    //noecho();               // Do not echo characters
    //nodelay(stdscr, TRUE);  // No delay for getch function
    //scrollok(stdscr, TRUE); // Set auto scrolling


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
    // TODO: initialize the Z80 configuration.


    // Loads the hex file into memory.
    uint8_t *buffer = (uint8_t *)malloc(0x8000);
    if (hex2array(ROM_PATH, buffer, 0x8000)) {
        LOG_ERROR("Unable to load the hex file (%s).\n", ROM_PATH);
    }

    ///////////////////////////////////////////////
    // TODO: Chunk test. To be removed.
    mem_chunk_t rom = {"ROM", 0x0, 0x8000, 0, buffer};
    memory_printChunk(&rom);

    ////////////////////////////////////////////////

    // TODO: if the hex is not leaded, the cpu is expected to spinning.
    // Memory is filled with NOPs.

/*
    if(!initZ80(rom)) {
        die("[ERROR] Unable to initialize the processor.\n");
    }

    // Hook up the ACIA
    init6850();
    z80.portIn = mc6850_toZ80;
    z80.portOut = mc6850_fromZ80;

    printw("\n");
    writeLog("\n");

    // ######### START HEX PROGRAM EXECUTION #########
    logInstr = 0;
    emulateZ80(-1);  // -1 for endless execution
    logInstr = 1;

    // ***** DUMP MEMORY AND REGISTERS *****
    printMemory(z80.ram, 0xFFFF);
    dumpRegisters();

    // Exit sequence after program termination
    char exitChar = 0x00;
    printw("\n\nPress Y to exit\n");
    refresh();

    while (exitChar != 'y' && exitChar != 'Y') {
        if (kbhit())
        exitChar = getch();
    }
*/


    logger_close();
    //endwin();

    return 0;
}
