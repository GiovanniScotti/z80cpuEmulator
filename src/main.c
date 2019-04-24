#include <ncurses.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include "debug.h"
#include "hexToArray.h"
#include "logger.h"
#include "mc6850.h"
#include "memory.h"
#include "Z80.h"
#include "roms.h"

//#define DEBUG_OP
#define ROM_PATH "./rom/ROM_32K.HEX"
#define LOG_PATH "./log.txt"


/*
* Default memory configuration:
* 32KB ROM + 32KB RAM
*
* The z80 cpu can directly address only up to 64KB of memory.
*/

// Memory initialization
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

int debugging = 0;
extern int logInstr;


// Exit handler in case of SIGINT
void exitHandler(int sigNumber) {
  printMemory(z80.ram, 0xFFFF);
  dumpRegisters();
  closeLog();       // Close the log file
  endwin();         // Gracefully close ncurses windows
  exit(-1);
}


// *************************************************
// *****              ENTRY POINT              *****
// *************************************************
int main(int argc, char *argv[]) {
  struct sigaction sa;
  memset(&sa, 0, sizeof(sa));
  sa.sa_handler = &exitHandler;

  initscr();              // Initialize terminal
  cbreak();               // Set per-character buffer
  noecho();               // Do not echo characters
  nodelay(stdscr, TRUE);  // No delay for getch function
  scrollok(stdscr, TRUE); // Set auto scrolling

  initLog(LOG_PATH);      // Initialize the logger

  sigaction(SIGINT, &sa, NULL);

  // DEBUG TEST SECTION
  #ifdef DEBUG_OP
    debugging = 1;
    // Don't log executed instructions during self testing
    logInstr = 0;
    selfTest();
    logInstr = 1;
    debugging = 0;
  #endif

  // Load HEX file into rom array
  if(!loadHEX(ROM_PATH)) {
    die("[ERROR] Unable to load the HEX file! Check out the log file.\n");
  }

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

  while(exitChar != 'y' && exitChar != 'Y') {
    if(kbhit())
      exitChar = getch();
  }

  closeLog();
  endwin();

  return 0;
}
