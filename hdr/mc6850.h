#ifndef MC6850_H
#define MC6850_H

#include <ncurses.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include "logger.h"
#include "memory.h"
#include "Z80.h"

#define RX_FULL   (1 << 0)
#define TX_EMPTY  (1 << 1)

/*
  IO ports to communicate with the 6850 are 0x80 and 0x81.
  In particular, the higher nibble activates the ACIA while
  the lower nibble drives the register select pin.
  0x80: R/W control/status registers
  0x81: R/W TX/RX data registers

  Transmitting interrupt is disabled. Receive interrupt is enabled.
*/

struct {
  u8 TDR; // Transmit Data Register
  u8 RDR; // Receive Data Register
  // Status Registers
  u8 status;
} m6850;


int  kbhit();
void init6850();
u8   mc6850_toZ80(int port);
void mc6850_fromZ80(int port, u8 value);
void m6850Status();

#endif
