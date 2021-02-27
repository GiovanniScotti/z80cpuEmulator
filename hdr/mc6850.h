#ifndef _MC6850_H_
#define _MC6850_H_

#include <stdint.h>

#define RX_FULL   (1 << 0)
#define TX_EMPTY  (1 << 1)

/*
  IO ports to communicate with the 6850 are 0x80 and 0x81.
  In particular, the higher nibble activates the ACIA while
  the lower nibble drives the register select pin.
  0x80: R/W control/status registers
  0x81: R/W TX/RX data registers

  Transmitting interrupt is disabled. Reception interrupt is enabled.
*/

typedef struct mc6850_t {
    uint8_t TDR;    // Transmit Data Register.
    uint8_t RDR;    // Receive Data Register.
    uint8_t status; // Status Register.
} mc6850_t;


int32_t mc6850_init(mc6850_t *mc6850);
uint8_t mc6850_getStatus(mc6850_t *mc6850);
void mc6850_setStatus(mc6850_t *mc6850, uint8_t data);
uint8_t mc6850_getTDR(mc6850_t *mc6850);
void mc6850_setRDR(mc6850_t *mc6850, uint8_t data);

void mc6850_dumpStatus(mc6850_t *mc6850);

#endif // _MC6850_H_
