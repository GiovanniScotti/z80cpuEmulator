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

typedef struct {
    uint8_t TDR;    // Transmit Data Register.
    uint8_t RDR;    // Receive Data Register.
    uint8_t status; // Status Register.
} mc6850_t;


int32_t mc6850_init(void);
uint8_t mc6850_getStatus(void);
void mc6850_setStatus(uint8_t data);
uint8_t mc6850_getTDR(void);
void mc6850_setRDR(uint8_t data);

uint8_t mc6850_cpuOut(uint8_t port);
void mc6850_cpuIn(uint8_t port, uint8_t data);

void mc6850_dumpStatus(void);
int32_t kbhit(void);

#endif // _MC6850_H_
