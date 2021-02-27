#include <string.h>

#include "mc6850.h"
#include "logger.h"


// Initializes the given MC6850 ACIA.
// Returns 0 if operation is successful.
int32_t mc6850_init(mc6850_t *mc6850) {
    memset(mc6850, 0, sizeof(mc6850_t));
    mc6850->status |= TX_EMPTY;
    LOG_INFO("MC6850 ACIA initialized.\n");

    return 0;
}


// Returns the status register.
uint8_t mc6850_getStatus(mc6850_t *mc6850) {
    return mc6850->status;
}


// Sets the status register.
void mc6850_setStatus(mc6850_t *mc6850, uint8_t data) {
    mc6850->status = data;
    return;
}


// Returns the TDR register.
uint8_t mc6850_getTDR(mc6850_t *mc6850) {
    return mc6850->TDR;
}


// Sets the RDR register.
void mc6850_setRDR(mc6850_t *mc6850, uint8_t data) {
    mc6850->RDR = data;
    return;
}


// Logs the MC6850 ACIA current status.
void mc6850_dumpStatus(mc6850_t *mc6850) {
    LOG_DEBUG("MC6850 RDR: 0x%02hhX, TDR: 0x%02hhX, status: 0x%02hhX\n",
        mc6850->RDR, mc6850->TDR, mc6850->status);
    return;
}
