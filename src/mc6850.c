#include <ncurses.h>
#include <string.h>

#include "mc6850.h"
#include "logger.h"


// TODO: find a way to have more than one ACIA per project.
static mc6850_t mc6850;


// Initializes the MC6850 ACIA.
// Returns 0 if operation is successful.
int32_t mc6850_init(void) {
    memset(&mc6850, 0, sizeof(mc6850_t));
    mc6850.status |= TX_EMPTY;
    LOG_INFO("MC6850 ACIA initialized.\n");

    return 0;
}


// Returns the status register.
uint8_t mc6850_getStatus(void) {
    return mc6850.status;
}


// Sets the status register.
void mc6850_setStatus(uint8_t data) {
    mc6850.status = data;
    return;
}


// Returns the TDR register.
uint8_t mc6850_getTDR(void) {
    return mc6850.TDR;
}


// Sets the RDR register.
void mc6850_setRDR(uint8_t data) {
    mc6850.RDR = data;
    return;
}


// Sends data to the cpu.
uint8_t mc6850_cpuOut(uint8_t port) {
    switch(port) {
        case 0x80: // Z80 wants to read status register
            return mc6850.status;
        case 0x81: // Z80 wants to read received data.
            mc6850.status &= ~(RX_FULL);
            return mc6850.RDR;
        default:
            LOG_WARNING("MC6850 ACIA: invalid input port (%d).\n", port);
    }

    return 0;
}


// Receives data from the cpu.
void mc6850_cpuIn(uint8_t port, uint8_t data) {
    switch(port) {
        case 0x80: // Z80 wants to write status register (not used).
            break;
        case 0x81: // Places in TDR data to be transmitted
            mc6850.TDR = data;
            mc6850.status &= ~(TX_EMPTY); // Clears TX empty bit.
            break;
        default:
            LOG_WARNING("MC6850 ACIA: invalid output port.\n");
    }
    return;
}


// Logs the MC6850 ACIA current status.
void mc6850_dumpStatus(void) {
    LOG_DEBUG("MC6850 RDR: 0x%02X, TDR: 0x%02X, status: 0x%02X\n",
        mc6850.RDR, mc6850.TDR, mc6850.status);
    return;
}


// Detects if any key is pressed.
int32_t kbhit(void) {
    int32_t ch = getch(); // Read the character.

    // If key is pressed...
    if (ch != ERR) {
        ungetch(ch); // Restore the character in the buffer.
        return 1;
    } else {
        return 0;
    }
}
