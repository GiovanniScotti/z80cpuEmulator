#include "mc6850.h"


// Detect pressed key
int kbhit() {
  int ch = getch(); // Read the character

  // If key pressed...
  if(ch != ERR) {
    ungetch(ch); // Restore the character in the buffer
    return 1;
  }
  else {
    return 0;
  }
}


void init6850() {
  memset(&m6850, 0x00, sizeof(m6850));
  m6850.status |= TX_EMPTY;
  writeLog("[INFO] M6850 initialized.\n");
}


u8 mc6850_toZ80(int port) {
  switch(port) {
    case 0x80: // Z80 wants to read status register
      return m6850.status;
      break;
    case 0x81:
      // Z80 wants to read received data
      m6850.status &= ~(RX_FULL);
      return m6850.RDR;
      break;
    default:
      writeLog("[WARNING] ACIA: invalid input port.\n");
  }

  return 0x00;
}


void mc6850_fromZ80(int port, u8 value) {
  switch(port) {
    case 0x80: // Z80 wants to write status register (not used)
      break;
    case 0x81:
      // Place in TDR data to be transmitted
      m6850.TDR = value;
      m6850.status &= ~(TX_EMPTY); // Clear TX empty bit
      break;
    default:
      writeLog("[WARNING] ACIA: invalid output port.\n");
  }
}


void m6850Status() {
  fprintf(fpLog, "[INFO] ACIA->RDR: %02X\n", m6850.RDR);
  fprintf(fpLog, "[INFO] ACIA->TDR: %02X\n", m6850.TDR);
  fprintf(fpLog, "[INFO] ACIA->status: %02X\n\n", m6850.status);
}
