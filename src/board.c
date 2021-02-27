#include <ncurses.h>
#include <stdlib.h>

#include "board.h"
#include "logger.h"
#include "hex2array.h"

#define ROM_START 0x0
#define RAM_START 0x8000

#define ROM_SIZE 0x8000 // 32KB.
#define RAM_SIZE 0x8000 // 32KB.


// Sends data from peripherals to the cpu.
static uint8_t board_cpuIOin(board_t *board, uint8_t port) {
    switch(port) {
        case 0x80: // CPU wants to read the acia status register.
            return board->acia->status;
        case 0x81: // CPU wants to read received data by the acia.
            board->acia->status &= ~(RX_FULL);
            return board->acia->RDR;
        default:
            LOG_WARNING("CPU IO IN: invalid port (%d).\n", port);
    }

    return 0;
}


// Receives data from the cpu and dispatches it to the proper peripheral.
static void board_cpuIOout(board_t *board, uint8_t port, uint8_t data) {
    switch(port) {
        case 0x80: // CPU wants to write the acia status register (not used).
            break;
        case 0x81: // CPU places in acia TDR data to be transmitted.
            board->acia->TDR = data;
            board->acia->status &= ~(TX_EMPTY); // Clears TX empty bit.
            break;
        default:
            LOG_WARNING("CPU IO OUT: invalid port.\n");
    }
    return;
}


// Detects if any key is pressed.
static int32_t kbhit(void) {
    int32_t ch = getch(); // Read the character.

    // If key is pressed...
    if (ch != ERR) {
        ungetch(ch); // Restore the character in the buffer.
        return 1;
    } else {
        return 0;
    }
}


// Initializes the given board. A board is a minimal Z80-based
// system made of the cpu itself, an uart, 32KB of ROM and 32KB of RAM,
// respectively mapped at 0x0 and at 0x8000 locations.
// Returns 0 if initialization is successful.
int32_t board_init(board_t *board, char *rom_file) {

    board->cpu = (cpu_t *)malloc(sizeof(cpu_t));
    board->acia = (mc6850_t *)malloc(sizeof(mc6850_t));

    ///////////////////////////////////////////////////////
    // MEMORY CONFIGURATION
    uint8_t *rom_buff = (uint8_t *)calloc(ROM_SIZE, sizeof(uint8_t));
    uint8_t *ram_buff = (uint8_t *)calloc(RAM_SIZE, sizeof(uint8_t));

    if (rom_buff != NULL && ram_buff != NULL) {
        // Loads the hex file into rom memory.
        if (hex2array(rom_file, rom_buff, ROM_SIZE)) {
            LOG_FATAL("Unable to load the hex file (%s).\n", rom_file);
            return 1;
        }
    } else {
        LOG_FATAL("Cannot allocate memory.\n");
        return 1;
    }

    // Creates ROM and RAM chunks.
    mem_chunk_t *ram = (mem_chunk_t *)malloc(sizeof(mem_chunk_t));
    mem_chunk_t *rom = (mem_chunk_t *)malloc(sizeof(mem_chunk_t));

    if (rom == NULL || ram == NULL) {
        LOG_FATAL("Cannot create memory chunks.\n");
        return 1;
    }

    *ram = (mem_chunk_t){"RAM", CHUNK_READWRITE, RAM_START, RAM_SIZE, ram_buff, NULL};
    *rom = (mem_chunk_t){"ROM", CHUNK_READONLY, ROM_START, ROM_SIZE, rom_buff, ram};

    ///////////////////////////////////////////////////////
    // CPU INITIALIZATION
    if (cpu_init(board->cpu, rom, board)) {
        LOG_FATAL("Cannot initialize the cpu.\n");
        return 1;
    }

    ///////////////////////////////////////////////////////
    // PERIPHERALS INITIALIZATION
    mc6850_init(board->acia);
    cpu_setIOcallbacks(board->cpu, board_cpuIOin, board_cpuIOout);

    LOG_INFO("Board initialized.\n");
    return 0;
}


// Starts emulation.
void board_emulate(board_t *board, int32_t instr_limit, bool is_terminal) {
    LOG_INFO("Emulation started.\n");
    bool inf_loop = (instr_limit < 0);

    while (inf_loop || instr_limit > 0) {
        // CPU MANAGEMENT
        // Executes one instruction.
        cpu_emulate(board->cpu);
        instr_limit--;

        // ACIA MANAGEMENT

        // After the execution of the current instruction, checks for
        // key pressed with kbhit(). If one key was pressed, then puts it
        // into RDR, set RX_FULL and is_pendingInterrupt.

        if (kbhit() && !(mc6850_getStatus(board->acia) & RX_FULL)) {
            char ch = getch();
            if (ch == 0x0A) {
                mc6850_setRDR(board->acia, 0x0D); // Carriage return.
            } else
                mc6850_setRDR(board->acia, ch);

            mc6850_setStatus(board->acia, mc6850_getStatus(board->acia) | RX_FULL);

            // Signals pending interrupt.
            board->cpu->is_pendingMI = 1;
        }

        // After checking incoming characters, checks the ACIA status register
        // and determines if a byte is ready to be transmitted.

        if (!(mc6850_getStatus(board->acia) & TX_EMPTY)) {
            char charToPrint = mc6850_getTDR(board->acia);
            if (is_terminal) {
                if (charToPrint == 0x0D) { // Carriage return.
                    printw("\n");
                } else if (charToPrint == 0x0C) { // New page - Form Feed.
                    clear();
                } else if (charToPrint == 0x0A) {
                    // Do nothing
                } else {
                    printw("%c", charToPrint);
                }
                refresh();
            }

            mc6850_setStatus(board->acia,
                mc6850_getStatus(board->acia) | TX_EMPTY);
        }
    }
    return;
}


// Destroys board deallocating memory.
int32_t board_destroy(board_t *board) {
    cpu_destroy(board->cpu);
    free(board->cpu);
    free(board->acia);

    LOG_INFO("Deallocated board memory.\n");
    return 0;
}