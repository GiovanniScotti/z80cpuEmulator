#include <ncurses.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <signal.h>

#include "cpu.h"
#include "opcodes.h"
#include "logger.h"
#include "mc6850.h"


// Initializes the CPU data structures.
// Returns 0 if no errors occur.
int32_t cpu_init(cpu_t *cpu, mem_chunk_t *mem_list) {

    if (mem_list == NULL) {
        LOG_ERROR("Cannot init the cpu. No memory chunks provided.\n");
        return 1;
    }

    bool is_romDefined = false;
    bool is_ramDefined = false;

    // Memory validation.
    for (mem_chunk_t *mc = mem_list; mc != NULL; mc = mc->next) {
        // Chunk type check.
        switch (mc->type) {
            case CHUNK_READONLY:
                is_romDefined = true;
                LOG_INFO("Detected ROM at 0x%04X of size 0x%04X.\n",
                    mc->start, mc->size);
                break;

            case CHUNK_READWRITE:
                is_ramDefined = true;
                LOG_INFO("Detected RAM at 0x%04X of size 0x%04X.\n",
                    mc->start, mc->size);
                break;

            case CHUNK_UNUSED:
                LOG_INFO("Detected UNUSED at 0x%04X of size 0x%04X.\n",
                    mc->start, mc->size);
                break;

            default:
                LOG_ERROR("Detected invalid type for chunk %s.\n", mc->label);
                return 1;
        }

        for (mem_chunk_t *mc_tmp = mc; mc_tmp != NULL; mc_tmp = mc_tmp->next) {
            if (mc == mc_tmp) {
                continue;
            }

            // Same label.
            if (strcmp(mc->label, mc_tmp->label) == 0) {
                LOG_ERROR("Detected memory chunks with the same label.\n");
                return 1;
            }

            // Overlapping addresses.
            bool do_overlap = mc->start < (mc_tmp->start + mc_tmp->size) &&
                mc_tmp->start < (mc->start + mc->size);

            if (do_overlap) {
                LOG_ERROR("Overlapping chunks: %s (start 0x%04X, size 0x%04X) "
                    "and %s (start 0x%04X, size 0x%04X).\n", mc->label, mc->start,
                    mc->size, mc_tmp->label, mc_tmp->start, mc_tmp->size);
                return 1;
            }
        }
    }

    if (!is_romDefined) {
        LOG_ERROR("No ROM chunk defined.\n");
        return 1;
    }

    if (!is_ramDefined)
        LOG_WARNING("No RAM chunk defined.\n");

    // Memory chunks registration.
    cpu->memory = mem_list;
    cpu_reset(cpu);
    return 0;
}


// Cleans up dynamically allocated memory.
// Returns 0 in case of success.
int32_t cpu_destroy(cpu_t *cpu) {
    for (mem_chunk_t *mc = cpu->memory; mc != NULL; mc = mc->next) {
        if (mc != NULL) {
            LOG_INFO("Deallocation of chunk %s.\n", mc->label);
            free(mc->buff);
        }
    }
    return 0;
}


// Resets the processor.
void cpu_reset(cpu_t *cpu) {
    LOG_INFO("Reset the processor.\n");

    cpu->cycles = 0;
    cpu->instr = 0;
    cpu->halt = 0;
    cpu->I = 0;
    cpu->R = 0;
    cpu->PC = 0;
    cpu->IFF1 = 0;
    cpu->IFF2 = 0;
    cpu->IM = INT_MODE_0;
    cpu->is_pendingMI = 0;
    cpu->is_pendingNMI = 0;

    return;
}


// Reads one byte at the given memory location. The CPU has 64KB of
// addressable memory.
uint8_t cpu_read(cpu_t *cpu, const uint16_t addr) {
    for (mem_chunk_t *mc = cpu->memory; mc != NULL; mc = mc->next) {
        if (WHITIN(addr, mc->start, mc->start + mc->size - 1) &&
            mc->type != CHUNK_UNUSED) {

            return mc->buff[addr - mc->start];
        }
    }

    LOG_FATAL("Memory read error at address 0x%04X.\n", addr);
    raise(SIGINT);
    return 0;
}


// Writes one byte at the given memory location. The CPU has 64KB of
// addressable memory.
void cpu_write(cpu_t *cpu, const uint8_t data, const uint16_t addr) {
    for (mem_chunk_t *mc = cpu->memory; mc != NULL; mc = mc->next) {
        if (WHITIN(addr, mc->start, mc->start + mc->size - 1)) {
            if (mc->type == CHUNK_READONLY) {
                LOG_FATAL("Cannot write to read-only memory at address 0x%04X.\n",
                    addr);
                raise(SIGINT);
            }

            if (mc->type == CHUNK_READWRITE) {
                mc->buff[addr - mc->start] = data;
                return;
            }
        }
    }

    LOG_FATAL("Memory write error at address 0x%04X.\n", addr);
    raise(SIGINT);
}


// Pushes the given data on the stack.
void cpu_stackPush(cpu_t *cpu, uint16_t data) {
    cpu_write(cpu, (data >> 8) & 0xFF, --cpu->SP);  // (SP-1) <- valueH
    cpu_write(cpu, data & 0xFF, --cpu->SP);         // (SP-2) <- valueL
    return;
}


// Pops data from the stack.
uint16_t cpu_stackPop(cpu_t *cpu) {
    uint16_t data = cpu_read(cpu, cpu->SP++);
    data |= (cpu_read(cpu, cpu->SP++) << 8);
    return data;
}


// Executes non-maskable interrupts. Restarts from 0x66;
static void cpu_doNonMaskableINT(cpu_t *cpu) {
    cpu->IFF1 = 0;
    cpu->is_pendingNMI = 0;
    cpu->halt = 0;
    cpu_stackPush(cpu, cpu->PC);
    cpu->PC = 0x0066;
    LOG_DEBUG("Caught non-maskable interrupt.\n");
    return;
}


// Executes maskable interrupts.
static void cpu_doMaskableINT(cpu_t *cpu) {
    // If the previous instruction is EI (0xFB), does not execute INT.
    if (cpu_read(cpu, cpu->PC - 1) != 0xFB) {
        // Checks IFF1 status.
        if (cpu->IFF1) { // If the interrupt is not masked.
            // Accepts the interrupt - IFF1 and IFF2 to 0.
            cpu->IFF1 = 0;
            cpu->IFF2 = 0;
            cpu->is_pendingMI = 0;
            cpu->halt = 0;
            // Interrupt mode 1.
            if (cpu->IM == INT_MODE_0) {
                cpu_stackPush(cpu, cpu->PC);
                /* TODO: instruction execution. */
                cpu->cycles += 2;
                LOG_FATAL("Interrupt mode 0 not supported yet.\n");
                raise(SIGINT);
            } else if (cpu->IM == INT_MODE_1) {
                cpu_stackPush(cpu, cpu->PC);
                cpu->PC = 0x0038;
                // Two additional cycles required for restarting.
                // RST p requires 11 cycles.
                cpu->cycles += 13;
                LOG_DEBUG("Caught mode 1 interrupt.\n");
            } else if (cpu->IM == INT_MODE_2) {
                cpu_stackPush(cpu, cpu->PC);
                uint16_t rst_addr = ((cpu->I << 8) | (cpu->int_data & 0xFE));
                uint8_t int_addrL = cpu_read(cpu, rst_addr);
                uint8_t int_addrH = cpu_read(cpu, rst_addr + 1);
                cpu->PC = (int_addrL | (int_addrH << 8));
                cpu->cycles += 19;
                LOG_DEBUG("Caught mode 2 interrupt.\n");
            } else {
                LOG_FATAL("Unknown interrupt mode.\n");
                raise(SIGINT);
            }
        }
    }
    return;
}


// Starts cpu emulation. Executes at most 'instr_limit' instructions or
// never stops if -1 is given.
void cpu_emulate(cpu_t *cpu, int32_t instr_limit, bool is_terminal) {
    LOG_INFO("Emulation started.\n");

    int32_t instr_count = 0;
    bool do_inf_loop = (instr_limit < 0);

    while (do_inf_loop || (instr_count < instr_limit)) {
        uint8_t opcode = 0; // NOP, default for HALT;

        if (!cpu->halt) {
            // Fetches instruction and increases the PC.
            opcode = opc_fetch8(cpu);
        }

        // Executes instruction.
        opc_tbl[opcode].execute(cpu, opcode);
        cpu->cycles += opc_tbl[opcode].TStates;
        cpu->instr++;
        instr_count++;

        // TODO: should we increment register R?

        // Detects interrupts at the end of instruction's execution.
        // NMIs have priority over MI.
        if (cpu->is_pendingNMI)
            cpu_doNonMaskableINT(cpu);
        else if (cpu->is_pendingMI)
            cpu_doMaskableINT(cpu);


        // TODO: peripheral management. Do we need callbacks of any kind?

        // After the execution of the current instruction, checks for
        // key pressed with kbhit(). If one key was pressed, then puts it
        // into RDR, set RX_FULL and is_pendingInterrupt.

        if (kbhit() && !(mc6850_getStatus() & RX_FULL)) {
            char ch = getch();
            if (ch == 0x0A) {
                mc6850_setRDR(0x0D); // Carriage return.
            } else
                mc6850_setRDR(ch);

            mc6850_setStatus(mc6850_getStatus() | RX_FULL);
            // FIXME: it's up to the peripheral overwriting a pending interrupt.
            cpu->is_pendingMI = 1;
        }

        // After checking incoming characters, checks the ACIA status register
        // and determines if a byte is ready to be transmitted.

        if (!(mc6850_getStatus() & TX_EMPTY)) {
            char charToPrint = mc6850_getTDR();
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

            mc6850_setStatus(mc6850_getStatus() | TX_EMPTY);
        }
    }

    return;
}


// Prints the content of the given memory chunk.
void cpu_printChunk(mem_chunk_t *chunk) {
    LOG_DEBUG("Memory chunk: %s\n", chunk->label);
    LOG_DEBUG("Addr.\t0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F\n");

    for (int32_t byte = 0; byte < chunk->size; byte++) {
        int32_t col = byte >> 4;

        // Right-hand side of the table.
        if (byte % 16 == 0) {
            LOG_DEBUG("\n");
            LOG_DEBUG("%03X0\t", col);
        }

        LOG_DEBUG("%02X ", *((chunk->buff) + byte));
    }
    LOG_DEBUG("\n\n");
    return;
}


// Dumps cpu registers.
void cpu_dumpRegisters(cpu_t *cpu) {
    LOG_DEBUG("CPU registers:\n");
    LOG_DEBUG("A: %02X  F: %02X    A': %02X  F': %02X\n"
              "B: %02X  C: %02X    B': %02X  C': %02X\n"
              "D: %02X  E: %02X    D': %02X  E': %02X\n"
              "H: %02X  L: %02X    H': %02X  L': %02X\n"
              "IX: %04X IY: %04X\n"
              "SP: %04X PC: %04X\n"
              "IFF1: %02X IFF2: %02X IM: %02X I: %02X\n"
              "SF: %01X ZF: %01X HF: %01X PF: %01X NF: %01X CF: %01X\n",
        cpu->A, cpu->F, cpu->Ar, cpu->Fr, cpu->B, cpu->C, cpu->Br, cpu->Cr,
        cpu->D, cpu->E, cpu->Dr, cpu->Er, cpu->H, cpu->L, cpu->Hr, cpu->Lr,
        cpu->IX, cpu->IY, cpu->SP, cpu->PC, cpu->IFF1, cpu->IFF2, cpu->IM,
        cpu->I, GET_FLAG_SIGN(cpu), GET_FLAG_ZERO(cpu),
        GET_FLAG_HCARRY(cpu), GET_FLAG_PARITY(cpu),
        GET_FLAG_ADDSUB(cpu), GET_FLAG_ADDSUB(cpu));

    return;
}
