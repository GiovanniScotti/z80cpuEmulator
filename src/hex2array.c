#include <stdio.h>
#include "hex2array.h"
#include "logger.h"

#define HEXLINE_BYTES 37


// Counts lines including the termination line.
static int32_t countHexLines(FILE *fp) {
    int line_counter = 0;
    char c;

    fseek(fp, 0, SEEK_SET);

    while ((c = fgetc(fp)) != EOF) {
        if (c == ':')
            line_counter++;
    }

    return line_counter;
}


// Converts ascii hexadecimal number to decimal.
static uint8_t asciihex2dec(const char c) {
    if (c >= '0' && c <= '9')
        return c - '0';
    else if (c >= 'A' && c <= 'F')
        return c - 'A' + 10;
    else if (c >= 'a' && c <= 'f')
        return c - 'a' + 10;

    return 0;
}


// Fills the given buffer of the given size with data coming from the
// converted hex file. Returns 0 if success.
int32_t hex2array(const char *filepath, uint8_t *buff, size_t buffsize) {
    FILE *fp;

    if (filepath == NULL)
        return 1;

    fp = fopen(filepath, "rb"); // Reads the file in binary mode.
    if (fp == NULL) {
        LOG_ERROR("Cannot open the HEX file (%s).\n", filepath);
        return 1;
    }

    int32_t numberOfLines = countHexLines(fp);
    int32_t readLines = 0;
    int32_t buffPtr = 0;

    // After ':' there are 37 bytes of data: 1 (byte count) + 2 (address) +
    // 1 (record type) + 32 (data) + 1 (checksum).

    fseek(fp, 0, SEEK_SET); // Start of file.

    while (readLines < numberOfLines) {
        uint8_t bytesInLine[HEXLINE_BYTES];
        if (fgetc(fp) == ':') {
            readLines++;
            if (readLines >= numberOfLines)
                break;

            // Extracts bytes in the line.
            for (int32_t i = 0; i < HEXLINE_BYTES; i++) {
                bytesInLine[i] =
                    (asciihex2dec(fgetc(fp)) << 4) + asciihex2dec(fgetc(fp));
            }

            // Checks checksum.
            uint8_t checksum = 0;
            for (int32_t i = 0; i < HEXLINE_BYTES-1; i++) {
                checksum += bytesInLine[i];
            }

            uint8_t checksum2comp = ~checksum + 1;

            if (checksum2comp == bytesInLine[HEXLINE_BYTES-1]) {
                for (int32_t i = 4; i < HEXLINE_BYTES-1; i++) {
                    if (buffPtr < buffsize) {
                        // Extracts data bytes.
                        buff[buffPtr] = bytesInLine[i];
                        buffPtr++;
                    } else {
                        LOG_ERROR("Cannot allocate the hex file into memory.\n");
                        return 1;
                    }
                }
            } else {
                LOG_ERROR("Detected incorrect checksum in hex file at line %d.\n",
                    readLines);
                return 1;
            }
        }
    }

    LOG_INFO("Hex file correctly parsed. Size: 0x%X bytes.\n", buffPtr);
    fclose(fp);
    return 0;
}
