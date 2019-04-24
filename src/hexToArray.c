#include "hexToArray.h"


// Count lines including the terminating line
int countHexFileLines(FILE *fp) {
  int line_counter = 0;
  char c;

  fseek(fp, 0, SEEK_SET); // Set pointer to start of file

  while((c = fgetc(fp)) != EOF) {
    if(c == ':') {
      line_counter++;
    }
  }
  return line_counter;
}


u8 ascii2Hex(char c) {
  if (c >= '0' && c <= '9') {
    return c - '0';
  }
  else if (c >= 'A' && c <= 'F') {
    return c - 'A' + 10;
  }
  else if (c >= 'a' && c <= 'f') {
    return c - 'a' + 10;
  }
  return 0;
}


int hexFileToArray(FILE *fp, char *rom) {
  int numberOfLines = countHexFileLines(fp); // Count lines in hex file
  int readLines = 0;
  int romPointer = 0;

  // After ':' there are 37 bytes of data: 1 (byte count) + 2 (address) +
  // 1 (record type) + 32 (data) + 1 (checksum)

  fseek(fp, 0, SEEK_SET); // Start of file

  while(readLines < numberOfLines) {
    u8 bytesInLine[37];
    if(fgetc(fp) == ':') {
      readLines++;
      if(readLines >= numberOfLines)
        break;

      // Extract bytes in the line
      for(int i = 0; i < 37; i++) {
        bytesInLine[i] = ascii2Hex(fgetc(fp))*0x10 + ascii2Hex(fgetc(fp));
      }

      unsigned int checksum = 0;
      for(int i = 0; i < 36; i++) {
        checksum += bytesInLine[i];
      }
      // Filter LSB and two's complement
      u8 checksumLSB = checksum & 0x000000FF;
      checksumLSB = ~checksumLSB + 1;

      if(checksumLSB == bytesInLine[36]) {
        for(int i = 4; i < 36; i++) {
          // Extract bytes of data
          rom[romPointer] = bytesInLine[i];
          romPointer++;
        }
      }
      else {
        writeLog("[ERROR] Unable to parse the HEX file: incorrect checksum.\n");
        return 0;
      }
    }
  }

  if(romPointer == 0x2000) {
    // Fill unused memory with 0xFF
    for(int i = romPointer; i < ROM_SIZE; i++) {
      rom[i] = 0xFF;
    }
    writeLog("[INFO] HEX file correctly parsed.\n");
    return 1;
  }
  else {
    writeLog("[ERROR] Unable to parse the HEX file: missing bytes!\n");
    return 0;
  }
}


// Returns 0 if error occurs, 1 otherwise
int loadHEX(char *path) {
  FILE *fp;

  fp = fopen(path, "rb"); // Read in binary mode
  if(fp == NULL) {
    writeLog("[ERROR] Unable to open the HEX file.\n");
  }

  if(hexFileToArray(fp, rom)) {
    fclose(fp);
    return 1;
  }
  else {
    fclose(fp);
  }

  return 0; // Return error
}
