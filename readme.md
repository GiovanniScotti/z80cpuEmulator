[![Codacy Badge](https://api.codacy.com/project/badge/Grade/1a9e2b93183043dea19b13113a0cdfc5)](https://www.codacy.com/app/hardware994/z80cpuEmulator?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=GiovanniScotti/z80cpuEmulator&amp;utm_campaign=Badge_Grade)

# Z80 CPU Emulator
## Introduction
This is a highly portable Zilog Z80 cpu emulator written in C programming language.
The project was originally born in order to provide an in-software version of the remarkable Grant's 7-chip Z80 computer ([link](http://searle.hostei.com/grant/z80/SimpleZ80.html)). Although the emulator runs a slightly modified version of the Microsoft BASIC as used in the Nascom 2 computer, the z80 core might be used as a starting point for other ambitious projects.

## Emulated system hardware
The baseline emulated hardware architecture has the following specification:
* Z80 CPU core
* 32KB ROM to store programs
* 32KB RAM to store runtime data
* MC6850 ACIA as serial communication interface

## Building
To compile and run the emulator, your system must have the `ncurses` library installed.
After having cloned the git repository, proceed with the commands below.

```console
$ cd z80cpuEmulator     # Enters the project's directory
$ make                  # Build the emulator
$ ./z80emulator         # Run the executable
```

In order to clean your system from compiled source files, logs and executables, execute `make clean`.

## System start up
As soon as the emulator is started, messages start popping up on the terminal.
At `Memory top?` press ENTER to use full memory (32KB), otherwise enter the required value. After testing memory, the following text appears:

```
Z80 BASIC Ver 4.7b
Copyright (C) 1978 by Microsoft
32382 Bytes free
Ok
```

The BASIC interpreter is now ready to use and accept commands.
To exit the emulator, press `CTRL+C`.

## Limitations
Currently, the project has the following known issues and limitations:
* DAA instruction not implemented
* IN/OUT group partially implemented
* Support for mode 1 interrupts only

## Credits
Please, keep in mind that the modified BASIC interpreter is a Grant Searle's intellectual property. In this repository you will find the .HEX file loaded up by the emulator. If you need further information and would like to delve into the hardware implementation, have a look at http://searle.hostei.com/grant/z80/SimpleZ80.html.
