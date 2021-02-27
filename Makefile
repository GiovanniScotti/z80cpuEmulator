###############################################################################
# Z80 CPU EMULATOR MAKEFILE
###############################################################################

CC      = gcc
NAME    = z80emulator
CFLAGS  = -g -O3 -Wall -I $(HDRDIR) -lncurses
SRCDIR  = ./src
HDRDIR  = ./hdr
SOURCES = $(SRCDIR)/main.c $(SRCDIR)/logger.c $(SRCDIR)/hex2array.c \
		  $(SRCDIR)/cpu.c $(SRCDIR)/opcodes.c $(SRCDIR)/mc6850.c \
		  $(SRCDIR)/board.c

OBJECTS = $(SOURCES:.c=.o)


all: $(NAME)

$(NAME): $(OBJECTS)
	$(CC) $^ -o $@ $(CFLAGS)

$(SRCDIR)/%.o: %.c
	$(CC) $^ -c $< $(CFLAGS)


clean:
	rm -f $(SRCDIR)/*.o
	rm -f $(NAME)

.PHONY: clean
