CC      = gcc
NAME    = z80emulator
CFLAGS  = -g -O3 -Wall -I $(HDRDIR) -lncurses
SRCDIR  = ./src
HDRDIR  = ./hdr
SOURCES = $(SRCDIR)/main.c $(SRCDIR)/Z80.c $(SRCDIR)/memory.c \
		  $(SRCDIR)/opcodes.c $(SRCDIR)/debug.c $(SRCDIR)/roms.c \
		  $(SRCDIR)/hexToArray.c $(SRCDIR)/mc6850.c $(SRCDIR)/logger.c
OBJECTS = $(SOURCES:.c=.o)
LOGFILE = log.txt

all : $(NAME)

$(NAME) : $(OBJECTS)
	$(CC) $^ -o $@ $(CFLAGS)

$(SRCDIR)/%.o : %.c
	$(CC) $^ -c $< $(CFLAGS)

clean :
	rm -f $(SRCDIR)/*.o
	rm -f $(NAME)
	rm -f $(LOGFILE)
