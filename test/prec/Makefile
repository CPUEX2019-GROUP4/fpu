RM = rm -rf
CC = gcc
CCFLAGS = -c -Wall
LDFLAGS = -Wall

INIT_BIN = finv_init sqrt_init sqrt_inv_init

.PHONY: all
all: $(INIT_BIN);

$(INIT_BIN): %: repl.o %.o
	$(CC) $(LDFLAGS) -o $@ $^

.c.o:
	$(CC) $(CCFLAGS) $<

.PHONY: clean
clean:
	$(RM) $(INIT_BIN) *.o
