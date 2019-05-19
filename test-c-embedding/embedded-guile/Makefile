CC = gcc
CDEBUG = -g -Wall
CFLAGS = $(CDEBUG) `guile-config compile`
LDFLAGS = `guile-config link`

SRCS = main.c
OBJS = main.o

hello_world: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)
