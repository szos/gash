CC=gcc
CFLAGS=`pkg-config --cflags readline guile-2.2`
# LIBS=`pkg-config --libs readline guile-2.2`
libgash.so: libgash.c
	$(CC) $(CFLAGS) -shared -o libgash.so -fPIC libgash.c -l readline
