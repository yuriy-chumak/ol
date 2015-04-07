
CFLAGS := -std=c99 -O3

all: ol

ol: src/olvm.c src/boot.c Makefile
	$(CC) $(CFLAGS) src/olvm.c src/boot.c -DSTANDALONE -ldl -o ol

#vm: src/olvm.c

