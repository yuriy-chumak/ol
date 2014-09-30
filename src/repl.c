/*
 *  Created on: 18 sep. 2014 г.
 *      Author: Yuriy Chumak
 *
 *  Этот проект запускает скрипт, переданный в командной строке, построчно
 */

#include "olvm.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <fcntl.h>
#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif

OL* ol;

/***************************************************
 * TESTS
 **************************************************/

unsigned char *readfile(const char *filename)
{
	struct stat st;
	int pos = 0;
	if (stat(filename, &st)) exit(1);

	char* ptr = (char*)malloc(st.st_size);
	if (ptr == NULL) exit(2);
	FILE *fd = fopen(filename, "rb");
	if (!fd) exit(3);
	while (pos < st.st_size) {
		int n = fread(ptr+pos, st.st_size-pos, 1, fd);
		if (n < 0) exit(4);
		pos += n;
	}
	fclose(fd);
	return (unsigned char*)ptr;
}

static void
from_stdin(void *args) // heap top
{
	if (args == 0) { // no input, please use stdin
		vm_puts(ol, "(define *interactive* #t)", 25);
		int ch;
		while ((ch = fgetc(stdin)) != EOF)
			vm_puts(ol, &ch, 1);
		vm_puts(ol, ",quit", 5);
	}
}
static void
from_file(void *args) // heap top
{
	FILE *f = fopen((char*)args, "r");
	if (f) {
		char source[1024];
		int read;
		while ((read = fread(source, 1, sizeof(source), f)) > 0)
			vm_puts(ol, source, read);
		fclose(f);
	}
	vm_puts(ol, ",quit", 5);
}
static void
from_line(void *args) // heap top
{
	vm_puts(ol, (char*)args, strlen((char*)args));
	vm_puts(ol, ",quit", 5);
}

// main
int main(int argc, char* argv[])
{
#ifdef WIN32
	WSADATA wsaData;
	int sock_init = WSAStartup(MAKEWORD(2,2), &wsaData);
	if (sock_init  != 0) {
		printf("WSAStartup failed with error: %d\n", sock_init);
		return 1;
	}
	AllocConsole();
#endif

	// disable buffering
	setvbuf(stderr, (void*)0, _IONBF, 0);
	set_signal_handler(); // set up signal handler
	set_blocking(2, 0);

#if 0
	unsigned char*
	language = readfile("fasl/boot.fasl");
//	if (*language == '#') { // skip hashbang
//		while (*language++ != '\n');
//	};
#else
	extern unsigned char* language;
#endif
	ol = vm_new(language);

	if (argc == 1) {
		CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)from_stdin, 0, 0, NULL);
	}
	if (argc == 2) {
		CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)from_file, argv[1], 0, NULL);
	}
	if (argc == 3) {
		if (strcmp(argv[1], "-e") == 0) {
			CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)from_line, argv[2], 0, NULL);
		}
	}

	char response[80];
	do {
		vm_gets(ol, response, sizeof(response));
		printf("%s", response);
		fflush(stdout);
	}
	while (!vm_feof(ol)); // еще не все забрали
	free(ol);

#ifdef WI32
	WSACleanup();
#endif
	return 0;
}
