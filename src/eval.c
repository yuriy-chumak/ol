/*
 *  Created on: 18 sep. 2014 г.
 *      Author: Yuriy Chumak
 *
 *  Этот проект запускает скрипт, переданный в командной строке, построчно
 */

#include "olvm.h"

#include <stdio.h>
#include <malloc.h>
#include <sys/stat.h>
#include <fcntl.h>
#ifdef WIN32
//#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#endif


#include <windows.h>
#include <gl\gl.h>			// Header File For The OpenGL32 Library
#include <gl\glu.h>			// Header File For The GLu32 Library
//#include <gl\glaux.h>		// Header File For The Glaux Library

#ifndef O_BINARY
#define O_BINARY 0
#endif

/***************************************************
 * TESTS
 **************************************************/

unsigned char *readfile(const char *filename)
{
	struct stat st;
	int fd, pos = 0;
	if (stat(filename, &st)) exit(1);

	char* ptr = (char*)malloc(st.st_size);
	if (ptr == NULL) exit(2);
	fd = open(filename, O_RDONLY | O_BINARY);
	if (fd < 0) exit(3);
	while (pos < st.st_size) {
		int n = read(fd, ptr+pos, st.st_size-pos);
		if (n < 0) exit(4);
		pos += n;
	}
	close(fd);
	return (unsigned char*)ptr;
}

// main
int main(int argc, char* argv[])
{
	if (argc < 2) {
		puts("usage: vm.exe evalscript");
		return -1;
	}
/*	float x = 0.0f;
	FILE* f = fopen("float", "wb");
	fwrite(&x, sizeof(float), 1, f);
	x = 1.0f;
	fwrite(&x, sizeof(float), 1, f);
	x = 2.0f;
	fwrite(&x, sizeof(float), 1, f);
	fclose(f);
*/

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
	OL* ol = vm_start(language);

	if (strcmp(argv[1], "-e") == 0) {
		vm_puts(ol, argv[2], strlen(argv[2]));
//		vm_puts(ol, "(halt 0)", 8);
	}
	else {
		FILE *f = fopen(argv[1], "r");
		fseek(f, 0, SEEK_END);
		int len = (int)ftell(f);
		fseek(f, 0, SEEK_SET);
		char* source = (char*)malloc(len);
		fread(source, 1, len, f);
		fclose(f);
		vm_puts(ol, source, len);
	}
	//	vm_puts(vm, "(sys-prim 6 0 0 0)", 18);
	vm_puts(ol, "(halt 0)#", 8);

//	vm_stop(ol); // automatically stop after evaluation
//	free(source);

	char response[1024];
	do {
		int got =
		vm_gets(ol, response, sizeof(response));
		if (got == 1023) {
			printf("%s", response);
		}
		else {
			printf("%s", response);
		}
//		if (got == sizeof(response) - 1)
//			printf("%d - %s", got, response);

//		puts(response);
/*		if (got == sizeof(response) - 1)
			;
		else
			printf("%s\n", response);*/
//		fflush(stdout);
	}
	while (!vm_feof(ol)); // еще не все забрали
	free(ol);

#ifdef WI32
	WSACleanup();
#endif
	return 0;
}
