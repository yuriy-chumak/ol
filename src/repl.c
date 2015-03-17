/*
 *  Created on: 18 sep. 2014 г.
 *      Author: Yuriy Chumak
 *
 *  Этот проект запускает скрипт, переданный в командной строке, построчно
 */

#include "olvm.h"
//#ifndef STANDALONE


#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <fcntl.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif


static OL* ol;

unsigned char *readfile(const char *filename)
{
	struct stat st;
	int pos = 0;
	if (stat(filename, &st)) exit(1);

	char* ptr = (char*)malloc(st.st_size + 1);
	if (ptr == NULL) exit(2);
	FILE *fd = fopen(filename, "rb");
	if (!fd) exit(3);
	while (pos < st.st_size) {
		int n = fread(ptr+pos, 1, st.st_size-pos, fd);
		if (n < 0) exit(4);
		pos += n;
	}
	ptr[pos] = 0;
	fclose(fd);
	return (unsigned char*)ptr;
}

static void
console(void *args) // heap top
{
	if (args == 0) {
		if (_isatty(_fileno(stdin))) // is character device (not redirected) (interactive session)
			vm_puts(ol, "(define *interactive* #t)", 25);
		int ch;
		while ((ch = fgetc(stdin)) != EOF)
			vm_puts(ol, (char*)&ch, 1);
	}
	else
		vm_puts(ol, (char*)args, strlen((char*)args));
	vm_puts(ol, ",quit", 5);
}

// main
extern unsigned char* language;
int main(int argc, char** argv)
{
#ifdef _WIN32
	WSADATA wsaData;
	int sock_init = WSAStartup(MAKEWORD(2,2), &wsaData);
	if (sock_init  != 0) {
		printf("WSAStartup failed with error: %d\n", sock_init);
		return 1;
	}
	AllocConsole();
#endif

	while (1) {
		if (argc == 1) { // без аргументов - настоящий REPL
			ol = vm_new(language, 0);
			CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)console, NULL, 0, NULL);
			break;
		}
		if (argc == 2) {
			unsigned char* file = readfile(argv[1]);
			if (*file < ' ') {
				ol = vm_new(file, free);
				break;
			}
			ol = vm_new(language, 0);
			CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)console, file, 0, NULL);

			break;
		}
		if (argc == 3 && strcmp(argv[1], "-e") == 0) {
			ol = vm_new(language, 0);
			CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)console, argv[2], 0, NULL);
			break;
		}

		printf("usage: repl [script]\n");
		exit(1);
	}



//	// disable buffering
//	setvbuf(stderr, (void*)0, _IONBF, 0);
//	set_signal_handler(); // set up signal handler
//	set_blocking(2, 0);

/*	if (argc > 2 && strcmp(argv[1], "-l") == 0) {
		unsigned char* language = readfile(argv[2]);
		ol = vm_new(language, free);

		argc -= 2;
		argv += 2;
	}*/
/*
	if (ol == 0) {
#if 0
		unsigned char*
		language = readfile("fasl/boot.fasl");
		//	if (*language == '#') { // skip hashbang
		//		while (*language++ != '\n');
		//	};
#else
		extern unsigned char* language;
#endif
		ol = vm_new(language, 0);
	}

	while (1) {
		if (argc == 1) {
			CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)from_stdin, 0, 0, NULL);
			break;
		}
		if (argc == 2) {
			CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)from_file, argv[1], 0, NULL);
			break;
		}
		if (argc == 3) {
			if (strcmp(argv[1], "-e") == 0) {
				CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)from_line, argv[2], 0, NULL);
				break;
			}
		}
		printf("error in command line\n");
		exit(1);
	}
*/
	char response[80];
	do {
		vm_gets(ol, response, sizeof(response));
		printf("%s", response);
		fflush(stdout);
	}
	while (!vm_feof(ol)); // еще не все забрали
	free(ol);

#ifdef _WIN32
	WSACleanup();
#endif
	return 0;
}

//#endif//STANDALONE
