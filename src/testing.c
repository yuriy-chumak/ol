/*
 *  Created on: 27 авг. 2014 г.
 *      Author: Yuriy Chumak
 *
 *  Этот проект запускает на проверку тесты, список можно посмотреть ниже.
 */

#include "olvm.h"

#include <stdio.h>
#include <malloc.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef WIN32
#	define WIN32_LEAN_AND_MEAN
#	include <windows.h>
#	include <winsock2.h>

#define PTW32_VERSION 2,9,1,0
typedef HANDLE pthread_t;
typedef struct pthread_attr_t {} pthread_attr_t;
static int
pthread_create(pthread_t * thread, const pthread_attr_t * attributes,
               void *(*function)(void *), void * argument)
{
	pthread_t th = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)function, argument, 0, NULL);
	if (thread != NULL)
		*thread = th;
	return (th != NULL);
}

#else
#	include <pthread.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif

static struct OL* vm;

/***************************************************
 * TESTS
 **************************************************/
static
void* function(void *args) // heap top
{
	FILE *f = fopen((char*)args, "r");
	if (f) {
		char source[1024];
		int read;
		while ((read = fread(source, 1, sizeof(source), f)) > 0)
			vm_puts(vm, source, read);
		fclose(f);
	}
	vm_puts(vm, ",quit", 5);
	return 0;
}


static
int test(char* test, FILE* o)
{
	pthread_create(NULL, NULL, &function, test);
	do {
//		char request[1024];
		char result[1024] = { '\0' };
		char response[1024];

		vm_gets(vm, response, sizeof(response)-1); // именно в такой последовательности
		if (0 == fgets(result, sizeof(result), o))
			if (*response != 0)
				return -1;

//		strcat(response, "\n");
		if (strcmp(result, response) != 0) {

			printf("Expected [\n%s] but got [\n%s]\n", result, response);
			return -1;
		}
	}
	while (!vm_feof(vm) && !feof(o)); // еще не все забрали

	return !vm_feof(vm); // если в буфере ничего лишнего не осталось
}

// main
int main(int nargs, char **argv)
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

//	// disable buffering
//	setvbuf(stderr, (void*)0, _IONBF, 0);
//	set_signal_handler(); // set up signal handler
//	set_blocking(2, 0);

	extern
	unsigned char*
	language;// = readfile("fasl/boot.fasl");
//	*language = readfile("foo.fasl"); // binary image with serialized data
//	if (*language == '#') { // skip hashbang
//		while (*language++ != '\n');
//	};

	char *testfiles[] = {
//			"tests/dlopen.scm", (все работает, просто дразнит выскакивающее окошко)
//			"tests/opengl.scm",
			"tests/apply.scm",
			"tests/banana.scm",
//			"tests/bingo-rand.scm", // failed, no "i has all" output
			"tests/bisect-rand.scm",
			"tests/callcc.scm",
			"tests/case-lambda.scm",
			"tests/circle.scm",
//			"tests/circular.scm", (сообщение об ошибке ушло в stderr а не на проверку)
			"tests/dir.scm",
			"tests/echo.scm",
			"tests/ellipsis.scm",
			"tests/eval.scm",
			"tests/factor-rand.scm",
			"tests/factorial.scm",
			"tests/fasl.scm",
			"tests/ff-call.scm",
			"tests/ff-del-rand.scm",
			"tests/ff-rand.scm",
			"tests/fib-rand.scm",
//			"tests/file.scm",    (vec-len: not a vector: #false of type 13)
			"tests/hashbang.scm",
			"tests/iff-rand.scm",
			"tests/library.scm",
			"tests/macro-capture.scm",
			"tests/macro-lambda.scm",
//			"tests/mail-async-rand.scm", // failed, no "ok 300" message
			"tests/mail-order.scm",
			"tests/math-rand.scm",
			"tests/par-nested.scm",
			"tests/par-nested-rand.scm",
			"tests/par-rand.scm",
			"tests/perm-rand.scm",
			"tests/por-prime-rand.scm",
			"tests/por-terminate.scm",
//			"tests/process.scm", (error)
			"tests/queue-rand.scm",
			"tests/r5rs.scm",
			"tests/r7rs.scm",
			"tests/record.scm",
//			"tests/regex.scm", (hangs)
			"tests/rlist-rand.scm",
			"tests/seven.scm",
			"tests/share.scm",
			"tests/stable-rand.scm",
			"tests/str-quote.scm",
			"tests/string.scm",
			"tests/suffix-rand.scm",
			"tests/theorem-rand.scm",
			"tests/toplevel-persist.scm",
			"tests/utf-8-rand.scm",
			"tests/vararg.scm",
			"tests/vector-rand.scm",
			"tests/numbers.scm",
			0};
	int i = 0, result = 0;
	char *filename;
	while ((filename = testfiles[i++]) != 0) {
		printf("Testing %s... ", filename);

		char okname[128] = {0};
		strcpy(okname, filename);
		strcat(okname, ".ok");
		FILE* ok = fopen(okname, "r");

		vm = vm_new(language);
		if (test(filename, ok) == 0)
			printf("ok.");
		else {
			printf("error!");
			result = -1;
		}
		fflush(stdout);
		free(vm); vm = 0;
		fclose(ok);

		printf("\n");
	}
//	free(language);

	if (result == 0)
		printf("\nTests passed.\n");
	else
		printf("\nTests failed.\n");

#ifdef WI32
	WSACleanup();
#endif
	return 0;
}
