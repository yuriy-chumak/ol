/*
 * tests.c
 *
 *  Created on: 27 авг. 2014 г.
 *      Author: Юра
 */

#include "vm.h"

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


int test(OL* vm, char* test, char* ok)
{
	FILE *i = fopen(test, "r");
	fseek(i, 0, SEEK_END);
	int len = (int)ftell(i);
	fseek(i, 0, SEEK_SET);
	char* source = (char*)malloc(len);
	fread(source, 1, len, i);
	fclose(i);

	vm_puts(vm, source, len);
	free(source);

	FILE *o = fopen(ok, "r");
	while (1) {
		char result[1024];
		char response[1024];
		if (0 == fgets(result, sizeof(result), o))
			break;
		vm_gets(vm, response, sizeof(response) - 1);

		strcat(response, "\n");
		if (strcmp(result, response) != 0) {
			fclose(o);

			printf("Expected [\n%s] but got [\n%s]\n", result, response);
			return 0;
		}
	}
	fclose(o);
	return 1;
}

// main
int main(int nargs, char **argv)
{
/*	float x = 0.0f;
	FILE* f = fopen("float", "wb");
	fwrite(&x, sizeof(float), 1, f);
	x = 1.0f;
	fwrite(&x, sizeof(float), 1, f);
	x = 2.0f;
	fwrite(&x, sizeof(float), 1, f);
	fclose(f);
*/

/*	int state = 0;
	while (state == 0) {
		state = GetKeyState(27);
	}*/


//	void *h = GetModuleHandle(0);
//	void *p = GetProcAddress(h, "wmain");

//	return
//	WinMain(0, 0, 0, 0);

/*	void *handle;
	void (*msgbox)(int, char*, char*, int);

	handle = dlopen("user32", RTLD_LAZY);
	*(void **) (&msgbox) = dlsym(handle, "MessageBoxA");

	msgbox(0, "Hallo!", "message", 0);


*/
#ifdef WIN32
	WSADATA wsaData;
	int sock_init = WSAStartup(MAKEWORD(2,2), &wsaData);
	if (sock_init  != 0) {
		printf("WSAStartup failed with error: %d\n", sock_init);
		return 1;
	}
//	AllocConsole();
#endif

	// disable buffering
	setvbuf(stderr, (void*)0, _IONBF, 0);
	set_signal_handler(); // set up signal handler
	set_blocking(2, 0);

	unsigned char*
	language = readfile("fasl/boot.fasl");
//	*language = readfile("foo.fasl"); // binary image with serialized data
//	if (*language == '#') { // skip hashbang
//		while (*language++ != '\n');
//	};

	/*
	{
		OL* ol = vm_start(language);

		FILE *f = fopen("tests/opengl.scm", "r");
		fseek(f, 0, SEEK_END);
		int len = (int)ftell(f);
		fseek(f, 0, SEEK_SET);
		char* source = (char*)malloc(len);
		fread(source, 1, len, f);
		fclose(f);
		vm_puts(ol, source, len);
		free(source);

		while (1) {
			char response[1024];
			vm_gets(ol, response, sizeof(response) - 1);
			printf(response);
			printf("\n");
			if (*response == '@')
				break;
		}

		vm_stop(ol);
	}
	return 0;//*/

//	free((void *) language);

// временно добавим себе сюда запуск тестов
	char *testfiles[] = {
//			"tests/dlopen.scm", (все работает, просто дразнит выскакивающее окошко)
			"tests/opengl.scm",
			"tests/apply.scm",
			"tests/banana.scm",
			"tests/bingo-rand.scm",
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
			"tests/mail-async-rand.scm",
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
	int i = 0;
	char *filename;
	while (filename = testfiles[i++]) {
		printf("Testing %s... ", filename);

		char ok[128] = {0};
		strcpy(ok, filename);
		strcat(ok, ".ok");

		OL *lisp = vm_start(language);
		if (test(lisp, filename, ok))
			printf("ok.");
		else {
			printf("error!");
			vm_stop(lisp);
			break;
		}
		vm_stop(lisp);
		printf("\n");
//		break;
	}
	free(language);

//	char response[1024];
//	eval("(write (fold + 0 (iota 1 2 100)))(print)", response, sizeof(response));
//	printf("[%s]\n", response);
//	stop();

#ifdef WI32
	WSACleanup();
#endif
	return 0;
}

