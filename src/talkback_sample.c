/*
 * talkback.c
 *
 *  Talkback interface to the olvm
 *  Created on: Apr 13, 2017
 *      Author: uri
 */

#if EMBEDDED_VM
#include <stdio.h>

#if _WIN32
#define TALKBACK_API __declspec(dllexport)
#else
#define TALKBACK_API __attribute__((__visibility__("default")))
#endif


// OL talkback interface:
void* OL_tb_start();
void OL_tb_stop(void* oltb);

void OL_tb_send(void* state, char* program);
int  OL_tb_recv(void* state, char* out, int size);
int  OL_tb_eval(void* oltb, char* program, char* out, int size);

// error processing extension:
// errno
static int OL_tb_error = 0;
int OL_tb_errno(void)
{
	return OL_tb_error;
}

int eval(void* oltb, char* program, char* out, int size) {
	OL_tb_error = 0;
	return OL_tb_eval(oltb, program, out, size);
}


TALKBACK_API
int OL_tb_seterror(void)
{
	return ++OL_tb_error;
}

// .

void* oltb; // OL talkback handle

TALKBACK_API
int add_ii(int fa, int fb)
{
	// math
	int a = fa;    fprintf(stderr, "DEBUG: add_ii (a) = %d\n", a);
	int b = fb;    fprintf(stderr, "DEBUG: add_ii (b) = %d\n", b);
	int r = a + b; fprintf(stderr, "DEBUG: add_ii (r) = %d\n", r);

	// result
	return r;
}

// few internal functions:
int a(int arg) {
	char buffer[256];
	char output[256];

	snprintf(buffer, sizeof(buffer), "(a %d)", arg);
	if (eval(oltb, buffer, output, sizeof(output)) > 0) {
		int out;
		sscanf(output, "%d", &out);
		return out;
	}
	return 0;
}
int b(int arg) {
	char buffer[256];
	char output[256];

	snprintf(buffer, sizeof(buffer), "(b %d)", arg);
	if (eval(oltb, buffer, output, sizeof(output)) > 0) {
		int out;
		sscanf(output, "%d", &out);
		return out;
	}
	return 0;
}


int main(int argc, char** argv)
{
	oltb = OL_tb_start();

	printf("Compiling script...");
	OL_tb_send(oltb, "(import (otus pinvoke) (owl io))"
	                 "(define $ (dlopen))" // get own handle
	                 "(define add (dlsym $ type-int+ \"add_ii\" type-int+ type-int+))"
	                 "(define seterrno (dlsym $ type-int+ \"OL_tb_seterror\"))"
	);

	OL_tb_send(oltb, "(define (a n) (add n 17))");
	OL_tb_send(oltb, "(define (b n) (* n 17))");
	OL_tb_send(oltb, "(define (f n) (fold * 1 (iota n 1 1)))");
	printf("Ok.\n");
	printf("Loading 'main.scm' script if exist...");

	OL_tb_send(oltb, ",load \"tutorial/sample-embed/main.scm\"\n");

	int got;
	char output[1024];
	got = eval(oltb, "1", output, sizeof(output)); // let's wait for full loading
	printf("Ok.\n");

	// calling "a" function
	printf("result of 'a(12)' function: %d\n", a(12));
	printf("result of 'b(72)' function: %d\n", b(72));

	printf("calling 'sum-factorsx(72)' /not existent/ function... ");
	got = eval(oltb, "(sum-factorsx 72)", output, sizeof(output));
	if (OL_tb_errno())
		printf("failed. Error: %s\n", output);
	else
		printf("ok. Len: %d, value %s\n", got, output);

	// calling "f" function
	printf("result of 'f(42)' function... ");
	got = eval(oltb, "(f 42)", output, sizeof(output));
	if (OL_tb_errno())
		printf("failed. Error: %s\n", output);
	else
		printf("ok. Len: %d, value %s\n", got, output);

	printf("result of 'a(12)' function: %d\n", a(12));
	printf("result of 'b(72)' function: %d\n", b(72));
	OL_tb_stop(oltb);
	return 0;
}

#endif
