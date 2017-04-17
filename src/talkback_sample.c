/*
 * talkback.c
 *
 *  Talkback interface to the olvm
 *  Created on: Apr 13, 2017
 *      Author: uri
 */

#if EMBEDDED_VM
#include <stdio.h>

// OL talkback interface:
void* OL_tb_start();
void OL_tb_stop(void* oltb);

void OL_tb_send(void* state, char* program);
int  OL_tb_recv(void* state, char* out, int size);
int  OL_tb_eval(void* oltb, char* program, char* out, int size);
// .

void* oltb; // OL talkback handle

// few internal functions:
int a(int arg) {
	char buffer[256];
	char output[256];

	snprintf(buffer, sizeof(buffer), "(a %d)", arg);
	if (OL_tb_eval(oltb, buffer, output, sizeof(output)) > 0) {
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
	if (OL_tb_eval(oltb, buffer, output, sizeof(output)) > 0) {
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
	OL_tb_send(oltb, "(define (a n) (+ n 17))");
	OL_tb_send(oltb, "(define (b n) (* n 17))");
	OL_tb_send(oltb, "(define (f n) (fold * 1 (iota n 1 1)))");
	printf("Ok.\n");
	printf("Loading 'main.scm' script if exist...");

	OL_tb_send(oltb, ",load \"tutorial/sample-embed/main.scm\"\n");
	printf("Ok.\n");

	int got;
	char output[1024];

	// calling "a" function
	printf("result of 'a(12)' function: %d\n", a(12));
	printf("result of 'b(72)' function: %d\n", b(72));

	printf("calling 'sum-factors(72)' function...\n");
	got = OL_tb_eval(oltb, "(sum-factors 72)", output, sizeof(output));
	printf("len %d: value %s\n", got, output);

	// calling "f" function
	printf("result of 'f(42)' function: ");
	got = OL_tb_eval(oltb, "(f 42)", output, sizeof(output));
	printf("len %d: value %s\n", got, output);

	OL_tb_stop(oltb);
	return 0;
}

#endif
