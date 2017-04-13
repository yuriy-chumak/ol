/*
 * talkback.c
 *
 *  Talkback interface to the olvm
 *  Created on: Apr 13, 2017
 *      Author: uri
 */
#include "olvm.h"
extern unsigned char* talkback;

#if EMBEDDED_VM
#include <stdio.h>
#include <pthread.h>

#if _WIN32
#	include <windows.h>

int pipe(int* pipes)
{
	static int id = 0;
	char name[128];
	snprintf(name, sizeof(name), "\\\\.\\pipe\\ol%d", ++id);

	HANDLE pipe1 = CreateNamedPipe(name,
			PIPE_ACCESS_DUPLEX|WRITE_DAC,
			PIPE_TYPE_BYTE|PIPE_READMODE_BYTE|PIPE_NOWAIT,
			2, 1024, 1024, 2000, NULL);

	HANDLE pipe2 = CreateFile(name,
			GENERIC_WRITE, 0,
			NULL,
			OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL,
			NULL);


	pipes[0] = pipe1;
	pipes[1] = pipe2;

	return 0;
	//ConnectNamedPipe(pipe, NULL);
}
#else
#	include <fcntl.h>
#endif

// embedded example
/*#if _WIN32
#define PUBLIC __declspec(dllexport)
#else
#define PUBLIC __attribute__((__visibility__("default")))
#endif*/
#define PUBLIC

typedef struct state_t
{
	OL* ol;
	pthread_t thread_id;
	int in[2];
	int out[2];
} state_t;

static void *
thread_start(void *arg)
{
	state_t* state = (state_t*)arg;
	OL* ol = state->ol;

	char in[12];
	char out[12];

	snprintf(in, sizeof(in), "%d", state->in[0]);
	snprintf(out, sizeof(out), "%d", state->out[1]);

	char* args[3] = { "-", in, out };
	OL_eval(ol, 3, args);

	return 0;
}

PUBLIC
state_t* OL_tb_start()
{
	OL* ol = OL_new(talkback, 0);

	pthread_attr_t attr;
	pthread_attr_init(&attr);

	state_t* state = (state_t*)malloc(sizeof(state_t));
	pipe(&state->in);
	pipe(&state->out);

#ifndef _WIN32
	fcntl(state->in[0], F_SETFL, fcntl(state->in[0], F_GETFL, 0) | O_NONBLOCK);
	fcntl(state->in[1], F_SETFL, fcntl(state->in[1], F_GETFL, 0) | O_NONBLOCK);
	fcntl(state->out[0], F_SETFL, fcntl(state->out[0], F_GETFL, 0) | O_NONBLOCK);
	fcntl(state->out[1], F_SETFL, fcntl(state->out[1], F_GETFL, 0) | O_NONBLOCK);
#endif


	state->ol = ol;
	pthread_create(&state->thread_id, &attr, &thread_start, state);

	return state;
}

PUBLIC
void OL_tb_stop(state_t* state)
{
#if _WIN32
	int a;
	WriteFile(state->in[1], "(exit-owl 1)", 12, &a, NULL);
#else
	write(state->in[1], "(exit-owl 1)", 12);
#endif

	pthread_cancel(state->thread_id);
	pthread_join(state->thread_id, 0);

	OL_free(state->ol);
	free(state);
}

// don't forget to free the answer?
PUBLIC
char* OL_tb_eval(state_t* state, char* program, char* out, int size)
{
	int len = strlen(program);

#ifdef _WIN32
	int a,b,c;
	WriteFile(state->in[1], "(display-to OUT ((lambda ()", 27, &a, NULL);
	WriteFile(state->in[1], program, len,                      &b, NULL);
	WriteFile(state->in[1], ")))", 3,                          &c, NULL);
#else
	write(state->in[1],     "(display-to OUT ((lambda ()", 27);
	write(state->in[1],     program, len);
	write(state->in[1],     ")))", 3);
#endif

	int bytes = 0;
#ifdef _WIN32
	do {
		Sleep(1); // time to process data by OL
		ReadFile(state->out[0], out, size, &bytes, NULL);
	}
	while (bytes == 0);
#else
	do {
		pthread_yield(); // time to process data by OL
		bytes = read(state->out[0], output, sizeof(output));
	}
	while (bytes == -1);
#endif

	return bytes;
}
#endif
