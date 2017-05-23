/*
 * talkback.c
 *
 *  Talkback interface to the olvm
 *  Created on: Apr 13, 2017
 *      Author: uri
 */
#include <olvm.h>
extern unsigned char* talkback;

#if EMBEDDED_VM
#include <stdio.h>
#include <pthread.h>

/** PIPING *********************************/
int pipe(int* pipes); // create the pipe
//int emptyq(int pipe); // check is pipe empty

#if _WIN32
#	include <windows.h>

int pipe(int* pipes)
{
	static int id = 0;
	char name[128];
	snprintf(name, sizeof(name), "\\\\.\\pipe\\ol%d", ++id);//__sync_fetch_and_add(&id, 1));

	HANDLE pipe1 = CreateNamedPipe(name,
			PIPE_ACCESS_DUPLEX|WRITE_DAC,
			PIPE_TYPE_BYTE|PIPE_READMODE_BYTE|PIPE_NOWAIT,
			2, 1024, 1024, 2000, NULL);

	HANDLE pipe2 = CreateFile(name,
			GENERIC_WRITE, 0,
			NULL,
			OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL,
			NULL);


	pipes[0] = (int)(size_t)pipe1; // may loss bits!
	pipes[1] = (int)(size_t)pipe2;

	return 0;
	//ConnectNamedPipe(pipe, NULL);
}

int emptyq(int pipe)
{
	DWORD bytesRead;
	DWORD totalBytesAvail;
	DWORD bytesLeftThisMessage;
	PeekNamedPipe((HANDLE)(size_t)pipe, NULL, 0,
			&bytesRead, &totalBytesAvail, &bytesLeftThisMessage);

	return totalBytesAvail == 0;
}
#else
#	include <fcntl.h>

/*int emptyq(int pipe) {
	return poll(&(struct pollfd){ .fd = fd, .events = POLLIN }, 1, 0)==1) == 0);
}*/
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

static int (*do_load_library)(const char* thename, char** output);

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
	pipe((int*)&state->in);
	pipe((int*)&state->out);

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
	DWORD a;
	WriteFile((HANDLE)(size_t)state->in[1], "(exit-owl 1)", 12, &a, NULL);
#else
	write(state->in[1], "(exit-owl 1)", 12);
#endif

	pthread_cancel(state->thread_id);
	pthread_join(state->thread_id, 0);

	OL_free(state->ol);
	free(state);
}

PUBLIC
void OL_tb_send(state_t* state, char* program)
{
	int len = strlen(program);

#ifdef _WIN32
	DWORD b;
	WriteFile((HANDLE)(size_t)state->in[1], program, len,                      &b, NULL);
#else
	write(state->in[1],     program, len);
#endif

	return;
}

PUBLIC
int OL_tb_recv(state_t* state, char* out, int size)
{
#ifdef _WIN32
	DWORD bytes = 0;
	do {
		Sleep(1); // time to process data by OL
		ReadFile((HANDLE)(size_t)state->out[0], out, size-1, &bytes, NULL);
	}
	while (bytes == 0);
#else
	int bytes = 0;
	do {
		pthread_yield(); // time to process data by OL
		bytes = read(state->out[0], out, size-1);
	}
	while (bytes == -1);
#endif

	if (bytes >= 0)
		out[bytes] = 0;
	return bytes;
}

PUBLIC
int OL_tb_eval(state_t* state, char* program, char* out, int size)
{
	int len = strlen(program);

#ifdef _WIN32
	DWORD a,b,c;
	WriteFile((HANDLE)(size_t)state->in[1], "(display-to OUT ((lambda ()", 27, &a, NULL);
	WriteFile((HANDLE)(size_t)state->in[1], program, len,                      &b, NULL);
	WriteFile((HANDLE)(size_t)state->in[1], ")))", 3,                          &c, NULL);
#else
	write(state->in[1],     "(display-to OUT ((lambda ()", 27);
	write(state->in[1],     program, len);
	write(state->in[1],     ")))", 3);
#endif

	return
	OL_tb_recv(state, out, size);
}

#if _WIN32
__declspec(dllexport)
#else
__attribute__((__visibility__("default")))
#endif
__attribute__((used))
char* OL_tb_hook_import(char* file)
{
	char* lib = 0;
	if (do_load_library(file, &lib) == 0)
		return lib;
	return 0;
}

void OL_tb_set_import_hook(state_t* state, int (*hook)(const char* thename, char** output))
{
	do_load_library = hook;

	// make hook:
	OL_tb_send(state,
	        "(import (otus pinvoke))"
	        "(define $ (dlopen))"
	        "(define hook:import (dlsym $ type-string \"OL_tb_hook_import\" type-string))"
	);
}

#endif
