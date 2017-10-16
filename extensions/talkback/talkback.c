/*
 * talkback.c
 *
 *  Talkback interface to the olvm
 *  Created on: Apr 13, 2017
 *      Author: uri
 */
#include "olvm.h"

#ifdef EMBEDDED_VM
#include <stdio.h>
#include <pthread.h>
#include <malloc.h>

#define PUBLIC
#if _WIN32
#define TALKBACK_API __declspec(dllexport)
#else
#define TALKBACK_API __attribute__((__visibility__("default")))
#endif



/** PIPING *********************************/
#if _WIN32
#	include <windows.h>

#	define PIPE HANDLE

int pipe(PIPE* pipes)
{
	static int id = 0;
	char name[128];
	snprintf(name, sizeof(name), "\\\\.\\pipe\\ol%d", ++id);//__sync_fetch_and_add(&id, 1));

	PIPE pipe1 = CreateNamedPipe(name,
			PIPE_ACCESS_DUPLEX|WRITE_DAC,
			PIPE_TYPE_BYTE|PIPE_READMODE_BYTE|PIPE_NOWAIT,
			2, 1024, 1024, 2000, NULL);

	PIPE pipe2 = CreateFile(name,
			GENERIC_WRITE, 0,
			NULL,
			OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL,
			NULL);


	pipes[0] = pipe1;
	pipes[1] = pipe2;

	return 0;
	//ConnectNamedPipe(pipe, NULL);
}

/*int emptyq(PIPE pipe)
{
	DWORD bytesRead;
	DWORD totalBytesAvail;
	DWORD bytesLeftThisMessage;
	PeekNamedPipe(pipe, NULL, 0,
			&bytesRead, &totalBytesAvail, &bytesLeftThisMessage);

	return totalBytesAvail == 0;
}*/

int cleanup(PIPE* pipes)
{
	CloseHandle(pipes[0]);
	CloseHandle(pipes[1]);

	return 0;
}
#else
#	include <fcntl.h>
#	include <signal.h>

#	define PIPE int


int pipe(PIPE* pipes); // create the pipe
/*int emptyq(int pipe) {
	return poll(&(struct pollfd){ .fd = fd, .events = POLLIN }, 1, 0)==1) == 0);
}*/
int cleanup(PIPE* pipes)
{
	close(pipes[0]);
	close(pipes[1]);

	return 0;
}

#endif

// talkback state
typedef struct state_t
{
	OL* ol;
	pthread_t thread_id;
	PIPE in[2];
	PIPE out[2];

	read_t* read;
	write_t* write;

	int errno;
} state_t;

static
ssize_t os_read(int fd, void *buf, size_t size, state_t* state)
{
	if (fd == 0) // change stdin to our pipe
		fd = state->in[0];
	return state->read(fd, buf, size, state);
}

static
ssize_t os_write(int fd, void *buf, size_t size, state_t* state)
{
	if (fd == 1 || fd == 2)
		fd = state->out[1]; // write stdout and stderr in same port
	return state->write(fd, buf, size, state);
}


static int (*do_load_library)(const char* thename, char** output);

static void *
thread_start(void *arg)
{
	state_t* state = (state_t*)arg;
	OL* ol = state->ol;

	OL_userdata(ol, state); // set new OL userdata
	state->read = OL_set_read(ol, os_read);
	state->write = OL_set_write(ol, os_write);

	// let's finally start!
	char* args[1] = { "-" };
	OL_run(ol, 1, args);

	return 0;
}

extern unsigned char _binary_repl_start[];

PUBLIC
state_t* OL_tb_start()
{
	unsigned char* bootstrap = _binary_repl_start;
	OL*ol = OL_new(bootstrap);

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
	DWORD a;
	WriteFile(state->in[1], "(exit-owl 1)", 12, &a, NULL);
#else
	write(state->in[1],     "(exit-owl 1)", 12);
#endif

	pthread_kill(state->thread_id, SIGSTOP);
	pthread_join(state->thread_id, 0);

	cleanup(&state->in);
	cleanup(&state->out);

	OL_free(state->ol);
	free(state);
}

PUBLIC
void OL_tb_send(state_t* state, char* program)
{
	int len = strlen(program);

#ifdef _WIN32
	DWORD b;
	WriteFile(state->in[1], program, len, &b, NULL);
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
		ReadFile(state->out[0], out, size-1, &bytes, NULL);
	}
	while (bytes == 0);
#else
	int bytes = 0;
	do {
		sched_yield(); // time to process data by OL
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

	state->errno = 0; // clear error mark
#ifdef _WIN32
	DWORD a,b,c;
	WriteFile(state->in[1], "(display ((lambda ()", 20, &a, NULL);
	WriteFile(state->in[1], program, len,               &b, NULL);
	WriteFile(state->in[1], ")))", 3,                   &c, NULL);
#else
	write(state->in[1],     "(display ((lambda ()", 20);
	write(state->in[1],     program, len);
	write(state->in[1],     ")))", 3);
#endif

	return
	OL_tb_recv(state, out, size);
}

// additional features:
PUBLIC
int OL_tb_get_istream(state_t* state)
{
	return state->in[1];
}

PUBLIC
int OL_tb_get_ostream(state_t* state)
{
	return state->out[0];
}

PUBLIC
int OL_tb_get_failed(state_t* state)
{
	return state->errno;
}


// -----------------------
// custom library loader

__attribute__((used))
TALKBACK_API
char* OL_tb_hook_import(char* file)
{
	char* lib = 0;
	if (do_load_library(file, &lib) == 0)
		return lib;
	return 0;
}

__attribute__((used))
TALKBACK_API
void OL_tb_hook_fail(void* userdata, const char* error)
{
	state_t* state = (state_t*)userdata;
	++state->errno;
}


void OL_tb_set_import_hook(state_t* state, int (*hook)(const char* thename, char** output))
{
	do_load_library = hook;

	// make hook:
	OL_tb_send(state,
	        "(import (otus ffi))"
	        "(define $ (dlopen))"
	        "(define hook:import (dlsym $ type-string \"OL_tb_hook_import\" type-string))"
			"(define hook:fail   (dlsym $ type-string \"OL_tb_hook_fail\" type-userdata type-string))"
	);
}

#endif
