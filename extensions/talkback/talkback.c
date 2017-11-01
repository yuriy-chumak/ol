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
#	include <io.h>
#	include <fcntl.h>

#	define PIPE HANDLE

int new_pipe(PIPE* pipes)
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


	pipes[0] = _open_osfhandle((intptr_t)pipe1, _O_APPEND | _O_RDONLY);
	pipes[1] = _open_osfhandle((intptr_t)pipe2, _O_APPEND | _O_WRONLY);

	// todo: https://stackoverflow.com/questions/7369445/is-there-a-windows-equivalent-to-fdopen-for-handles

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
	_close(pipes[0]);
	_close(pipes[1]);

	return 0;
}
#else
#	include <fcntl.h>
#	include <signal.h>

#	define PIPE int

int new_pipe(int* pipes) // create the pipe
{
	pipe(pipes);
}

int pipe(PIPE* pipes);
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
	int in[2];
	int out[2];

	FILE* inf;

	read_t* read;
	write_t* write;

	int failed;
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
	state->read  = OL_set_read(ol, os_read);
	state->write = OL_set_write(ol, os_write);

	OL_setstd(ol, 0, state->in[0]);
	OL_setstd(ol, 1, state->out[1]);
	OL_setstd(ol, 2, state->out[1]);

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

	state->inf = fdopen(state->in[1], "w");

	state->ol = ol;
	pthread_create(&state->thread_id, &attr, &thread_start, state);

	OL_tb_send(state,
	        "(import (otus ffi))"
	        "(define $ (dlopen))"
	        "(define hook:fail   (dlsym $ type-void \"OL_tb_hook_fail\" type-string type-userdata))"
	);

	return state;
}

PUBLIC
void OL_tb_stop(state_t* state)
{
	fprintf(state->inf, "%s", "(exit-owl 1)");

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
	fprintf(state->inf, "%s", program);
	fflush(state->inf);
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
	while ((bytes == 0) && !state->failed);
#else
	int bytes = 0;
	do {
		sched_yield(); // time to process data by OL
		bytes = read(state->out[0], out, size-1);
	}
	while ((bytes == -1) && !state->failed);
#endif

	if (bytes >= 0)
		out[bytes] = 0;
	return bytes;
}

PUBLIC
int OL_tb_eval(state_t* state, char* program, char* out, int size)
{
	state->failed = 0; // clear error mark
	fprintf(state->inf, "(display ((lambda () %s )))", program);
	fflush(state->inf);

	return OL_tb_recv(state, out, size);
}

// additional features:
PUBLIC
FILE* OL_tb_get_istream(state_t* state)
{
	return state->inf;
}

PUBLIC
int OL_tb_get_ostream(state_t* state)
{
	return state->out[0];
}

PUBLIC
int OL_tb_get_failed(state_t* state)
{
	return state->failed;
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
void OL_tb_hook_fail(const char* error, void* userdata)
{
	state_t* state = (state_t*)userdata;
	++state->failed;
}


void OL_tb_set_import_hook(state_t* state, int (*hook)(const char* thename, char** output))
{
	do_load_library = hook;

	// make hook:
	OL_tb_send(state,
	        "(import (otus ffi))"
	        "(define $ (dlopen))"
	        "(define hook:import (dlsym $ type-string \"OL_tb_hook_import\" type-string))"
	);
}

#endif
