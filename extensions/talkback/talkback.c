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

#include <signal.h>

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

int pipe(int* pipes); // already implemented in olvm.c
int cleanup(int* pipes)
{
	// note from MSDN: The underlying handle is also closed by a call to _close,
	//     so it is not necessary to call the Win32 function CloseHandle on the
	//     original handle.
	_close(pipes[0]);
	_close(pipes[1]);

	return 0;
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

#else
#	include <fcntl.h>

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

/* talkback ********************************/
// talkback state
typedef struct state_t
{
	OL* ol;
	pthread_t thread_id;
	int in[2];  // internal
	int out[2]; // internal

	FILE* inf;// file to send the data into vm

	void* ok; // answer from virtual machine
	void* error; // error from vm

	// sync primitives
	pthread_mutex_t olvm_mutex;
	pthread_cond_t olvm_condv;
	pthread_mutex_t me_mutex;
	pthread_cond_t me_condv;
} state_t;

#define wakeup(who) {\
	pthread_mutex_lock(&state->who ## _mutex);\
	pthread_cond_signal(&state->who ## _condv);\
	pthread_mutex_unlock(&state->who ## _mutex);\
}
#define wait(who) {\
	pthread_mutex_lock(&state->who ## _mutex);\
	pthread_cond_wait(&state->who ## _condv, &state->who ## _mutex);\
	pthread_mutex_unlock(&state->who ## _mutex);\
}


// oltb interface:
void OL_tb_send(state_t* state, char* program);

static void *
thread_start(void *arg)
{
	state_t* state = (state_t*)arg;
	OL* ol = state->ol;

	OL_userdata(ol, state); // set new OL userdata

	// replace std handles for ol:
	OL_setstd(ol, 0, state->in[0]);
//	OL_setstd(ol, 1, state->out[1]); // just print some data to the console
//	OL_setstd(ol, 2, state->out[1]);

	// start
	char* args[1] = { "-" };
	OL_run(ol, 1, args);

	return 0;
}

__attribute__((used))
TALKBACK_API
void OL_tb_hook_fail(void* error, void* userdata)
{
	state_t* state = (state_t*)userdata;
	state->error = error;

	// notify main thread that we got answer from ol
	wakeup(me);

	// let's sleep and make main thread to process answer
	// (avoiding this answer to be GC'ed)
	wait(olvm);
}

__attribute__((used))
TALKBACK_API
void OL_tb_an_answer(void* answer, void* userdata)
{
	state_t* state = (state_t*)userdata;
	state->ok = answer;

	// notify main thread that we got answer from ol
	wakeup(me);

	// let's sleep and make main thread to process answer
	// (avoiding this answer to be GC'ed)
	wait(olvm);
}

// simply send some data to the vm
PUBLIC
void OL_tb_send(state_t* state, char* program)
{
	fprintf(state->inf, "%s", program);
	fflush(state->inf);

	// wakeup the ol thread (and no wait for answer)
	wakeup(olvm);

	return;
}

// wait for answer (or error)
PUBLIC
void* OL_tb_eval(state_t* state, char* program)
{
	if (state->error)
		return 0;

	if (program) {
		fprintf(state->inf, "(an:answer (vm:new type-vptr ((lambda () %s ))) (syscall 1002 #f #f #f))", program);
		fflush(state->inf);
	}

	state->ok = 0;
	wakeup(olvm);

	pthread_mutex_lock(&state->me_mutex);
	if (state->error != 0) {
		pthread_mutex_unlock(&state->me_mutex);
		return 0;
	}
	if (state->ok != 0) {
		pthread_mutex_unlock(&state->me_mutex);
		return state->ok;
	}

	// still no answer, no error. so let's sleep
	pthread_cond_wait(&state->me_condv, &state->me_mutex);
	pthread_mutex_unlock(&state->me_mutex);

	if (state->error)
		return 0;
	return state->ok;
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
	pipe((int*)&state->in);
	pipe((int*)&state->out);

#ifndef _WIN32
	fcntl(state->in[0], F_SETFL, fcntl(state->in[0], F_GETFL, 0) | O_NONBLOCK);
	fcntl(state->in[1], F_SETFL, fcntl(state->in[1], F_GETFL, 0) | O_NONBLOCK);
	fcntl(state->out[0], F_SETFL, fcntl(state->out[0], F_GETFL, 0) | O_NONBLOCK);
	fcntl(state->out[1], F_SETFL, fcntl(state->out[1], F_GETFL, 0) | O_NONBLOCK);
#endif

	// let's initialize sync primitives
	pthread_mutex_init(&state->me_mutex, 0);
	pthread_cond_init(&state->me_condv, 0);
	pthread_mutex_init(&state->olvm_mutex, 0);
	pthread_cond_init(&state->olvm_condv, 0);
	fflush(stderr);

	// processing data part
	state->inf = fdopen(state->in[1], "w");
	state->error = 0;
	state->ok = 0;

	// finally start
	state->ol = ol;
	pthread_create(&state->thread_id, &attr, &thread_start, state);

	// connect our 'ok' and 'fail' processors
	OL_tb_send(state,
	        "(import (otus ffi))"
	        "(define $ (dlopen))"
	        "(define hook:fail (dlsym $ type-void \"OL_tb_hook_fail\" type-vptr type-userdata))"
			"(define an:answer (dlsym $ type-void \"OL_tb_an_answer\" type-vptr type-userdata))"
	);

	return state;
}

PUBLIC
void OL_tb_stop(state_t* state)
{
	OL_tb_send(state, "(exit-owl 0)");
	if (pthread_cancel(state->thread_id) == 0) {
		wakeup(olvm);
		pthread_join(state->thread_id,0);
	}

	cleanup((int*)&state->in);
	cleanup((int*)&state->out);

	OL_free(state->ol);
	free(state);
}

/*// additional features:
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
*/
PUBLIC
void* OL_tb_get_error(state_t* state)
{
	return state->error;
}


// -----------------------
// custom library loader
static int (*do_load_library)(const char* thename, char** output);

__attribute__((used))
TALKBACK_API
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
	        "(import (otus ffi))"
	        "(define $ (dlopen))"
	        "(define hook:import (dlsym $ type-string \"OL_tb_hook_import\" type-string))"
	);
}

#endif
