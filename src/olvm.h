/**
 * This program is free software;  you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
**/
#pragma once

#ifndef __OLVM_H__0F78631C_47C6_11E4_BBBE_64241D5D46B0__
#define __OLVM_H__0F78631C_47C6_11E4_BBBE_64241D5D46B0__
#ifdef __cplusplus
	extern "C" {
#endif

// (игра слов)
// OL:
//	* сокращение от предка - Owl Lisp'a,
//	* нулевой порог вхождения (0L - число 0, L - level) (Lisp - ОЧЕНЬ простой язык),
//	* тег нумерованного списка в html - (еще одна отсылка к lisp - языку обработки списков),
//	* ol' - сокращение от old (старый), отсылка к тому, что lisp - один из старейших языков.
struct ol_t;

// internal option
//#define NO_SECCOMP

struct ol_t* OL_new(unsigned char* bootstrap, void (*release)(void*));
struct ol_t* OL_free(struct ol_t* ol);

void* OL_eval(struct ol_t* ol, int argc, char** argv);

// c++ interface:
#ifdef __cplusplus
class OL
{
private:
	OLvm* vm;
public:
	OL(unsigned char* language) { vm = ol_new(language, 0); }
	virtual ~OLvm() { free(vm); }


	int eval() {
		return 0;
	}
};
#else
typedef struct ol_t OL;
#endif

// ------------------------------------------------------------------
// -=( Error Checking )=---------------------------------------------
//                         to disable (to increase speed for example)
//                         check remove comment corresponded #define.
//                                  (doesn't increase speed for real)

//#define CAR_CHECK(arg) 1
//#define CDR_CHECK(arg) 1

// comment this to enable overflow checking in binary program decoder
//#define OVERFLOW_KILLS(n)


/* tips and tricks: */

// uncomment next linex to remove correspondent syscall from vm,
// or change in to any number (except 0) to assign new syscall number

// #define SYSCALL_IOCTL 0
// #define SYSCALL_SYSINFO 0
// #define SYSCALL_GETRUSAGE 0

#ifdef _WIN32
#define SYSCALL_PRCTL 0
#endif


//-- end of header
#ifdef __cplusplus
	}
#endif
#endif//__OLVM_H__
