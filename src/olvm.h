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
//	* сокращение от названия проекта - Otus Lisp (вырос из Owl Lisp'а),
//	* нулевой порог вхождения (0L - число 0, L - level) (Lisp - ОЧЕНЬ простой язык),
//	* тег нумерованного списка в html - (еще одна отсылка к lisp - языку обработки списков),
//	* ol' - сокращение от old (старый), отсылка к тому, что lisp - один из старейших языков.
//  * А еще, в корейском похожий иероглиф (이) переводится как "Это", "This".
// Otus - совка (англ) - подтип сов, создание (фин).
struct ol_t;

// internal option
//#define NO_SECCOMP

struct
ol_t* OL_new(unsigned char* bootstrap, void (*release)(void*));
void  OL_free(struct ol_t* ol);
void* OL_eval(struct ol_t* ol, int argc, char** argv);

// c++ interface:
#ifdef __cplusplus
class OL
{
private:
	ol_t* vm;
public:
	OL(unsigned char* language) { vm = ol_new(language, 0); }
	virtual ~OL() { OL_free(vm); }


	int eval(int argc, char** argv) {
		return (int)OL_eval(argc, argv);
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

//#define UVTOI_CHECK(v) 1
//#define SVTOI_CHECK(v) 1

//#define VMRAW_CHECK 0

// comment this to enable overflow checking in binary program decoder
//#define OVERFLOW_KILLS(n)


/* tips and tricks: */

// uncomment next linex to remove correspondent syscall from vm,
// or change in to any number (except 0) to assign new syscall number

// #define SYSCALL_IOCTL 0
// #define SYSCALL_SYSINFO 0
// #define SYSCALL_GETRUSAGE 0

//-- end of header
#ifdef __cplusplus
	}
#endif
#endif//__OLVM_H__
