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

#include <stdint.h>
#include <unistd.h>

// (игра слов)
// OL:
//	* сокращение от названия проекта - Otus Lisp (вырос из Owl Lisp'а),
//	* нулевой порог вхождения (0L - число 0, L - level) (Lisp - ОЧЕНЬ простой язык),
//	* тег нумерованного списка в html - (еще одна отсылка к lisp - языку обработки списков),
//	* ol' - сокращение от old (старый), отсылка к тому, что lisp - один из старейших языков.
//  * А еще, в корейском похожий иероглиф (이) переводится как "Это", "This".
// Otus - совка (англ) - подтип сов, создание (фин).
struct ol_t;

/**
 * Create new OL virtual machine (olvm)
 *
 * \param[in] Bootstrap binary code to be executed by olvm.
 * \return Created olvm instance
 */
struct ol_t*
OL_new(unsigned char* bootstrap);

/**
 * Release the OL virtual machine (olvm)
 *
 * \param[in] ol Valid olvm instance
 */
void
OL_free(struct ol_t* ol);

/**
 * Run the OL virtual machine (olvm)
 *
 * \param[in] ol Valid olvm instance
 * \param[in] argc Arguments count
 * \param[in] argv Arguments array
 */
uintptr_t
OL_run(struct ol_t* ol, int argc, char** argv);

uintptr_t
OL_continue(struct ol_t* ol, int argc, void** argv);

void*
OL_userdata(struct ol_t* ol, void* userdata);

/**
 * Register a function to be called at olvm termination
 *
 * \param[in] ol Valid olvm instance
 * \param[in] function Function to be called
 */
void
(*OL_atexit(struct ol_t* ol, void (*function)(int status))) (int status);

int OL_setstd(struct ol_t* ol, int id, int fd);

// handle read
typedef ssize_t (read_t)(int fd, void *buf, size_t count, void* userdata);
read_t* OL_set_read(struct ol_t* ol, read_t read);

typedef size_t (write_t)(int fd, void *buf, size_t count, void* userdata);
write_t* OL_set_write(struct ol_t* ol, write_t read);

// handle write

// c++ interface:
#ifdef __cplusplus
class OL
{
private:
	ol_t* vm;

public:
	OL(unsigned char* language) { vm = OL_new(language); }
	virtual ~OL() { OL_free(vm); }

	int run(int argc, char** argv) {
		return (int)OL_run(vm, argc, argv);
	}
	
	void* userdata(void* userdata)
	{
	   return OL_userdata(vm, userdata);
	}
};
#else
typedef struct ol_t OL;
#endif

// ------------------------------------------------------------------
// -=( Error Checking )=---------------------------------------------
//                  to disable (to increase speed, for example) check
//                        please uncomment corresponded #define macro
//                                  (doesn't increase speed for real)

//#define CAR_CHECK(arg) 1
//#define CDR_CHECK(arg) 1

//#define UVTOI_CHECK(v) 1
//#define SVTOI_CHECK(v) 1

//#define VMRAW_CHECK 0

// comment this to enable overflow checking in binary program decoder
//#define OVERFLOW_KILLS(n)


// -=( tips and tricks )=--------------------------------------------
//      uncomment next linex to remove correspondent syscall from vm,
// or change in to any number (except 0) to assign new syscall number

// #define SYSCALL_IOCTL 0
// #define SYSCALL_SYSINFO 0
// #define SYSCALL_GETRUSAGE 0

// #define HAS_UNSAFES 0
// #define HAS_SANDBOX 0

// #define OLVM_FFI 0
// #define OLVM_CALLABLES 0

//-- end of header
#ifdef __cplusplus
	}
#endif
#endif//__OLVM_H__
