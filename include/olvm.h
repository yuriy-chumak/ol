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

/**
 * Otus Lisp (Ol in short) is a purely functional dialect of Lisp.
 * 
 * It implements an extended subset of R5RS Scheme including, but
 * not limited to, some of the SRFIs. It's tiny(42kb), embeddable
 * and crossplatform; can run in own sandbox; provides a portable,
 * highlevel way for using the code written in another languages.
 * 
 * You can use Ol in Linux, Windows, Unixes (macOS, kinds of BSD),
 * Android, webOS and lot of any other operation systems based on
 * various hardware architectures (x86, x86_64, arm, aarch64, ppc,
 * mips, etc).
**/

#ifndef __OLVM_H__65374DBFFB460CF7E3F765DF2A1F3A24__
#define __OLVM_H__65374DBFFB460CF7E3F765DF2A1F3A24__
#ifdef __cplusplus
	extern "C" {
#endif

#include <stdint.h>
#include <unistd.h>

// (игра слов)
// OL:
//	* сокращение от названия проекта - Otus Lisp (вырос из Owl Lisp'а),
//	* низкий порог вхождения (0L - число 0, L - level) (Lisp - ОЧЕНЬ простой язык),
//	* тег нумерованного списка в html - (еще одна отсылка к lisp - языку обработки списков),
//	* ol' - сокращение от old (старый), отсылка к тому, что lisp - один из старейших языков.
//  * А еще, в корейском похожий иероглиф (이) переводится как "Это", "This".
// Otus (англ) - совка, подтип сов; создание (фин).
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
 *
 */
uintptr_t
OL_run(struct ol_t* ol, int argc, char** argv);

/**
 * Continue OL vm after stop (if possible)
 * 
 * \param[in] ol Valid olvm instance
 * \param[in] argc Arguments count
 * \param[in] argv Arguments array
 * 
 * \note First argument must be valid olvm runnable object (function, bytecode, etc.)
 */
uintptr_t
OL_continue(struct ol_t* ol, int argc, void** argv);

/**
 * Set and return userdata associated with olvm instance
 */
void*
OL_userdata(struct ol_t* ol, void* userdata);


// handle read
typedef ssize_t (read_t)(int fd, void *buf, size_t count, void* userdata);
read_t* OL_set_read(struct ol_t* ol, read_t read);

// handle write
typedef size_t (write_t)(int fd, void *buf, size_t count, void* userdata);
write_t* OL_set_write(struct ol_t* ol, write_t read);

// =================================================
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
//                 to disable (to increase speed, for example) checks
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
