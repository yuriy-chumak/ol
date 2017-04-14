/**
 * Simple purely functional Lisp, mostly
 *
 * Version 1.1
 *
 * Copyright (c) 2014 Aki Helin
 * Copyright (c) 2014- 2017 Yuriy Chumak
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * This program is free software;  you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * How to build:
 *   make; make install
 *
 * Project page:
 *   http://yuriy-chumak.github.io/ol/
 *
 * The parent project - Owl Lisp:
 *   https://github.com/aoh/owl-lisp
 *   https://code.google.com/p/owl-lisp/
 *
 * Related:
 *   http://people.csail.mit.edu/jaffer/Scheme (r5rs)
 *   http://groups.csail.mit.edu/mac/projects/scheme/
 *   http://www.s48.org/
 *
 */

// gcc profiling:
// 1) gcc --coverage
// 2) gcov -b olvm.c

// Emscripten is a 32-bit platform, so size_t is a 32-bit unsigned integer,
//  __POINTER_WIDTH__=32, __SIZEOF_LONG__=4 and __LONG_MAX__ equals 2147483647L.

// http://beefchunk.com/documentation/lang/c/pre-defined-c/precomp.html
#ifndef __GNUC__
#	warning "This code must be compiled by Gnu C compiler"
#else
#	define GCC_VERSION (__GNUC__ * 10000 \
	                  + __GNUC_MINOR__ * 100 \
	                  + __GNUC_PATCHLEVEL__)
#	if GCC_VERSION < 30200
#		error "Required gcc version > 3.2"
#	endif

#	if __STDC_VERSION__ < 199901L
#		error "Required c99 enabled (-std=c99)"
#	endif
#endif

#if	HAS_CONFIG
#include "olvm.h"
#endif

//
#ifdef _WIN32
#	define SYSCALL_PRCTL 0     // no sandbox for windows yet, sorry
#	define SYSCALL_GETRLIMIT 0
#	define PUBLIC __declspec(dllexport)
// qemu for windows: https://qemu.weilnetz.de/
// images for qemu: http://4pda.ru/forum/index.php?showtopic=318284
#endif

// todo: use __unix__ instead both __FreeBSD__ and __NetBSD__ ?
#ifdef __unix__

// FreeBSD, NetBSD, OpenBSD, MacOS, etc.
# ifndef __linux__
#	define SYSCALL_PRCTL 0
#	define SYSCALL_SYSINFO 0
#	define SYSCALL_GETRUSAGE 0
#	define SYSCALL_GETRLIMIT 0
# endif

#	define PUBLIC __attribute__ ((__visibility__("default")))
#endif

#ifdef __asmjs__
#	define NO_SECCOMP

#	define HAS_SOCKETS 0
#	define HAS_DLOPEN  0
#	define HAS_PINVOKE 0
#	define HAS_STRFTIME 0 // why?
#endif


#ifdef __ANDROID__
// gdb for android: https://dan.drown.org/android/howto/gdb.html
// http://resources.infosecinstitute.com/android-hacking-and-security-part-20-debugging-apps-on-android-emulator-using-gdb/

// android supports seccomp only for Lollipop and Nougat
// https://security.googleblog.com/2016/07/protecting-android-with-more-linux.html
#	if __ANDROID_API__<15
#		define NO_SECCOMP
#	endif
#	define SYSCALL_SYSINFO 0
#	define SYSCALL_GETRLIMIT 0
#define PUBLIC __attribute__ ((__visibility__("default")))
#endif

#ifdef NO_SECCOMP
#	define SYSCALL_PRCTL 0
#endif


// http://man7.org/linux/man-pages/man7/posixoptions.7.html

// максимальные атомарные числа для элементарной математики:
//	для 32-bit: 16777215 (24 бита, 0xFFFFFF)
//  для 64-bit: 72057594037927935 (56 бит, 0xFFFFFFFFFFFFFF)

// математику считать так: (receive (vm:add (fxmax) 1) (lambda (n carry) (list carry n)))
//                   либо: (let* ((n carry (fx+ (fxmax) 1))) (...))
// при превышении выдает, естественно, мусор
//
// Z80: http://www.emuverse.ru/wiki/Zilog_Z80/%D0%A1%D0%B8%D1%81%D1%82%D0%B5%D0%BC%D0%B0_%D0%BA%D0%BE%D0%BC%D0%B0%D0%BD%D0%B4
//      http://igorkov.org/pdf/Z80-Central-Processor-Unit.pdf
//      https://ru.wikipedia.org/wiki/Zilog_Z80
// Всякие LISP идеи и примеры:
//      http://habrahabr.ru/post/204992/
//      http://habrahabr.ru/post/211100/
// Книга http://ilammy.github.io/lisp/
// https://www.cs.utah.edu/flux/oskit/html/oskit-wwwch14.html
//
// GC в чикенлиспе: http://en.wikipedia.org/wiki/Cheney%27s_algorithm
//
// кастомные типы: https://www.gnu.org/software/guile/manual/html_node/Describing-a-New-Type.html#Describing-a-New-Type
// список функций: http://jscheme.sourceforge.net/jscheme/doc/R4RSprimitives.html

// интересное из мира Lisp 2015: https://habrahabr.ru/post/265589/

// check the libuv https://github.com/libuv/libuv

// pinned objects - если это будут просто какие-то равки, то можно их размещать ДО основной памяти,
//	при этом основную память при переполнении pinned размера можно сдвигать вверх.

// todo: support ALL of this OS
// http://sourceforge.net/p/predef/wiki/OperatingSystems/
//    Linux-i386                                                                  +
//    Linux-x86_64 (amd64)                                                        +
//    Linux-powerpc
//    Linux-sparc
//    Linux-ARM                                                                   +
//    Win32-i386 (2000/XP, WinNT or later)                                        +
//    Win64-x86_64 (XP or later)                                                  +
//    Wince-ARM (cross compiled from win32-i386)
//    FreeBSD-i386                                                                +
//    FreeBSD-x86_64                                                              +
//    NetBSD-i386                                                                 +
//    NetBSD-x86_64                                                               +
//    Mac OS X/Darwin for PowerPC (32 and 64 bit)
//    Mac OS X/Darwin for Intel (32 and 64 bit)
//    iOS (ARM and AArch64/ARM64) and iPhoneSimulator (32 and 64 bit)
//    OS/2-i386 (OS/2 Warp v3.0, 4.0, WarpServer for e-Business and eComStation)
//    Haiku-i386
//    GO32v2-i386
//    Nintendo Gameboy Advance-ARM (cross compile from win32-i386)
//    Nintendo DS-ARM (cross compile from win32-i386)
//    Nintendo Wii-powerpc (cross compile from win32-i386)
//    AIX 5.3 and later for PowerPC (32 and 64 bit)
//    Java JVM (1.5 and later) and Android Dalvik (Android 4.0 and later)
//    Android (ARM, i386, MIPS) via cross-compiling.                              +
//    MSDos-i8086 (cross compiled from win32-i386 or Linux)                       -
//    Amiga, MorphOS and AROS

// ABIs https://developer.android.com/ndk/guides/abis.html

// todo: strip ELF http://habrahabr.ru/post/137706/
// http://www.catch22.net/tuts/reducing-executable-size

// todo: add setsockopt syscall https://www.freebsd.org/doc/en/books/developers-handbook/ipv6.html
// todo: перенести регистры в топ heap памяти, так их размер можно будет сделать динамическим,
//       а заодно там же колбеки будут лежать нормально. и, главное, gc станет работать быстрее.
// todo: колбеки выбирать из списка - первый нулевой

// http://nadeausoftware.com/articles/2012/02/c_c_tip_how_detect_processor_type_using_compiler_predefined_macros

#define __OLVM_NAME__ "OL"
#define __OLVM_VERSION__ "1.1"

// defaults. please don't change. use -DOPTIONSYMBOL gcc command line defines instead
#ifndef HAS_SOCKETS
#define HAS_SOCKETS 1 // system sockets support
#endif

#ifndef HAS_DLOPEN
#define HAS_DLOPEN 1  // dlopen/dlsym support
#endif

#ifndef HAS_PINVOKE
#define HAS_PINVOKE 1 // pinvoke (for dlopen/dlsym) support
#endif

#ifndef HAS_STRFTIME
#define HAS_STRFTIME 1
#endif

#ifndef EMBEDDED_VM   // use as embedded vm in project
#define EMBEDDED_VM 0
#endif

// http://www.gnu.org/software/libc/manual/html_node/Feature-Test-Macros.html
#define _POSIX_SOURCE // enable functionality from the POSIX.1 standard (IEEE Standard 1003.1),
                      // as well as all of the ISO C facilities.

#define _BSD_SOURCE
#define _GNU_SOURCE   // nanosleep, etc.

#ifdef __NetBSD__     // make all NetBSD features available
#	ifndef _NETBSD_SOURCE
#	define _NETBSD_SOURCE 1
#	endif
#endif

// http://man7.org/tlpi/code/faq.html#use_default_source
//  glibc version 6+ uses __GLIBC__/__GLIBC_MINOR__
#define _DEFAULT_SOURCE

#include <stdint.h>

// possible data types: LP64 ILP64 LLP64 ILP32 LP32
// http://www.unix.org/version2/whatsnew/lp64_wp.html
// http://stackoverflow.com/questions/384502/what-is-the-bit-size-of-long-on-64-bit-windows
#if INTPTR_MAX == INT64_MAX
#define MATH_64BIT 1
#else
#define MATH_64BIT 0
#endif


#ifdef __linux__
#include <features.h>
#endif

#ifdef __unix__
# ifndef __asmjs__
#	include <sys/cdefs.h>
# endif
#endif


// check this for nested functions:
//	https://github.com/Leushenko/C99-Lambda

// posix or not:
//	http://stackoverflow.com/questions/11350878/how-can-i-determine-if-the-operating-system-is-posix-in-c
// http://nadeausoftware.com/articles/2012/01/c_c_tip_how_use_compiler_predefined_macros_detect_operating_system#WindowswithCygwinPOSIX

#ifdef __MINGW32__ // bugs in mingw
#define _cdecl __cdecl

#ifdef __STRICT_ANSI__
#undef __STRICT_ANSI__
#endif
#endif

// http://joeq.sourceforge.net/about/other_os_java.html
// call/cc - http://fprog.ru/lib/ferguson-dwight-call-cc-patterns/

#ifdef __WINCE__
#undef __COREDLL__
#endif

// компилятор otus-lisp поддерживает несколько специальных форм:
//	quote, lambda, receive, values, ol:let, ifeq, setq, ol:ifa, (env.scm)
//	все остальное - макросы или функции/процедуры

#include <assert.h>
#include <unistd.h> // posix, https://ru.wikipedia.org/wiki/C_POSIX_library
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <dirent.h>
#include <string.h>
// no <alloca.h>, use http://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html
#include <setjmp.h>

#include <errno.h>
#include <stdio.h>
#include <inttypes.h>
#include <fcntl.h>
#ifndef _WIN32
#include <termios.h>
#endif

#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef _WIN32
#	define lstat stat
#endif

#include <time.h>

#ifdef __unix__
#	include <sys/utsname.h>
#endif

#ifdef __linux__
#	include <sys/utsname.h> // uname
#	include <sys/resource.h>// getrusage
#	ifndef NO_SECCOMP
#		include <sys/prctl.h>
#		include <linux/seccomp.h>
#	endif

#	include <sys/mman.h>
#	include <sys/sendfile.h>
#endif

#ifdef _WIN32
#	define WIN32_LEAN_AND_MEAN
#	include <windows.h>

#	include <malloc.h>
#endif

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#endif

// additional defines:
#ifndef O_BINARY
#	define O_BINARY 0
#endif

// https://gcc.gnu.org/onlinedocs/gcc/Diagnostic-Pragmas.html
//#pragma GCC diagnostic push
//#pragma GCC diagnostic error "-Wuninitialized"
//#pragma GCC diagnostic pop

// some portability issues (mainly for freebsd)
#ifndef EWOULDBLOCK
#define EWOULDBLOCK EAGAIN
#endif

#ifndef VMRAW_CHECK
#define VMRAW_CHECK 1
#endif

// ========================================
static
void STDERR(char* format, ...)
{
	va_list args;

	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	fprintf(stderr, "\n");
}

#if !EMBEDDED_VM
static
void crash(int code, char* format, ...)
{
	va_list args;

	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);

	fprintf(stderr, "\n");
	exit(code);
}
#endif

// ========================================
//  HAS_SOCKETS 1
#if HAS_SOCKETS

// headers
#ifdef __linux__
#	include <sys/socket.h>
#	include <netinet/in.h>
#	include <netdb.h>     // for gethostbyname()
#	include <arpa/inet.h> // for inet_addr()
#endif

#ifdef _WIN32
#	include <winsock2.h>
#	include <ws2tcpip.h>
#endif

#ifdef _WIN32
#	include <conio.h>
	typedef unsigned long in_addr_t;

#	ifndef EWOULDBLOCK
#	define EWOULDBLOCK WSAEWOULDBLOCK
#	endif

#	undef ERROR // due to macro redefinition
#endif

#ifdef __unix__
#	include <sys/socket.h>
#	include <netinet/in.h>
#	include <sys/select.h>

#	include <arpa/inet.h> // for inet_addr()
#	include <netdb.h>     // for gethostbyname()

#	ifndef PF_INET
#	define PF_INET AF_INET
#	endif

#	ifndef INADDR_NONE
#	define INADDR_NONE	0xffffffff
#	endif
#endif

//#ifdef __ANDROID__
//	typedef unsigned long in_addr_t;
//#endif

#ifdef __APPLE__
#	include "TargetConditionals.h"
#	if TARGET_IPHONE_SIMULATOR
     // iOS Simulator
#	elif TARGET_OS_IPHONE
    // iOS device
#	elif TARGET_OS_MAC
    // Other kinds of Mac OS
#	else
    // Unsupported platform
#	endif
#endif

#endif

// --------------------------------------------------------
// -=( dl )=-----------------------------------------------
#if HAS_DLOPEN
// интерфейс к динамическому связыванию системных библиотек

#ifdef _WIN32
// seen at https://github.com/dlfcn-win32/dlfcn-win32/blob/master/dlfcn.c

static DWORD dlerrno = 0;
static
void *dlopen(const char *filename, int mode/*unused*/)
{
	HMODULE hModule;
	// Do not let Windows display the critical-error-handler message box */
	// UINT uMode = SetErrorMode( SEM_FAILCRITICALERRORS );

	if (filename == 0) {
		hModule = GetModuleHandle(NULL);
	}
	else {
		/* POSIX says the search path is implementation-defined.
		 * LOAD_WITH_ALTERED_SEARCH_PATH is used to make it behave more closely
		 * to UNIX's search paths (start with system folders instead of current
		 * folder).
		 */
		hModule = LoadLibraryEx((LPSTR)filename, NULL, LOAD_WITH_ALTERED_SEARCH_PATH);
	}

	dlerrno = GetLastError();
	return hModule;
}

static
int dlclose(void *handle)
{
	return FreeLibrary((HMODULE)handle);
}

static
void *dlsym(void *handle, const char *name)
{
	FARPROC function;

	function = GetProcAddress((HANDLE)handle, name);
	return function;
}

static
char* dlerror() {
//	size_t size = FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, dlerrno, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT))
	return "description unavailable";
}

#else
#	include <dlfcn.h>
#endif

#endif //HAS_DLOPEN


// --------------------------------------------------------
// -=( fork )=---------------------------------------------
// sample implementation can be found at
// https://github.com/jonclayden/multicore/blob/master/src/forknt.c
// originally from: "Windows NT/2000 native API reference" ISBN 1-57870-199-6.
#if _WIN32

#endif

// ----------
// -=( OL )=----------------------------------------------------------------------
// --
//
// виртуальная машина:
typedef struct ol_t OL;

// unsigned int that is capable of storing a pointer
// основной data type, зависит от разрядности машины
//   базируется на C99 стандарте, <stdint.h>
typedef uintptr_t word;

// descriptor format
// заголовок объекта, то, что лежит у него в ob[0] (*ob)
// object headers are further
//  [... ssssssss ????rppp tttttt10] // bit "immediate" у заголовков всегда(!) выставлен в 1 (почему?, а для GC!)
//   '----------| '--||'-| '----|
//              |    ||  |      '-----> object type
//              |    ||  '------------> number of padding (unused) bytes at end of object if raw (0-(wordsize-1))
//              |    |'---------------> rawness bit (raw objects have no decriptors(pointers) in them)
//              |    '----------------> your tags here! e.g. tag for closing file descriptors in gc
//              '---------------------> object size in words
//  первый бит тага я заберу, наверное, для объектров, которые указывают слева направо, нарушая
//	общий порядок. чтобы можно было их корректно перемещать в памяти при gc()
//
// а это то, что лежит в объектах - либо непосредственное значение, либо указатель на другой объект
//                       .------------> 24-bit payload if immediate
//                       |      .-----> type tag if immediate
//                       |      |.----> immediateness
//   .-------------------| .----||.---> mark bit (can only be 1 during gc, removable?)
//  [... pppppppp pppppppp tttttti0]
//   '--------------------------|
//                              '-----> 4- or 8-byte aligned pointer if not immediate
//      младшие 2 нулевые бита для указателя (mark бит снимается при работе) позволяют работать только с выравненными
//       внутренними указателями - таким образом, ВСЕ объекты в куче выравнены по границе слова
//
//
//; note - there are 6 type bits, but one is currently wasted in old header position
//; to the right of them, so all types must be <32 until they can be slid to right
//; position.

// todo: вот те 4 бита можно использовать для кастомных типов - в спецполя складывать ptr на функцию, что вызывает mark для подпоинтеров,
//	и ptr на функцию, что делает финализацию.
// todo: один бит из них я заберу на индикатор "неперемещенных" заголовков во время GC !!!
//	(идея: просто останавливаться на таких объектах, как на generation линии)
// http://publications.gbdirect.co.uk/c_book/chapter6/bitfields.html

#define IPOS      8  // === offsetof (struct direct, payload)

struct __attribute__ ((aligned(sizeof(word)), packed)) value_t
{
	unsigned mark : 1;    // mark bit (can only be 1 during gc)
	unsigned i    : 1;    // for directs always 1
	unsigned type : 5;    // object type
	unsigned sign : 1;    // sign

	word  payload : 8 * sizeof(word) - (1+1+5+1);
};


#define SPOS     16  // === offsetof (struct header, size)
#define TPOS      2  // === offsetof (struct header, type)
#define RPOS     11  // === offsetof (struct header, rawness)

struct __attribute__ ((aligned(sizeof(word)), packed)) header_t
{
	unsigned mark : 1;    // mark bit (can only be 1 during gc)
	unsigned i    : 1;    // for headers always 1
	unsigned type : 6;    // object type

	unsigned padding : 3; // number of padding (unused) bytes at end of object
	unsigned rawness : 1;
	unsigned         : 4; // unused

	word     size : 8 * sizeof(word) - (1+1+6+3+1+4);
};

struct __attribute__ ((aligned(sizeof(word)), packed)) object_t
{
	union {
		struct header_t header;
		word ref[1];
	} u;
};

// ------------------------------------------------------

OL* OL_new(unsigned char* bootstrap, void (*release)(void*));
void OL_free(OL* ol);
void* OL_eval(struct ol_t* ol, int argc, char** argv);

// ------------------------------------------------------

#define W                           sizeof (word)
#define F(val)                      (((word)(val) << IPOS) | 2) // same as make_value(TFIX, val)

#define FBITS                       ((sizeof (word) * 8) - 8) // bits in value (short number)
#define HIGHBIT                     ((int_t)1 << FBITS) // maximum value value + 1
#define FMAX                        (HIGHBIT - 1)       // maximum value value (and most negative value)
// todo: remove MAXOBJ!
#define MAXOBJ                      0xffff         // max words in tuple including header

// http://www.delorie.com/gnu/docs/gcc/gccint_53.html
#if MATH_64BIT
typedef unsigned big_t __attribute__ ((mode (TI))); // __uint128_t
typedef signed int_t __attribute__ ((mode (DI))); // signed 64-bit
#else
typedef unsigned big_t __attribute__ ((mode (DI))); // __uint64_t
typedef signed int_t __attribute__ ((mode (SI))); // signed 32-bit
#endif

#define RAWBIT                      (1 << RPOS)
#define RAWH(t)                     (t | (RAWBIT >> TPOS))

#define make_value(type, value)        ((((word)value) << IPOS) | ((type) << TPOS)                         | 2)
#define make_header(type, size)        (( (word)(size) << SPOS) | ((type) << TPOS)                         | 2)
#define make_raw_header(type, size, p) (( (word)(size) << SPOS) | ((type) << TPOS) | (RAWBIT) | ((p) << 8) | 2)
// p is padding

// два главных класса аргументов:
#define is_value(x)                 (((word)(x)) & 2)
#define is_reference(x)             (!is_value(x))
#define is_rawobject(x)             (((word)(x)) & RAWBIT)

// всякая всячина:
#define hdrsize(x)                  (((word)x) >> SPOS)
#define padsize(x)                  (unsigned char)((((word)(x)) >> IPOS) & 7)

#define thetype(x)                  (unsigned char)((((word)(x)) >> TPOS) & 0x3F)
#define valuetype(x)                (thetype (x) & 0x1F)
#define reftype(x)                  (thetype (*(word*)(x)))

// todo: объединить типы TFIX и TINT, TFIXN и TINTN, так как они различаются битом I
#define TPAIR                        (1)
#define TTUPLE                       (2)
#define TSTRING                      (3)
#define TSYMBOL                      (4)

#define TPORT                       (12)
#define TCONST                      (13)

#define TBYTECODE                   (16) // must be RAW type
#define TPROC                       (17)
#define TCLOS                       (18)

#define TFF                         (24) // // 26,27 same
#	define FFRIGHT                     1 // flags for TFF
#	define FFRED                       2

#define TBVEC                       (19)   // must be RAW type
#define TSTRINGWIDE                 (22)   // must be RAW type

#define TTHREAD                     (31) // type-thread-state

// numbers (value type)
#define TFIX                         (0)  // type-fix+ // todo: rename to TSHORT or TSMALL
#define TFIXN                       (32)  // type-fix-
// numbers (reference type)
#define TINT                        (40)  // type-int+ // todo: rename to TINTEGER (?)
#define TINTN                       (41)  // type-int-
#define TRATIONAL                   (42)
#define TCOMPLEX                    (43)
//efine TINEXACT                    (44)  // float or double, depends on machine bitness

// pinvoke types:
#define TVOID                       (48)
#define TVPTR                       (49) // void*, only RAW, can't be 0
#define TUSERDATA                   (62) // only for pinvoke, must be RAW, can have 0
#define TLONG                       (50) // 32 for 32-bit architecture, 64 for 64-bit

#define TINT16                      (51)
#define TINT32                      (52)
#define TINT64                      (53)

#define TFLOAT                      (46) // '.' symbol
#define TDOUBLE                     (47) // '/' symbol

// todo: сделать два типа колбеков - короткий (такой как я сейчас сделаю)
//       и "длинный", который будет запускать отдельный поток (сопрограмму) и позволит в это же время
//       работать остальным сопрограммам.
#define TCALLBACK                   (61) // type-callback, receives '(description . callback-lambda)
#define TANY                        (63) //

// constants:
#define IFALSE                      make_value(TCONST, 0)
#define ITRUE                       make_value(TCONST, 1)
#define INULL                       make_value(TCONST, 2)
#define IEMPTY                      make_value(TCONST, 3) // empty ff
#define IEOF                        make_value(TCONST, 4)
#define IHALT                       INULL // FIXME: adde a distinct IHALT, TODO: rename all IHALT to INULL, use IHALT to other things.
#define IRETURN                     make_value(TCONST, 6)

//#define likely(x)                   __builtin_expect((x), 1)
//#define unlikely(x)                 __builtin_expect((x), 0)

//#define is_const(ob)                (is_value(ob)     && thetype (ob) == TCONST)
#define is_port(ob)                 (is_value(ob)     && valuetype (ob) == TPORT)

#define is_fix(ob)                  (is_value(ob)     && valuetype (ob) == TFIX)
#define is_fixn(ob)                 (is_value(ob)     && valuetype (ob) == TFIXN)
#define is_pair(ob)                 (is_reference(ob) &&  (*(word*)(ob)) == make_header(TPAIR,     3))
#define is_npair(ob)                (is_reference(ob) &&  (*(word*)(ob)) == make_header(TINT,      3))
#define is_npairn(ob)               (is_reference(ob) &&  (*(word*)(ob)) == make_header(TINTN,     3))
#define is_rational(ob)             (is_reference(ob) &&  (*(word*)(ob)) == make_header(TRATIONAL, 3))
#define is_complex(ob)              (is_reference(ob) &&  (*(word*)(ob)) == make_header(TCOMPLEX,  3))

#define is_string(ob)               (is_reference(ob) &&   reftype (ob) == TSTRING)
#define is_tuple(ob)                (is_reference(ob) &&   reftype (ob) == TTUPLE)

#define is_vptr(ob)                 (is_reference(ob) &&  (*(word*)(ob)) == make_raw_header(TVPTR, 2, 0))
#define is_callback(ob)             (is_reference(ob) &&  (*(word*)(ob)) == make_raw_header(TCALLBACK, 2, 0))

#define is_number(ob)               (is_fix(ob) || is_npair(ob))
#define is_numbern(ob)              (is_fixn(ob) || is_npairn(ob))


// взять значение аргумента:
#define value(v)                    ({ word x = (word)(v); assert(is_value(x));     (((word)(x)) >> IPOS); })
#define reference(v)                ({ word x = (word)(v); assert(is_reference(x)); *(word*)(x); })

#define ref(ob, n)                  (((word*)(ob))[n])
#define car(ob)                     ref(ob, 1)
#define cdr(ob)                     ref(ob, 2)

#define caar(o)                     car(car(o))
#define cadr(o)                     car(cdr(o))
#define cdar(o)                     cdr(car(o))
#define cddr(o)                     cdr(cdr(o))

#define port(o)                     value(o)


// набор макросов - проверок для команд
// car, cdr:
#ifndef CAR_CHECK
#define CAR_CHECK(arg) is_pair(T) || is_npair(T) || is_npairn(T) || is_rational(T) || is_complex(T)
#endif
#ifndef CDR_CHECK
#define CDR_CHECK(arg) is_pair(T) || is_npair(T) || is_npairn(T) || is_rational(T) || is_complex(T)
#endif


#define CR                          16 // available callbacks
#define NR                          256 // see n-registers in register.scm

#define GCPAD(nr)                  (nr+3) // space after end of heap to guarantee the GC work
#define MEMPAD                     (1024) // резервируемое место для работы apply в памяти
// 1024 - некое магическое число, подразумевающее количество
// памяти, используемой между вызовами apply. мои тесты пока показывают максимальное число 32

// http://outflux.net/teach-seccomp/
// http://mirrors.neusoft.edu.cn/rpi-kernel/samples/seccomp/bpf-direct.c
// https://www.kernel.org/doc/Documentation/prctl/seccomp_filter.txt
#define SECCOMP                     10000 // todo: change to x1000 или что-то такое
static int sandboxp = 0;     /* are we in seccomp? а также дельта для оптимизации syscall's */
//static unsigned long seccomp_time; /* virtual time within seccomp sandbox in ms */

//static int breaked = 0;    /* set in signal handler, passed over to owl in thread switch */

// -----------------------------------------------------//--------------------
// -=( GC )=------------------------------------------------------------------

/*** Garbage Collector,
 * based on "Efficient Garbage Compaction Algorithm" by Johannes Martin (1982)
 **/
// "на почитать" по теме GC:
// shamil.free.fr/comp/ocaml/html/book011.html

// память машины, управляемая сборщиком мусора
typedef struct heap_t
{
	//  begin <= genstart <= end
	word *begin;     // begin of heap memory block
	word *end;       // end of heap
	word *genstart;  // new generation begin pointer
	// new (size) === *(size*)fp++
	word *fp;        // allocation pointer
} heap_t;


// -= new =--------------------------------------------
// создает порт, НЕ аллоцирует память
#define make_port(a)               ({ assert ((((word)(a)) << IPOS) >> IPOS == (word)(a)); make_value(TPORT, a); })

// выделить блок памяти, unsafe
#define NEW(size) ({\
	word* addr = fp;\
	fp += size;\
	/*return*/ addr;\
})

// аллоцировать новый объект (указанного типа)
#define NEW_OBJECT(type, size) ({\
word*p = NEW (size);\
	*p = make_header(type, size);\
	/*return*/ p;\
})

// аллоцировать новый "сырой" объект (указанного типа),
//  данные объекта не проверяются сборщиком мусора и не
//  должны содержать другие объекты!
#define NEW_RAW_OBJECT(type, size, pads) ({\
word*p = NEW (size);\
	*p = make_raw_header(type, size, pads);\
	/*return*/ p;\
})

// new(size) - allocate memory, without type
// new(type, size) - allocate object, with type
// new(type, size, pads) - allocate RAW object, with RAWH(type)
#define NEW_MACRO(_1, _2, _3, NAME, ...) NAME
#define new(...) NEW_MACRO(__VA_ARGS__, NEW_RAW_OBJECT, NEW_OBJECT, NEW, NOTHING)(__VA_ARGS__)

// -= new_value =---------------------------------------
#define new_value(a) ((word*)F(a)) // same as make_value(TFIX, a)

// -= new_pair =----------------------------------------

// a1 и a2 надо предвычислить перед тем, как выделим память,
// так как они в свою очередь могут быть аллоцируемыми объектами.
#define NEW_TYPED_PAIR(type, a1, a2) ({\
	word data1 = (word) a1;\
	word data2 = (word) a2;\
	/* точка следования */ \
word*p = NEW_OBJECT (type, 3);\
	p[1] = data1;\
	p[2] = data2;\
	/*return*/ p;\
})

#define NEW_PAIR(a1, a2) NEW_TYPED_PAIR(TPAIR, a1, a2)

#define NEW_PAIR_MACRO(_1, _2, _3, NAME, ...) NAME
#define new_pair(...) NEW_PAIR_MACRO(__VA_ARGS__, NEW_TYPED_PAIR, NEW_PAIR, NOTHING, NOTHING)(__VA_ARGS__)

// -= new_list =----------------------------------------

// аллокаторы списоков (ставить в качестве типа частей TPAIR! так как часть списка - список)
#define new_list2(type, a1) \
	new_pair (type, a1, INULL)
#define new_list3(type, a1, a2) \
	new_pair (type,\
		a1, new_pair (TPAIR,\
			a2, INULL))
#define new_list4(type, a1, a2, a3) \
	new_pair (type,\
		a1, new_pair (TPAIR,\
			a2, new_pair (TPAIR,\
				a3, INULL)))
#define new_list5(type, a1, a2, a3, a4) \
	new_pair (type,\
		a1, new_pair (TPAIR,\
			a2, new_pair (TPAIR,\
				a3, new_pair (TPAIR,\
					a4, INULL))))
#define new_list6(type, a1, a2, a3, a4, a5) \
	new_pair (type,\
		a1, new_pair (TPAIR,\
			a2, new_pair (TPAIR,\
				a3, new_pair (TPAIR,\
					a4, new_pair (TPAIR,\
						a5, INULL)))))

#define NEW_LIST(_1, _2, _3, _4, _5, _6, NAME, ...) NAME
#define new_list(...) NEW_LIST(__VA_ARGS__, new_list6, new_list5, new_list4, new_list3, new_list2, NOTHING, NOTHING)(__VA_ARGS__)

// -= new_tuple =---------------------------------------

// кортеж:
#define new_tuple1(a1) ({\
	word data1 = (word) a1;\
	/* точка следования */ \
word*p = new (TTUPLE, 1+1);\
	p[1] = data1;\
	/*return*/ p;\
})
#define new_tuple2(a1,a2) ({\
	word data1 = (word) a1;\
	word data2 = (word) a2;\
	/* точка следования */ \
word*p = new (TTUPLE, 2+1);\
	p[1] = data1;\
	p[2] = data2;\
	/*return*/ p;\
})
#define new_tuple3(a1,a2,a3) ({\
	word data1 = (word) a1;\
	word data2 = (word) a2;\
	word data3 = (word) a3;\
	/* точка следования */ \
word*p = new (TTUPLE, 3+1);\
	p[1] = data1;\
	p[2] = data2;\
	p[3] = data3;\
	/*return*/ p;\
})
#define new_tuple5(a1,a2,a3,a4,a5) ({\
	word data1 = (word) a1;\
	word data2 = (word) a2;\
	word data3 = (word) a3;\
	word data4 = (word) a4;\
	word data5 = (word) a5;\
	/* точка следования */ \
word*p = new (TTUPLE, 5+1);\
	p[1] = data1;\
	p[2] = data2;\
	p[3] = data3;\
	p[4] = data4;\
	p[5] = data5;\
	/*return*/ p;\
})
#define new_tuple9(a1,a2,a3,a4,a5,a6,a7,a8,a9) ({\
	word data1 = (word) a1;\
	word data2 = (word) a2;\
	word data3 = (word) a3;\
	word data4 = (word) a4;\
	word data5 = (word) a5;\
	word data6 = (word) a6;\
	word data7 = (word) a7;\
	word data8 = (word) a8;\
	word data9 = (word) a9;\
	/* точка следования */ \
word*p = new (TTUPLE, 9+1);\
	p[1] = data1;\
	p[2] = data2;\
	p[3] = data3;\
	p[4] = data4;\
	p[5] = data5;\
	p[6] = data6;\
	p[7] = data7;\
	p[8] = data8;\
	p[9] = data9;\
	/*return*/ p;\
})
#define new_tuple13(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) ({\
	word data1 = (word) a1;\
	word data2 = (word) a2;\
	word data3 = (word) a3;\
	word data4 = (word) a4;\
	word data5 = (word) a5;\
	word data6 = (word) a6;\
	word data7 = (word) a7;\
	word data8 = (word) a8;\
	word data9 = (word) a9;\
	word data10 = (word) a10;\
	word data11 = (word) a11;\
	word data12 = (word) a12;\
	word data13 = (word) a13;\
	/* точка следования */ \
word*p = new (TTUPLE, 13+1);\
	p[1] = data1;\
	p[2] = data2;\
	p[3] = data3;\
	p[4] = data4;\
	p[5] = data5;\
	p[6] = data6;\
	p[7] = data7;\
	p[8] = data8;\
	p[9] = data9;\
	p[10] = data10;\
	p[11] = data11;\
	p[12] = data12;\
	p[13] = data13;\
	/*return*/ p;\
})

#define NEW_TUPLE(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, NAME, ...) NAME
#define new_tuple(...) NEW_TUPLE(__VA_ARGS__, new_tuple13, new_tuple12, new_tuple11,\
			new_tuple10, new_tuple9, new_tuple8, new_tuple7, new_tuple6, new_tuple5,\
			new_tuple4, new_tuple3, new_tuple2, new_tuple1, NOTHING)(__VA_ARGS__)


// -= остальные аллокаторы =----------------------------

#define new_bytevector(type, length) ({\
	int size = (length);\
	int words = (size + W - 1) / W;\
	int pads = (words * W - size);\
	\
word* p = new (type, words + 1, pads);\
	/*return*/ p;\
})


#define NEW_STRING2(string, length) ({\
	char* data = string;\
	int size = (length);\
word* p = new_bytevector(TSTRING, length);\
	char* ptr = (char*)&p[1];\
	while (size--)\
		*ptr++ = *data++;\
	*ptr = '\0'; \
	/*return*/ p;\
})

#define NEW_STRING(string) ({\
	char* str = string;\
	int strln = strlen(str);\
	NEW_STRING2(str, strln);\
})

#define NEW_STRING_MACRO(_1, _2, NAME, ...) NAME
#define new_string(...) NEW_STRING_MACRO(__VA_ARGS__, NEW_STRING2, NEW_STRING, NOTHING)(__VA_ARGS__)


#define new_vptr(a) ({\
word data = (word) a;\
	word* me = new (TVPTR, 2, 0);\
	me[1] = data;\
	/*return*/me;\
})

#define new_userdata(a) ({\
word data = (word) a;\
	word* me = new (TUSERDATA, 2, 0);\
	me[1] = data;\
	/*return*/me;\
})

#define new_callback(a) ({\
word data = (word) a;\
	word* me = new (TCALLBACK, 2, 0);\
	me[1] = data;\
	/*return*/me;\
})

// -= gc implementation =-----------
#define is_flagged(x) (((word)(x)) & 1)  // mark for GC


// возвращается по цепочке "flagged" указателей назад
static __inline__
word *chase(word* pos) {
	//	assert(pos IS flagged)
	word* ppos;
	while (1) {
		ppos = *(word**) ((word)pos & ~1);      // ppos = *pos; ~ = bitwise NOT, (корректное разименование указателя, без учета бита mark)
		if (!is_reference(ppos) || !is_flagged(ppos)) // ppos & 0x3 == 0x1
			return (word*)((word)pos & ~1);
		pos = ppos;
	}
}

// cells - на сколько увеличить (уменьшить) кучу (в словах)
static __inline__
ptrdiff_t adjust_heap(heap_t *heap, int cells)
{
	if (sandboxp) /* realloc is not allowed within seccomp */
		return 0;

	// add newobj realloc + heap fixer here later
	word nwords = heap->end - heap->begin;
	word new_words = nwords + cells; // was: ((cells > 0xffffff) ? 0xffffff : cells); // limit heap growth speed
	if (((cells > 0) && (new_words*W < nwords*W)) || ((cells < 0) && (new_words*W > nwords*W))) {
		STDERR("size_t would overflow in realloc!");
		return 0; // don't try to adjust heap if the size_t would overflow in realloc
	}

	word *old = heap->begin;
	heap->begin = realloc(heap->begin, (new_words + GCPAD(NR)) * sizeof(word));
	heap->end = heap->begin + new_words;

	if (heap->begin == old) // whee, no heap slide \o/
		return 0;

	if (heap->begin) { // d'oh! we need to O(n) all the pointers...
		ptrdiff_t delta = heap->begin - old;

		heap->fp += delta;
		word* pos = heap->begin;
		word* end = heap->fp;

		// fix_pointers
		while (pos < end) {
			word hdr = *pos;
			int sz = hdrsize(hdr);
			if (is_rawobject(hdr))
				pos += sz; // no pointers in raw objects
			else {
				pos++, sz--;
				while (sz--) {
					word val = *pos;
					if (is_reference(val))
						*pos = val + delta*W;
					pos++;
				}
			}
		}
		return delta;
	} else {
		fprintf(stderr, "adjust_heap failed.\n"); // crash
		exit(101);
		#if GCC_VERSION > 40500
		__builtin_unreachable();
		#endif
		//breaked |= 8; // will be passed over to mcp at thread switch
	}
	return 0;
}

/* input desired allocation size and (the only) pointer to root object
   return a pointer to the same object after heap compaction, possible heap size change and relocation */

// todo: ввести третий generation
// просматривает список справа налево
static
void mark(word *pos, word *end, heap_t* heap)
{
//	marked = 0;
//	assert(pos is NOT flagged)
	while (pos != end) {
		word val = pos[0]; // pos header
		if (is_reference(val) && val >= ((word) heap->genstart)) { // genstart - начало молодой генерации
			if (is_flagged(val)) {
				pos = chase((word*) val);
				pos--;
			}
			else {
				word hdr = *(word *) val;
//				//if (is_value(hdr))
//					*(word *) val |= 1; // flag this ? (таки надо, иначе часть объектов не распознается как pinned!)
//				marked++;

				word* ptr = (word*)val;
				*pos = *ptr;
				*ptr = ((word)pos | 1);

				if (hdr & (RAWBIT|1))
					pos--;
				else
					pos = ((word *) val) + (hdrsize(hdr)-1);
			}
		}
		else
			pos--;
	}
}

// TODO: вот здесь можно провести очень неплохую оптимизацию
//       а именно - проверить обратные функции (R[128+], которые
//       лежат в памяти по адресу root+128 - если данные в регистре
//       не изменились, значит больше нету никого, кто ссылается на
//       тот же элемент данных (обратный вызов), а значит его можно
//       просто удалить!

// на самом деле compact & sweep
static
word *sweep(word* end, heap_t* heap)
{
	word *old, *newobject;

	newobject = old = heap->genstart;
	while (old < end) {
		if (is_flagged(*old)) {
			word val = *newobject = *old;
			while (is_flagged(val)) {
				val &= ~1; //clear mark

				word* ptr = (word*)val;
				*newobject = *ptr;
				*ptr = (word)newobject;

				val = *newobject;
			}

			word h = hdrsize(val);
			if (old == newobject) {
				old += h;
				newobject += h;
			}
			else {
				while (--h)
					*++newobject = *++old;
				old++;
				newobject++;
			}
		}
		else
			old += hdrsize(*old);
	}
	return newobject;
}

//__attribute__ ((aligned(sizeof(word))))
// query: запрос на выделение query слов
static
word gc(heap_t *heap, int query, word regs)
{
	if (query == 0) // сделать полную сборку?
		heap->genstart = heap->begin; // start full generation

	//if (heap->genstart == heap->begin)
	//	fprintf(stderr, "+");
	//else
	//	fprintf(stderr, ".");
	#if DEBUG_GC
	fprintf(stderr, "(%6lld)", sizeof(word) * (heap->end - heap->fp));
	#endif

	// gc:
	word *fp;
	fp = heap->fp;
	{
		*fp = make_header(TTUPLE, 2); // этого можно не делать
		word *root = &fp[1];
	//	word *root = fp + 1; // same

		#if DEBUG_GC
			clock_t gctime;
			gctime = -(1000 * clock()) / CLOCKS_PER_SEC;
		#endif

		// непосредственно сам процесс сборки
		root[0] = regs;
		mark(root, fp, heap);        // assert (root > fp)
		// todo: проверить о очистить callbacks перед sweep
		fp = sweep(fp, heap);
		regs = root[0];

		// todo: add diagnostik callback "if(heap->oncb) heap->oncb(heap, deltatime)"
		#if DEBUG_GC
			gctime += (1000 * clock()) / CLOCKS_PER_SEC;
			struct tm tm = *localtime(&(time_t){time(NULL)});
			char buff[70]; strftime(buff, sizeof buff, "%c", &tm);
			STDERR("%s, GC done in %d ms (use: %7d from %8d bytes - %2d%%): (%6d).", //marked %6d, moved %6d, pinned %2d, moved %8d bytes total\n",
					buff/*asctime(&tm)*/, gctime,
					((regs - (word)heap->begin)),        (sizeof(word) * (heap->end - heap->begin)),
					((regs - (word)heap->begin) * 100) / (sizeof(word) * (heap->end - heap->begin)),
					((word) heap->end - regs)
				);
		//				-1, -1, -1, -1);
		#endif
	}
	heap->fp = fp;

	// кучу перетрясли и уплотнили, посмотрим надо ли ее увеличить/уменьшить
	query += MEMPAD; // гарантия места для apply

	ptrdiff_t hsize = heap->end - heap->begin; // вся куча в словах
	ptrdiff_t nfree = heap->end - (word*)regs; // свободно в словах
	ptrdiff_t nused = hsize - nfree + query; // использовано слов
	if (heap->genstart == heap->begin) {
		// напоминаю, сюда мы попадаем только после полной(!) сборки
		if (hsize < nused)
			hsize = nused;

		// Please grow your buffers exponentially:
		//  https://blog.mozilla.org/nnethercote/2014/11/04/please-grow-your-buffers-exponentially/
		//  ! https://habrahabr.ru/post/242279/

		// выделим на "старое" поколение не менее 50% кучи, при этом кучу будем увеличивать на 33%
		if (nused > (hsize / 2)) {
			//fprintf(stderr, ">");
			// !!! множитель увеличения кучи должен быть меньше золотого сечения: 1.618
			regs += adjust_heap(heap, hsize / 3) * sizeof(W);
		}
		// decrease heap size if more than 33% is free by 11% of the free space
		else if (nused < (hsize / 3)) {
			//fprintf(stderr, "<");
			regs += adjust_heap(heap,-hsize / 9) * sizeof(W);
		}
		heap->genstart = (word*)regs; // always start new generation
	}
	// полная сборка, если осталось меньше 20% свободного места в куче
	//  иначе, у нас слишком часто будет вызываться сборщик
	// TODO: посчитать математически, на каком именно числе
	//  стоит остановиться, чтобы ни слишком часто не проводить молодую сборку,
	//  ни слишком часто старую. мне, по тестам кажется, что 20% это вполне ок.
	else if ((nfree - query) < hsize / 5) {
		heap->genstart = heap->begin; // force start full generation
		return gc(heap, query, regs);
	}
	else
		heap->genstart = (word*)regs; // simply start new generation

	return regs;
}


// MATH
// todo: потом переделать в трюк
// ! трюк, в общем, не нужен. gcc вполне неплохо сам оптимизирует код (на x64, например, использует cmov)
// алгоритмические трюки:
// x = (x xor t) - t, где t - y >>(s) 31 (все 1, или все 0)
// signed fix to int

// i - machine integer
// ui - unsigned, si - signed
// v - value number (internal, that fits in one register), type-fix
//  or small numbers,
//  or short numbers
// uv, sv - unsigned/signed respectively.
// Z - mножество целых чисел.

// работа с numeric value типами
#ifndef UVTOI_CHECK
#define UVTOI_CHECK(v) assert (is_value(v) && valuetype(v) == TFIX);
#endif
#define uvtoi(v)  (int_t)({ word x1 = (word)(v); UVTOI_CHECK(x1); (word) (x1 >> IPOS); })
#define itouv(i)  (word) ({ word x2 = (word)(i);                  (word) (x2 << IPOS) | 2; })
		// (((struct value_t*)(&v))->payload);

// todo: add overflow checking...
#ifndef SVTOI_CHECK
#define SVTOI_CHECK(v) assert (is_value(v) && valuetype(v) == TFIX);
#endif
#define svtoi(v) \
	({ \
		word x3 = (word)(v); SVTOI_CHECK(x3); \
		int_t y = (x3 >> IPOS); \
		(x3 & 0x80) ? -y : y; \
	})

#define itosv(i)  (word)({ int_t x4 = (int_t)(i);  (x4 < 0) ? (-x4 << IPOS) | 0x82 : (x4 << IPOS) | 2; })
// todo: check this automation - ((struct value)(v).sign) ? -uvtoi (v) : uvtoi (v);

// арифметика целых (возможно больших)
// прошу внимания!
//  в числовой паре надо сначала положить старшую часть, и только потом младшую!
#define untoi(num)  ({\
	is_value(num) ? uvtoi(num)\
		: uvtoi(car(num)) | uvtoi(cadr(num)) << FBITS; \
	}) //(is_reference(cdr(num)) ? uftoi(cadr(num)) << FBITS : 0); })

// something wrong: looks like __builtin_choose_expr doesn't work as expected!
#define itoun(val)  ({\
	__builtin_choose_expr(sizeof(val) < sizeof(word), \
		(word*)itouv(val),\
		(word*)({ \
			int_t x5 = (int_t)(val); \
			x5 <= FMAX ? \
					(word)itouv(x5): \
					(word)new_list(TINT, itouv(x5 & FMAX), itouv(x5 >> FBITS)); \
		})); \
	})
#define itosn(val)  ({\
	__builtin_choose_expr(sizeof(val) < sizeof(word), \
		(word*)itosv(val),\
		(word*)({ \
			int_t x5 = (int_t)(val); \
			x5 <= FMAX ? \
					(word)itosv(x5): \
					(word)new_list(x5 < 0 ? TINTN : TINT, itouv(x5 & FMAX), itouv(x5 >> FBITS)); \
		})); \
	})

#define make_integer(val) itoun(val)



/*** OS Interaction and Helpers ***/
static
void set_blocking(int sock, int blockp) {
#ifdef _WIN32
//   unsigned long flags = 1;
//   if (sock > 3) { // stdin is read differently, out&err block
//      // tbd. ioctlsocket(sock, FIONBIO, &flags);
//   }
#else
	fcntl(sock, F_SETFL, (blockp ?: O_NONBLOCK));
#endif
}

#if 0
#ifndef _WIN32
static
void signal_handler(int signal) {
	STDERR("signal %d!", signal);
	switch(signal) {
//      case SIGINT:
//        breaked |= 2; break;
      case SIGPIPE: break; // can cause loop when reporting errors
      default:
         // printf("vm: signal %d\n", signal);
         breaked |= 4;
   }
}
#endif
#endif

/* small functions defined locally after hitting some portability issues */
//static __inline__ void bytecopy(char *from, char *to, int n) { while (n--) *to++ = *from++; }
static __inline__ void wordcopy(word *from, word *to, int n) { while (n--) *to++ = *from++; }
static __inline__
unsigned int lenn(char *pos, size_t max) { // added here, strnlen was missing in win32 compile
	unsigned int p = 0;
	while (p < max && *pos++) p++;
	return p;
}
unsigned int llen(word list)
{
	unsigned int p = 0;
	while (list != INULL)
		list = cdr(list), p++;
	return p;
}

void set_signal_handler()
{
#ifndef _WIN32
//	signal(SIGINT, signal_handler);
	signal(SIGPIPE, SIG_IGN);
#endif
}

#if HAS_SOCKETS
# ifndef __linux__

size_t
sendfile(int out_fd, int in_fd, off_t *offset, size_t count)
{
	char buf[2*4096];
	size_t toRead, numRead, numSent, totSent;

	totSent = 0;
	while (count > 0) {
		toRead = (sizeof(buf) < count) ? sizeof(buf) : count;

		// read
		numRead = read(in_fd, buf, toRead);
		if (numRead == -1) {
			STDERR("sendfile: read() returns -1, error: %d", errno);
			return -1;
		}
		if (numRead == 0) {
			STDERR("sendfile: read() returns 0, error: %d", errno);
			break;                      /* EOF */
		}

		// send
		numSent = send(out_fd, buf, numRead, 0);
		if (numSent == SOCKET_ERROR) {
			int err = WSAGetLastError();
			STDERR("sendfile: send() returns -1, error: %d", err);
			if (err != WSAEWOULDBLOCK)
				return -1;

			Sleep(1);
			continue;
		}
		if (numSent == 0) {               /* Should never happen */
			STDERR("sendfile: send() transferred 0 bytes, error: %d", WSAGetLastError());
			return 0;
		}

		count -= numSent;
		totSent += numSent;
	}
	if (shutdown(out_fd, SD_SEND) == SOCKET_ERROR) {
		STDERR("sendfile: shutdown() returns -1, error: %d", WSAGetLastError());
		return -1;
	}
	return totSent;
}
# endif
#endif

/***********************************************************************************
 * OL
 */
struct ol_t
{
	struct heap_t heap; // must be first member
	word max_heap_size; // max heap size in MB

	// вызвать GC если в памяти мало места в словах
	// для безусловного вызова передать 0
	// возвращает 1, если была проведена сборка
	int (*gc)(OL* ol, int kb);

	// 0 - mcp, 1 - clos, 2 - env, 3 - a0, often cont
	// todo: перенести R в конец кучи, а сам R в heap
	word R[NR + CR];   // регистры виртуальной машины

	// текущий контекст с арностью
	word *this;
	long arity;
};

static //__attribute__((aligned(8)))
void* runtime(OL* ol);  // главный цикл виртуальной машины
// требует полностью вализную структуру ol_t

#define TICKS                       10000 // # of function calls in a thread quantum

#define OCLOSE(proctype)            { \
	word size = *ip++, tmp; word *T = new (proctype, size); \
	tmp = R[*ip++]; tmp = ((word *) tmp)[*ip++]; T[1] = tmp; tmp = 2; \
	while (tmp != size) { T[tmp++] = R[*ip++]; } R[*ip++] = (word) T; }
#define CLOSE1(proctype)            { \
	word size = *ip++, tmp; word *T = new (proctype, size); \
	tmp = R[  1  ]; tmp = ((word *) tmp)[*ip++]; T[1] = tmp; tmp = 2; \
	while (tmp != size) { T[tmp++] = R[*ip++]; } R[*ip++] = (word) T; }

#define A0                          R[ip[0]]
#define A1                          R[ip[1]]
#define A2                          R[ip[2]]
#define A3                          R[ip[3]]
#define A4                          R[ip[4]]
#define A5                          R[ip[5]]
#define R0                          (word*)R[0]
#define R1                          (word*)R[1]
#define R2                          (word*)R[2]
#define R3                          (word*)R[3]

// todo: добавить возможность вызова колбека как сопрограммы (так и назвать - сопрограмма)
//       который будет запускать отдельный поток и в контексте колбека ВМ сможет выполнять
//       все остальные свои сопрограммы.
// ret is ret address to the caller function
#if HAS_DLOPEN
	long callback(OL* ol, int id, int_t* argi
	#if __amd64__
		, double* argf, int_t* rest
	#endif
	);

#	include "olni.c"
#endif

// проверить достаточно ли места в стеке, и если нет - вызвать сборщик мусора
static int OL__gc(OL* ol, int ws) // ws - required size in words
{
	word *fp = ol->heap.fp; // memory allocation pointer

	// если места еще хватит, не будем ничего делать
	// TODO: переделать на другую проверку
	if (ws != 0 && fp < ol->heap.end - MEMPAD - ws) // какая-то стремная проверка...
		return 0;

	word* R = ol->R;
	int p = 0, N = NR+CR;

	// если нам не хватило магических 1024, то у нас проблема
	//assert (fp + N + 3 < ol->heap.end);

	// TODO: складывать регистры не в топе, а в heap->real-end - NR - 2

	// создадим в топе временный объект со значениями всех регистров
	word *regs = (word*) new (TTUPLE, N + 2); // N for regs, 1 for this, and 1 for header
	while (++p <= N) regs[p] = R[p-1];
	regs[p] = (word) ol->this;
	// выполним сборку мусора
	ol->heap.fp = fp;
	regs = (word*)gc(&ol->heap, ws, (word)regs); // GC занимает 0-15 ms
	// и восстановим все регистры, уже подкорректированные сборщиком
	ol->this = (word *) regs[p];
	while (--p >= 1) R[p-1] = regs[p];

	// закончили, почистим за собой:
	ol->heap.fp = regs; // (вручную сразу удалим временный объект, это такая оптимизация)

	return 1;
}

// Несколько замечаний по WIN32::ThreadProc
//  http://msdn.microsoft.com/en-us/library/windows/desktop/ms686736(v=vs.85).aspx
//  The return value should never be set to STILL_ACTIVE (259), as noted in GetExitCodeThread.

static
word get(word *ff, word key, word def)
{
	while ((word) ff != IEMPTY) { // ff = [header key value [maybe left] [maybe right]]
		word this = ff[1], hdr;
		if (this == key)
			return ff[2];
		hdr = ff[0];
		switch (hdrsize(hdr)) {
		case 5: ff = (word *) ((key < this) ? ff[3] : ff[4]);
			break;
		case 3: return def;
		case 4:
			if (key < this)
				ff = (word *) ((hdr & (1 << TPOS)) ? IEMPTY : ff[3]);
			else
				ff = (word *) ((hdr & (1 << TPOS)) ? ff[3] : IEMPTY);
			break;
		default:
			STDERR("assert! hdrsize(hdr) == %d", (int)hdrsize(hdr));
			assert (0);
			//ff = (word *) ((key < this) ? ff[3] : ff[4]);
		}
	}
	return def;
}


#ifdef ERROR
#undef ERROR
#endif
#define ERROR(opcode, a, b) \
	{ \
		STDERR("ERROR: %s/%d", __FILE__, __LINE__); /* TEMP */\
		R[4] = F (opcode);\
		R[5] = (word) (a);\
		R[6] = (word) (b);\
		goto error; \
	}
#define CHECK(exp,val,errorcode)    if (!(exp)) ERROR(errorcode, val, ITRUE);

static //__attribute__((aligned(8)))
void* runtime(OL* ol)
{
	heap_t* heap = &ol->heap;

	word*this = ol->this;
	long acc = ol->arity;

	word* R = ol->R;   // регистры виртуальной машины
	word *fp = heap->fp; // memory allocation pointer

	unsigned char *ip = 0; // указатель на инструкции

	int breaked = 0;
	int ticker = TICKS; // any initial value ok
	int bank = 0;  // ticks deposited at interop

apply:;
	if ((word)this == IEMPTY && acc > 1) { /* ff application: (False key def) -> def */
		this = (word *) R[3];              /* call cont */
		R[3] = (acc > 2) ? R[5] : IFALSE;  /* default arg or false if none */
		acc = 1;

		goto apply;
	}

	if ((word)this == IHALT) {
		// a thread or mcp is calling the final continuation
		this = (word *) R[0];
		if (!is_reference(this)) {
			// no, this is expected exit!
			// STDERR("Unexpected virtual machine exit");
			goto done;
		}

		R[0] = IFALSE; // set mcp yes?
		R[4] = R[3];
		R[3] = F(2);   // 2 = thread finished, look at (mcp-syscalls-during-profiling) in lang/thread.scm
		R[5] = IFALSE;
		R[6] = IFALSE;
		breaked = 0;
		ticker = TICKS;// ?
		bank = 0;
		acc = 4;

		goto apply;
	}

	if ((word)this == IRETURN) {
		// в R[3] находится код возврата
		goto done;       // колбек закончен! надо просто выйти наверх
	}

	// ...
	if (is_reference(this)) { // если это аллоцированный объект
		//word hdr = *this & 0x0FFF; // cut size out, take just header info
		word type = reftype (this);
		if (type == TPROC) { //hdr == make_header(TPROC, 0)) { // proc (58% for "yes")
			R[1] = (word) this; this = (word *) this[1]; // ob = car(ob)
		}
		else
		if (type == TCLOS) { //hdr == make_header(TCLOS, 0)) { // clos (66% for "yes")
			R[1] = (word) this; this = (word *) this[1]; // ob = car(ob)
			R[2] = (word) this; this = (word *) this[1]; // ob = car(ob)
		}
		else
		if ((type & 60) == TFF) { // low bits have special meaning (95% for "no")
			// ff assumed to be valid
			word *cont = (word *) R[3];
			switch (acc)
			{
			case 2:
				R[3] = get(this, R[4],    0);
				if (!R[3])
					ERROR(260, this, R[4]);
				break;
			case 3:
				R[3] = get(this, R[4], R[5]);
				break;
			default:
				ERROR(259, this, INULL);
			}
			this = cont;
			acc = 1;

			goto apply;
		}
		else
			if ((type & 63) != TBYTECODE) //((hdr >> TPOS) & 63) != TBYTECODE)
				ERROR(259, this, INULL);

		// А не стоит ли нам переключить поток?
		if (--ticker < 0) {
			// время потока вышло, переключим на следующий
			ticker = TICKS;
			if (R[0] != IFALSE) { // if mcp present:
				// save vm state and enter mcp cont at R0!
				bank = 0;
				acc += 4;

				word *thread;

				thread = (word*) new (TTHREAD, acc);
				for (ptrdiff_t pos = 1; pos < acc-1; pos++)
					thread[pos] = R[pos];

				R[acc] = (word)this;
				thread[acc-1] = (word) this;
				this = (word*) R[0]; // mcp

				R[0] = IFALSE; // remove mcp cont
				// R3 marks the interop to perform
				// 1 - runnig and time slice exhausted
				// 10: breaked - call signal handler
				// 14: memory limit was exceeded
				R[3] = breaked ? ((breaked & 8) ? F(14) : F(10)) : F(1); // fixme - handle also different signals via one handler
				R[4] = (word) thread; // thread state
				R[5] = F(breaked); // сюда можно передать userdata из потока
				R[6] = IFALSE;
				acc = 4; // вот эти 4 аргумента, что возвращаются из (run) после его завершения
				breaked = 0;

				// reapply new thread
				goto apply;
			}
		}

		// теперь проверим доступную память

		// приблизительно сколько памяти может потребоваться для одного эпплая?
		// теоретически, это можно вычислить проанализировав текущий контекст
		// а практически, пока поюзаем количество доступных регистров
		//int reserved = 16 * 1024; // todo: change this to adequate value
		//reserved = MEMPAD;
		// nargs and regs ready, maybe gc and execute ob

		// если места в буфере не хватает, то мы вызываем GC,
		//	а чтобы автоматически подкорректировались регистры,
		//	мы их складываем в память во временный объект.
		if (fp >= heap->end - MEMPAD) { // TODO: переделать
			heap->fp = fp; ol->this = this;
			ol->gc(ol, 1);
			fp = heap->fp; this = ol->this;

			word heapsize = (word) heap->end - (word) heap->begin;
			if ((heapsize / (1024*1024)) >= ol->max_heap_size)
				breaked |= 8; // will be passed over to mcp at thread switch
		}

		ip = (unsigned char *) &this[1];
		goto mainloop; // let's execute
	}
	else
		ERROR(257, this, INULL); // not callable

mainloop:;
	// free numbers: 29(ncons), 30(ncar), 31(ncdr)

	// ip - счетчик команд (опкод - младшие 6 бит команды, старшие 2 бита - модификатор(если есть) опкода)
	// Rn - регистр машины (R[n])
	// An - регистр, на который ссылается операнд N (записанный в параметре n команды, начиная с 0)
	// todo: добавить в комменты к команде теоретическое количество тактов на операцию

	// todo: add "NOP" function (may be 0x0 ?)
	// todo: add "HLT" function (may be 0x0 ?)

	// безусловные переходы
	#	define GOTO   2       // jmp a, nargs
	//#	define GOTO_CODE 18   //
	//#	define GOTO_PROC 19   //
	//#	define GOTO_CLOS 21   //

	// управляющие команды:
	#	define APPLY 20 // apply-cont = 20+64
	#	define RET   24
	#	define RUN   50
	#	define ARITY_ERROR 17

	#	define SYS   27

	// 3, 4: OCLOSE
	// 6, 7: CLOSE1

		// список команд смотреть в assembly.scm
	#	define LDI   13       // LDE (13), LDN (77), LDT (141), LDF (205)
	#	define LD    14

	#	define REFI   1       // refi a, p, t:   Rt = Ra[p], p unsigned (indirect-ref from-reg offset to-reg)
	#	define MOVE   9       //
	#	define MOV2   5       //

	#	define JEQ    8       // jeq
	#	define JP    16       // JZ (16), JN (80), JT (144), JF (208)
	#	define JAF   25       // jafx (89)

		// примитивы языка:
	#	define VMNEW 23    // make object
	#	define VMRAW 60    // make raw object
	#	define RAWQ  48    // raw? (временное решение пока не придумаю как от него совсем избавиться)

	#	define CONS  51

	#	define TYPE  15
	#	define SIZE  36
	#	define CAST  22

	#	define CAR   52
	#	define CDR   53
	#	define REF   47

		// ?
	#	define SETREF 45
	#	define SETREFE 10

		// ?
	#	define EQ    54
	#	define LESS  44

	#	define CLOCK 61 // todo: remove and change to SYSCALL_GETTIMEOFDATE

	#	define SYSCALL 63
			// read, write, open, close must exist
	#		define SYSCALL_READ 0
	#		define SYSCALL_WRITE 1
	#		define SYSCALL_OPEN 2
	#		define SYSCALL_CLOSE 3
	#		define SYSCALL_STAT 4 // same for fstat and lstat
	// 5, 6 - free
	//#		define SYSCALL_POLL 7
	// 12 - reserved for memory functions
	#		define SYSCALL_BRK 12
	// 14 - todo: set signal handling

	#		ifndef SYSCALL_IOCTL
	#		define SYSCALL_IOCTL 16
	#		endif
	#		define SYSCALL_IOCTL_TIOCGETA 19

	#		define SYSCALL_NANOSLEEP 35
	#		define SYSCALL_SENDFILE 40

	#		define SYSCALL_EXIT 60
	#		define SYSCALL_GETDENTS 78

	#		define SYSCALL_GETTIMEOFDATE 96

	#		ifndef SYSCALL_GETRLIMIT
	#		define SYSCALL_GETRLIMIT 97
	#		endif
	#		ifndef SYSCALL_GETRUSAGE
	#		define SYSCALL_GETRUSAGE 98
	#		endif
	#		ifndef SYSCALL_SYSINFO
	#		define SYSCALL_SYSINFO 99
	#		endif
	#		if SYSCALL_SYSINFO
	#		 ifndef _WIN32 // win32 does not have sysinfo
	#			include <sys/sysinfo.h>
	#		 endif
	#		endif

	#		ifndef SYSCALL_PRCTL
	#		define SYSCALL_PRCTL 157
	#		endif
	#		define SYSCALL_KILL 62
	#		define SYSCALL_TIME 201

	#		define SYSCALL_DLOPEN 174
	#		define SYSCALL_DLCLOSE 176
	#		define SYSCALL_DLSYM 177
	#		define SYSCALL_DLERROR 178
	#		define SYSCALL_MKCB 175

		// tuples, trees
	#	define UNREEL 35   // list -> typed tuple
	#	define TUPLEAPPLY 32
	#	define FFAPPLY 49

	#	define MKRED    43
	#	define MKBLACK  42
	#	define FFTOGGLE 46
	#	define FFREDQ   41
	#	define FFRIGHTQ 37

		// ALU
	#	define ADDITION       38
	#	define DIVISION       26
	#	define MULTIPLICATION 39
	#	define SUBTRACTION    40
	#	define BINARY_AND     55
	#	define BINARY_OR      56
	#	define BINARY_XOR     57
	#	define SHIFT_RIGHT    58
	#	define SHIFT_LEFT     59

	// ENTRY LOOP POINT
	int op;//operation to execute:
loop:;
	switch ((op = *ip++) & 0x3F) {
	case 0: // todo: change 0 to NOP, add new code for super_dispatch
		op = (ip[0] << 8) | ip[1]; // big endian
		// super_dispatch: run user instructions
		switch (op) {
		/* AUTOGENERATED INSTRUCTIONS */
		// TODO: JIT!
		//	https://gcc.gnu.org/onlinedocs/gcc-5.1.0/jit/intro/tutorial04.html
		default:
			ERROR(258, F(op), ITRUE);
		}
		goto apply; // ???

	// free commands
	#ifdef HAS_PINVOKE
	/*		case 33: { // IN ref-atom, len
		int len = untoi(A1);
		word* address = car (A0);
		A2 = new_bytevector (TBVEC, len);
		bytecopy(address, &car(A2), len);

		ip += 3; break;
	}*/
	#endif

	case GOTO: // (10%)
		this = (word *)A0;
		acc = ip[1];
		goto apply;
	//	case GOTO_CODE:
	//		this = (word *)A0; acc = ip[1];
	//		ip = (unsigned char*) &this[1];
	//		goto invoke;
	//	case GOTO_PROC:
	//		this = (word *)A0; acc = ip[1];
	//		R1 = (word) this;
	//		this = (word *) this[1];
	//		ip = (unsigned char*) &this[1];
	//		goto invoke;
	//	case GOTO_CLOS:
	//		this = (word *)A0; acc = ip[1];
	//		R1 = (word) this;
	//		this = (word *) this[1];
	//		R2 = (word) this;
	//		this = (word *) this[1];
	//		ip = (unsigned char*) &this[1];
	//		goto invoke;

	// apply
	// todo:? include apply-tuple, apply-values and apply-ff to the APPLY
	case APPLY: { // (0%)
		int reg, arity;
		if (op == APPLY) { // normal apply: cont=r3, fn=r4, a0=r5,
			reg = 4; // include cont
			arity = 1;
			this = (word *) R[reg];
			acc -= 3; // ignore cont, function and stop before last one (the list)
		}
		else { // apply-cont (_sans_cps apply): func=r3, a0=r4,
			reg = 3; // include cont
			arity = 0;
			this = (word *) R[reg];
			acc -= 2; // ignore function and stop before last one (the list)
		}

		while (acc--) { // move explicitly given arguments down by one to correct positions
			R[reg] = R[reg+1]; // copy args down
			reg++;
			arity++;
		}
		word *lst = (word *) R[reg+1];

		while (is_pair(lst)) { // unwind argument list
			// FIXME: unwind only up to last register and add limited rewinding to arity check
			// тут бага, количество регистров может стать больше, чем надо, и пиздец. todo: исправить!!
			// todo: исправить с помощью динамического количества регистров!
			if (reg > NR) { // dummy handling for now
				// TODO: add changing the size of R array!
				STDERR("TOO LARGE APPLY");
				exit(3);
			}
			R[reg++] = car (lst);
			lst = (word *) cdr (lst);
			arity++;
		}
		acc = arity;

		goto apply;
	}

	case RET: // (3%) return value
		this = (word *) R[3];
		R[3] = A0;
		acc = 1;

		goto apply;

	// return to continuation?
	case SYS: // (1%) sys continuation op arg1 arg2
		this = (word *) R[0];
		R[0] = IFALSE; // let's call mcp
		R[3] = A1; R[4] = A0; R[5] = A2; R[6] = A3;
		acc = 4;
		if (ticker > 10)
			bank = ticker; // deposit remaining ticks for return to thread
		ticker = TICKS;

		goto apply;

	case RUN: { // (1%) run thunk quantum
	//			if (ip[0] != 4 || ip[1] != 5)
	//				STDERR("run R[%d], R[%d]", ip[0], ip[1]);
		this = (word *) A0;
		R[0] = R[3];
		ticker = bank ? bank : uvtoi (A1);
		bank = 0;
		CHECK(is_reference(this), this, RUN);

		word hdr = *this;
		if (thetype (hdr) == TTHREAD) {
			int pos = hdrsize(hdr) - 1;
			word code = this[pos];
			acc = pos - 3;
			while (--pos)
				R[pos] = this[pos];
			ip = ((unsigned char *) code) + W;
			break;  // no apply, continue
		}
		// else call a thunk with terminal continuation:
		R[3] = IHALT; // exit via R0 when the time comes
		acc = 1;

		goto apply;
	}
	// ошибка арности
	case ARITY_ERROR: // (0%)
		// TODO: добавить в .scm вывод ошибки четности
		ERROR(17, this, F(acc));
		break;


	/************************************************************************************/
	// операции с данными
	//	смотреть "vm-instructions" в "lang/assembly.scm"
	case LDI: {  // (1%) 13,  -> ldi(lde, ldn, ldt, ldf){2bit what} [to]
		static
		const word I[] = { IEMPTY, INULL, ITRUE, IFALSE };
		A0 = I[op>>6];
		ip += 1; break;
	}
	case LD: // (5%)
		A1 = F(ip[0]);
		ip += 2; break;


	case REFI: { // (24%) 1,  -> refi a, p, t:   Rt = Ra[p], p unsigned
		word* Ra = (word*)A0; A2 = Ra[ip[1]]; // A2 = A0[p]
		ip += 3; break;
	}
	case MOVE: // (3%) move a, t:      Rt = Ra
		A1 = A0;
		ip += 2; break;
	case MOV2: // (6%) mov2 from1 to1 from2 to2
		A1 = A0;
		A3 = A2;
		ip += 4; break;


	// условные переходы
	case JEQ: // (5%) jeq a b o, extended jump
		if (A0 == A1) // 30% for "yes"
			ip += (ip[3] << 8) + ip[2]; // little-endian
		ip += 4; break;

	case JP: { // (10%) JZ, JN, JT, JF a hi lo
		static
		const word I[] = { F(0), INULL, IEMPTY, IFALSE };
		if (A0 == I[op>>6]) // 49% for "yes"
			ip += (ip[2] << 8) + ip[1]; // little-endian
		ip += 3; break;
	}

	// jmp if arity not equal (arity failed)
	case JAF: { // (13%) jmp-nargs (>=) a hi lo
		int arity = ip[0];
		if (acc == arity) { // 99% for "yes"
			if (op & 0x40) // add empty extra arg list
				R[acc + 3] = INULL;
		}
		else
		if (acc > arity && (op & 0x40)) { // JAFx
			word tail = INULL;  // todo: no call overflow handling yet
			while (acc > arity) {
				tail = (word)new_pair (R[acc + 2], tail);
				acc--;
			}
			R[acc + 3] = tail;
		}
		else
			ip += (ip[1] << 8) | ip[2];

		ip += 3; break;
	}


	case 3: OCLOSE(TCLOS); break; //continue; (2%)
	case 4: OCLOSE(TPROC); break; //continue; (1%)
	case 6: CLOSE1(TCLOS); break; //continue; (2%)
	case 7: CLOSE1(TPROC); break; //continue; (1%)

	// others: 1,2,3 %%)

	/************************************************************************************/
	// более высокоуровневые конструкции
	//	смотреть "owl/primop.scm" и "lang/assemble.scm"

	// make object
	case VMNEW: { // mkt t s f1 .. fs r
		word type = *ip++;
		word size = *ip++ + 1; // the argument is n-1 to allow making a 256-tuple with 255, and avoid 0 length objects
		word *p = new (type, size+1), i = 0; // s fields + header
		while (i < size) {
			p[i+1] = R[ip[i]];
			i++;
		}
		R[ip[i]] = (word) p;
		ip += size+1; break;
	}

	// todo: add numeric argument as "length" parameter
	case VMRAW: { // (vm:raw type list|size)
		int type = uvtoi(A0);
		int len = 0;
		// size or #null ?
		if (is_value(A1)) {
			len = (A1 == INULL)  ? 0
				: (A1 == IFALSE) ? 0 // todo: is it required?
				: uvtoi(A1);
			// if there are no place for raw object:
			if (len / sizeof(word) > heap->end - fp) {
				int sp = ip - (unsigned char*)this;

				heap->fp = fp; ol->this = this;
				ol->gc(ol, len / sizeof(word));
				fp = heap->fp; this = ol->this;

				ip = (unsigned char*)this + sp;
			}
			word *raw = new_bytevector (type, len);
			A2 = (word) raw;
		}
		// list?
		else {
			word *lst = (word*) A1;

			word* p = lst;
			while (is_pair(p)) { // check the list
				len++;
				p = (word*)cdr (p);
			}

			if ((word) p == INULL) {
				#if VMRAW_CHECK
				// it's safe to trunk not rounding
				if (len / sizeof(word) > heap->end - fp) {
					heap->fp = fp; ol->this = this;
					ol->gc(ol, len / sizeof(word));
					fp = heap->fp; this = ol->this;
				}
				#endif
				word *raw = new_bytevector (type, len);

				unsigned char *pos;
				pos = (unsigned char *) &raw[1];
				p = lst;
				while ((word) p != INULL) {
					*pos++ = uvtoi(car(p)) & 255;
					p = (word*)cdr(p);
				}

				while ((word)pos % sizeof(word)) // clear the padding bytes,
					*pos++ = 0;                  //             required!!!
				A2 = (word)raw;
			}
			else
				A2 = IFALSE;
		}
		ip += 3; break; }

	case RAWQ: {  // raw? a -> r : Rr = (raw? Ra)
		word* T = (word*) A0;
		if (is_reference(T) && is_rawobject(*T))
			A1 = ITRUE;
		else
			A1 = IFALSE;
		ip += 2; break;
	}

	// операции посложнее
	case CONS:   // cons a b -> r : Rr = (cons Ra Rb)
		A2 = (word) new_pair(A0, A1); // видимо, вызывается очень часто, так как замена на макрос дает +10% к скорости
		ip += 3; break;


	case TYPE: { // type o -> r
		word T = A0;
		if (is_reference(T))
			T = *((word *) (T)); // todo: add RAWNESS to this (?)
		A1 = F(thetype (T));
		ip += 2; break;
	}

	case SIZE: { // size o -> r
	//			word T = A0;
	//			A1 = is_value(T) ? IFALSE : F(hdrsize(*(word*)T) - 1);
	//
		word* T = (word*) A0;
		if (is_value(T))
			A1 = IFALSE;
		else {
			word hdr = *T;
			if (is_rawobject(hdr))
				A1 = F((hdrsize(hdr)-1)*W - padsize(hdr));
			else
				A1 = F(hdrsize(*(word*)T) - 1);
		}
		ip += 2; break;
	}

	// todo: переделать! и вообще, найти как от этой команды избавиться!
	case CAST: { // cast obj type -> result
		if (!is_value(A1))
			break;
		word T = A0;
		word type = uvtoi(A1) & 63;

	//			if (type == TPORT && thetype(T) == TINT) {
	//				A2 = IFALSE;
	//			}
		// todo: добавить каст с конверсией. например, из большого целого числа в handle или float
		// это лучше сделать тут, наверное, а не отдельной командой
		if (is_value(T)) {
			word val = value(T);
			if (type == TPORT) {
				//if (val <= 2)
					A2 = make_port(val);
				//else
				//	A2 = IFALSE;
			}
			else
				A2 = make_value(type, val);
		}
		else
		{
			// make a clone of more desired type
			word* ob = (word*)T;
			word hdr = *ob++;
			int size = hdrsize(hdr);
			word *newobj = new (size);
			word *res = newobj;
			/* (hdr & 0b...11111111111111111111100000000111) | tttttttt000 */
			//*newobj++ = (hdr&(~2040))|(type<<TPOS);
			*newobj++ = (hdr & (~252)) | (type << TPOS); /* <- hardcoded ...111100000011 */
			wordcopy(ob, newobj, size-1);
			A2 = (word)res;
		}

		ip += 3; break;
	}


	case CAR: {  // car a -> r
		word T = A0;
		CHECK(CAR_CHECK(T), T, CAR);
		A1 = car(T);//((word*)T)[1];
		ip += 2; break;
	}

	case CDR: {  // cdr a -> r
		word T = A0;
		CHECK(CDR_CHECK(T), T, CDR);
		A1 = cdr(T);//((word*)T)[2];
		ip += 2; break;
	}

	case REF: {  // ref t o -> r
		word *p = (word *) A0;
		if (!is_reference(p))
			A2 = IFALSE;
		else {
			word hdr = *p;
			if (is_rawobject(hdr)) { // raw data is #[hdrbyte{W} b0 .. bn 0{0,W-1}]
				word pos = uvtoi (A1);
				word size = ((hdrsize(hdr)-1)*W) - padsize(hdr);
				if (pos >= size)
					A2 = IFALSE;
				else
					A2 = F(((unsigned char *) p)[pos+W]);
			}
			else {
				word pos = uvtoi (A1);
				word size = hdrsize(hdr);
				if (!pos || size <= pos) // tuples are indexed from 1
					A2 = IFALSE;
				else
					A2 = p[pos];
			}
		}
		ip += 3; break;
	}


	case SETREF: { // (set-ref object position value), position starts from 1
		word *p = (word *)A0;
		word pos = uvtoi(A1);

		if (!is_reference(p))
			A3 = IFALSE;
		else
		if (is_rawobject(*p)) {
			CHECK(is_value(A2), A2, 10001)
			word hdr = *p;
			word size = hdrsize (hdr);
			word *newobj = new (size);
			for (ptrdiff_t i = 0; i < size; i++)
				newobj[i] = p[i];
			if (pos < (size-1)*sizeof(word) - padsize(hdr) + 1)
				((char*)&car(newobj))[pos] = (char)svtoi(A2);
			A3 = (word)newobj;
		}
		else
		if (hdrsize(*p) < pos || !pos)
			A3 = IFALSE;
		else {
			//if (is_tuple(p)) {
			word hdr = *p;
			word size = hdrsize (hdr);
			word *newobj = new (size);
			word val = A2;
			for (ptrdiff_t i = 0; i < size; i++)
				newobj[i] = p[i];
			newobj[pos] = val;
			A3 = (word)newobj;
		}
		ip += 4; break; }

	case SETREFE: { // (set-ref! variable position value)
		word *p = (word *)A0;
		word pos = uvtoi (A1);

		CHECK(is_value(A2), A2, 10001); // todo: move to silent return IFALSE

		A3 = IFALSE;
		if (is_reference(p)) {
			if (is_rawobject(*p)) {
				if (pos < (hdrsize(*p)-1)*W - padsize(*p) + 1)
					((char*)&car(p))[pos] = (char) svtoi(A2);
				A3 = (word) p;
			}
			else
			if (pos >= 1 && pos <= hdrsize(*p)) {
				p[pos] = A2;
				A3 = (word) p;
			}
		}

		ip += 4; break; }

	case EQ: // (eq? a b)
		A2 = (A0 == A1) ? ITRUE : IFALSE;
		ip += 3; break;

	case LESS: { // (less? a b)
		word a = A0;
		word b = A1;

		A2 = is_value(a)
			? is_value(b)    // imm < alloc
				? a < b
					? ITRUE
					: IFALSE
				: ITRUE
			: is_value(b)    // alloc > imm
				? IFALSE
				: a < b
					? ITRUE
					: IFALSE;
		ip += 3; break; }


	// АЛУ (арифметическо-логическое устройство)
	case ADDITION: { // vm:add a b  r o, types prechecked, signs ignored, assume fixnumbits+1 fits to machine word
		int_t r = value(A0) + value(A1);
		A2 = F(r & FMAX);
		A3 = (r & HIGHBIT) ? ITRUE : IFALSE; // overflow?
		ip += 4; break; }
	case SUBTRACTION: { // vm:sub a b  r u, args prechecked, signs ignored
		int_t r = (value(A0) | HIGHBIT) - value(A1);
		A2 = F(r & FMAX);
		A3 = (r & HIGHBIT) ? IFALSE : ITRUE; // unsigned?
		ip += 4; break; }

	case MULTIPLICATION: { // vm:mul a b l h
		big_t r = (big_t) value(A0) * (big_t) value(A1);
		A2 = F(r & FMAX);
		A3 = F(r>>FBITS); //  & FMAX)
		ip += 4; break; }
	case DIVISION: { // vm:div ah al b  qh ql r, b != 0, int64(32) / int32(16) -> int64(32), as fix-es
		big_t a = (big_t) value(A1) | (((big_t) value(A0)) << FBITS);
		big_t b = (big_t) value(A2);

		// http://stackoverflow.com/questions/7070346/c-best-way-to-get-integer-division-and-remainder
		big_t q = a / b;
		big_t r = a % b;

		A3 = F(q>>FBITS);
		A4 = F(q & FMAX);
		A5 = F(r);

		ip += 6; break; }


	case BINARY_AND: // vm:and a b r, prechecked
		A2 = (A0 & A1);
		ip += 3; break;
	// disjunction
	case BINARY_OR:  // vm:or a b r, prechecked
		A2 = (A0 | A1);
		ip += 3; break;
	case BINARY_XOR: // vm:xor a b r, prechecked
		A2 = (A0 ^ (A1 & (FMAX << IPOS))); // inherit a's type info
		ip += 3; break;

	case SHIFT_RIGHT: { // vm:shr a b hi lo
		big_t r = ((big_t) value(A0)) << (FBITS - value(A1));
		A2 = F(r>>FBITS);
		A3 = F(r & FMAX);
		ip += 4; break; }
	case SHIFT_LEFT: { // vm:shl a b hi lo
		big_t r = ((big_t) value(A0)) << (value(A1));
		A2 = F(r>>FBITS);
		A3 = F(r & FMAX);
		ip += 4; break; }


	// todo: add the instruction name
	case 28: {// (vm:endianness)
		union
		{
			int_t l;
			char c[sizeof (int_t)];
		} u;

		u.l = 1;
		int endianess =
		    (u.c[0] == 1) ? 1 :                  // little-endian
		    (u.c[sizeof (int_t) - 1] == 1) ? 2 : // big-endian
		    (0);                                 // unknown
		A0 = F(endianess);
		ip += 1; break; }
	case 29: // (vm:wordsize)
		A0 = F(W);
		ip += 1; break;
	case 30: // (fxmax)
		A0 = F(FMAX);
		ip += 1; break;
	case 31: // (fxmbits)
		A0 = F(FBITS);
		ip += 1; break;

	// (vm:version)
	case 62: // get virtual machine info
		A0 = (word) new_pair(TPAIR,
				new_string(__OLVM_NAME__,    sizeof(__OLVM_NAME__)   -1),
				new_string(__OLVM_VERSION__, sizeof(__OLVM_VERSION__)-1));
		ip += 1; break;

	/*	case 11: { // (set-car! pair value)
		word *pair = (word *)A0;
		word cargo = A1;

		// we can't set ref as part of pair due to gc specific
		CHECK(is_pair(pair), pair, 11);
		CHECK(is_value(cargo), cargo, 11);

		car(pair) = cargo;

		A2 = A0;
		ip += 3; break;
	}
	case 12: { // (set-cdr! pair cargo)
		word *pair = (word *)A0;
		word cargo = A1;

		// case as (set-car!)
		CHECK(is_pair(pair), pair, 12);
		CHECK(is_value(cargo), cargo, 12);

		cdr(pair) = cargo;

		A2 = A0;
		ip += 3; break;
	}*/


	/*	case LISTUPLE: { // listuple type size lst to
		word type = uvtoi (A0);
		word size = uvtoi (A1);
		word list = A2;
		word *p = new (size+1);
		A3 = (word) p;
		*p++ = make_header(type, size+1);
		while (size--) {
			CHECK(is_pair(list), list, LISTUPLE);
			*p++ = car (list);
			list = cdr (list);
		}
		ip += 4; break;
	}*/
	// make typed reference from list
	case UNREEL: {
		word type = uvtoi (A0);
		word list = A1;

		// think: проверку можно убрать ради скорости
		if (is_pair(list)) {
			word *ptr = fp;
			A2 = (word)ptr;

			word* me = ptr;
			while (list != INULL) {
				*++fp = car (list);
				list = cdr (list);
			}
			*me = make_header(type, ++fp - ptr);
		}
		else {
			A2 = IFALSE;
		}
		ip += 3; break;
	}



	// bind tuple to registers, todo: rename to bind-t or bindt or bnt
	case TUPLEAPPLY: { /* bind <tuple > <n> <r0> .. <rn> */
		word *tuple = (word *) R[*ip++];
		//CHECK(is_reference(tuple), tuple, BIND);

		word pos = 1, n = *ip++;
		//word hdr = *tuple;
		//CHECK(!(is_raw(hdr) || hdrsize(hdr)-1 != n), tuple, BIND);
		while (n--)
			R[*ip++] = tuple[pos++];

		break;
	}

	/** ff's ---------------------------------------------------
	 *
	 */
	// bind ff to registers
	case FFAPPLY: { // ff:bind <node >l k v r   - bind node left key val right, filling in #false when implicit
		word *ff = (word *) A0;
		word hdr = *ff++;
		A2 = *ff++; // key
		A3 = *ff++; // value
		switch (hdrsize(hdr)) {
		case 3: A1 = A4 = IEMPTY; break;
		case 4:
			if (hdr & (1 << TPOS)) // has right?
				A1 = IEMPTY, A4 = *ff;
			else
				A1 = *ff, A4 = IEMPTY;
			break;
		default:
			A1 = *ff++;
			A4 = *ff;
		}
		ip += 5; break;
	}

	// create red/black node
	case MKBLACK: // ff:black l k v r t
	case MKRED: { // ff:red l k v r t
		word t = op == MKBLACK ? TFF : TFF|FFRED;
		word l = A0;
		word r = A3;

		word *me;
		if (l == IEMPTY) {
			if (r == IEMPTY)
				me = new (t, 3);
			else {
				me = new (t|FFRIGHT, 4);
				me[3] = r;
			}
		}
		else
		if (r == IEMPTY) {
			me = new (t, 4);
			me[3] = l;
		}
		else {
			me = new (t, 5);
			me[3] = l;
			me[4] = r;
		}
		me[1] = (word) A1; // k
		me[2] = (word) A2; // v

		A4 = (word) me;
		ip += 5; break;
	}

	// toggle node color
	case FFTOGGLE: { // ff:toggle
		word *node = (word *) A0;
		assert (is_reference(node));

		word *p = fp;
		A1 = (word)p;

		word h = *node++;
		*p++ = (h ^ (FFRED << TPOS));
		switch (hdrsize(h)) {
			case 5:  *p++ = *node++;
			case 4:  *p++ = *node++;
			default: *p++ = *node++;
					 *p++ = *node++;
		}
		fp = (word*) p;
		ip += 2; break;
	}

	// is node red predicate
	case FFREDQ: { // ff:red? node r
		word node = A0;
		if (is_reference(node)) // assert to IEMPTY || is_reference() ?
			node = *(word*)node;
		if ((thetype (node) & (0x3C | FFRED)) == (TFF|FFRED))
			A1 = ITRUE;
		else
			A1 = IFALSE;
		ip += 2; break;
	}

	// is node right predicate?
	case FFRIGHTQ: { // ff:right? node r
		word node = A0;
		if (is_reference(node)) // assert to IEMPTY || is_reference() ?
			node = *(word*)node;
		if ((thetype (node) & (0x3C | FFRIGHT)) == (TFF|FFRIGHT))
			A1 = ITRUE;
		else
			A1 = IFALSE;
		ip += 2; break;
	}


	// ---------------------------------------------------------
	case CLOCK: { // clock <secs> <ticks>
		struct timeval tp;
		gettimeofday(&tp, NULL);

		A0 = (word) itoun (tp.tv_sec);
		A1 = (word) itoun (tp.tv_usec / 1000);
		ip += 2; break;
	}

	// этот case должен остаться тут - как последний из кейсов
	// http://docs.cs.up.ac.za/programming/asm/derick_tut/syscalls.html (32-bit)
	// https://filippo.io/linux-syscall-table/
	case SYSCALL: { // sys-call (was sys-prim) op arg1 arg2 arg3  r1
		// main link: http://man7.org/linux/man-pages/man2/syscall.2.html
		//            http://man7.org/linux/man-pages/dir_section_2.html
		// linux syscall list: http://blog.rchapman.org/post/36801038863/linux-system-call-table-for-x86-64
		//                     http://www.x86-64.org/documentation/abi.pdf
		word* result = (word*)IFALSE;  // default returned value is #false
	//	CHECK(is_fixed(A0) && thetype (A0) == TFIX, A0, SYSCALL);
		word op = uvtoi (A0);
		word a = A1, b = A2, c = A3;

		switch (op + sandboxp) {

		// (READ fd count) -> buf
		// http://linux.die.net/man/2/read
		// count<0 means read all
		case SYSCALL_READ + SECCOMP:
		case SYSCALL_READ: {
			CHECK(is_port(a), a, SYSCALL);
			int portfd = port(a);
			int size = svtoi (b); // в байтах

			size = ((size + W - 1) / W) + 1; // в словах
			if (size < 0)
				size = (heap->end - fp);
			else
			if (size > (heap->end - fp)) {
				ol->gc(ol, size);
				fp = heap->fp; // не забывать про изменение fp в процессе сборки!
			}

			int got;
	#ifdef _WIN32
			if (!_isatty(portfd) || _kbhit()) { // we don't get hit by kb in pipe
				got = read(portfd, (char *) &fp[1], size);
			} else {
				got = -1;
				errno = EAGAIN;
			}
			// Win32 socket workaround
			if (got == -1 && errno == EBADF) {
				got = recv(portfd, (char *) &fp[1], size, 0);
	#ifdef _WIN32
				if (got == -1 && errno == EBADF)
					if (!ReadFile(portfd, (char *) &fp[1], size, &got, NULL)) {
						result = (word*)ITRUE;
						break;
					}
	#endif
				if (got < 0)
					if (WSAGetLastError() == WSAEWOULDBLOCK)
						errno = EAGAIN;
			}
	#else
			got = read(portfd, (char *) &fp[1], size);
	#endif

			if (got > 0) {
				// todo: обработать когда приняли не все,
				//	     вызвать gc() и допринять. и т.д.
				result = new_bytevector (TBVEC, got);
			}
			else if (got == 0)
				result = (word*)IEOF;
			else if (errno == EAGAIN) // (may be the same value as EWOULDBLOCK) (POSIX.1)
				result = (word*)ITRUE;
			break;
		}

		// (WRITE fd buffer size) -> wrote
		// http://linux.die.net/man/2/write
		// size<0 means write all
		// n if wrote, 0 if busy, #false if error (argument or write)
		case SYSCALL_WRITE + SECCOMP:
		case SYSCALL_WRITE: {
			CHECK(is_port(a), a, SYSCALL);
			int portfd = port(a);
	//				CHECK(is_port(a) || (is_value(a) && (uvtoi(a) <= 2)), a, SYSCALL);
	//				int portfd = is_port(a) ? car (a) : uvtoi(a);
			int size = svtoi (c);

			word *buff = (word *) b;
			if (is_value(buff))
				break;

			int length = (hdrsize(*buff) - 1) * sizeof(word); // todo: pads!
			if (size > length || size == 0)
				size = length;

			int wrote;

			wrote = write(portfd, (char*)&buff[1], size);

	#ifdef _WIN32
			// Win32 socket workaround
			if (wrote == -1 && errno == EBADF) {
				wrote = send(portfd, (char*) &buff[1], size, 0);
				if (wrote == -1 && errno == EBADF) {
					if (!WriteFile(portfd, (char*) &buff[1], size, &wrote, NULL))
						wrote = -1;
				}
			}
	#endif

			if (wrote > 0)
				result = (word*) itoun (wrote);
			else if (errno == EAGAIN || errno == EWOULDBLOCK)
				result = (word*) itoun (0);

			break;
		}

#ifndef __asmjs__
		// (OPEN "path" mode)
		// http://man7.org/linux/man-pages/man2/open.2.html
		case SYSCALL_OPEN: {
			CHECK(is_string(a), a, SYSCALL);
			word* s = & car(a);
			int mode = uvtoi (b);
			mode |= O_BINARY | ((mode > 0) ? O_CREAT | O_TRUNC : 0);

			int file = open((char*)s, mode, (S_IRUSR | S_IWUSR));
			if (file < 0)
				break;

			struct stat sb;
			if (fstat(file, &sb) < 0 || S_ISDIR(sb.st_mode)) {
				close(file);
				break;
			}
			set_blocking(file, 0);
			result = (word*)make_port(file);

			break;
		}

		// CLOSE
		case SYSCALL_CLOSE: {
			CHECK(is_port(a), a, SYSCALL);
			int portfd = port(a);

			if (close(portfd) == 0)
				result = (word*)ITRUE;
	#ifdef _WIN32
			// Win32 socket workaround
			else if (errno == EBADF) {
				if (closesocket(portfd) == 0)
					result = (word*)ITRUE;
			}
	#endif

			break;
		}

		case SYSCALL_STAT: {
			struct stat st;

			if (is_port(a)) {
				if (fstat(port (a), &st) < 0)
					break;
			}
			else
			if (is_string(a)) {
				if (b == ITRUE) {
					if (lstat((char*) &car (a), &st) < 0)
						break;
				}
				else {
					if (stat((char*) &car (a), &st) < 0)
						break;
				}
			}
			else
				break;

			result = new_tuple(
					itoun(st.st_dev),    // устройство
					itoun(st.st_ino),    // inode
					itoun(st.st_mode),   // режим доступа
					itoun(st.st_nlink),  // количество жестких ссылок
					itoun(st.st_uid),    // идентификатор пользователя-владельца
					itoun(st.st_gid),    // идентификатор группы-владельца
					itoun(st.st_rdev),   // тип устройства (если это устройство)
					itoun(st.st_size),   // общий размер в байтах
					itoun(0), // itoun(st.st_blksize),// размер блока ввода-вывода в файловой системе
					itoun(0), // itoun(st.st_blocks), // количество выделенных блоков
					// Since Linux 2.6, the kernel supports nanosecond
					//   precision for the following timestamp fields.
					// but we do not support this for a while
					itoun(st.st_atime),  // время последнего доступа (в секундах)
					itoun(st.st_mtime),  // время последней модификации (в секундах)
					itoun(st.st_ctime)   // время последнего изменения (в секундах)
			);
			break;
		}

		case SYSCALL_BRK: // get or set memory limit (in mb)
			// b, c is reserved for feature use
			result = itoun (ol->max_heap_size);
			//if (a == F(0))
			//	ol->gc(0);
			if (is_number(a))
				ol->max_heap_size = uvtoi (a);
			break;

		// IOCTL (syscall 16 fd request #f)
		case SYSCALL_IOCTL + SECCOMP:
		case SYSCALL_IOCTL: {
			if (!is_port(a))
				break;

			int portfd = port(a);
			int ioctl = uvtoi(b);

			switch (ioctl + sandboxp) {
				case SYSCALL_IOCTL_TIOCGETA: {
					#ifdef _WIN32
						if (_isatty(portfd))
					#else
						struct termios t;
						if (tcgetattr(portfd, &t) != -1)
					#endif
							result = (word*)ITRUE;
					break;
				}
				case SYSCALL_IOCTL_TIOCGETA + SECCOMP: {
					if ((portfd == STDIN_FILENO) || (portfd == STDOUT_FILENO) || (portfd == STDERR_FILENO))
						result = (word*)ITRUE;
					break;
				}
			}
			break;
		}

		case SYSCALL_SENDFILE + SECCOMP:
		case SYSCALL_SENDFILE: { // (syscall 40 fd file-fd size)
			if (!is_port(a) || !is_port(b))
				break;

			int socket = port(a);
			int filefd = port(b);
			int size = svtoi (c);

			int wrote = sendfile(socket, filefd, NULL, size);
			if (wrote < 0)
				break;

			result = itoun(wrote);
			break;
		}


		// directories
		case 1011: { /* sys-opendir path _ _ -> False | dirobjptr */
			word* A = (word*)a;
			DIR *dirp = opendir((char*) &A[1]);
			if (dirp)
				result = (word*)make_port(dirp);
			break;
		}
		// get directory entry
		case SYSCALL_GETDENTS:
		case 1012: { /* sys-readdir dirp _ _ -> bvec | eof | False */
			CHECK(is_port(a), a, SYSCALL);
			DIR* dirp = (DIR*) port(a);

			struct dirent *dire = readdir(dirp);
			if (!dire) {
				result = (word*)IEOF; // eof at end of dir stream
				break;
			}

			// todo: check the heap overflow!
			unsigned int len;
			len = lenn(dire->d_name, FMAX+1);
			if (len == FMAX+1)
				break; /* false for errors, like too long file names */
			result = new_string(dire->d_name, len);
			break;
		}
		case 1013: /* sys-closedir dirp _ _ -> ITRUE */
			closedir((DIR *)car(a));
			result = (word*)ITRUE;
			break;
		case 1020: { /* chdir path res */
			char *path = ((char *)a) + W;
			if (chdir(path) >= 0)
				result = (word*) ITRUE;
			break;
		}


		// PIPE
		case 22: {
			// TBD.
			break;
		}


		// ==================================================
		//  network part:
		//
		// http://www.kegel.com/c10k.html
	#if HAS_SOCKETS
		// todo: add getsockname() and getpeername() syscalls

		// SOCKET
		case 41: { // socket (todo: options: STREAM or DGRAM)
			// http://beej.us/net2/html/syscalls.html
			// right way: use PF_INET in socket call
	#ifdef _WIN32
			int sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
			unsigned long v = 1;
			ioctlsocket(sock, FIONBIO, &v); // set blocking mode
	#else
			int sock = socket(PF_INET, SOCK_STREAM, 0);
	#endif
			if (sock != -1)
				result = (word*)make_port (sock);
			break;
		}

		// CONNECT
		case 42: { // (connect sockfd host port)
			CHECK(is_port(a), a, SYSCALL);
			int sockfd = port(a);
			word* host = (word*) b; // todo: check for string type
			int port = uvtoi (c);

			struct sockaddr_in addr;
			addr.sin_family = AF_INET;
			addr.sin_addr.s_addr = inet_addr((char *) &host[1]);
			addr.sin_port = htons(port);

			if (addr.sin_addr.s_addr == INADDR_NONE) {
				struct hostent *he = gethostbyname((char *) &host[1]);
				if (he != NULL)
					memcpy(&addr.sin_addr, he->h_addr_list[0], sizeof(addr.sin_addr));
			}

	//				ipfull = (ip[0]<<24) | (ip[1]<<16) | (ip[2]<<8) | ip[3];
	//				addr.sin_addr.s_addr = htonl(ipfull);
			if (connect(sockfd, (struct sockaddr *) &addr, sizeof(struct sockaddr_in)) >= 0)
				result = (word*)ITRUE;
	//				set_blocking(sock, 0);
			break;
		}

		// SHUTDOWN
		// http://linux.die.net/man/2/shutdown
		case 48: { // (shutdown socket #f #f)
			CHECK(is_port(a), a, SYSCALL);
			int socket = port(a);

			// On error, -1 is returned
			if (shutdown(socket, 2) == 0) // both
				result = (word*)ITRUE;

			break;
		}

		// BIND
		// http://linux.die.net/man/2/bind
		case 49: { //  (socket, port, #false) // todo: c for options
			CHECK(is_port(a), a, SYSCALL);
			int sockfd = port(a);
			int port = uvtoi (b);

			// todo: assert on argument types
			struct sockaddr_in interface;
			interface.sin_family = AF_INET;
			interface.sin_port = htons(port);
			interface.sin_addr.s_addr = INADDR_ANY;

			// On success, zero is returned.
			if (bind(sockfd, (struct sockaddr *) &interface, sizeof(interface)) == 0)
				result = (word*)ITRUE;
			break;
		}

		// LISTEN (socket)
		// http://linux.die.net/man/2/listen
		// listen() marks the socket referred to by sockfd as a passive socket, that is,
		// as a socket that will be used to accept incoming connection requests using accept(2).
		case 50: {
			CHECK(is_port(a), a, SYSCALL);
			int sockfd = port(a);

			// On success, zero is returned.
			if (listen(sockfd, 42) == 0) {
	//					set_blocking(sockfd, 0);
				result = (word*)ITRUE;
			}

			break;
		}

		// ACCEPT
		// http://linux.die.net/man/2/accept
		case 43: { // (accept sockfd)
			CHECK(is_port(a), a, SYSCALL);
			int sockfd = port(a);

			struct sockaddr_in addr;
			socklen_t len = sizeof(addr);
			int sock = accept(sockfd, (struct sockaddr *)&addr, &len);
			// On error, -1 is returned
			if (sock < 0)
				break;
	#if _WIN32
			unsigned long mode = 1; // non blocking
			if (ioctlsocket(sock, FIONBIO, &mode) == 0)
	#else
			int flags = fcntl(sock, F_GETFL, 0);
			if (flags < 0)
				break;
			flags = (flags | O_NONBLOCK);
			if (fcntl(sock, F_SETFL, flags) == 0)
	#endif
				result = (word*)make_port (sock);
			break;
		}

		// SELECT
		// http://linux.die.net/man/2/select
		case 23: { // (select sockfd)
			CHECK(is_port(a), a, SYSCALL);
			int sockfd = port(a);
			int timeus = is_number(b) ? untoi (b) : 100000;
			// todo: timeout as "b"

			fd_set fds;
			FD_ZERO(&fds); FD_SET(sockfd, &fds);

			struct timeval timeout = { timeus / 1000000, timeus % 1000000 }; // µs
			if (select(sockfd + 1, &fds, NULL, NULL, &timeout) > 0
					&& FD_ISSET(sockfd, &fds))
				result = (word*)ITRUE;

			break;
		}

		// GETPEERNAME
		// http://linux.die.net/man/2/getpeername
		case 51: { // (getpeername sockfd)
			CHECK(is_port(a), a, SYSCALL);
			int sockfd = port(a);

			// todo: https://svn.code.sf.net/p/plibc/code/trunk/plibc/src/inet_ntop.c
			struct sockaddr_storage peer;
			socklen_t len = sizeof(peer);

			// On success, zero is returned.
			if (getpeername(sockfd, (struct sockaddr *) &peer, &len) != 0)
				break;
	#ifdef _WIN32
			char* ipaddress = inet_ntoa(((struct sockaddr_in *)&peer)->sin_addr);
			unsigned short port = ntohs(((struct sockaddr_in *)&peer)->sin_port);

			result = new_pair(new_string(ipaddress), F(port));

	#else
			unsigned short port;

			if (peer.ss_family == AF_INET) {
				char ipaddress[INET_ADDRSTRLEN];

				struct sockaddr_in *s = (struct sockaddr_in *)&peer;
				port = ntohs(s->sin_port);
				inet_ntop(AF_INET, &s->sin_addr, ipaddress, sizeof ipaddress);
				result = new_pair(new_string(ipaddress), F(port));
			}
			/* temporary disable IP_v6, todo: return back
			else
			if (peer.ss_family == AF_INET6) {
				char ipaddress[INET6_ADDRSTRLEN];

				struct sockaddr_in6 *s = (struct sockaddr_in6 *)&peer;
				port = ntohs(s->sin6_port);
				inet_ntop(AF_INET6, &s->sin6_addr, ipaddress, sizeof ipaddress);
				result = new_pair(new_string(ipaddress), F(port));
			}*/
			else
				break;
	#endif

			break;
		}

	#endif
		// ==================================================


		// todo: http://man7.org/linux/man-pages/man2/nanosleep.2.html
		// TODO: change to "select" call (?)
		case SYSCALL_NANOSLEEP: { // time-in-micro(!)seconds
			CHECK(is_number(a), a, SYSCALL);
			int_t us = untoi (a);

			result = (word*) ITRUE;

			#ifdef __EMSCRIPTEN__
				int_t ms = us / 1000;
				emscripten_sleep(ms);
			#endif
			#ifdef _WIN32// for Windows
				int_t ms = us / 1000;
				Sleep(ms); // in ms
			#endif
			#ifdef __unix__ // Linux, *BSD, MacOS, etc.
				struct timespec ts = { us / 1000000, (us % 1000000) * 1000 };
				struct timespec rem;
				if (nanosleep(&ts, &rem) != 0)
					result = itoun((rem.tv_sec * 1000000 + rem.tv_nsec / 1000));
			#endif
			break;
		}

		// (FORK)
		case 57: {
			//result = (word*) itosv(fork());
			break;
		}

		// (EXECVE program-or-function env (tuple port port port))
		// http://linux.die.net/man/3/execve
		case 59: {
	#if HAS_DLOPEN
			// if a is result of dlsym
			if (is_vptr(a)) {
				// a - function address (port)
				// b - arguments (may be pair with req type in car and arg in cdr - not yet done)
				word* A = (word*)a;
				word* B = (word*)b;
	//					word* C = (word*)c;

				assert ((word)B == INULL || is_pair(B));
	//					assert ((word)C == IFALSE);
				word* (*function)(OL*, word*) = (word* (*)(OL*, word*)) car(A);  assert (function);

				ol->heap.fp = fp;  ol->this = this;
				result = function(ol, B);
				fp = ol->heap.fp;

				// а вдруг вызвали gc?
//				int sub
//				= ip - (unsigned char *) &this[1];
				this = ol->this;
//				ip = (unsigned char *) &this[sub];

				// todo: проверить, но похожу что этот выхов всегда сопровождается вызовом RET
				// а значит мы можем тут делать goto apply, и не заботиться о сохранности ip
				break;
			}
	#endif
			// if a is string:
			// todo: add case (cons program environment)
			if (is_string(a)) {
				char* command = (char*)&car(a);
				#ifdef __unix__
					int child = fork();
					if (child == 0) {
						STDERR("forking %s", command);
						if (is_pair (c)) {
							const int in[3] = { STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO };
							for (ptrdiff_t i = 0; i < sizeof(in) / sizeof(in[0]) && is_pair(c); i++)
								if (is_port(car(c)))
									dup2(port(car(c)), in[i]), c = cdr (c);
						}

						char** args = NULL;
						if (is_pair(b)) {
							word p;
							int l = 1;
							p = b;
							while (p != INULL)
								l++, p = cdr(p);
							char** arg = args = __builtin_alloca(sizeof(char**) * (l + 1));
							p = b;
							while (p != INULL)
								*arg++ = (char*)&caar(p), p = cdr(p);
							*arg = 0;
						}

						exit(execve(command, args, 0));
						assert(0);
					}
					else if (child > 0)
						result = (word*)ITRUE;
				#endif
				#ifdef _WIN32
					STARTUPINFO si;
					ZeroMemory( &si, sizeof(STARTUPINFO) );
					si.cb = sizeof(STARTUPINFO);
					si.dwFlags |= STARTF_USESTDHANDLES;
					if (is_pair(c)) {
						if (is_port(car(c)))
							si.hStdInput = (HANDLE) port(c);
						c = cdr(c);
					}
					if (is_pair(c)) {
						if (is_port(car(c)))
							si.hStdOutput = (HANDLE) port(c);
						c = cdr(c);
					}
					if (is_pair(c)) {
						if (is_port(car(c)))
							si.hStdError = (HANDLE) port(c);
						c = cdr(c);
					}

					// черновой вариант
					char* args = (char*) &car (fp);
					// todo: add length check!
					sprintf(args, "\"%s\"", command);

					if (is_pair(b)) {
						word p = b;
						while (p != INULL) {
							strcat(args, " ");
							strcat(args, (char*)&caar(p)); p = cdr(p);
						}
					}

					PROCESS_INFORMATION pi;
					ZeroMemory(&pi, sizeof(PROCESS_INFORMATION));

					if (CreateProcess(NULL, args, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
						CloseHandle(pi.hThread);
						CloseHandle(pi.hProcess);
						result = itoun(pi.dwProcessId);
					}
				#endif
				break;
			}
			break;
		}

		// wait4
		case 61: {
			#ifdef _WIN32
			int_t pid = untoi(a);

			HANDLE processHandle = OpenProcess(PROCESS_ALL_ACCESS, FALSE, pid);
			WaitForSingleObject(processHandle, INFINITE);
			CloseHandle(processHandle);

			result = (word*) ITRUE;
			#endif
			break;
		}

		// (gettimeofday)
		// todo: change (clock) call to this one
		case SYSCALL_GETTIMEOFDATE: {
			struct timeval tv;
			if (gettimeofday(&tv, NULL) == 0)
				result = new_pair (itoun(tv.tv_sec), itoun(tv.tv_usec));
			break;
		}

		/**
		 * @brief (time format seconds #f)
		 * @arg format return string, else seconds
		 * @arg if seconds == false, get current seconds
		 * @see http://man7.org/linux/man-pages/man2/time.2.html
		 */
		case SYSCALL_TIME: {
			word* B = (word*) b;
			time_t seconds;
			if ((word) B == IFALSE)
				seconds = time (0);
			else if (is_number(B))
				seconds = untoi(B);
			else
				break;
	#if HAS_STRFTIME
			word* A = (word*) a;
			if (is_string(A)) {
				struct tm * timeinfo = localtime(&seconds);
				if (!timeinfo) // error???
					break;
				// The environment variables TZ and LC_TIME are used!
				size_t len = strftime(
						(char*) &(fp[1]),
						(size_t) (heap->end - fp - 1) * sizeof(word),
						(char*) &(A[1]), timeinfo);
				result = new_bytevector(TSTRING, len);
			}
			else
	#endif
				result = itoun (seconds);
			break;
		}

		// EXIT errorcode
		// http://linux.die.net/man/2/exit
		// exit - cause normal process termination, function does not return.
		case 60: {
			if (!sandboxp)
				free(heap->begin); // освободим занятую память
			heap->begin = 0;
			R[3] = a;
			goto done;
			//was: exit(svtoi(a));
		}

		// UNAME (uname)
		// http://linux.die.net/man/2/uname
		case 63: {
			#ifdef _WIN32
			struct utsname
			{
				char sysname[65];  //
				char nodename[65];
				char release[65];
				char version[65];
				char machine[65];
			};

			int uname(struct utsname* out) {
				DWORD nodenamesize = sizeof(out->nodename);
				GetComputerNameA(out->nodename, &nodenamesize);

				SYSTEM_INFO si;
				VOID (WINAPI *GetNativeSystemInfo)(LPSYSTEM_INFO) = (VOID (WINAPI*)(LPSYSTEM_INFO))
						GetProcAddress(GetModuleHandle("kernel32.dll"), "GetNativeSystemInfo");
				if (GetNativeSystemInfo)
					GetNativeSystemInfo(&si);
				else
					GetSystemInfo(&si); // todo: make as getprocaddress

				OSVERSIONINFOEXA oi;
				oi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEXA);
				if (!GetVersionExA((OSVERSIONINFOA*)&oi)) {
					DWORD dwVersion = GetVersion();
					oi.dwMajorVersion = (DWORD)(LOBYTE(LOWORD(dwVersion)));
					oi.dwMinorVersion = (DWORD)(HIBYTE(LOWORD(dwVersion)));

					if (dwVersion < 0x80000000)
						oi.dwBuildNumber = (DWORD)(HIWORD(dwVersion));
					else
						oi.dwBuildNumber = 0;
				}

				strncpy(out->sysname, "Windows", sizeof(out->sysname));
				if (oi.dwPlatformId < VER_PLATFORM_WIN32_NT) {
					oi.dwBuildNumber = (DWORD)LOWORD(oi.dwBuildNumber);
					if (oi.dwMinorVersion == 0) {
						strcat(out->sysname, " 95");
						if (oi.dwBuildNumber >= 1111) {
							strcat(out->sysname, ", OSR2");
							if (oi.dwBuildNumber >= 1212) strcat(out->sysname, ".5");
						}
					}
					else if (oi.dwMinorVersion == 0x90) {
						strcat(out->sysname, " Me");
					}
					else {
						strcat(out->sysname, " 98");
						if (oi.dwBuildNumber >= 2222) strcat(out->sysname, ", Second Edition");
					}
				}
				else {
					if (oi.dwMajorVersion <= 4) {
						strcat(out->sysname, " NT");
						itoa(oi.dwMajorVersion, &out->sysname[strlen(out->sysname)], 10);
						strcat(out->sysname, ".");
						itoa(oi.dwMinorVersion, &out->sysname[strlen(out->sysname)], 10);
						if (oi.dwMajorVersion >= 4) {
							switch (oi.wProductType) {
							case VER_NT_WORKSTATION:       strcat(out->sysname, ", Workstation");       break;
							case VER_NT_DOMAIN_CONTROLLER: strcat(out->sysname, ", Domain Controller"); break;
							case VER_NT_SERVER:            strcat(out->sysname, ", Server");            break;
							}
						}
					}
					else {
						switch (0x100 * oi.dwMajorVersion + oi.dwMinorVersion) {
						case 0x500:
							strcat(out->sysname, " 2000");
							if      (oi.wProductType == VER_NT_WORKSTATION) strcat(out->sysname, " Professional");
							else if (oi.wSuiteMask & VER_SUITE_DATACENTER ) strcat(out->sysname, " Datacenter Server");
							else if (oi.wSuiteMask & VER_SUITE_ENTERPRISE ) strcat(out->sysname, " Advanced Server");
							else strcat(out->sysname, " Server");
							break;

						case 0x501:
							strcat(out->sysname, " XP");
							if (oi.wSuiteMask & VER_SUITE_PERSONAL)
								strcat(out->sysname, " Home Edition");
							else
								strcat(out->sysname, " Professional");
							break;

						case 0x502: {
							#ifndef VER_SUITE_WH_SERVER
								#define VER_SUITE_WH_SERVER 0x00008000
							#endif

							char *name, *type;

							if (GetSystemMetrics(SM_SERVERR2))
								name = "Server 2003 R2";
							else if (oi.wSuiteMask == VER_SUITE_STORAGE_SERVER)
								name = "Storage Server 2003";
							else if (oi.wSuiteMask == VER_SUITE_WH_SERVER)
								name = "Home Server";
							else if (oi.wProductType == VER_NT_WORKSTATION && si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64) {
								name = "XP"; type = "Professional x64 Edition";
							}
							else
								name = "Server 2003";

							if (oi.wProductType != VER_NT_WORKSTATION) {
								if (si.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_IA64) {
									if (oi.wSuiteMask & VER_SUITE_DATACENTER)
										type = "Datacenter Edition for Itanium-based Systems";
									else if (oi.wSuiteMask & VER_SUITE_ENTERPRISE)
										type = "Enterprise Edition for Itanium-based Systems";
								}
								else if (si.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_AMD64) {
									if (oi.wSuiteMask & VER_SUITE_DATACENTER)
										type = "Datacenter x64 Edition";
									else if (oi.wSuiteMask & VER_SUITE_ENTERPRISE)
										type = "Enterprise x64 Edition";
									else
										type = "Standard x64 Edition";
								}
								else {
									if (oi.wSuiteMask & VER_SUITE_COMPUTE_SERVER)  type = "Compute Cluster Edition";
									else if (oi.wSuiteMask & VER_SUITE_DATACENTER) type = "Datacenter Edition";
									else if (oi.wSuiteMask & VER_SUITE_ENTERPRISE) type = "Enterprise Edition";
									else if (oi.wSuiteMask & VER_SUITE_BLADE)      type = "Web Edition";
									else                                           type = "Standard Edition";
								}
							}
							snprintf(out->sysname, sizeof(out->sysname), "Windows %s, %s", name, type);
							break;
						}

						case 0x600: case 0x601: {
						//	const char* ps1;

						//	if (oi.wProductType == VER_NT_WORKSTATION)
						//		ps1 = oi.dwMinorVersion == 0 ? "Vista" : "7";
						//	else
						//		ps1 = oi.dwMinorVersion == 0 ? "Server 2008" : "Server 2008 R2";

						//   DWORD dwType = PRODUCT_UNDEFINED;
						//   if (NULL != (u.f=get_func("GetProductInfo"))) u.GetProductInfo(oi.dwMajorVersion,oi.dwMinorVersion,0,0,&dwType);
						//   switch( dwType ) {
						//	  case PRODUCT_ULTIMATE:          ps2 = "Ultimate Edition";       break;
						//	  case PRODUCT_HOME_PREMIUM:      ps2 = "Home Premium Edition";   break;
						//	  case PRODUCT_HOME_BASIC:        ps2 = "Home Basic Edition";     break;
						//	  case PRODUCT_ENTERPRISE:        ps2 = "Enterprise Edition";     break;
						//	  case PRODUCT_BUSINESS:          ps2 = "Business Edition";       break;
						//	  case PRODUCT_STARTER:           ps2 = "Starter Edition";        break;
						//	  case PRODUCT_CLUSTER_SERVER:    ps2 = "Cluster Server Edition"; break;
						//	  case PRODUCT_DATACENTER_SERVER: ps2 = "Datacenter Edition";     break;
						//	  case PRODUCT_DATACENTER_SERVER_CORE: ps2 = "Datacenter Edition (core installation)"; break;
						//	  case PRODUCT_ENTERPRISE_SERVER: ps2 = "Enterprise Edition";     break;
						//	  case PRODUCT_ENTERPRISE_SERVER_CORE: ps2 = "Enterprise Edition (core installation)"; break;
						//	  case PRODUCT_ENTERPRISE_SERVER_IA64: ps2 = "Enterprise Edition for Itanium-based Systems"; break;
						//	  case PRODUCT_SMALLBUSINESS_SERVER: ps2 = "Small Business Server"; break;
						//	  case PRODUCT_SMALLBUSINESS_SERVER_PREMIUM: ps2 = "Small Business Server Premium Edition"; break;
						//	  case PRODUCT_STANDARD_SERVER:   ps2 = "Standard Edition";       break;
						//	  case PRODUCT_STANDARD_SERVER_CORE: ps2 = "Standard Edition (core installation)"; break;
						//	  case PRODUCT_WEB_SERVER:        ps2 = "Web Server Edition";     break;
						//   }
							break;
						}
						default:
							break;
						}
					}
				}
	//					   add_sp(os, oi.szCSDVersion);//*/


				strncpy(out->release, "", sizeof(out->release)); // oi.dwMajorVersion, oi.dwMinorVersion, oi.dwBuildNumber
				strncpy(out->version, "", sizeof(out->version)); // kernel + " " + release
				strncpy(out->machine, "", sizeof(out->machine));
				return 0;
			};
			#endif

			struct utsname name;
			if (uname(&name))
				break;

			result = new_tuple(
				new_string(name.sysname),
				new_string(name.nodename),
				new_string(name.release),
				new_string(name.version),
				new_string(name.machine)
			);

			break;
		}

		#if SYSCALL_GETRLIMIT
		// GETRLIMIT (getrlimit)
		case SYSCALL_GETRLIMIT: {
			struct rlimit r;
			// arguments currently ignored. used RUSAGE_SELF
			if (getrlimit(uvtoi(a), &r) == 0)
				result = new_tuple(
						itoun(r.rlim_cur),
						itoun(r.rlim_max));
			break;

		}
		#endif

		#if SYSCALL_GETRUSAGE
		// GETRUSAGE (getrusage)
		// @returns: (tuple utime stime)
		//           utime: total amount of time spent executing in user mode, expressed in a timeval structure (seconds plus microseconds)
		//           stime: total amount of time spent executing in kernel mode, expressed in a timeval structure (seconds plus microseconds)
		case SYSCALL_GETRUSAGE: {
			#ifdef _WIN32
			struct rusage
			{
				struct timeval ru_utime;
				struct timeval ru_stime;
			};

			#define RUSAGE_SELF 0
			int getrusage(int who, struct rusage* usage) {
				FILETIME createTime;
				FILETIME exitTime;
				FILETIME kernelTime;
				FILETIME userTime;
				if (GetProcessTimes(GetCurrentProcess(), &createTime, &exitTime, &kernelTime, &userTime) != -1) {
					ULARGE_INTEGER li;

					li.LowPart = userTime.dwLowDateTime;
					li.HighPart = userTime.dwHighDateTime;
					usage->ru_utime.tv_sec = li.QuadPart / 10000000;
					usage->ru_utime.tv_usec = li.QuadPart % 10000000;

					li.LowPart = kernelTime.dwLowDateTime;
					li.HighPart = kernelTime.dwHighDateTime;
					usage->ru_stime.tv_sec = li.QuadPart / 10000000;
					usage->ru_stime.tv_usec = li.QuadPart % 10000000;
					return 0;
				}
				else
					return -1;
			}
			#endif

			struct rusage u;
			// arguments currently ignored. used RUSAGE_SELF
			if (getrusage(RUSAGE_SELF, &u) == 0)
				result = new_tuple(
						new_pair (itoun(u.ru_utime.tv_sec), itoun(u.ru_utime.tv_usec)),
						new_pair (itoun(u.ru_stime.tv_sec), itoun(u.ru_stime.tv_usec))
				);
			break;

		}
		#endif

		#if SYSCALL_SYSINFO
		// SYSINFO (sysinfo)
		case SYSCALL_SYSINFO: {
			#ifdef _WIN32
			struct sysinfo
			{
				long uptime;             /* Seconds since boot */
				unsigned long loads[3];  /* 1, 5, and 15 minute load averages */
				unsigned long totalram;  /* Total usable main memory size */
				unsigned long freeram;   /* Available memory size */
				unsigned long sharedram; /* Amount of shared memory */
				unsigned long bufferram; /* Memory used by buffers */
				unsigned long totalswap; /* Total swap space size */
				unsigned long freeswap;  /* Swap space still available */
				unsigned short procs;    /* Number of current processes */
				unsigned long totalhigh; /* Total high memory size */
				unsigned long freehigh;  /* Available high memory size */
				unsigned int mem_unit;   /* Memory unit size in bytes */
				char _f[20-2*sizeof(long)-sizeof(int)];
			};

			int sysinfo(struct sysinfo *info) {
				ZeroMemory(info, sizeof(*info));
				info->uptime = GetTickCount() / 1000;
				return 0;
			}
			#endif

			struct sysinfo info;
			if (sysinfo(&info) == 0)
				result = new_tuple(
						itoun(info.uptime),
						new_tuple(itoun(info.loads[0]),
								  itoun(info.loads[1]),
								  itoun(info.loads[2])),
						itoun(info.totalram),
						itoun(info.freeram),
						itoun(info.sharedram),
						itoun(info.bufferram),
						itoun(info.totalswap),
						itoun(info.freeswap),
						itoun(info.procs)
				);
			break;
		}
		#endif

		// todo: add syscall 100 (times)


		// =- 1000+ -===========================================================================
		// other internal commands
		case 1000:
			ol->gc(ol, 0);
			break;
		case 1001: // is raw object?
			if (is_reference(a)) {
				word hdr = *(word*)a;
				if (is_rawobject(hdr))
					result = (word*)ITRUE;
			}
			break;
		case 1007: // set memory limit (in mb) / // todo: переделать на другой номер
			result = itoun (ol->max_heap_size);
			ol->max_heap_size = uvtoi (a);
			break;
		case 1009: // get memory limit (in mb) / // todo: переделать на другой номер
			result = itoun (ol->max_heap_size);
			break;
		case 1008: /* get machine word size (in bytes) */ // todo: переделать на другой номер
			result = itoun (sizeof (word));
			break;

		// todo: сюда надо перенести все prim_sys операции, что зависят от глобальных переменных
		//  остальное можно спокойно оформлять отдельными функциями

		case 1022: // set ticker
			result = itoun (ticker);
			ticker = uvtoi (a);
			break;
	//		case 1014: { /* set-ticks n _ _ -> old */
	//			result = itoun (ol->slice);
	//			ol->slice  = uvtoi (a);
	//			break;
	//		}

		case 1016: { // getenv <owl-raw-bvec-or-ascii-leaf-string>
			word *name = (word *)a;
			if (is_string(name)) {
				char* env = getenv((char*)&name[1]);
				if (env)
					result = new_string(env, lenn(env, FMAX));
			}
			break;
		}
		case 1017: { // system (char*) // todo: remove this
			int r = system((char*)&car (a));
			if (r >= 0)
				result = itoun(r);
			break;
		}

		case 1117: { // get memory stats -> #[generation fp total]
			int g = heap->genstart - heap->begin;
			int f = fp - heap->begin;
			int t = heap->end - heap->begin;
			result = new_tuple(F(g), F(f), F(t));
			break;
		}

	#if HAS_DLOPEN
		// -=( dlopen )=-------------------------------------------------
		case SYSCALL_DLOPEN: { // (dlopen filename mode #false)
			word *filename = (word*)a;
			int mode = (int) uvtoi(b);

			// android 2.2 segfaults on dlopen(NULL, ...)
			// http://code.google.com/p/android/issues/detail?id=5049
			void* module;
			if ((word) filename == INULL || (word) filename == IFALSE) {
				module = dlopen(NULL, mode); // If filename is NULL, then the returned handle is for the main program.
			}
			else if (is_reference(filename) && reftype (filename) == TSTRING) {
				module = dlopen((char*) &filename[1], mode);
			}
			else
				break; // invalid filename, return #false

			if (module)
				result = new_vptr(module);
			break;
		}

		case SYSCALL_DLCLOSE: {
			CHECK(is_vptr(a), a, SYSCALL);
			void* module = (void*)car (a);

			if (dlclose(module) == 0)
				result = (word*) ITRUE;
			break;
		}

		case SYSCALL_DLSYM: { // (dlsym module function #false)
			CHECK(is_vptr(a), a, SYSCALL);
			void* module = (void*)car (a);

			word* symbol = (word*) b;
			// http://www.symantec.com/connect/articles/dynamic-linking-linux-and-windows-part-one
			if (!(is_value(symbol) || reftype (symbol) == TSTRING))
				break;

			word function = (word)dlsym(module, is_value(symbol)
					? (char*) value((word)symbol)
					: (char*) &symbol[1]);
			if (function)
				result = new_vptr(function);
			else
				STDERR("dlsym failed: %s", dlerror());
			break;
		}
		case SYSCALL_DLERROR: { // (dlerror)
			char* error = (char*)dlerror();
			if (error)
				result = new_string(error);
			break;
		}
		case SYSCALL_MKCB: {
			// TCALLBACK
			int c;
			// TODO: увеличить heap->CR если маловато колбеков!
			for (c = 4; c < CR; c++) {
				if (R[NR+c] == IFALSE) {
					R[NR+c] = a;
					break;
				}
			}

			char* ptr = 0;
#ifdef __i386__ // x86
			// JIT howto: http://eli.thegreenplace.net/2013/11/05/how-to-jit-an-introduction
			static char bytecode[] =
					"\x90"  // nop
					"\x8D\x44\x24\x04" // lea eax, [esp+4]
					"\x50"     // push eax
					// 6
					"\x68----" // push $0
					"\x68----" // push ol
					"\xB8----" // mov eax, ...
					"\xFF\xD0" // call eax
					"\x83\xC4\x0C" // add esp, 3*4
					"\xC3"; // ret
			#ifdef _WIN32
				HANDLE mh = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_EXECUTE_READWRITE,
						0, sizeof(bytecode), NULL);
				if (!mh)
					STDERR("Can't create memory mapped object");
				ptr = MapViewOfFile(mh, FILE_MAP_ALL_ACCESS,
						0, 0, sizeof(bytecode));
				CloseHandle(mh);
			#else
				ptr = mmap(0, sizeof(bytecode), PROT_READ | PROT_WRITE | PROT_EXEC,
						MAP_PRIVATE, -1, 0);
			#endif

			memcpy(ptr, &bytecode, sizeof(bytecode));
			*(long*)&ptr[ 7] = c;
			*(long*)&ptr[12] = (long)ol;
			*(long*)&ptr[17] = (long)&callback;
#elif __amd64__
			// Windows x64
			#ifdef _WIN32
			//long long callback(OL* ol, int id, long long* argi, double* argf, long long* rest)
			static char bytecode[] =
					"\x90" // nop
					"\x48\x8D\x44\x24\x28"  // lea rax, [rsp+40] (rest)
					"\x55"                  // push rbp
					"\x48\x89\xE5"          // mov ebp, rsp
					"\x41\x51"              // push r9
					"\x41\x50"              // push r8
					"\x52"                  // push rdx
					"\x51"                  // push rcx
					"\x49\x89\xE0"          // mov r8, esp         // argi
					// 19
					"\x48\x83\xEC\x20"      // sub esp, 32
					"\x67\xF2\x0F\x11\x44\x24\x00"    // movsd [esp+ 0], xmm0
					"\x67\xF2\x0F\x11\x4C\x24\x08"    // movsd [esp+ 8], xmm1
				    "\x67\xF2\x0F\x11\x54\x24\x10"    // movsd [esp+16], xmm2
				    "\x67\xF2\x0F\x11\x5C\x24\x18"    // movsd [esp+24], xmm3
					"\x49\x89\xE1"          // mov r9, esp         // argf
					// 54
					"\x48\xBA--------"      // mov rdx, 0          // id
					"\x48\xB9--------"      // mov rcx, 0          // ol
					// 74
					"\x50"	                // push rax // dummy
					"\x50"                  // push rax            // rest
					"\x48\x83\xEC\x20"      // sub rsp, 32         // free space
					// 80
					"\x48\xB8--------"      // mov rax, callback
					"\xFF\xD0"              // call rax
					"\xC9"                  // leave
					"\xC3";                 // ret

			HANDLE mh = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_EXECUTE_READWRITE,
					0, sizeof(bytecode), NULL);
			if (!mh)
				STDERR("Can't create memory mapped object");
			ptr = MapViewOfFile(mh, FILE_MAP_ALL_ACCESS | FILE_MAP_EXECUTE,
					0, 0, sizeof(bytecode));
			CloseHandle(mh);

			memcpy(ptr, &bytecode, sizeof(bytecode));
			*(long long*)&ptr[56] = c;
			*(long long*)&ptr[66] = (long long)ol;
			*(long long*)&ptr[82] = (long long)&callback;
			#else

			//long long callback(OL* ol, int id, long long* argi, double* argf, long long* rest)
			static char bytecode[] =
					"\x90" // nop
					"\x48\x8D\x44\x24\x28"  // lea rax, [rsp+40] (rest)
					"\x55"                  // push rbp
					"\x48\x89\xE5"          // mov rbp, rsp
					"\x41\x51"              // push r9
					"\x41\x50"              // push r8
					"\x51"                  // push rcx
					"\x52"                  // push rdx
					"\x56"                  // push rsi
					"\x57"                  // push rdi
					"\x48\x89\xE2"          // mov rdx, rsp         // argi
					// 21
					"\x48\x83\xEC\x40"      // sub rsp, 8*8
					"\xF2\x0F\x11\x44\x24\x00"    // movsd [esp+ 0], xmm0
					"\xF2\x0F\x11\x4C\x24\x08"    // movsd [esp+ 8], xmm1
				    "\xF2\x0F\x11\x54\x24\x10"    // movsd [esp+16], xmm2
				    "\xF2\x0F\x11\x5C\x24\x18"    // movsd [esp+24], xmm3
				    "\xF2\x0F\x11\x54\x24\x20"    // movsd [esp+32], xmm4
				    "\xF2\x0F\x11\x6C\x24\x28"    // movsd [esp+40], xmm5
				    "\xF2\x0F\x11\x74\x24\x30"    // movsd [esp+48], xmm6
				    "\xF2\x0F\x11\x7C\x24\x38"    // movsd [esp+56], xmm7
					"\x48\x89\xE1"          // mov rcx, rsp         // argf
					// 76
					"\x48\xBE--------"      // mov rsi, 0          // id
					// 86
					"\x48\xBF--------"      // mov rdi, 0          // ol
					// 96
					"\x48\xB8--------"      // mov rax, callback
					"\xFF\xD0"              // call rax
					"\xC9"                  // leave
					"\xC3";                 // ret
			ptr = mmap(0, sizeof(bytecode), PROT_READ | PROT_WRITE | PROT_EXEC,
					MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);

			memcpy(ptr, &bytecode, sizeof(bytecode));
			*(long long*)&ptr[78] = c;
			*(long long*)&ptr[88] = (long long)ol;
			*(long long*)&ptr[98] = (long long)&callback;
			#endif
#endif

			result = new_callback(ptr);
			break;
		}
	#endif// HAS_DLOPEN

		// https://www.mindcollapse.com/blog/processes-isolation.html
		// http://outflux.net/teach-seccomp/
		#if SYSCALL_PRCTL
		case SYSCALL_PRCTL:
			//seccomp_time = 1000 * time(NULL); /* no time calls are allowed from seccomp, so start emulating a time if success */
			/*struct sock_filter filter[] = {
				// http://outflux.net/teach-seccomp/
			};*/
			if (prctl(PR_SET_SECCOMP, SECCOMP_MODE_STRICT, 0, 0, 0) != -1) { /* true if no problem going seccomp */
				sandboxp = SECCOMP;
				result = (word*)ITRUE;
			}
			break;
		#endif

		case SYSCALL_KILL:
	#ifndef _WIN32
			if (kill(uvtoi (a), uvtoi (b)) >= 0)
				result = (word*) ITRUE;
	#endif
			break;
#endif
		case 1200:
#ifdef __EMSCRIPTEN__
			emscripten_sleep(1);
#endif
			result = (word*) ITRUE;
			break;

		case 1201:
#ifdef __EMSCRIPTEN__
			CHECK(is_number(a), a, SYSCALL);
			CHECK(is_string(b), b, SYSCALL);
			char* string = (char*)&car(b);

			switch (value(a)) {
			case TSTRING: {
				char* v = emscripten_run_script_string(string);
				if (v)
					result = new_string(v);
				break;
			}
			case TINT: {
				int v = emscripten_run_script_int(string);
				result = (word*)itosv(v);
				break;
			}
			default:
				emscripten_run_script(string);
				result = (word*) ITRUE;
			}
#endif
			break;

		}// case

		A4 = (word) result;
		ip += 5; break;
	}

	default:
		ERROR(op, new_string("Invalid opcode"), ITRUE);
		break;
	}
	goto loop;


error:; // R4-R6 set, and call mcp (if any)
	this = (word *) R[0];
	R[0] = IFALSE;
	R[3] = F(3); // vm thrown error
	if (is_reference(this)) {
		acc = 4;
		goto apply;
	}
	STDERR("invoke_mcp failed");
	goto done; // no mcp to handle error (fail in it?), so nonzero exit

done:;
	ol->this = this;
	ol->arity = acc;

	ol->heap.fp = fp;

	if (is_number(R[3]))
		return (void*)untoi(R[3]);
	else
		return (R[3] == IFALSE) ? (void*)0 : (void*)1;
}

// ======================================================================
//       загрузчик скомпилированного образа и его десериализатор
//

// fasl decoding
// tbd: comment
// todo: есть неприятный момент - 64-битный код иногда вставляет в fasl последовательность большие числа
//	а в 32-битном коде это число должно быть другим. что делать? пока х.з.
static __inline__
word get_nat(unsigned char** hp)
{
	word nat = 0;
	char i;

	#ifndef OVERFLOW_KILLS
	#define OVERFLOW_KILLS(n) exit(n)
	#endif
	do {
		long long underflow = nat; // can be removed for release
		nat <<= 7;
		if (nat >> 7 != underflow) // can be removed for release
			OVERFLOW_KILLS(9);     // can be removed for release
		i = *(*hp)++;
		nat = nat + (i & 127);
	} while (i & 128); // (1 << 7)
	return nat;
}
static __inline__
void decode_field(unsigned char** hp, word *ptrs, int pos, word** fp) {
	if (*(*hp) == 0) { // fixnum
		(*hp)++;
		unsigned char type = *(*hp)++;
		word val = make_value(type, get_nat(hp));
		*(*fp)++ = val;
	} else {
		word diff = get_nat(hp);
		*(*fp)++ = ptrs[pos-diff];
	}
}

// возвращает новый топ стека
static
word* deserialize(word *ptrs, int nobjs, unsigned char *bootstrap, word* fp)
{
	unsigned char* hp = bootstrap;
//	if (*hp == '#') // этот код не нужен, так как сюда приходит уже без шабанга
//		while (*hp++ != '\n') continue;

/*	word (^get_nat_x)() = ^()
	{
		word nat = 0;
		char i;

		#ifndef OVERFLOW_KILLS
		#define OVERFLOW_KILLS(n) exit(n)
		#endif
		do {
			long long underflow = nat; // can be removed for release
			nat <<= 7;
			if (nat >> 7 != underflow) // can be removed for release
				OVERFLOW_KILLS(9);     // can be removed for release
			i = *hp++;
			nat = nat + (i & 127);
		} while (i & 128); // (1 << 7)
		return nat;
	};*/

	// function entry:
	for (ptrdiff_t me = 0; me < nobjs; me++) {
		ptrs[me] = (word) fp;

		switch (*hp++) { // todo: adding type information here would reduce fasl and executable size
		case 1: {
			int type = *hp++;
			int size = get_nat(&hp);
			*fp++ = make_header(type, size+1); // +1 to include header in size
			while (size--)
				decode_field(&hp, ptrs, me, &fp);
			break;
		}
		case 2: {
			int type = *hp++ & 31; /* low 5 bits, the others are pads */
			int size = get_nat(&hp);
			int words = (size + W - 1) / W;
			int pads = words * W - size;//(W - (size % W));

			unsigned char *wp = (unsigned char*) &car(new (type, words+1, pads));
			while (size--)
				*wp++ = *hp++;
			while (pads--)
				*wp++ = 0;
			break;
		}
		default:
			puts("Bad object in heap");
			return 0;
		}
	}
	return fp;
}

static __inline__
word decode_word(unsigned char** hp) {
	word nat = 0;
	char i;
	do {
		nat <<= 7;
		i = *(*hp)++;
		nat = nat + (i & 127);
	}
	while (i & 128);
	return nat;
}

static
// функция подсчета количества объектов в загружаемом образе
int count_fasl_objects(word *words, unsigned char *lang) {
	unsigned char* hp;

	// count:
	int n = 0;
	hp = lang;

	int allocated = 0;
	while (*hp != 0) {
		switch (*hp++) {
		case 1: { // fix
			hp++; ++allocated;
			int size = decode_word(&hp);
			while (size--) {
				//decode_field:
				if (*hp == 0)
					hp += 2;
				decode_word(&hp); // simply skip word
				++allocated;
			}
			break;
		}
		case 2: { // pointer
			hp++;// ++allocated;
			int size = decode_word(&hp);
			hp += size;

			int words = (size / W) + ((size % W) ? 2 : 1);
			allocated += words;

			break;
		}

		default:
			puts("Bad object in heap");
			exit(42);
		}

		n++;
	}

	*words = allocated;
	return n;
}

// ----------------------------------------------------------------
// -=( virtual machine functions )=--------------------------------
//

#ifndef NAKED_VM
extern unsigned char* language;
#else
unsigned char* language = NULL;
#endif

#if !EMBEDDED_VM
int main(int argc, char** argv)
{
	unsigned char* bootstrap = language;

	// обработка аргументов:
	//	первый из них (если есть) - название исполняемого скрипта
	//	                            или "-", если это будет stdin
	//  остальные - командная строка
	if (argc > 1 && strcmp(argv[1], "-") != 0) {
		// todo: use mmap()
		struct stat st;

		if (stat(argv[1], &st) || st.st_size == 0)
			crash(errno, "File not found or empty");	// не найден файл или он пустой

		char bom;
		int bin = open(argv[1], O_RDONLY | O_BINARY, (S_IRUSR | S_IWUSR));
		if (!bin)
			crash(errno, "Can't open file");	// не смогли файл открыть

		int pos = read(bin, &bom, 1); // прочитаем один байт
		if (pos < 1)
			crash(errno, "Can't read file");	// не смогли файл прочитать

		// переделать
		if (bom == '#') { // skip possible hashbang
			while (read(bin, &bom, 1) == 1 && bom != '\n')
				st.st_size--;
			st.st_size--;
			if (read(bin, &bom, 1) < 0)
				crash(errno, "Can't read file");
			st.st_size--;
		}

		if (bom > 3) {	// ха, это текстовая программа (скрипт)!
#ifdef NAKED_VM
			crash(6, "Invalid binary script"); // некому проинтерпретировать скрипт
#else
			close(bin); // todo: сместить аргументы на 1 вперед
#endif
		}
		else {
			// иначе загрузим его
			unsigned char* ptr = (unsigned char*)malloc(st.st_size);
			if (ptr == NULL)
				crash(3, "Can't alloc memory");	// опа, не смогли выделить память...

			ptr[0] = bom;
			while (pos < st.st_size) {
				int n = read(bin, &ptr[pos], st.st_size - pos);
				if (n < 0)
					crash(errno, "Can't read file"); // не смогли прочитать
				pos += n;
			}
			close(bin);

			bootstrap = ptr;
		}
	}
#ifdef NAKED_VM
	else
		crash(7, "Invalid binary script");

	argc--; argv++;
#endif

	//set_signal_handler();

#if	HAS_SOCKETS && defined(_WIN32)
	WSADATA wsaData;
	int sock_init = WSAStartup(MAKEWORD(2,2), &wsaData);
	if (sock_init  != 0) {
		STDERR("WSAStartup failed with error: %d", sock_init);
		return 1;
	}
	AllocConsole();
#endif

	OL* olvm =
		OL_new(bootstrap, bootstrap != language ? free : NULL);

	void* r = OL_eval(olvm, argc, argv);
	OL_free(olvm);

#if	HAS_SOCKETS && defined(_WIN32)
	WSACleanup();
#endif

	return (int)(word)r;
}
#endif

OL*
OL_new(unsigned char* bootstrap, void (*release)(void*))
{
	// если отсутствует исполнимый образ
	if (bootstrap == 0) {
		return 0;
	}

	// ===============================================================
	// создадим виртуальную машину
	OL *handle = malloc(sizeof(OL));
	memset(handle, 0x0, sizeof(OL));

	// подготовим очереди в/в
	//fifo_clear(&handle->i);
	//fifo_clear(&handle->o); (не надо, так как хватает memset вверху)
#if EMBEDDED_VM
	char* S = 0;
	if (bootstrap && *bootstrap >= 0x20) {
		S = (char*)bootstrap;
		bootstrap = language;
	}
#endif

	// а теперь поработаем с сериализованным образом:
	word nwords = 0;
	word nobjs = count_fasl_objects(&nwords, bootstrap); // подсчет количества слов и объектов в образе
	nwords += (nobjs + 2); // for ptrs

	word *fp;
	heap_t* heap = &handle->heap;

	// выделим память машине:
	int max_heap_size = (W == 4) ? 4096 : 65535; // can be set at runtime
	//int required_memory_size = (INITCELLS + MEMPAD + nwords + 64 * 1024); // 64k objects for memory

	// в соответствии со стратегией сборки 50*1.3-33*0.9, и так как данные в бинарнике
	// практически гарантированно "старое" поколение, выделим в два раза больше места.
	int required_memory_size = nwords*2 + MEMPAD;

	fp = heap->begin = (word*) malloc((required_memory_size + GCPAD(NR)) * sizeof(word)); // at least one argument string always fits
	if (!heap->begin) {
		STDERR("Failed to allocate %d bytes in memory for vm", required_memory_size * sizeof(word));
		goto fail;
	}
	// ok
	heap->end = heap->begin + required_memory_size;
	heap->genstart = heap->begin;

	handle->max_heap_size = max_heap_size;

	// Десериализация загруженного образа в объекты
	word *ptrs = new(TTUPLE, nobjs+1, 0);
	fp = deserialize(&ptrs[1], nobjs, bootstrap, fp);

	if (fp == 0)
		goto fail;

	// все, программа в памяти, можно освобождать исходник
	if (release)
		release(bootstrap);


	// а теперь подготовим аргументы:
	word* userdata = (word*) INULL;
#if EMBEDDED_VM
	if (S) {
		char* filename = tempnam(0, "ol");

		FILE* file = fopen(filename, "w");
		fwrite(S, strlen(S), 1, file);
		fclose(file);

		userdata = new_pair (new_string (filename), userdata);
	}
#endif

	// обязательно почистим регистры! иначе gc() сбойнет, пытаясь работать с мусором
	word* R = handle->R; // регистры виртуальной машины:
	for (ptrdiff_t i = 0; i < NR+CR; i++)
		R[i] = IFALSE; // was: INULL, why??
	R[0] = IFALSE; // MCP - master control program (in this case NO mcp)
	R[3] = IHALT;  // continuation, in this case simply notify mcp about thread finish
	R[4] = (word) userdata; // first argument: command line as '(script arg0 arg1 arg2 ...)

	heap->fp = fp;
	return handle;
fail:
	if (release)
		release(bootstrap);
	OL_free(handle);
	return 0;
}

void OL_free(OL* ol)
{
	free(ol->heap.begin);
	free(ol);
}

// ===============================================================
void*
OL_eval(OL* handle, int argc, char** argv)
{
#	ifndef _WIN32
//	setvbuf(stderr, (void*)0, _IONBF, 0);
//	setvbuf(stdout, (void*)0, _IONBF, 0);
	set_blocking(STDOUT_FILENO, 0);
	set_blocking(STDERR_FILENO, 0);
#	endif

	// подготовим аргументы:
	word userdata = handle->R[4];
	{
		word* fp = handle->heap.fp;
//#if !EMBEDDED_VM
		argv += argc - 1;
		for (ptrdiff_t i = argc; i > 1; i--, argv--) {
			char *pos = (char*)(fp + 1);
			char *v = *argv;
			while ((*pos = *v++) != 0)
				pos++;
			int length = pos - (char*)fp - W;
			if (length > 0) // если есть что добавить
				userdata = (word) new_pair (new_bytevector(TSTRING, length), userdata);
		}
//#endif
		handle->heap.fp = fp;
	}
	handle->R[4] = userdata;

	// результат выполнения скрипта
	OL* ol = handle;
	heap_t* heap = &ol->heap;
	sandboxp = 0;    // static variable

	word* ptrs = (word*) heap->begin;
	int nobjs = hdrsize(ptrs[0]) - 1;

	// точка входа в программу - это последняя лямбда загруженного образа (λ (args))
	// thinkme: может стоит искать и загружать какой-нибудь main() ?
	word* this = (word*) ptrs[nobjs];

	unsigned short acc = 2; // boot always calls with 1+1 args, no support for >255arg functions

	// все готово для выполнения главного цикла виртуальной машины
	ol->this = this;
	ol->arity = acc;

	ol->gc = OL__gc;

	return runtime(handle);
}

#if HAS_PINVOKE
#	include "pinvoke.c"
#endif
