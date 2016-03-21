/**
 * Copyright (c) 2014 Aki Helin
 * Copyright (c) 2014- 2016 Yuriy Chumak
 *
 * Simple purely functional Lisp, mostly
 *
 * Version 1.0.0 RC4
 * ~~~~~~~~~~~~~~~~~
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
 * Related:
 *   http://people.csail.mit.edu/jaffer/Scheme (r5rs)
 *   http://groups.csail.mit.edu/mac/projects/scheme/
 *
 * How to build:
 *   make; make install
 *
 *
 * Original Owl Lisp project can be found at
 *   https://code.google.com/p/owl-lisp/
 *   https://github.com/aoh/owl-lisp
 *
 */

// http://beefchunk.com/documentation/lang/c/pre-defined-c/precomp.html
#ifndef __GNUC__
#	warning "This code written only for Gnu C compiler"
#else
#	define GCC_VERSION (__GNUC__ * 10000 \
	                  + __GNUC_MINOR__ * 100 \
	                  + __GNUC_PATCHLEVEL__)
#	if GCC_VERSION < 30200
#		error "Code require gcc version > 3.2 (with nested functions support)"
#	endif

#	if __STDC_VERSION__ < 199901L
#		error "Code require c99 enabled (-std=c99)"
#	endif
#endif

#include "olvm.h"

// TODO: JIT!
//	https://gcc.gnu.org/onlinedocs/gcc-5.1.0/jit/intro/tutorial04.html

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

// pinned objects - если это будут просто какие-то равки, то можно их размещать ДО основной памяти,
//	при этом основную память при переполнении pinned размера можно сдвигать вверх.

// todo: support ALL of this OS
//    Linux-i386                                                                  +
//    Linux-x86_64 (amd64)                                                        +
//    Linux-powerpc
//    Linux-sparc
//    Linux-ARM
//    Win32-i386 (2000/XP, WinNT or later)                                        +
//    Win64-x86_64 (XP or later)                                                  .
//    Wince-ARM (cross compiled from win32-i386)
//    FreeBSD-i386                                                                .
//    FreeBSD-x86_64                                                              +
//    NetBSD-i386                                                                 .
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
//    Android (ARM, i386, MIPS) via cross-compiling.
//    MSDos-i8086 (cross compiled from win32-i386 or Linux)                       -
//    Amiga, MorphOS and AROS
// Обратить внимание на проект http://sourceforge.net/p/predef/wiki/OperatingSystems/

// todo: strip ELF http://habrahabr.ru/post/137706/
// http://www.catch22.net/tuts/reducing-executable-size

// todo: add setsockopt syscall https://www.freebsd.org/doc/en/books/developers-handbook/ipv6.html


#define __OLVM_NAME__ "OL"
#define __OLVM_VERSION__ "1.0"

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

#define _POSIX_SOURCE //

#ifdef __NetBSD__     // make all NetBSD features available
#	ifndef _NETBSD_SOURCE
#	define _NETBSD_SOURCE 1
#	endif
#endif


// TODO: ref, set, must base on 0 (like list-ref, vector-ref, etc.), not on 1. create temporary ref1, set1 based on 1 (?)

#ifdef __linux__
#include <features.h>
#endif

#ifdef __unix__
#include <sys/cdefs.h>
#endif


// check this for nested functions:
//	https://github.com/Leushenko/C99-Lambda

// posix or not:
//	http://stackoverflow.com/questions/11350878/how-can-i-determine-if-the-operating-system-is-posix-in-c
// http://nadeausoftware.com/articles/2012/01/c_c_tip_how_use_compiler_predefined_macros_detect_operating_system#WindowswithCygwinPOSIX

#define __USE_POSIX199309 // nanosleep, etc.

#ifdef __MINGW32__ // bug in mingw
#define _cdecl __cdecl
#endif

// http://joeq.sourceforge.net/about/other_os_java.html
// call/cc - http://fprog.ru/lib/ferguson-dwight-call-cc-patterns/


// компилятор owl-lisp поддерживает только несколько специальных форм:
//	lambda, quote, rlambda (recursive lambda), receive, _branch, _define, _case-lambda, values (смотреть env.scm)
//	все остальное - макросы

#include <assert.h>
#include <unistd.h> // posix, https://ru.wikipedia.org/wiki/C_POSIX_library
#include <stddef.h>
#include <stdlib.h>
#include <signal.h>
#include <dirent.h>
#include <string.h>
// no alloca.h, use https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html


#include <errno.h>
#include <stdio.h>
#include <inttypes.h>
#include <fcntl.h>
#ifndef _WIN32
#include <termios.h>
#else
#include <malloc.h>
#endif

#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef __linux__
#include <sys/utsname.h> // uname
#include <sys/resource.h>// getrusage
#endif

#include <time.h>

#ifdef __unix__
#include <sys/utsname.h>
#endif

#ifdef __linux__
#include <sys/prctl.h>
#include <linux/seccomp.h>
#endif

// ?
#ifndef O_BINARY
#	define O_BINARY 0
#endif

extern int mkstemp (char *__template);

// https://gcc.gnu.org/onlinedocs/gcc/Diagnostic-Pragmas.html
//#pragma GCC diagnostic push
//#pragma GCC diagnostic error "-Wuninitialized"
//#pragma GCC diagnostic pop

// some portability issues (mainly for freebsd)
#ifndef EWOULDBLOCK
#define EWOULDBLOCK EAGAIN
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
#	define WIN32_LEAN_AND_MEAN
#	define VC_EXTRALEAN
#	include <windows.h>
#	include <winsock2.h>
#	include <ws2tcpip.h>
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

#ifdef __ANDROID__
	typedef unsigned long in_addr_t;
#endif

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
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
// seen at https://github.com/dlfcn-win32/dlfcn-win32/blob/master/dlfcn.c

static DWORD dlerrno = 0;
static
void *dlopen(const char *filename, int mode/*unused*/)
{
	HMODULE hModule;
	// Do not let Windows display the critical-error-handler message box */
	// UINT uMode = SetErrorMode( SEM_FAILCRITICALERRORS );

	if (filename == 0)
		/* POSIX says that if the value of file is 0, a handle on a global
		 * symbol object must be provided. That object must be able to access
		 * all symbols from the original program file, and any objects loaded
		 * with the RTLD_GLOBAL flag.
		 * The return value from GetModuleHandle( ) allows us to retrieve
		 * symbols only from the original program file. For objects loaded with
		 * the RTLD_GLOBAL flag, we create our own list later on.
		 */
		hModule = GetModuleHandle(NULL);
	else
		/* POSIX says the search path is implementation-defined.
		 * LOAD_WITH_ALTERED_SEARCH_PATH is used to make it behave more closely
		 * to UNIX's search paths (start with system folders instead of current
		 * folder).
		 */
		hModule = LoadLibraryEx((LPSTR)filename, NULL, LOAD_WITH_ALTERED_SEARCH_PATH);
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


// ----------
// -=( OL )=----------------------------------------------------------------------
// --
//
// виртуальная машина:

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
// todo: один бит из них я заберу на индикатор "неперемещенных" заголовков во время GC
// http://publications.gbdirect.co.uk/c_book/chapter6/bitfields.html

#define IPOS      8  // === offsetof (struct direct, payload)

__attribute__ ((aligned(sizeof(word)), packed))
struct value_t
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

__attribute__ ((aligned(sizeof(word)), packed))
struct header_t
{
	unsigned mark : 1;    // mark bit (can only be 1 during gc)
	unsigned i    : 1;    // for headers always 1
	unsigned type : 6;    // object type

	unsigned padding : 3; // number of padding (unused) bytes at end of object
	unsigned rawness : 1;
	unsigned         : 4; // unused

	word     size : 8 * sizeof(word) - (1+1+6+3+1+4);
};

__attribute__ ((aligned(sizeof(word)), packed))
struct object_t
{
	union {
		struct header_t header;
		word ref[0];
	};
};

// ------------------------------------------------------

#define W                           sizeof (word)
#define F(val)                      (((word)(val) << IPOS) | 2)


//#define NWORDS                    1024*1024*8    /* static malloc'd heap size if used as a library */
#define FBITS                       ((__SIZEOF_LONG__ * 8) - 8) // bits in value (short number)
#define HIGHBIT                     ((unsigned long)1 << FBITS) // high long bit set
#define FMAX                        (((long)1 << FBITS)-1)      // maximum value value (and most negative value)
// todo: remove MAXOBJ!
#define MAXOBJ                      0xffff         // max words in tuple including header

#if __amd64__
#define big                         unsigned __int128
#else
#define big                         unsigned long long //__int64
#endif

#define RAWBIT                      ((1 << RPOS))
#define RAWH(t)                     (t | (RAWBIT >> TPOS))

#define make_value(type, value)        ((((word)value) << IPOS) | ((type) << TPOS)                         | 2)
#define make_header(type, size)        (( (word)(size) << SPOS) | ((type) << TPOS)                         | 2)
#define make_raw_header(type, size, p) (( (word)(size) << SPOS) | ((type) << TPOS) | (RAWBIT) | ((p) << 8) | 2)
// p is padding

#define TRUEFALSE(cval)             ((cval) ? ITRUE : IFALSE)
#define fliptag(ptr)                ((word)ptr ^ 2) /* make a pointer look like some (usually bad) immediate object */
// fliptag used in dir sys-prims

// два главных класса аргументов:
#define is_value(x)                 (((word)(x)) & 2)
#define is_reference(x)             (!is_value(x))
#define is_rawobject(x)             (((word)(x)) & RAWBIT) // todo: rename to is_rawobject?

// всякая всячина:
#define hdrsize(x)                  (((word)x) >> SPOS)
#define padsize(x)                  (unsigned char)((((word)(x)) >> IPOS) & 7)

#define typeof(x)                   (unsigned char)((((word)(x)) >> TPOS) & 0x3F)
#define valuetype(x)                (typeof (x) & 0x1F)
#define reftype(x)                  (typeof (*(word*)(x)))

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
#define TFIXN                       (32 + TFIX)  // type-fix-
// numbers (reference type)
#define TINT                        (40)  // type-int+ // todo: rename to TINTEGER (?)
#define TINTN                       (41)  // type-int-
#define TRATIONAL                   (42)
#define TCOMPLEX                    (43)

// for pinvoke
#define TVOID                       (48) // only for pinvoke
#define TVPTR                       (49) // void*, only RAW, can't be 0
#define TUSERDATA                   (62) // only for pinvoke, must be RAW, can be 0

// only pinvoke argument types
#define TINT64                      44
#define TFLOAT                      46
#define TDOUBLE                     47

// constants:
#define IFALSE                      make_value(TCONST, 0)
#define ITRUE                       make_value(TCONST, 1)
#define INULL                       make_value(TCONST, 2)
#define IEMPTY                      make_value(TCONST, 3) // empty ff
#define IEOF                        make_value(TCONST, 4)
#define IHALT                       INULL // FIXME: adde a distinct IHALT


#define HVPTR                       make_raw_header(TVPTR, 2, 0) // must be RAW

//#define likely(x)                   __builtin_expect((x), 1)
//#define unlikely(x)                 __builtin_expect((x), 0)

#define is_const(ob)                (typeof (ob) == TCONST)
#define is_port(ob)                 (is_value(ob)     && typeof (ob) == TPORT)

#define is_fix(ob)                  (is_value(ob)     && typeof (ob) == TFIX)
#define is_fixn(ob)                 (is_value(ob)     && typeof (ob) == TFIXN)
#define is_pair(ob)                 (is_reference(ob) &&        (*(word*)(ob)) == make_header(TPAIR,     3))
#define is_npair(ob)                (is_reference(ob) &&        (*(word*)(ob)) == make_header(TINT,      3))
#define is_npairn(ob)               (is_reference(ob) &&        (*(word*)(ob)) == make_header(TINTN,     3))
#define is_rational(ob)             (is_reference(ob) &&        (*(word*)(ob)) == make_header(TRATIONAL, 3))
#define is_complex(ob)              (is_reference(ob) &&        (*(word*)(ob)) == make_header(TCOMPLEX,  3))

#define is_string(ob)               (is_reference(ob) && typeof (*(word*)(ob)) == TSTRING)
#define is_tuple(ob)                (is_reference(ob) && typeof (*(word*)(ob)) == TTUPLE)

#define is_vptr(ob)                 (is_reference(ob) &&        (*(word*)(ob)) == HVPTR)

#define is_number(ob)               (is_npair(ob)  || is_fix(ob))
#define is_numbern(ob)              (is_npairn(ob) || is_fixn(ob))


// взять значение аргумента:
#define value(x)                    ({ assert(is_value(x));     (((word)(x)) >> IPOS); })
#define reference(x)                ({ assert(is_reference(x)); *(word*)(x); })

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



// todo: потом переделать в трюк
// алгоритмические трюки:
// x = (x xor t) - t, где t - y >>(s) 31 (все 1, или все 0)
// signed fix to int

// i - machine integer
// ui - unsigned, si - signed
// todo: add this
// a - atomic number (internal, that fits in one register), type-fix
//  or small numbers,
//  or short numbers
// ua, sa - unsigned/signed respectively.
// Z - mножество целых чисел.

// работа с numeric value типами
#ifndef UVTOI_CHECK
#define UVTOI_CHECK(v) assert (is_value(v) && valuetype(v) == TFIX);
#endif
#define uvtoi(v)        ({ word x = (word)v; UVTOI_CHECK(x); (word) (x >> IPOS); })
#define uvtol(v)  (long)({ word x = (word)v; UVTOI_CHECK(x); (word) (x >> IPOS); })
#define itouv(i)  (word)({ word x = (word)i;                 (word) (x << IPOS) | 2; })
		// (((struct value*)(&v))->payload);

// todo: add overflow checking...
#ifndef SVTOI_CHECK
#define SVTOI_CHECK(v) assert (is_value(v) && valuetype(v) == TFIX);
#endif
#define svtoi(v)        ({ word x = (word)v; SVTOI_CHECK(x); (x & 0x80) ? -(x >> IPOS)        : (x >> IPOS); })
#define svtol(v)  (long)({ word x = (word)v; SVTOI_CHECK(x); (x & 0x80) ? -(x >> IPOS)        : (x >> IPOS); })
#define itosv(i)  (word)({ long x = (long)i;                 (x < 0)    ? (-x << IPOS) | 0x82 : (x << IPOS) | 2; })

		// ((struct value)(v).sign) ? -uvtoi (v) : uvtoi (v);


// арифметика целых (возможно больших)
// прошу внимания!
//  в числовой паре надо сначала положить старшую часть, и только потом младшую!
#define untoi(num)  ({\
	is_value(num) ? uvtoi(num)\
		: uvtoi(car(num)) | uvtoi(cadr(num)) << FBITS; }) //(is_reference(cdr(num)) ? uftoi(cadr(num)) << FBITS : 0); })
#define itoun(val)  ({\
	(word*)(\
	__builtin_choose_expr((sizeof(val) < sizeof(word)),\
		itouv(val),\
		(val <= FMAX ? itouv(val) \
			: (word)new_list(TINT, itouv(val & FMAX), itouv((val) >> FBITS)))));})

#define make_integer(val) itoun(val)

#define NR                          128 // see n-registers in register.scm

#define GCPAD                      ((NR + 2) * sizeof(word)) // space at end of heap for starting GC
#define MEMPAD                     (GCPAD + 1024) // резервируемое место для работы в памяти
#define MINGEN                     (1024 * 32)  /* minimum generation size before doing full GC  */
#define INITCELLS                  (1000)

// http://outflux.net/teach-seccomp/
// http://mirrors.neusoft.edu.cn/rpi-kernel/samples/seccomp/bpf-direct.c
// https://www.kernel.org/doc/Documentation/prctl/seccomp_filter.txt
#define SECCOMP                     10000
static int seccompp = 0;     /* are we in seccomp? а также дельта для оптимизации syscall's */
//static unsigned long seccomp_time; /* virtual time within seccomp sandbox in ms */

//static int breaked = 0;    /* set in signal handler, passed over to owl in thread switch */

// -----------------------------------------------------//--------------------
// -=( GC )=------------------------------------------------------------------

/*** Garbage Collector,
 * based on "Efficient Garbage Compaction Algorithm" by Johannes Martin (1982)
 ***/
// "на почитать" по теме GC:
// shamil.free.fr/comp/ocaml/html/book011.html

// память машины, управляемая сборщиком мусора
typedef struct heap_t
{
	//  begin <= genstart <= end
	word *begin;     // begin of heap memory block
	word *end;       // end of heap
	word *genstart;  // new generation begin pointer
	// new (size) == *(size*)fp++
	word *fp;        // allocation pointer
} heap_t;


// -= new =--------------------------------------------
// создает порт, НЕ аллоцирует память
#define make_port(a)               ({ assert((((word)(a)) << IPOS) >> IPOS == (word)(a)); make_value(TPORT, a); })

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
#define new(...) NEW_MACRO(__VA_ARGS__, NEW_RAW_OBJECT, NEW_OBJECT, NEW)(__VA_ARGS__)

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
#define new_pair(...) NEW_PAIR_MACRO(__VA_ARGS__, NEW_TYPED_PAIR, NEW_PAIR, NOTHING)(__VA_ARGS__)

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
#define new_string(...) NEW_STRING_MACRO(__VA_ARGS__, NEW_STRING2, NEW_STRING)(__VA_ARGS__)


#define new_vptr(a) ({\
word data = (word) a;\
	word* me = new (TVPTR, 2, 0);\
	me[1] = data;\
	/*return*/me;\
})

#define new_native_function(a) ({\
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

/* n-cells-wanted → heap-delta (to be added to pointers), updates memstart and memend  */
static __inline__
ptrdiff_t adjust_heap(heap_t *heap, int cells)
{
	if (seccompp) /* realloc is not allowed within seccomp */
		return 0;

	// add newobj realloc + heap fixer here later
	word nwords = heap->end - heap->begin; // MEMPAD is after memend
	word new_words = nwords + ((cells > 0xffffff) ? 0xffffff : cells); // limit heap growth speed
	if (((cells > 0) && (new_words*W < nwords*W)) || ((cells < 0) && (new_words*W > nwords*W)))
		return 0; // don't try to adjust heap if the size_t would overflow in realloc

	new_words += MEMPAD;

	word *old = heap->begin;
	heap->begin = realloc(heap->begin, new_words * W);
	if (heap->begin == old) { // whee, no heap slide \o/
		heap->end = heap->begin + new_words - MEMPAD; // leave MEMPAD words alone

		return 0;
	}

	if (heap->begin) { // d'oh! we need to O(n) all the pointers...
		heap->end = heap->begin + new_words - MEMPAD; // leave MEMPAD words alone

		ptrdiff_t delta = heap->begin - old;

		heap->fp += delta;
		word* pos = heap->begin;
		word* end = heap->fp;

		// fix_pointers
		delta *= sizeof(word);
		while (pos < end) {
			word hdr = *pos;
			int n = hdrsize(hdr);
			if (is_rawobject(hdr))
				pos += n; // no pointers in raw objects
			else {
				pos++, n--;
				while (n--) {
					word val = *pos;
					if (is_reference(val))
						*pos = val + delta;
					pos++;
				}
			}
		}
		return delta;
	} else {
		printf("adjust_heap failed.\n");
		//breaked |= 8; // will be passed over to mcp at thread switch
		return 0;
	}
}

/* input desired allocation size and (the only) pointer to root object
   return a pointer to the same object after heap compaction, possible heap size change and relocation */

// todo: ввести третий generation
__attribute__ ((aligned(sizeof(word))))
static word gc(heap_t *heap, int size, word regs) {
	// просматривает список справа налево
	void mark(word *pos, word *end)
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

	// на самом деле - compact & sweep
	word *sweep(word* end)
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

	// gc:
	{
		word *fp;

		fp = heap->fp;
		word *root = &fp[1]; // skip header
	//	word *root = fp + 1; // same
	//в *fp спокойно можно оставить мусор
		*fp = make_header(TTUPLE, 2);

		if (size == 0)
			heap->genstart = heap->begin; // start full generation

		// непосредственно сам GC
	//	clock_t uptime;
	//	uptime = -(1000 * clock()) / CLOCKS_PER_SEC;
		root[0] = regs;
		mark(root, fp);        // assert (root > fp)
		fp = sweep(fp);
		regs = root[0];
	//	uptime += (1000 * clock()) / CLOCKS_PER_SEC;

		heap->fp = fp;
	}

	#if DEBUG_GC
		struct tm tm = *localtime(&(time_t){time(NULL)});
		char buff[70]; strftime(buff, sizeof buff, "%c", &tm);
		fprintf(stderr, "%s, GC done in %2d ms (use: %7d from %8d bytes - %2d%%): tbd.\n", //marked %6d, moved %6d, pinned %2d, moved %8d bytes total\n",
				buff/*asctime(&tm)*/, uptime,
				(sizeof(word) * (fp - heap->begin)),        (sizeof(word) * (heap->end - heap->begin)),
				(sizeof(word) * (fp - heap->begin) * 100) / (sizeof(word) * (heap->end - heap->begin)));
//				-1, -1, -1, -1);
	#endif

	// кучу перетрясли и уплотнили, посмотрим надо ли ее увеличить/уменьшить
	int nfree = (word)heap->end - (word)regs;
	if (heap->genstart == heap->begin) {
		word heapsize = (word) heap->end - (word) heap->begin;
		word nused = heapsize - nfree;

		nfree -= size*W + MEMPAD;   // how much really could be snipped off
		if (nfree < (heapsize / 10) || nfree < 0) {
			/* increase heap size if less than 10% is free by ~10% of heap size (growth usually implies more growth) */
			regs += adjust_heap(heap, size*W + nused/10 + 4096);
			nfree = (word)heap->end - regs;

//			if (nfree <= size)
//				breaked |= 8; /* will be passed over to mcp at thread switch. may cause owl<->gc loop if handled poorly on lisp side! */
		}
		else if (nfree > (heapsize/5)) {
			/* decrease heap size if more than 20% is free by 10% of the free space */
			int dec = -(nfree/10);
			int newobj = nfree - dec;
			if (newobj > size*W*2) {
				regs += adjust_heap(heap, dec);
				heapsize = (word) heap->end - (word) heap->begin;
				nfree = (word) heap->end - regs;
			}
		}
		heap->genstart = (word*)regs; // always start newobj generation
	}
	else if (nfree < MINGEN || nfree < size*W*2) {
		heap->genstart = heap->begin; // start full generation
		return gc(heap, size, regs);
	}
	else
		heap->genstart = (word*)regs; // simply start newobj generation
	return regs;
}


/*** OS Interaction and Helpers ***/
//static
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
	fprintf(stderr, "signal %d!\n", signal);
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
static __inline__ void bytecopy(char *from, char *to, int n) { while (n--) *to++ = *from++; }
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

/***********************************************************************************
 * OL
 */
struct ol_t
{
	struct heap_t heap; // must be first member
	word max_heap_size; // max heap size in MB

	// вызвать GC если в памяти мало места в КБ
	// для безусловного вызова передать -1
	void (*gc)(int kb);
};


#define TICKS                       10000 // # of function calls in a thread quantum

#define OCLOSE(proctype)            { \
	word size = *ip++, tmp; word *T = new (proctype, size); \
	tmp = R[*ip++]; tmp = ((word *) tmp)[*ip++]; T[1] = tmp; tmp = 2; \
	while (tmp != size) { T[tmp++] = R[*ip++]; } R[*ip++] = (word) T; }
#define CLOSE1(proctype)            { \
	word size = *ip++, tmp; word *T = new (proctype, size); \
	tmp = R[  1  ]; tmp = ((word *) tmp)[*ip++]; T[1] = tmp; tmp = 2; \
	while (tmp != size) { T[tmp++] = R[*ip++]; } R[*ip++] = (word) T; }

#define ERROR(opcode, a, b)         { \
	fprintf(stderr, "ERROR: %s/%d\n", __FILE__, __LINE__); /* TEMP */\
	R[4] = F (opcode);\
	R[5] = (word) (a);\
	R[6] = (word) (b);\
	goto invoke_mcp; }
#define CHECK(exp,val,errorcode)    if (!(exp)) ERROR(errorcode, val, ITRUE);

#define A0                          R[ip[0]]
#define A1                          R[ip[1]]
#define A2                          R[ip[2]]
#define A3                          R[ip[3]]
#define A4                          R[ip[4]]
#define A5                          R[ip[5]]
#define R0                          R[0]
#define R1                          R[1]
#define R2                          R[2]
#define R3                          R[3]
#define R4                          R[4]

// Несколько замечаний по WIN32::ThreadProc
//  http://msdn.microsoft.com/en-us/library/windows/desktop/ms686736(v=vs.85).aspx
//  The return value should never be set to STILL_ACTIVE (259), as noted in GetExitCodeThread.

static //__attribute__((aligned(8)))
void* runtime(OL* ol, word* userdata) // userdata - is command line
{
	heap_t* heap;
	register word *fp; // memory allocation pointer
	int slice = TICKS; // default thread slice (n calls per slice)

	int max_heap_size;

	// регистры виртуальной машины:
	word R[NR]; // 0 - mcp, 1 - clos, 2 - env, 3 - a0, often cont

	int breaked = 0;
	seccompp = 0;

	// инициализируем локальную память
	heap          = &ol->heap;
	max_heap_size =  ol->max_heap_size; // max heap size in MB

	fp = heap->fp;

	// все, машина инициализирована, отсигналимся
//	((struct args*)args)->signal = 1;

	word* ptrs = (word*) heap->begin;
	int nobjs = hdrsize(ptrs[0]) - 1;

	// точка входа в программу - это последняя лямбда загруженного образа (λ (args))
	// thinkme: может стоит искать и загружать какой-нибудь main() ?
	word* this = (word*) ptrs[nobjs];

	// обязательно почистим регистры! иначе gc() сбойнет, пытаясь работать с мусором
	for (ptrdiff_t i = 0; i < NR; i++)
		R[i] = INULL;
	R[0] = IFALSE; // MCP - master control program
	R[3] = IHALT;  // continuation
	R[4] = (word) userdata; // command line as '(script arg0 arg1 arg2 ...)
	unsigned short acc = 2; // boot always calls with 1+1 args, no support for >255arg functions

	void dogc(int size)
	{
		int p = 0, N = NR;
		// создадим в топе временный объект со значениями всех регистров
		word *regs = (word*) new (TTUPLE, N + 2); // N for regs, 1 for this, and 1 for header
		while (++p <= N) regs[p] = R[p-1];
		regs[p] = (word) this;
		// выполним сборку мусора
		heap->fp = fp;
		regs = (word*)gc(heap, size, (word)regs); // GC занимает 0-15 ms
		fp = heap->fp;
		// и восстановим все регистры, уже подкорректированные сборщиком
		this = (word *) regs[p];
		while (--p >= 1) R[p-1] = regs[p];

		// закончили, почистим за собой:
		ol->heap.fp = fp = regs; // (вручную сразу удалим временный объект, это такая оптимизация)
	}
	ol->gc = ({ void $(int kb) {
		if (kb == 0 || fp >= heap->end - kb * 1024) dogc(kb/W);
	}$; });

	int bank = 0; // ticks deposited at interop
	int ticker = slice; // any initial value ok

	// instruction pointer
	unsigned char *ip = 0;

	while (1) {
		apply: // apply something at "this" to values in regs, or maybe switch context
		if ((word)this == IEMPTY && acc > 1) { /* ff application: (False key def) -> def */
			this = (word *) R[3]; /* call cont */
			R[3] = (acc > 2) ? R[5] : IFALSE; /* default arg or false if none */
			acc = 1;
			continue;
		}
		if ((word)this == IHALT) {
			// a tread or mcp is calling the final continuation
			this = (word *) R[0];
			if (!is_reference(this)) {
				fprintf(stderr, "Unexpected virtual machine exit\n");
				return (void*) uvtol(R[3]);
			}

			R[0] = IFALSE; // set no mcp
			R[4] = R[3];
			R[3] = make_value(TFIX, 2);   // 2 = thread finished, look at (mcp-syscalls-during-profiling) in lang/thread.scm
			R[5] = IFALSE;
			R[6] = IFALSE;
			breaked = 0;
			ticker = TICKS;// ?
			bank = 0;
			acc = 4;
			continue;
		} /* <- add a way to call the newobj vm prim table also here? */

		// ...
		if (is_reference(this)) { // если это аллоцированный объект
			//word hdr = *this & 0x0FFF; // cut size out, take just header info
			word type = typeof (*this);
			if (type == TPROC) { //hdr == make_header(TPROC, 0)) { // proc
				R[1] = (word) this; this = (word *) this[1]; // ob = car(ob)
			}
			else
			if (type == TCLOS) { //hdr == make_header(TCLOS, 0)) { // clos
				R[1] = (word) this; this = (word *) this[1]; // ob = car(ob)
				R[2] = (word) this; this = (word *) this[1]; // ob = car(ob)
			}
			else
			if ((type & 60) == TFF) { //((hdr>>TPOS) & 60) == TFF) { /* low bits have special meaning */
				word get(word *ff, word key, word def) { // ff assumed to be valid
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
						    fprintf(stderr, "assert! hdrsize(hdr) == %d\n", (int)hdrsize(hdr));
							assert (0);
							//ff = (word *) ((key < this) ? ff[3] : ff[4]);
						}
					}
					return def;
				}

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
				continue;
			}
			else
				if ((type & 63) != TBYTECODE) //((hdr >> TPOS) & 63) != TBYTECODE) /* not even code, extend bits later */
					ERROR(259, this, INULL);

			// todo: сюда надо добавить реакцию на внешние колбеки
			if (--ticker < 0) {
				// время потока вышло, переключим на следующий
				ticker = TICKS;
				if (R[0] != IFALSE) { // if no mcp, ignore
					// save vm state and enter mcp cont at R0
					bank = 0;
					acc += 4; //
					R[acc] = (word) this;

					word *state;
					state = (word*) new (TTHREAD, acc);
					for (ptrdiff_t pos = 1; pos < acc-1; pos++)
						state[pos] = R[pos];
					state[acc-1] = R[acc];

					this = (word *) R[0]; // mcp

					R[0] = IFALSE; // remove mcp cont
					// R3 marks the interop to perform
					// 1 - runnig and time slice exhausted
					// 10: breaked - call signal handler
					// 14: memory limit was exceeded
					R[3] = breaked ? ((breaked & 8) ? F(14) : F(10)) : F(1); // fixme - handle also different signals via one handler
					R[4] = (word) state;
					R[5] = F(breaked);
					R[6] = IFALSE;
					acc = 4; // вот эти 4 аргумента, что возвращаются из (run) после его завершения
					breaked = 0;
				}
				continue;
			}

			ip = (unsigned char *) &this[1];
			break; // goto invoke
		}
		// else
		ERROR(257, this, INULL); // not callable
	}

invoke:;
	// nargs and regs ready, maybe gc and execute ob

	// если места в буфере не хватает, то мы вызываем GC,
	//	а чтобы автоматически подкорректировались регистры,
	//	мы их складываем в память во временный кортеж.
	if (/*forcegc || */(fp >= heap->end - 16 * 1024)) {
		dogc (16 * 1024 * sizeof(word));
		ip = (unsigned char *) &this[1];

		// проверим, не слишком ли мы зажрались
		word heapsize = (word) heap->end - (word) heap->begin;
		if ((heapsize / (1024*1024)) > max_heap_size)
			breaked |= 8; // will be passed over to mcp at thread switch
	}

	// todo: add "NOP" function (may be 0x0 ?)
	// todo: add "HLT" function (may be 0x0 ?)

	// управляющие команды:
#	define APPLY 20 // apply-cont = 20+64
#	define RET   24
#	define RUN   50
	// безусловные переходы
#	define GOTO   2       // jmp a, nargs
//#	define GOTO_CODE 18   //
//#	define GOTO_PROC 19   //
//#	define GOTO_CLOS 21   //

#	define SYS   27

// 3, 4: OCLOSE
// 6, 7: CLOSE1

	// список команд смотреть в assembly.scm
#	define LDI   13       // похоже, именно 13я команда не используется, а только 77 (LDN), 141 (LDT), 205 (LDF)
#	define LD    14

#	define REFI   1       // refi a, p, t:   Rt = Ra[p], p unsigned (indirect-ref from-reg offset to-reg)
#	define MOVE   9       //
#	define MOV2   5       //

#	define JEQ    8       // jeq
#	define JP    16       // JZ, JN, JT, JF
#	define JF2   25       // jf2

	// примитивы языка:
#	define RAW   60
#	define RAWq  48       // (raw?), временное решение пока не придумаю как от него совсем избавиться

#	define CONS  51

#	define TYPE  15
#	define SIZE  36
#	define CAST  22

#	define CAR   52
#	define CDR   53
#	define REF   47

	// ?
#	define SET   45
#	define SETe  10

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
#		define SYSCALL_STAT 4
#		define SYSCALL_FSTAT 5
//#		define SYSCALL_LSTAT 6

#		ifndef SYSCALL_IOCTL
#		define SYSCALL_IOCTL 16
#		endif
#		define SYSCALL_IOCTL_TIOCGETA 19

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
#			include <sys/sysinfo.h>
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

	// tuples, trees
#	define MKT      23   // make tuple
#	define BIND     32
#	define LISTUPLE 35   // list -> typed tuple
#	define BINDFF   49

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

	// free numbers: 29(ncons), 30(ncar), 31(ncdr)

	// ip - счетчик команд (опкод - младшие 6 бит команды, старшие 2 бита - модификатор(если есть) опкода)
	// Rn - регистр машины (R[n])
	// An - регистр, на который ссылается операнд N (записанный в параметре n команды, начиная с 0)
	// todo: добавить в комменты к команде теоретическое количество тактов на операцию
	for(;;) {
		int op;//operation to execute:
		switch ((op = *ip++) & 0x3F) {
		case 0: // todo: change 0 to NOP, add new code for super_dispatch
			op = (ip[0] << 8) | ip[1]; // big endian
			// super_dispatch: run user instructions
			switch (op) {
			/* AUTOGENERATED INSTRUCTIONS */
			default:
				ERROR(258, F(op), ITRUE);
			}
			goto apply;

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

//		unused:
//		case 30:
//		case 31:

		case GOTO:
			this = (word *)A0; acc = ip[1];
			goto apply;

//		case GOTO_CODE:
//			this = (word *)A0; acc = ip[1];
//			ip = (unsigned char*) &this[1];
//			goto invoke;
//		case GOTO_PROC:
//			this = (word *)A0; acc = ip[1];
//			R1 = (word) this;
//			this = (word *) this[1];
//			ip = (unsigned char*) &this[1];
//			goto invoke;
//		case GOTO_CLOS:
//			this = (word *)A0; acc = ip[1];
//			R1 = (word) this;
//			this = (word *) this[1];
//			R2 = (word) this;
//			this = (word *) this[1];
//			ip = (unsigned char*) &this[1];
//			goto invoke;

		// apply
		case APPLY: {
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
				if (reg > NR) { // dummy handling for now
					fprintf(stderr, "TOO LARGE APPLY\n");
					exit(3);
				}
				R[reg++] = car (lst);
				lst = (word *) cdr (lst);
				arity++;
			}
			acc = arity;
			goto apply;
		}

		case RET: // return value
			this = (word *) R[3];
			R[3] = A0;
			acc = 1;
			goto apply;

		case SYS: // sys continuation op arg1 arg2
			this = (word *) R[0];
			R[0] = IFALSE;
			R[3] = A1; R[4] = A0; R[5] = A2; R[6] = A3;
			acc = 4;
			if (ticker > 10)
				bank = ticker; // deposit remaining ticks for return to thread
			ticker = TICKS;
			goto apply;

		case RUN: { // run thunk quantum
//			if (ip[0] != 4 || ip[1] != 5)
//				fprintf(stderr, "run R[%d], R[%d]\n", ip[0], ip[1]);
			this = (word *) A0;
			R[0] = R[3];
			ticker = bank ? bank : uvtoi (A1);
			bank = 0;
			CHECK(is_reference(this), this, RUN);

			word hdr = *this;
			if (typeof (hdr) == TTHREAD) {
				int pos = hdrsize(hdr) - 1;
				word code = this[pos];
				acc = pos - 3;
				while (--pos)
					R[pos] = this[pos];
				ip = ((unsigned char *) code) + W;
				continue; // no apply, continue
			}
			// else call a thunk with terminal continuation:
			R[3] = IHALT; // exit via R0 when the time comes
			acc = 1;
			goto apply;
		}


		/************************************************************************************/
		// операции с данными
		//	смотреть "vm-instructions" в "lang/assembly.scm"
		case LDI: {  // 13,  -> ldi(ldn, ldt, ldf){2bit what} [to]
			const word I[] = { F(0), INULL, ITRUE, IFALSE };
			A0 = I[op>>6];
			ip += 1; break;
		}
		case LD:
			A1 = F(ip[0]);
			ip += 2; break;


		case REFI: { //  1,  -> refi a, p, t:   Rt = Ra[p], p unsigned
			word* Ra = (word*)A0; A2 = Ra[ip[1]];
			ip += 3; break;
		}
		case MOVE: // move a, t:      Rt = Ra
			A1 = A0;
			ip += 2; break;
		case MOV2: // mov2 from1 to1 from2 to2
			A1 = A0;
			A3 = A2;
			ip += 4; break;


		case JEQ: /* jeq a b o, extended jump  */
			if (A0 == A1)
				ip += (ip[3] << 8) + ip[2]; // little-endian
			ip += 4; break;

		case JP: {  // JZ, JN, JT, JF a hi lo
			// was: FIXME, convert this to jump-const <n> comparing to make_value(<n>,TCONST),
			//  но я считаю, что надо просто добавить еще одну команду, а эти так и оставить
			const word I[] = { F(0), INULL, ITRUE, IFALSE };
			if (A0 == I[op>>6])
				ip += (ip[2] << 8) + ip[1]; // little-endian
			ip += 3; break;
		}

		// используется в (func ...) в primop.scm
		case JF2: { // jmp-nargs (>=) a hi lo
			int arity = ip[0];
			if (acc == arity) {
				if (op & 0x40) // add empty extra arg list
					R[acc + 3] = INULL;
			}
			else
			if (acc > arity && (op & 0x40)) {
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


		case 3: OCLOSE(TCLOS); continue;
		case 4: OCLOSE(TPROC); continue;
		case 6: CLOSE1(TCLOS); continue;
		case 7: CLOSE1(TPROC); continue;

		/************************************************************************************/
		// более высокоуровневые конструкции
		//	смотреть "owl/primop.scm" и "lang/assemble.scm"

		// todo: add numeric argument as "length" parameter
		case RAW: { // raw type lst
			word *lst = (word*) A1;
			int len = 0;
			word* p = lst;
			while (is_pair(p)) {
				len++;
				p = (word*)cdr (p);
			}

			if ((word) p == INULL && len <= MAXOBJ) {
				int type = uvtoi (A0);
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

			ip += 3; break;
		}
		case RAWq: {
			word* T = (word*) A0;
			if (is_reference(T) && is_rawobject(*T))
				A1 = ITRUE;
			else
				A1 = IFALSE;
			ip += 2; break;
		}

		// операции посложнее
		case CONS:   // cons a b r:   Rr = (cons Ra Rb)
			A2 = (word) new_pair(A0, A1); // видимо, вызывается очень часто, так как замена на макрос дает +10% к скорости
			ip += 3; break;


		case TYPE: { // type o r <- actually sixtet
			word T = A0;
			if (is_reference(T))
				T = *((word *) (T)); // todo: add RAWNESS to this (?)
			A1 = F(typeof (T));
			ip += 2; break;
		}

		case SIZE: { // size o r
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

		// todo: переделать! и вообще, найти как от этой команды избавится!
		case CAST: { // cast obj type -> result
			if (!is_value(A1))
				break;
			word T = A0;
			word type = uvtoi(A1) & 63;

//			if (type == TPORT && typeof(T) == TINT) {
//				A2 = IFALSE;
//			}
			// todo: добавить каст с конверсией. например, из большого целого числа в handle или float
			// это лучше сделать тут, наверное, а не отдельной командой
			if (is_value(T)) {
				int val = value(T);
				if (type == TPORT) {
					if (val >= 0 && val <= 2)
						A2 = make_port(val);
					else
						A2 = IFALSE;
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

		case CDR: {  // car a -> r
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


		case SET: { // (set object position value), position starts from 1
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
					((char*)&car(newobj))[pos - 1] = (char)uvtoi(A2);
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

		case SETe: { // (set! variable position value)
			word *p = (word *)A0;
			word pos = uvtoi (A1);

			CHECK(is_value(A2), A2, 10001); // todo: move to silent return IFALSE

			if (!is_reference(p))
				A3 = IFALSE;
			else
			if (is_rawobject(*p)) {
				if (pos < (hdrsize(*p)-1)*W - padsize(*p) + 1)
					((char*)&car(p))[pos - 1] = (char) uvtoi(A2);
				A3 = (word) p;
			}
			else
			if (hdrsize(*p) < pos || !pos)
				A3 = IFALSE;
			else {
				p[pos] = A2;
				A3 = (word) p;
			}
			ip += 4; break; }

		case EQ: // eq a b r
			A2 = (A0 == A1) ? ITRUE : IFALSE;
			ip += 3; break;

		case LESS: {// less? a b r
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
			word r = value(A0) + value(A1);
			A2 = F(r & FMAX);
			A3 = (r & HIGHBIT) ? ITRUE : IFALSE; // overflow?
			ip += 4; break; }
		case SUBTRACTION: { // vm:sub a b  r u, args prechecked, signs ignored
			word r = (value(A0) | HIGHBIT) - value(A1);
			A2 = F(r & FMAX);
			A3 = (r & HIGHBIT) ? IFALSE : ITRUE; // unsigned?
			ip += 4; break; }

		case MULTIPLICATION: { // vm:mul a b l h
			big r = (big) value(A0) * (big) value(A1);
			A2 = F(r & FMAX);
			A3 = F(r>>FBITS); //  & FMAX)
			ip += 4; break; }
		case DIVISION: { // vm:div ah al b  qh ql r, b != 0, int64(32) / int32(16) -> int64(32), as fixnums
			big a = (big) value(A1) | (((big) value(A0)) << FBITS);
			big b = (big) value(A2);

			// http://stackoverflow.com/questions/7070346/c-best-way-to-get-integer-division-and-remainder
			big q = a / b;
			big r = a % b;

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
			big r = ((big) value(A0)) << (FBITS - value(A1));
			A2 = F(r>>FBITS);
			A3 = F(r & FMAX);
			ip += 4; break; }
		case SHIFT_LEFT: { // vm:shl a b hi lo
			big r = ((big) value(A0)) << (value(A1));
			A2 = F(r>>FBITS);
			A3 = F(r & FMAX);
			ip += 4; break; }


		// ошибка арности
		case 17:
			ERROR(17, this, F(acc));
			break;

		// todo: add the instruction name
		case 29:
			A0 = F(W);
			ip += 1; break;
		case 30:
		case 33: // todo: change to 30
			A0 = F(FMAX);
			ip += 1; break;
		// todo: add the instruction name
		case 31:
		case 34: // todo: change to 31
			A0 = F(FBITS);
			ip += 1; break;
			// todo: add the instruction name

		// (vm:version)
		case 62: // get virtual machine info
			A0 = (word) new_pair(TPAIR,
					new_string(__OLVM_NAME__,    sizeof(__OLVM_NAME__)   -1),
					new_string(__OLVM_VERSION__, sizeof(__OLVM_VERSION__)-1));
			ip += 1; break;

		case 11: { // (set-car! pair value)
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
		}


		// make tuple
		case MKT: { // mkt t s f1 .. fs r
			word type = *ip++;
			word size = *ip++ + 1; // the argument is n-1 to allow making a 256-tuple with 255, and avoid 0-tuples
			word *p = new (type, size+1), i = 0; // s fields + header
			while (i < size) {
				p[i+1] = R[ip[i]];
				i++;
			}
			R[ip[i]] = (word) p;
			ip += size+1; break;
		}

		// make typed tuple from list
		case LISTUPLE: { // listuple type size lst to
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
		}



		// bind tuple to registers, todo: rename to bind-t or bindt or bnt
		case BIND: { /* bind <tuple > <n> <r0> .. <rn> */
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
		case BINDFF: { // with-ff <node >l k v r */ // bindff - bind node left key val right, filling in #false when implicit
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
		case MKBLACK: // mkblack l k v r t
		case MKRED: { // mkred l k v r t
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

		// fftoggle - toggle node color
		case FFTOGGLE: {
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

		case FFREDQ: { // red? node r
			word node = A0;
			if (is_reference(node)) // assert to IEMPTY || is_reference() ?
				node = *(word*)node;
			if ((typeof (node) & (0x3C | FFRED)) == (TFF|FFRED))
				A1 = ITRUE;
			else
				A1 = IFALSE;
			ip += 2; break;
		}

		case FFRIGHTQ: { // ff:right? node r
			word node = A0;
			if (is_reference(node)) // assert to IEMPTY || is_reference() ?
				node = *(word*)node;
			if ((typeof (node) & (0x3C | FFRIGHT)) == (TFF|FFRIGHT))
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
		//	CHECK(is_fixed(A0) && typeof (A0) == TFIX, A0, SYSCALL);
			word op = uvtoi (A0);
			word a = A1, b = A2, c = A3;
//			fprintf(stderr, "SYSCALL(%d, %d, %d, %d)\n", op, a, b, c);

			switch (op + seccompp) {

			// (READ fd count) -> buf
			// http://linux.die.net/man/2/read
			// count<0 means read all
			case SYSCALL_READ + SECCOMP:
			case SYSCALL_READ: {
				CHECK(is_port(a), a, SYSCALL);
				int portfd = port(a);
				int size = svtoi (b);

				if (size < 0)
					size = (heap->end - fp) * W - MEMPAD;
				else // todo: change to ol->gc(size) or similar
				if (size > (heap->end - fp) * W - MEMPAD)
					dogc(size);

				int got;
#ifdef _WIN32
				if (!_isatty(portfd) || _kbhit()) { // we don't get hit by kb in pipe
					got = read(portfd, (char *) &fp[1], size);
				} else {
					got = -1;
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
				if (size > length || size == -1)
					size = length;

				int wrote;

#if 0//EMBEDDED_VM
				if (fd == 1) // stdout wrote to the fo
					wrote = fifo_puts(fo, ((char *)buff)+W, len);
				else
#endif
					wrote = write(portfd, (char*)&buff[1], size);

				if (wrote > 0)
					result = (word*) itoun (wrote);
				else if (errno == EAGAIN || errno == EWOULDBLOCK)
					result = (word*) itoun (0);

				break;
			}

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

				break;
			}

			// STATs
				word* unstat(struct stat* st) {
					return new_tuple(
							IFALSE, // st_dev   - устройство
							IFALSE, // st_ino   - inode
							IFALSE, // st_mode  - режим доступа
							IFALSE, // st_nlink - количество жестких ссылок
							itoun(st->st_uid),//- идентификатор пользователя-владельца
							itoun(st->st_gid),//- идентификатор группы-владельца
							IFALSE, // st_rdev  - тип устройства (если это устройство)
							itoun(st->st_size),// общий размер в байтах
							IFALSE, // st_blksize размер блока ввода-вывода в файловой системе
							IFALSE, // st_blocks  количество выделенных блоков
							// Since Linux 2.6, the kernel supports nanosecond
							//   precision for the following timestamp fields.
							// but we do not support this for a while
							itoun(st->st_atime),//время последнего доступа (в секундах)
							itoun(st->st_mtime),//время последней модификации (в секундах)
							itoun(st->st_ctime) //время последнего изменения (в секундах)
					);
				}
			case SYSCALL_STAT: {
				if (! is_string(a))
					break;
				CHECK(is_string(a), a, SYSCALL);
				word* s = &car (a);

				struct stat st;
				if (stat((char*) s, &st) < 0)
					break;

				result = unstat(&st);
				break;
			}
			case SYSCALL_FSTAT: {
				CHECK(is_port(a), a, SYSCALL);
				int portfd = port(a);

				struct stat st;
				if (fstat(portfd, &st) < 0)
					break;

				result = unstat(&st);
				break;
			}

			// IOCTL (syscall 16 fd request #f)
			case SYSCALL_IOCTL + SECCOMP:
			case SYSCALL_IOCTL: {
				if (!is_port(a))
					break;

				int portfd = port(a);
				int ioctl = uvtoi(b);

				switch (ioctl + seccompp) {
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
				int sock = socket(PF_INET, SOCK_STREAM, 0);
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
				if (shutdown(socket, 2) != 0) // both
					break;

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
			// TODO: change to "select" call
			// NANOSLEEP
			case 35: {
				//CHECK(is_number(a), a, 35);

				if (seccompp) {
					result = (word*) ITRUE;
					break;
				}

#ifdef _WIN32// for Windows
				Sleep(untoi (a) / 1000000); // in ms
#else//			for Linux:
				struct timespec ts = { untoi(a) / 1000000000, untoi(a) % 1000000000 };
				struct timespec rem;
				if (nanosleep(&ts, &rem) == 0)
					result = (word*) ITRUE;
				else
					result = itoun((rem.tv_sec * 1000000000 + rem.tv_nsec));
#endif
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
					word* C = (word*)c;

					assert ((word)B == INULL || is_pair(B));
					assert ((word)C == IFALSE);

					word* (*function)(OL*, word*) = (word* (*)(OL*, word*)) car(A);  assert (function);

					ol->heap.fp = fp;
					result = function(ol, B);
					fp = ol->heap.fp;
					break;
				}
#endif
				// if a is string:
				// todo: add case (cons program environment)
				if (is_string(a)) {
					char* command = (char*)&car(a);
#ifndef _WIN32
					int child = fork();
					if (child == 0) {
						fprintf(stderr, "forking %s\n", command);
						if (is_pair (c)) {
							const int in[3] = { STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO };
							for (ptrdiff_t i = 0; i < sizeof(in) / sizeof(in[0]) && is_pair(c); i++)
								if (is_port(car(c)))
									dup2(port(car(c)), in[i]), c = cdr (c);
						}
// DEBUG:					else if (c != IFALSE)
//								fprintf(stderr, "invalid value for execve\n");
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
					break;
				}
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
				else if (typeof (B) == TFIX)
					seconds = uvtoi(B);
 				else if (is_reference(B) && typeof (*B) == TINT)
					seconds = untoi(B);
				else
					break;
#if HAS_STRFTIME
				word* A = (word*) a;
				if (is_string(A)) {
					char* ptr = (char*) &fp[1];
					struct tm * timeinfo = localtime(&seconds);
					if (!timeinfo) // error???
						break;
					// The environment variables TZ and LC_TIME are used!
					size_t len = strftime(ptr, (size_t) (heap->end - fp - MEMPAD), (char*)&A[1], timeinfo);
					result = new_bytevector(TSTRING, len+1);
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
				if (!seccompp)
					free(heap->begin); // освободим занятую память
				heap->begin = 0;
				exit(svtoi(a));
				__builtin_unreachable(); // сюда мы уже не попадем
			}

			// UNAME (uname)
			// http://linux.die.net/man/2/uname
			case 63: {
				#ifdef _WIN32
				struct utsname
				{
					char sysname[65];
				    char nodename[65];
				    char release[65];
				    char version[65];
				    char machine[65];
				};

				int uname(struct utsname* out) {
					DWORD nodenamesize = sizeof(out->nodename);
					GetComputerNameA(out->nodename, &nodenamesize);

/*					SYSTEM_INFO si;
					VOID (WINAPI *GetNativeSystemInfo)(LPSYSTEM_INFO) = (VOID (WINAPI*)(LPSYSTEM_INFO))
							GetProcAddress(GetModuleHandle("kernel32.dll"), "GetNativeSystemInfo");
					if (GetNativeSystemInfo)
						GetNativeSystemInfo(&si);
					else
						GetSystemInfo(&si);

					OSVERSIONINFOEXA oi;
					oi.dwOSVersionInfoSize = (DWORD)sizeof(OSVERSIONINFOEXA);
					if (!GetVersionExA((OSVERSIONINFOA*)&oi)) {
						oi.dwOSVersionInfoSize = (DWORD)sizeof(OSVERSIONINFOA);
						if (!GetVersionExA((LPOSVERSIONINFOA)&oi)) {
							// todo: try GetVersion
							fprintf(stderr, "GetVersionEx failed");
							return +13;
						}
					}*/

					strncpy(out->sysname, "Windows", sizeof(out->sysname));
/*					if (oi.dwPlatformId < VER_PLATFORM_WIN32_NT) {
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
							switch (256 * oi.dwMajorVersion + oi.dwMinorVersion) {
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
//
//					            case 0x600: case 0x601:
//					               if (oi.wProductType == VER_NT_WORKSTATION) ps1 = oi.dwMinorVersion == 0 ? "Vista" : "7";
//					               else ps1 =  oi.dwMinorVersion == 0 ? "Server 2008" : "Server 2008 R2";
//
//					               DWORD dwType = PRODUCT_UNDEFINED;
//					               if (NULL != (u.f=get_func("GetProductInfo"))) u.GetProductInfo(oi.dwMajorVersion,oi.dwMinorVersion,0,0,&dwType);
//					               switch( dwType ) {
//					                  case PRODUCT_ULTIMATE:          ps2 = "Ultimate Edition";       break;
//					                  case PRODUCT_HOME_PREMIUM:      ps2 = "Home Premium Edition";   break;
//					                  case PRODUCT_HOME_BASIC:        ps2 = "Home Basic Edition";     break;
//					                  case PRODUCT_ENTERPRISE:        ps2 = "Enterprise Edition";     break;
//					                  case PRODUCT_BUSINESS:          ps2 = "Business Edition";       break;
//					                  case PRODUCT_STARTER:           ps2 = "Starter Edition";        break;
//					                  case PRODUCT_CLUSTER_SERVER:    ps2 = "Cluster Server Edition"; break;
//					                  case PRODUCT_DATACENTER_SERVER: ps2 = "Datacenter Edition";     break;
//					                  case PRODUCT_DATACENTER_SERVER_CORE: ps2 = "Datacenter Edition (core installation)"; break;
//					                  case PRODUCT_ENTERPRISE_SERVER: ps2 = "Enterprise Edition";     break;
//					                  case PRODUCT_ENTERPRISE_SERVER_CORE: ps2 = "Enterprise Edition (core installation)"; break;
//					                  case PRODUCT_ENTERPRISE_SERVER_IA64: ps2 = "Enterprise Edition for Itanium-based Systems"; break;
//					                  case PRODUCT_SMALLBUSINESS_SERVER: ps2 = "Small Business Server"; break;
//					                  case PRODUCT_SMALLBUSINESS_SERVER_PREMIUM: ps2 = "Small Business Server Premium Edition"; break;
//					                  case PRODUCT_STANDARD_SERVER:   ps2 = "Standard Edition";       break;
//					                  case PRODUCT_STANDARD_SERVER_CORE: ps2 = "Standard Edition (core installation)"; break;
//					                  case PRODUCT_WEB_SERVER:        ps2 = "Web Server Edition";     break;
//					               }
//					            break;
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
			// GETRUSAGE (getrusage)
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
			case SYSCALL_GETRUSAGE: {
				struct rusage u;
				// arguments currently ignored. used RUSAGE_SELF
				if (getrusage(RUSAGE_SELF, &u) == 0)
					result = new_tuple(
							new_pair (itoun(u.ru_utime.tv_sec), itoun(u.ru_utime.tv_usec)),
							new_pair (itoun(u.ru_stime.tv_sec), itoun(u.ru_stime.tv_usec))
/*
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
							itoun(info.procs) // procs is short*/
					);
				break;

			}
			#endif

			#if SYSCALL_SYSINFO
			// SYSINFO (sysinfo)
			case SYSCALL_SYSINFO: {
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
							itoun(info.procs) // procs is short
					);
				break;
			}
			#endif

			// todo: add syscall 100 (times)


			// =- 1000+ -===========================================================================
			// other internal commands
			case 1000:
				dogc(0);
				break;
			case 1001:
				if (is_reference(a)) {
					word hdr = *(word*)a;
					if (is_rawobject(hdr))
						result = (word*)ITRUE;
				}
				break;
			case 1007: // set memory limit (in mb) / // todo: переделать на другой номер
				result = itoun (max_heap_size);
				max_heap_size = uvtoi (a);
				break;
			case 1009: // get memory limit (in mb) / // todo: переделать на другой номер
				result = itoun (max_heap_size);
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
			case 1014: { /* set-ticks n _ _ -> old */
				result = itoun (slice);
				slice  = uvtoi (a);
				break;
			}

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

				void* module;
				if ((word) filename == INULL)
					module = dlopen(NULL, mode); // If filename is NULL, then the returned handle is for the main program.
				else if (is_reference(filename) && typeof (*filename) == TSTRING)
					module = dlopen((char*) &filename[1], mode);
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
				if (!(is_value(symbol) || typeof (*symbol) == TSTRING))
					break;

				word function = (word)dlsym(module, is_value(symbol)
						? (char*) value((word)symbol)
						: (char*) &symbol[1]);
				if (function)
					result = new_native_function(function);
				else
					fprintf(stderr, "dlsym failed: %s\n", dlerror());
				break;
			}
			case SYSCALL_DLERROR: { // (dlerror)
				char* error = dlerror();
				if (error)
					result = new_string(error);
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
					seccompp = SECCOMP;
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
			}
			A4 = (word) result;
			ip += 5; break;
		}
		default:
			ERROR(op, new_string("Invalid opcode"), ITRUE);
			break;
		}
		continue;
	}// while(1);

invoke_mcp: /* R4-R6 set, set R3=cont and R4=interop and call mcp */
	this = (word *) R[0];
	R[0] = IFALSE;
	R[3] = F(3);
	if (is_reference(this)) {
		acc = 4;
		goto apply;
	}
	fprintf(stderr, "invoke_mcp failed\n");
	return (void*) -1; // no mcp to handle error (fail in it?), so nonzero exit
}


// ======================================================================
//       загрузчик скомпилированного образа и его десериализатор
//

// fasl decoding
// возвращает новый топ стека
static __inline__
word* deserialize(word *ptrs, int nobjs, unsigned char *bootstrap, word* fp)
{
	unsigned char* hp = bootstrap;
//	if (*hp == '#') // этот код не нужен, так как сюда приходит уже без шабанга
//		while (*hp++ != '\n') continue;

	// tbd: comment
	// todo: есть неприятный момент - 64-битный код иногда вставляет в fasl последовательность большие числа
	//	а в 32-битном коде это число должно быть другим. что делать? пока х.з.
	word get_nat()
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
	}

	// tbd: comment
	void decode_field(word *ptrs, int pos) {
		if (*hp == 0) { // fixnum
			hp++;
			unsigned char type = *hp++;
			word val = make_value(type, get_nat());
			*fp++ = val;
		} else {
			word diff = get_nat();
			*fp++ = ptrs[pos-diff];
		}
	}

	// function entry:
	for (ptrdiff_t me = 0; me < nobjs; me++) {
		ptrs[me] = (word) fp;

		switch (*hp++) { // todo: adding type information here would reduce fasl and executable size
		case 1: {
			int type = *hp++;
			int size = get_nat();
			*fp++ = make_header(type, size+1); // +1 to include header in size
			while (size--)
				decode_field(ptrs, me);
			break;
		}
		case 2: {
			int type = *hp++ & 31; /* low 5 bits, the others are pads */
			int size = get_nat();
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

static
// функция подсчета количества объектов в загружаемом образе
int count_fasl_objects(word *words, unsigned char *lang) {
	unsigned char* hp;

	word decode_word() {
		word nat = 0;
		char i;
		do {
			nat <<= 7;
			i = *hp++;
			nat = nat + (i & 127);
		}
		while (i & 128);
		return nat;
	}

	// count:
	int n = 0;
	hp = lang;

	int allocated = 0;
	while (*hp != 0) {
		switch (*hp++) {
		case 1: { // fix
			hp++; ++allocated;
			int size = decode_word();
			while (size--) {
				//decode_field:
				if (*hp == 0)
					hp += 2;
				decode_word(); // simply skip word
				++allocated;
			}
			break;
		}
		case 2: { // pointer
			hp++;// ++allocated;
			int size = decode_word();
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

void fail(int num, char* message)
{
	fprintf(stderr, "%s", message);
	exit(num);
}

#if !EMBEDDED_VM
int main(int argc, char** argv)
{
	unsigned char* bootstrap = language;

	// обработка аргументов:
	//	первый из них (если есть) - название исполняемого скрипта
	//	                            или "-", если это будет stdin
	//  остальные - командная строка
	// todo: перенести в eval !
	if (argc > 1 && strcmp(argv[1], "-") != 0) {
		// todo: use mmap()
		struct stat st;
		if (stat(argv[1], &st) || st.st_size == 0)
			fail(errno, "File not found or empty");	// не найден файл или он пустой

		char bom;
		int bin = open(argv[1], O_RDONLY | O_BINARY, (S_IRUSR | S_IWUSR));
		if (!bin)
			fail(errno, "Can't open file");	// не смогли файл открыть

		int pos = read(bin, &bom, 1); // прочитаем один байт
		if (pos < 1)
			fail(errno, "Can't read file");	// не смогли файл прочитать

		// переделать
		if (bom == '#') { // skip possible hashbang
			while (read(bin, &bom, 1) == 1 && bom != '\n')
				st.st_size--;
			st.st_size--;
			if (read(bin, &bom, 1) < 0)
				fail(errno, "Can't read file");
			st.st_size--;
		}

		if (bom > 3) {	// ха, это текстовая программа (скрипт)!
			// а значит что? что файл надо замапить вместо stdin (нет!!! - надо передать его первым параметром в загрузчик)
			// rollback назад, на 1 прочитанный символ
#ifndef NAKED_VM
			lseek(bin, -1, SEEK_CUR);
			dup2(bin, STDIN_FILENO);
			close(bin);
#else
			fail(6, "Invalid binary script"); // некому проинтерпретировать скрипт
#endif
		}
		else {
			// иначе загрузим его
			unsigned char* ptr = (unsigned char*)malloc(st.st_size);
			if (ptr == NULL)
				fail(3, "Can't alloc memory");	// опа, не смогли выделить память...

			ptr[0] = bom;
			while (pos < st.st_size) {
				int n = read(bin, &ptr[pos], st.st_size - pos);
				if (n < 0)
					fail(errno, "Can't read file"); // не смогли прочитать
				pos += n;
			}
			close(bin);

			bootstrap = ptr;
		}
	}
#ifdef NAKED_VM
	else
		fail(7, "Invalid binary script");
#endif

#if	HAS_SOCKETS && defined(_WIN32)
	WSADATA wsaData;
	int sock_init = WSAStartup(MAKEWORD(2,2), &wsaData);
	if (sock_init  != 0) {
		printf("WSAStartup failed with error: %d\n", sock_init);
		return 1;
	}
	AllocConsole();
#endif

	set_signal_handler();

	OL* olvm = OL_new(bootstrap, bootstrap != language ? free : NULL);
	void* r = OL_eval(olvm,
#ifdef NAKED_VM
	argc-1, &argv[1]);
#else
	argc, argv);
#endif
	OL_free(olvm);

#if	HAS_SOCKETS && defined(_WIN32)
	WSACleanup();
#endif

	return is_value(r) ? svtoi (r) : -1;
}
#endif

//int    // this is NOT thread safe function
//olvm(unsigned char* bootstrap, void (*release)(void*))
OL*
OL_new(unsigned char* bootstrap, void (*release)(void*))
{
	// если это текстовый скрипт, замапим его на stdin, а сами используем встроенный (если) язык
	if (bootstrap[0] > 3) {
		char filename[16]; // lenght of above string
		strncpy(filename, "/tmp/olvmXXXXXX", sizeof(filename));

		int f = mkstemp(filename); // временный файл
		if (!write(f, bootstrap, strlen((char*) bootstrap)))
			;
		close(f);

		dup2(open(filename, O_BINARY, S_IRUSR), STDIN_FILENO);
		unlink(filename); // сразу приберем за собой

		if (release)
			release(bootstrap);
		bootstrap = language;
		release = 0;
	}

	// если отсутствует исполнимый образ
	if (bootstrap == 0) {
		fprintf(stderr, "no boot image found\n");
		return 0;
	}

	// ===============================================================
	// создадим виртуальную машину
	OL *handle = malloc(sizeof(OL));
	memset(handle, 0x0, sizeof(OL));

	// подготовим очереди в/в
	//fifo_clear(&handle->i);
	//fifo_clear(&handle->o); (не надо, так как хватает memset вверху)


	// а теперь поработаем с сериализованным образом:
	word nwords = 0;
	word nobjs = count_fasl_objects(&nwords, bootstrap); // подсчет количества слов и объектов в образе
	nwords += (nobjs + 2); // for ptrs

	heap_t* heap = &handle->heap;
	word *fp;

	// выделим память машине:
	int max_heap_size = (W == 4) ? 4096 : 65535; // can be set at runtime
	int required_memory_size = (INITCELLS + MEMPAD + nwords + 64 * 1024); // 64k objects for memory
	heap->begin =
	heap->genstart = (word*) malloc(required_memory_size * sizeof(word)); // at least one argument string always fits
	if (!heap->begin) {
		fprintf(stderr, "Failed to allocate %d words for vm memory\n", required_memory_size);
		goto fail;
	}
	// ok
	heap->end = heap->begin + required_memory_size;
	handle->max_heap_size = max_heap_size;

	// Десериализация загруженного образа в объекты
	fp = heap->begin;
	word *ptrs = new(TCONST, nobjs+1, 0);
	fp = deserialize(&ptrs[1], nobjs, bootstrap, fp);

	if (fp == 0)
		goto fail;

	// все, программа в памяти, можно освобождать исходник
	if (release)
		release(bootstrap);

	heap->fp = fp;
	return handle;
fail:
	if (release)
		release(bootstrap);
	OL_free(handle);
	return 0;
}

OL* OL_free(OL* ol)
{
	free(ol->heap.begin);
	free(ol);
	return 0;
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
	word* userdata = (word*) INULL;
	{
		word* fp = handle->heap.fp;
#if !EMBEDDED_VM
		argv += argc - 1;
		for (ptrdiff_t i = argc; i > 1; i--, argv--) {
			char *pos = (char*)(fp + 1);
			char *v = *argv;
			while ((*pos = *v++) != 0)
				pos++;
			int length = pos - (char*)fp - W;
			if (length > 0) // если есть что добавить
				userdata = new_pair (new_bytevector(TSTRING, length), userdata);
		}
#else
		{
			char* filename = "-";
			char *pos = filename;

			int len = 0;
			while (*pos++) len++;

			userdata = new_pair (new_string (filename, len), userdata);
		}
#endif
		handle->heap.fp = fp;
	}

	// результат выполнения скрипта
	return runtime(handle, userdata);
}

/**
 * PInvoke - Platform Invoke
 *
 * а тут у нас реализация pinvoke механизма. пример в lib/opengl.scm, lib/sqlite.scm, etc.
 */
#if HAS_PINVOKE
#ifdef _WIN32
__declspec(dllexport)
#else
__attribute__
		((__visibility__("default")))
#endif
word* pinvoke(OL* self, word* arguments)
{
	// get memory pointer
	heap_t* heap = &self->heap;
	word*
	fp = heap->fp;

	// http://byteworm.com/2010/10/12/container/ (lambdas in c)

	// https://en.wikipedia.org/wiki/X86_calling_conventions
	// x86 conventions: cdecl, syscall(OS/2), optlink(IBM)
	// pascal(OS/2, MsWin 3.x, Delphi), stdcall(Win32),
	// fastcall(ms), vectorcall(ms), safecall(delphi),
	// thiscall(ms)

	// x64 calling conventions: linux, windows
#if	__amd64__
	word call(void* function, word argv[], int argc) {
#else
	word call(int returntype, void* function, word argv[], int argc) {
#endif

		// todo: ограничиться количеством функций поменьше
		//	а можно сделать все в одной switch:
		// i += 5 * (returntype >> 6); // 5 - количество поддерживаемых функций
		// todo: а можно лямбдой оформить и засунуть эту лябмду в функцию еще в get-proc-address
		// todo: проанализировать частоту количества аргументов и переделать все в
		//   бинарный if

		// __attribute__((stdcall))
/*						__stdcall // gcc style for lambdas in pure C
		int (*stdcall[])(char*) = {
				({ int $(char *str){ printf("Test: %s\n", str); } $; })
		};*/
		// http://www.agner.org/optimize/calling_conventions.pdf
#if __amd64__
		#define CALLFLOATS(conv) \
			case 1 + 0x0100:\
			         return ((conv word (*)  (float))\
			                 function) (*(float*)&argv[0]);\
			case 2 + 0x0200:\
			         return ((conv word (*)  (word, float))\
			                 function) (argv[0], *(float*)&argv[1]);\
			case 3 + 0x0400:\
			         return ((conv word (*)  (word, word, float))\
			                 function) (argv[0], argv[1],\
			                            *(float*)&argv[2]);\
			case 3 + 0x0600:\
			         return ((conv word (*)  (word, float, float))\
			                 function) (argv[0], *(float*)&argv[1],\
			                            *(float*)&argv[2]);\
			case 4 + 0x0E00:\
			         return ((conv word (*)  (word, float, float, float))\
			                 function) (argv[0], *(float*)&argv[1],\
			                            *(float*)&argv[2], *(float*)&argv[3]);\
			\
			case 2 + 0x0300:\
			         return ((conv word (*)  (float, float))\
			                 function) (*(float*)&argv[0], *(float*)&argv[1]);\
			case 3 + 0x0700:\
			         return ((conv word (*)  (float, float, float))\
			                 function) (*(float*)&argv[0], *(float*)&argv[1],\
			                            *(float*)&argv[2]);\
			case 4 + 0x0F00:\
			         return ((conv word (*)  (float, float, float, float))\
			                 function) (*(float*)&argv[0], *(float*)&argv[1],\
			                            *(float*)&argv[2], *(float*)&argv[3]);
#else
		#define CALLFLOATS(conv)
#endif

#if __amd64__
		#define CALLDOUBLES(conv) \
			case 4 + 0x0020000:\
			         return ((conv word (*)  (word, double, word, word))\
			                 function) (argv[0], *(double*)&argv[1], argv[2], argv[3]);\
			case 2 + 0x0030000:\
			         return ((conv word (*)  (double, double))\
			                 function) (*(double*)&argv[0], *(double*)&argv[1]);\
			case 3 + 0x0070000:\
			         return ((conv word (*)  (double, double, double))\
			                 function) (*(double*)&argv[0], *(double*)&argv[1],\
			                            *(double*)&argv[2]);\
			case 4 + 0x00F0000:\
			         return ((conv word (*)  (double, double, double, double))\
			                 function) (*(double*)&argv[0], *(double*)&argv[1],\
			                            *(double*)&argv[2], *(double*)&argv[3]);\
			case 6 + 0x03F0000:\
			         return ((conv word (*)  (double, double, double, double, double, double))\
			                 function) (*(double*)&argv[0], *(double*)&argv[1],\
			                            *(double*)&argv[2], *(double*)&argv[3],\
			                            *(double*)&argv[4], *(double*)&argv[5]);\
			case 9 + 0x1FF0000:\
			         return ((conv word (*)  (double, double, double,\
			                                  double, double, double,\
									          double, double, double))\
			                 function) (*(double*)&argv[0], *(double*)&argv[1], *(double*)&argv[2],\
			                            *(double*)&argv[3], *(double*)&argv[4], *(double*)&argv[5],\
								        *(double*)&argv[6], *(double*)&argv[7], *(double*)&argv[8]);
#else
		#define CALLDOUBLES(conv)\
			case 18: return ((conv word (*)  (word, word, word, word, word, word, \
			                                  word, word, word, word, word, word, \
			                                  word, word, word, word, word, word))\
			                 function) (argv[ 0], argv[ 1], argv[ 2], argv[ 3], \
			                            argv[ 4], argv[ 5], argv[ 6], argv[ 7], \
			                            argv[ 8], argv[ 9], argv[10], argv[11], \
			                            argv[12], argv[13], argv[14], argv[15], \
			                            argv[16], argv[17]);
#endif

		#define CALL(conv) \
			switch (argc) {\
			case  0: return ((conv word (*)  ())\
							 function) ();\
			case  1: return ((conv word (*)  (word))\
							 function) (argv[ 0]);\
			case  2: return ((conv word (*)  (word, word))\
			                 function) (argv[ 0], argv[ 1]);\
			case  3: return ((conv word (*)  (word, word, word))\
			                 function) (argv[ 0], argv[ 1], argv[ 2]);\
			case  4: return ((conv word (*)  (word, word, word, word))\
			                 function) (argv[ 0], argv[ 1], argv[ 2], argv[ 3]);\
			case  5: return ((conv word (*)  (word, word, word, word, word))\
			                 function) (argv[ 0], argv[ 1], argv[ 2], argv[ 3],\
			                            argv[ 4]);\
			case  6: return ((conv word (*)  (word, word, word, word, word, word))\
			                 function) (argv[ 0], argv[ 1], argv[ 2], argv[ 3],\
			                            argv[ 4], argv[ 5]);\
			case  7: return ((conv word (*)  (word, word, word, word, word, word, \
			                                  word))\
			                 function) (argv[ 0], argv[ 1], argv[ 2], argv[ 3],\
			                            argv[ 4], argv[ 5], argv[ 6]);\
			case  8: return ((conv word (*)  (word, word, word, word, word, word, \
			                                  word, word))\
			                 function) (argv[ 0], argv[ 1], argv[ 2], argv[ 3],\
			                            argv[ 4], argv[ 5], argv[ 6], argv[ 7]);\
			case  9: return ((conv word (*)  (word, word, word, word, word, word, \
			                                  word, word, word))\
			                 function) (argv[ 0], argv[ 1], argv[ 2], argv[ 3],\
			                            argv[ 4], argv[ 5], argv[ 6], argv[ 7],\
			                            argv[ 8]);\
			case 10: return ((conv word (*)  (word, word, word, word, word, word, \
			                                  word, word, word, word))\
			                 function) (argv[ 0], argv[ 1], argv[ 2], argv[ 3],\
			                            argv[ 4], argv[ 5], argv[ 6], argv[ 7],\
			                            argv[ 8], argv[ 9]);\
			case 11: return ((conv word (*)  (word, word, word, word, word, word, \
			                                  word, word, word, word, word))\
			                 function) (argv[ 0], argv[ 1], argv[ 2], argv[ 3],\
			                            argv[ 4], argv[ 5], argv[ 6], argv[ 7],\
			                            argv[ 8], argv[ 9], argv[10]);\
			case 12: return ((conv word (*)  (word, word, word, word, word, word, \
			                                  word, word, word, word, word, word))\
			                 function) (argv[ 0], argv[ 1], argv[ 2], argv[ 3], \
			                            argv[ 4], argv[ 5], argv[ 6], argv[ 7], \
			                            argv[ 8], argv[ 9], argv[10], argv[11]);\
			CALLFLOATS(conv)\
			CALLDOUBLES(conv)\
			default: fprintf(stderr, "Unsupported parameters count for pinvoke function: %d", argc);\
				break;\
			}
#ifdef __linux__
		CALL(); // cdecl
#endif
#ifdef __unix__
		CALL(); // cdecl
#endif
#ifdef _WIN32
		// default calling convention - stdcall
		//todo: set __cdecl = 0, and __stdcall = 1
		switch (returntype >> 8) {
		case 0:
			CALL(__stdcall);
			break;
		case 1:
			CALL(__cdecl);
			break;
		case 2:
			CALL(__fastcall);
			break;
		default:
			fprintf(stderr, "Unsupported calling convention %d", returntype >> 8);
			break;
		}
#endif
		return 0; // if no call have made
	}

	long from_int(word arg) {
		// так как в стек мы все равно большое сложить не сможем, то возьмем
		// только то, что влазит (первые два члена)
//		assert (is_value(arg[1]));
//		assert (is_reference(arg[2]));

		return (car(arg) >> 8) | ((car(cdr(arg)) >> 8) << FBITS);
	}

	float from_int_to_float(word* arg) {
		// читаем длинное число в float формат
		assert (is_value(car(arg)));
		float f = (unsigned long)uvtoi(car(arg));
		float mul = 0x1000000; // 1 << 24
		while (is_reference(cdr(arg))) {
			arg = (word*)cdr(arg);
			f += (unsigned long)uvtoi(cdr(arg)) * mul;
			mul *= 0x1000000;
		}
		assert (cdr(arg) == INULL);

		return f;
	}
	float from_rational(word arg) {
		word* pa = (word*)car(arg);
		word* pb = (word*)cdr(arg);

		float a, b;
		if (is_value(pa))
			a = svtol(pa);
		else {
			switch (reftype(pa)) {
			case TINT:
				a = +from_int_to_float(pa);
				break;
			case TINTN:
				a = -from_int_to_float(pa);
				break;
			}
		}
		if (is_value(pb))
			b = svtol(pb);
		else {
			switch (reftype(pb)) {
			case TINT:
				b = +from_int_to_float(pb);
				break;
			case TINTN:
				b = -from_int_to_float(pb);
				break;
			}
		}

		return (a / b);
	}

	int to_int(word arg) {
		if (is_value(arg))
			return svtoi(arg);

		switch (reftype(arg)) {
		case TINT:
			return (int)+from_int(arg);
		case TINTN:
			return (int)-from_int(arg);
		case TRATIONAL:
			return (int) from_rational(arg);
		case TCOMPLEX:
			return to_int(car(arg)); // return real part of value
		default:
			fprintf(stderr, "can't get int from %d\n", reftype(arg));
		}

		return 0;
	}

	long to_long(word arg) {
		if (is_value(arg))
			return svtol(arg);

		switch (reftype(arg)) {
		case TINT:
			return (long)+from_int(arg);
		case TINTN:
			return (long)-from_int(arg);
		case TRATIONAL:
			return (long) from_rational(arg);
		case TCOMPLEX:
			return to_long(car(arg)); // return real part of value
		default:
			fprintf(stderr, "can't get int from %d\n", reftype(arg));
		}

		return 0;
	}

	float to_float(word arg) {
		if (is_value(arg))
			return svtol(arg);

		switch (reftype(arg)) {
		case TINT:
			return (float)+from_int(arg);
		case TINTN:
			return (float)-from_int(arg);
		case TRATIONAL:
			return (float) from_rational(arg);
		case TCOMPLEX:
			return to_float(car(arg)); // return real part of value
		}
		return 0;
	}

	double to_double(word arg) {
		if (is_value(arg))
			return svtol (arg);

		switch (reftype(arg)) {
		case TINT:
			return (double)+from_int(arg);
			break;
		case TINTN:
			return (double)-from_int(arg);
			break;
		case TRATIONAL:
			return (double) from_rational(arg);
			break;
		case TCOMPLEX:
			return to_double(car(arg)); // return real part of value
		}
		return 0;
	}


	// a - function address
	// b - arguments (may be pair with req type in car and arg in cdr - not yet done)
	// c - '(return-type . argument-types-list)
	word* A = (word*)car(arguments); arguments = (word*)cdr(arguments); // function
	word* B = (word*)car(arguments); arguments = (word*)cdr(arguments); // rtty
	word* C = (word*)car(arguments); arguments = (word*)cdr(arguments); // args

//	assert(is_port(A), A, 1032);
	assert ((word)B != INULL && (is_reference(B) && reftype(B) == TPAIR));
	assert ((word)C == INULL || (is_reference(B) && reftype(C) == TPAIR));
	// C[1] = return-type
	// C[2] = argument-types

	// todo: может выделять в общей куче,а не стеке?
	word args[18]; // пока только 12 аргумента максимум (18 - специально для gluLookAt)
	void *function = (void*)car(A);  assert (function);
	int returntype = uvtoi(car(B));
	int floats = 0; // для amd64
#if __amd64__
	int doubles = 0; // temp
#endif

	int i = 0;     // количество аргументов
	word* p = (word*)C;   // сами аргументы
	word* t = (word*)cdr(B); // rtty

	while ((word)p != INULL) { // пока есть аргументы
		assert (reftype(p) == TPAIR); // assert(list)
		assert (reftype(t) == TPAIR); // assert(list)

		int type = uvtoi(car(t));
		word arg = (word) car(p);

/*		// todo: add argument overriding as PAIR as argument value
		if (typeof (p[1]) == TPAIR) {
			type = value (((word*)p[1])[1]);
			arg = ((word*)p[1])[2];
		}*/

		args[i] = 0; // обнулим (так как потом можем перезаписать только часть)
		// может и не надо.

		// destination type
		switch (type) {
		// целочисленные типы:
		case TFIX:
			if (is_value(arg))
				args[i] = (int)svtoi(arg);
			else
			switch (reftype(arg)) {
			case TINT:
				args[i] = (int)+from_int(arg);
				break;
			case TINTN:
				args[i] = (int)-from_int(arg);
				break;
			default:
				fprintf(stderr, "can't cast %d to int\n", type);
				args[i] = 0; // todo: error
			}
			break;
		case TFIX + 0x40: { // int*
			int c = llen(arg);
			int* p = (int*) __builtin_alloca(c * sizeof(int)); // todo: new_raw_vector()
			args[i] = (word)p;

			word l = arg;
			while (c--)
				*p++ = to_int(car(l)), l = cdr(l);
			break;
		}

		case TINT:
			if (is_value(arg))
				args[i] = (long)svtoi(arg);
			else
			switch (reftype(arg)) {
			case TINT: // source type
				args[i] = (long)+from_int(arg);
				break;
			case TINTN:
				args[i] = (long)-from_int(arg);
				break;
			default:
				fprintf(stderr, "can't cast %d to int\n", type);
				args[i] = 0; // todo: error
			}
			break;
		case TINT + 0x40: { // long*
			int c = llen(arg);
			long* p = (long*) __builtin_alloca(c * sizeof(long)); // todo: use new()
			args[i] = (word)p;

			word l = arg;
			while (c--)
				*p++ = to_long(car(l)), l = cdr(l);
			break;
		}

		case TINT64: { // long long
			if (is_value(arg))
				*(long long*)&args[i] = svtoi(arg);
			else
			switch (reftype(arg)) {
			case TINT: // source type
				*(long long*)&args[i] = +from_int(arg);
				break;
			case TINTN:
				*(long long*)&args[i] = -from_int(arg);
				break;
			case TRATIONAL:
				*(long long*)&args[i] = from_rational(arg);
				break;
			default:
				fprintf(stderr, "can't cast %d to long\n", type);
			}
			#if !__amd64__
				i++; // for x86 long values (fills two ints)
			#endif

			break;
		}
		// todo: case TINT64 + 0x40:

		// с плавающей запятой:
		case TFLOAT:
			*(float*)&args[i] = to_float(arg);

			floats |= (0x100 << i);
			break;
		case TFLOAT + 0x40: {
			int c = llen(arg);
			float* p = (float*) __builtin_alloca(c * sizeof(float)); // todo: use new()
			args[i] = (word)p;

			word l = arg;
			while (c--)
				*p++ = to_float(car(l)), l = cdr(l);
			break;
		}

		case TDOUBLE:
			*(double*)&args[i] = to_double(arg);
			#if __amd64__
				doubles |= (0x10000 << i);
			#else
				i++; // for x86 double fills two floats (words)
			#endif
			break;

		case TDOUBLE + 0x40: {
			int c = llen(arg);
			double* p = (double*) __builtin_alloca(c * sizeof(double)); // todo: use new()
			args[i] = (word)p;

			word l = arg;
			while (c--)
				*p++ = to_double(car(l)), l = cdr(l);
			break;
		}

		// запрос порта - это запрос значения порта
		// todo: добавить тип "указатель на порт"
//		case TUSERDATA:
		case TVPTR:
			if ((word)arg == INULL || (word)arg == IFALSE)
				args[i] = (word) (void*)0;
			else
			switch (reftype(arg)) {
			case TVPTR:
				args[i] = car(arg);
				break;
			case TBVEC:
			case TSTRING:
				args[i] = (word) &car(arg);
				break;
			default:
				fprintf(stderr, "invalid parameter value (requested vptr)\n");
			}
			break;
		case TVPTR + 0x40: {
			if ((word)arg == INULL || (word)arg == IFALSE)
				args[i] = (word) (void*)0;
			else
			switch (reftype(arg)) {
			case TVPTR:
				args[i] = (word) &car(arg);
				break;
			default:
				fprintf(stderr, "invalid parameter value (requested vptr)\n");
			}
			break;
		}

		case TSTRING:
			if ((word)arg == INULL || (word)arg == IFALSE)
				args[i] = (word) (void*)0;
			else
			switch (reftype(arg)) {
			case TSTRING:
				args[i] = (word) &car(arg);
				break;
			default:
				fprintf(stderr, "invalid parameter values (requested string)\n");
			}
			break;
		case TSTRING + 0x40: {
			int c = llen(arg);
			char** p = (char**) __builtin_alloca(c * sizeof(char*));
			args[i] = (word)p;

			word l = arg;
			while (c--)
				*p++ = (char*) &caar(l), l = cdr(l);

/*			int size = llen(arg) + 1;
			*fp++ = make_raw_header(TBVEC, size, 0);
			args[i] = (word)fp;

			word src = arg;
			while (--size)
				*fp++ = (char*) &caar(src), src = cdr(src);

			break;*/
		}
/*
		case TTUPLE:
			if ((word)arg == INULL || (word)arg == IFALSE)
				args[i] = (word) (void*)0;
			else
			switch (reftype(arg)) {
			case TTUPLE: { // ?
				// аллоцировать массив и сложить в него указатели на элементы кортежа
				int size = hdrsize(*(word*)arg);
				*fp++ = make_raw_header(TBVEC, size, 0);
				args[i] = (word)fp; // ссылка на массив указателей на элементы

				word* src = &car(arg);
				while (--size)
					*fp++ = (word)((word*)*src++ + 1);
				}
				break;
			default:
				args[i] = INULL; // todo: error
			}
			break;*/
//		case TRAWVALUE:
//			args[i] = (word)arg;
//			break;
		default:
			fprintf(stderr, "can't recognize %d type\n", type);
		}

		p = (word*)cdr(p); // (cdr p)
		t = (word*)cdr(t); // (cdr t)
		i++;
	}
	assert ((word)t == INULL); // количество аргументов совпало!

	long got;   // результат вызова функции

//	#00000000 00000000 - dword
//	#00000000 000000xx - count of arguments
//	#00000000 000xxx00 - count of floats - up to 12 ?
//	#00000000 xxx00000 - count of doubles - up to 12 ?
#if __amd64__
	// http://locklessinc.com/articles/gcc_asm/
	// http://www.agner.org/optimize/calling_conventions.pdf


//	int foo = 3, bar = 4, ppp = 1;
//	__asm__ ("\
//		add %2, %1    \n\
//		call *%3       \n\
//		mov %1, %0    \n"
//	: "=rax"(ppp) 		// ouput
//	: "r10"(foo), "rax"(bar)// input
//	, "m"(function)
//	: "cc"			// modify flags
//	);
//	// movss %xmm0,-0x1e4(%rbp) - float
//	// movss 0x160a(%rip),%xmm0 - float
//	// movsd 0x160e(%rip),%xmm0 - double
//
//	// linux calling convention:
//	// rdi, rsi, rdx, rcx, r8, r9, xmm0..7
//
//	// register int out asm("r10") x;
//
//	float R = ((float (*)  (int, double, int))function) (1, 2.0, 3);

	got = call(function, args, i + floats+doubles);
#else
	got = call(returntype >> 8, function, args, i);
#endif

	word* result = (word*)IFALSE;
	switch (returntype & 0x3F) {
		case TFIX: // type-fix+ - если я уверен, что число заведомо меньше 0x00FFFFFF! (или сколько там в x64)
			result = (word*) itosv (got);
			break;
		case TINT:
			result = (word*) itoun (got);
			break;
			// else goto case 0 (иначе вернем type-fx+)
//		case TPORT:
//			result = new_port(got);
//			break;
		case TUSERDATA:
			result = new_native_function (got);
			break;
		case TVPTR:
			if (got)
				result = new_vptr (got);
			break;
//		case TRAWVALUE:
//			result = (word*) got;
//			break;

		case TSTRING:
			if (got != 0)
				result = new_string ((char*)got, lenn((char*)got, FMAX+1));
			break;
		case TVOID:
			result = (word*) ITRUE;
			break;
//      todo TRATIONAL:
	}

	heap->fp = fp;
	return result;
}
#endif//HAS_PINVOKE
#if 0
	__attribute__
			((__visibility__("default")))
word* test(OL* self, word* arguments)
{
	// get memory pointer
	heap_t* heap = &self->heap;
	word*
	fp = heap->fp;

	arguments = (word*)car(arguments);

	word* A = (word*)car(arguments); arguments = (word*)cdr(arguments); // function
	word* B = (word*)car(arguments); arguments = (word*)cdr(arguments); // rtty
	word* C = (word*)car(arguments); arguments = (word*)cdr(arguments); // args

	heap->fp = fp;
	return 0;
}
#endif
