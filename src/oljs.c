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

#define NO_SECCOMP

#ifdef NO_SECCOMP
#	define SYSCALL_PRCTL 0
#endif

// TODO: JIT!
//	https://gcc.gnu.org/onlinedocs/gcc-5.1.0/jit/intro/tutorial04.html

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
#define HAS_SOCKETS 0 // system sockets support
#endif

#ifndef HAS_DLOPEN
#define HAS_DLOPEN 0  // dlopen/dlsym support
#endif

#ifndef HAS_PINVOKE
#define HAS_PINVOKE 0 // pinvoke (for dlopen/dlsym) support
#endif

#ifndef HAS_STRFTIME
#define HAS_STRFTIME 0
#endif

#ifndef EMBEDDED_VM   // use as embedded vm in project
#define EMBEDDED_VM 0
#endif

// http://www.gnu.org/software/libc/manual/html_node/Feature-Test-Macros.html
#define _POSIX_SOURCE // enable functionality from the POSIX.1 standard (IEEE Standard 1003.1),
                      // as well as all of the ISO C facilities.
#define _BSD_SOURCE
#define _GNU_SOURCE   // nanosleep, etc.

#include <stdint.h>

// possible data types: LP64 ILP64 LLP64 ILP32 LP32
// http://www.unix.org/version2/whatsnew/lp64_wp.html
// http://stackoverflow.com/questions/384502/what-is-the-bit-size-of-long-on-64-bit-windows
#if INTPTR_MAX == INT64_MAX
#define MATH_64BIT 1
#else
#define MATH_64BIT 0
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
#include <termios.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <time.h>

#ifdef __unix__
#	include <sys/utsname.h>
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
//__attribute__ ((aligned(sizeof(word))))

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

// на самом деле compact & sweep
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

// query: запрос на выделение query слов
static
word gc(heap_t *heap, int query, word regs) {
	// просматривает список справа налево

	// TODO: вот здесь можно провести очень неплохую оптимизацию
	//       а именно - проверить обратные функции (R[128+], которые
	//       лежат в памяти по адресу root+128 - если данные в регистре
	//       не изменились, значит больше нету никого, кто ссылается на
	//       тот же элемент данных (обратный вызов), а значит его можно
	//       просто удалить!

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

#define make_integer(val) itoun(val)



/*** OS Interaction and Helpers ***/
static
void set_blocking(int sock, int blockp) {
}

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
}

#ifndef __linux__

size_t
sendfile(int out_fd, int in_fd, off_t *offset, size_t count)
{
	char buf[8192];
	size_t toRead, numRead, numSent, totSent;

	totSent = 0;
	while (count > 0) {
		toRead = (sizeof(buf) < count) ? sizeof(buf) : count;

		numRead = read(in_fd, buf, toRead);
		if (numRead == -1)
			return -1;
		if (numRead == 0)
			break;                      /* EOF */

		numSent = write(out_fd, buf, numRead);
		if (numSent == -1)
			return -1;
		if (numSent == 0) {               /* Should never happen */
			STDERR("sendfile: write() transferred 0 bytes");
			return 0;
		}

		count -= numSent;
		totSent += numSent;
	}
	return totSent;
}
#endif

/***********************************************************************************
 * OL
 */
struct ol_t
{
	struct heap_t heap; // must be first member
	word max_heap_size; // max heap size in MB

	// вызвать GC если в памяти мало места в КБ
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
long callback(OL* ol, int id, int_t* argi
#if __amd64__
		, double* argf, int_t* rest
#endif
		);
#include "olni.c"

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
	return 0;
}

// ======================================================================
//       загрузчик скомпилированного образа и его десериализатор
//

// fasl decoding
// tbd: comment
// todo: есть неприятный момент - 64-битный код иногда вставляет в fasl последовательность большие числа
//	а в 32-битном коде это число должно быть другим. что делать? пока х.з.
static
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
static
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
static __inline__
word* deserialize(word *ptrs, int nobjs, unsigned char *bootstrap, word* fp)
{
	unsigned char* hp = bootstrap;
//	if (*hp == '#') // этот код не нужен, так как сюда приходит уже без шабанга
//		while (*hp++ != '\n') continue;

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

#ifndef NAKED_VM
extern unsigned char* language;
#else
unsigned char* language = NULL;
#endif

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

//	set_signal_handler();

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


	// а теперь поработаем с сериализованным образом:
	word nwords = 0;
	word nobjs = count_fasl_objects(&nwords, bootstrap); // подсчет количества слов и объектов в образе
	nwords += (nobjs + 2); // for ptrs

	printf("nwords: %d, nobjs: %d\n", nwords, nobjs);

	heap_t* heap = &handle->heap;
	word *fp;

	// выделим память машине:
	int max_heap_size = (W == 4) ? 4096 : 65535; // can be set at runtime
	//int required_memory_size = (INITCELLS + MEMPAD + nwords + 64 * 1024); // 64k objects for memory

	// в соответствии со стратегией сборки 50*1.3-33*0.9, и так как данные в бинарнике
	// практически гарантированно "старое" поколение, выделим в два раза больше места.
	int required_memory_size = nwords*2 + MEMPAD;

	heap->begin = (word*) malloc((required_memory_size + GCPAD(NR)) * sizeof(word)); // at least one argument string always fits
	if (!heap->begin) {
		STDERR("Failed to allocate %d bytes in memory for vm", required_memory_size * sizeof(word));
		goto fail;
	}
	// ok
	heap->end = heap->begin + required_memory_size;
	heap->genstart = heap->begin;

	printf("heap: %p, %p, %d\n", heap->begin, heap->end, heap->end - heap->begin);

	handle->max_heap_size = max_heap_size;

	// Десериализация загруженного образа в объекты
	fp = heap->begin;
	word *ptrs = new(TTUPLE, nobjs+1, 0);
	fp = deserialize(&ptrs[1], nobjs, bootstrap, fp);

	if (fp == 0)
		goto fail;

	// все, программа в памяти, можно освобождать исходник
	if (release)
		release(bootstrap);

	printf("heap: %p, %p, %p\n", heap->begin, heap->end, fp);

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
	OL* ol = handle;
	heap_t* heap = &ol->heap;
	sandboxp = 0;    // static variable

	word* ptrs = (word*) heap->begin;
	int nobjs = hdrsize(ptrs[0]) - 1;

	// точка входа в программу - это последняя лямбда загруженного образа (λ (args))
	// thinkme: может стоит искать и загружать какой-нибудь main() ?
	word* this = (word*) ptrs[nobjs];

	// обязательно почистим регистры! иначе gc() сбойнет, пытаясь работать с мусором
	word* R = ol->R; // регистры виртуальной машины:
	for (ptrdiff_t i = 0; i < NR+CR; i++)
		R[i] = IFALSE; // was: INULL, why??
	R[0] = IFALSE; // MCP - master control program (in this case NO mcp)
	R[3] = IHALT;  // continuation, in this case simply notify mcp about thread finish
	R[4] = (word) userdata; // first argument: command line as '(script arg0 arg1 arg2 ...)
	unsigned short acc = 2; // boot always calls with 1+1 args, no support for >255arg functions

	// все готово для выполнения главного цикла виртуальной машины
	ol->this = this;
	ol->arity = acc;

	ol->gc = OL__gc;

	return runtime(handle);
}
