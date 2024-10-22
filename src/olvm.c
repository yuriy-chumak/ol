/*!# Otus Lisp
 * ```
 *                     small,
 *         `___`           embeddable
 *         (O,O)               and
 *         \)  )            purely
 *       ---"-"---       functional!
 * 
 * Copyright(c) 2014 - 2023 Yuriy Chumak
 * ```
 * *Based on Aki Helin's [Owl-Lisp](https://gitlab.com/owl-lisp/owl)*
 * - - -
 *
 * ## LICENSES
 * You are free to choose an MIT or LGPLv3 license.
 * 
 * * MIT:  
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 *   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 *   OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 *   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 *   ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 *   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 *   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * 
 * * LGPLv3:  
 *   This program is free software;  you can redistribute it and/or
 *   modify it under the terms of the GNU General Public License as
 *   published by the Free Software Foundation; either version 3 of
 *   the License, or (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * ## BUILD
 * `make; make install`
 *
 * ## THE PROJECT
 * - https://github.com/yuriy-chumak/ol
 * - https://yuriy-chumak.github.io/ol/
 */

#ifndef __OLVM_H__
#define __OLVM_H__

// <!-- standard definitions
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600 // (Since glibc 2.2) The value 600 or greater additionally
                          // exposes definitions for SUSv3 (UNIX 03; i.e., the
                          // POSIX.1-2001 base specification plus the XSI extension)
                          // and C99 definitions.
#endif

// http://man7.org/linux/man-pages/man7/posixoptions.7.html
#define _GNU_SOURCE 1     // nanosleep, mmap, etc.
#define _BSD_SOURCE 1
#define _DEFAULT_SOURCE 1

#ifdef __NetBSD__         // make all NetBSD features available
#	ifndef _NETBSD_SOURCE
#	define _NETBSD_SOURCE 1
#	endif
#endif

#ifdef __APPLE__
#	ifndef _DARWIN_C_SOURCE
#	define _DARWIN_C_SOURCE
#	endif
#endif

#ifdef _WIN32             // we need no modern windows features
#	define WINVER _WIN32_WINNT_NT4
#	define _WIN32_WINNT _WIN32_WINNT_NT4
#endif

// assume we use posix
#ifndef HAVE_UNISTD_H
#define HAVE_UNISTD_H 1
#endif

// public attributes
#if defined(__unix__)
#	define OLVM_PUBLIC __attribute__ ((__visibility__("default"))) __attribute__((used))
#elif defined(_WIN32)
#	define OLVM_PUBLIC __declspec(dllexport)
#elif defined(__APPLE__)
#	define OLVM_PUBLIC __attribute__ ((__visibility__("default"))) __attribute__((used))
#else
#	define OLVM_PUBLIC
#endif

// --> end of standard definitions

/*!- - -
 * ## Otus Lisp Virtual Machine
 * ### Source file: src/olvm.c
 */
#include <stdlib.h>

#include <stdint.h>

// unsigned int that is capable of storing a pointer
// основной data type, зависит от разрядности машины
typedef uintptr_t word;

// arm, armv7-a, armv8-a: 4;  arch64: 8;
// risc-v 32: 4;           risc-v 64: 8;
// wasm:      4;
// x86:       4;              x86-64: 8;
// mips:      4;              mips64: 8;
// ppc:       4;      ppc64, ppc64le: 8;

// raspberty pi:  armv-8
// apple m1: arch64


// OL Virtual Machine type
typedef struct olvm_t olvm_t;

// -----------------------------------------------------
// PUBLIC API:

olvm_t* OLVM_new(unsigned char* bootstrap);
void OLVM_delete(olvm_t* ol);
word OLVM_run(olvm_t* ol, int argc, char** argv);
word OLVM_evaluate(olvm_t* ol, word function, int argc, word* argv);

void*OLVM_userdata(olvm_t* ol, void* userdata);
void*OLVM_allocate(olvm_t* ol, unsigned words);

// pinned objects support functions
#ifndef OLVM_NOPINS
# ifndef OLVM_PIN_PROTOTYPES
size_t OLVM_pin(olvm_t* ol, word ref); // pin can realloc RPS (Register and Pin Set)
word OLVM_deref(olvm_t* ol, size_t p);
word OLVM_unpin(olvm_t* ol, size_t p);
# else
typedef size_t (*olvm_pin_t)(olvm_t* ol, word ref); extern olvm_pin_t OLVM_pin;
typedef word (*olvm_deref_t)(olvm_t* ol, size_t p); extern olvm_deref_t OLVM_deref;
typedef word (*olvm_unpin_t)(olvm_t* ol, size_t p); extern olvm_unpin_t OLVM_unpin;
# endif
#endif

// ffi callbacks support
word OLVM_apply(olvm_t* ol, word function, word args);

// embed type conversion helpers
# ifndef OLVM_FCONV_PROTOTYPES
float OL2F(word arg);
double OL2D(word arg);
# else
typedef float (*ol2f_t)(word arg);                  extern ol2f_t OL2F;
typedef double (*ol2d_t)(word arg);                 extern ol2d_t OL2D;
#endif

// -----------------------------------------------------
// descriptor format
// заголовок объекта, находится по адресу 0 (ob[0], *ob):
//  [s...sss ????rppp tttttt10] // bit 2 у заголовков всегда выставлен в 1 (используется GC)
//   '-----| '--||'-| '----|
//         |    ||  |      '-----> tttttt,  object type
//         |    ||  '------------> ppp,     number of padding (unused) bytes at the end of object, if raw (0-(wordsize-1))
//         |    |'---------------> r,       rawness bit (raw objects have no references(pointers) in them)
//         |    '----------------> ????,    your tags here! e.g. tag for closing file descriptors in gc, etc. not used for now
//         '---------------------> s...sss, object size in words
//
// а это то, что лежит в объектах - либо непосредственные значения, либо указатели на другие объекты:
//                       .------------> value, if 'v' is set
//                       |      .-----> type, if 'v' is set
//                       |      |.----> 'value' bit
//   .-------------------| .----||.---> mark bit (can be 1 only during gc process)
//  [... pppppppp pppppppp ttttttv0]
//   '----------------------------|
//                                '-----> word aligned pointer if not a value ('v' cleared) (4- or 8-byte)
//      младшие 2 нулевые бита для указателя (mark бит снимается при работе) позволяют работать только с выравненными
//       внутренними указателями - таким образом, ВСЕ объекты в куче выравнены по границе слова
//
//
// todo: вот те 4 бита можно использовать для кастомных типов,
// например, в спецполя складывать id функции, что вызывает mark для подпоинтеров,
//	         и ptr на функцию, что делает финализацию.
// todo: move "r" bit left to allow 128-bit machines
// http://publications.gbdirect.co.uk/c_book/chapter6/bitfields.html

#define IPOS      8  // === bits offset of (struct value_t, value), deprecated name
#define VPOS      8  // === bits offset of (struct value_t, value)

#define TPOS      2  // === bits offset of (struct header_t, type) and (struct value_t, type)
#define PPOS      8  // === bits offset of (struct header_t, padding)
#define RPOS     11  // === bits offset of (struct header_t, rawness)
#define SPOS     16  // === bits offset of (struct header_t, size)

// ---==( value_t )==---
// immediate Ol value
struct __attribute__ ((aligned(sizeof(word)), packed))
value_t
{
	unsigned char mark : 1;    // always 0, (1 only during GC) =
	unsigned char v    : 1;    // always 1                      = 8 bits
	unsigned char type : 6;    // value type                   =

	unsigned char value[sizeof(word) - 1];
};

// some critical vm limitations:
//static_assert(sizeof(struct value_t) == sizeof(word), "Size of value_t structure should be equal to size of virtual machine word");


// ---==( reference_t )==---
// pointer to the object_t
struct __attribute__ ((aligned(sizeof(word)), packed))
reference_t
{
	union {
		struct {
			unsigned mark : 1;    // always 0, (1 only during GC)
			unsigned v    : 1;    // always 0
		};
		uintptr_t ptr; // btw, normally lower two bits is always 0, so this pointer always word-aligned
	};
};

// some critical vm limitations:
//static_assert(sizeof(struct reference_t) == sizeof(word), "Size of reference_t structure should be equal to size of virtual machine word");


// ---==( object_t )==---
struct __attribute__ ((aligned(sizeof(word)), packed))
object_t
{
	union {
		struct header_t {
			unsigned char mark : 1;    // always 0, (1 only during GC) =
			unsigned char i    : 1;    // always 1, (0 only during GC)  = 8 bits
			unsigned char type : 6;    // object type                  =

			unsigned char padding : 3; // number of padding (empty) bytes after the end of reasonable object data
			unsigned char rawness : 1; // 1 for raw stream, 0 for vectors (tuples)
				               // : 4; // reserved bits bits

			unsigned char size[sizeof(word) - 2]; // object size in words, including header one
		} header;
		word ref[1]; // we don't like empty objects
	};
};

// some critical vm limitations:
//static_assert(sizeof(struct object_t) == sizeof(word), "Minimal size of object_t structure should be equal to size of virtual machine word");


// ------------------------------------------------------
// ANSI integers - 
#ifndef OLVM_ANSI_INT_LIMITS
#define OLVM_ANSI_INT_LIMITS 0
#endif

// floating point numbers (inexact numbers in terms of Scheme) support
#ifndef OLVM_INEXACTS
#define OLVM_INEXACTS 1
#endif

#ifndef OLVM_INEXACT_TYPE
#	define inexact_t double
#else
#	define inexact_t OLVM_INEXACT_TYPE
#endif

#include <limits.h>
#include <assert.h>

// only 32- and 64-bit machines supported.
// https://gcc.gnu.org/onlinedocs/gccint/Machine-Modes.html
// define internal math types based on sizeof(size_t):
#if SIZE_MAX == 0xffffffffU
	typedef signed int_t __attribute__ ((mode (SI))); // four-byte integer (32 bits)
	typedef unsigned big_t __attribute__ ((mode (DI))); // eight-byte integer (64 bits)
	#define INT_T_MIN INT32_MIN
#elif SIZE_MAX == 0xffffffffffffffffU
	typedef signed int_t __attribute__ ((mode (DI))); // eight-byte integer (64 bits)
	typedef unsigned big_t __attribute__ ((mode (TI))); // sixteen-byte integer (128 bits)
	#define INT_T_MIN INT64_MIN
#else
#	error Unsupported platform bitness, only 32- and 64-bit versions are supported!
#endif

// ------------------------------------------------------

#define VBITS                       ((sizeof (word) * 8) - 8) // bits in Value (short number aka 'enum')
#define VSIZE                       ((sizeof (word) * 8) - 8) // bits in Value (short number aka 'enum')
#define HIGHBIT                     ((int_t)1 << VSIZE) // maximum Value value + 1
#define VMAX                        (HIGHBIT - 1)       // maximum Value value (and most negative value)

#define RAWBIT                      (1 << RPOS) // todo: rename to BSBIT (rawstream bit)
#define BINARY                      (RAWBIT >> TPOS)

// create a value
#define make_value(type, value)     (2 | ((type) << TPOS) | ((word)(value) << VPOS))

// header making macro
#define header3(type, size, padding)(2 | ((type) << TPOS) | ((word) (size) << SPOS) | RAWBIT | ((padding) << PPOS))
#define header2(type, size)         (2 | ((type) << TPOS) | ((word) (size) << SPOS))

#define HEADER_MACRO(_1, _2, _3, NAME, ...) NAME
#define make_header(...)            HEADER_MACRO(__VA_ARGS__, header3, header2, NOTHING, NOTHING)(__VA_ARGS__)


// three main classes:
#define is_value(x)                 (( (V)(x)) & 2)
#define is_reference(x)             (!is_value(x))
#define is_rawstream(x)             ((*(R)(x)) & RAWBIT) //((struct object_t*)(x))->rawness // ((*(word*)(x)) & RAWBIT)

#define W                           (sizeof (word))

#define WALIGN(x)                   (((x) + W - 1) / W)
#define WPADS(x)                    (WALIGN(x) * W - x) // (W - (x % W));

// V means Value
typedef word V;
// makes positive olvm integer value from int
#define I(val) \
		(make_value(TENUMP, val))  // === (value << VPOS) | 2

// R means Reference
typedef word* R;
// makes olvm reference from system pointer (and do sanity check in DEBUG mode)
#define reference(v) ({\
		word _reference = (word)(v);\
		assert (!(_reference & (W-1)) && "olvm references must be aligned to word boundary");\
		(word*) _reference; })

// всякая всячина:
#define header_size(x)              (((word)(x)) >> SPOS) // header_t(x).size
#define object_size(x)              (header_size(x))
#define header_pads(x)              (unsigned char) ((((word)(x)) >> VPOS) & 7) // header_t(x).padding
#define object_payload(x)           (((word)(x)) + 1)

#define value_type(x)               (unsigned char) ((((word)(x)) >> TPOS) & 0x3F)
#define reference_type(x)           (value_type (*reference(x)))

#define reference_size(x)           ((header_size(*reference(x)) - 1))
#define rawstream_size(x)           ((header_size(*reference(x)) - 1) * sizeof(word) - header_pads(*reference(x)))


// types:
#define TPAIR                       (1)  // type-pair
#define TVECTOR                     (2)  // type-vector
#define TSTRING                     (3)  // type-string
#define TSYMBOL                     (4)  // type-symbol
#define TSTRINGWIDE                 (5)  // type-string-wide

#define TPORT                       (12) // type-port
#define TCONST                      (13) // type-const

#define TBYTECODE                   (16) // type-bytecode
#define TPROCEDURE                  (17) // type-procedure
#define TCLOSURE                    (18) // type-closure
#define TCONSTRUCTOR                (63) // вызываемая процедура (не байткод! не замыкание!), TODO: проверить, что точно работает

#define TFF                         (24) // 25, 26 are same
#	define TRIGHT                     1  // flags for TFF
#	define TRED                       2
// static_assert (TFF & ~3 == TFF);

#define TBYTEVECTOR                 (19) // type-bytevector
#define TSTRINGDISPATCH             (21) // type-string-displatch

#define TVECTORLEAF                 (11) // type-vector-leaf
#define TVECTORDISPATCH             (15) // type-vector-dispatch

#define TTHREAD                     (31) // type-thread-state

// numbers (value type)
// A FIXNUM is an exact integer that is small enough to fit in a machine word.
// todo: rename TFIX to TSHORT or TSMALLINT, TINT to TLARGE or TLARGEINT
#define TENUMP                       (0) // type-enum+ // small integer
#define TENUMN                      (32) // type-enum-
// numbers (reference type)
#define TINTP                       (40) // type-int+ // large integer
#define TINTN                       (41) // type-int-
#define TRATIONAL                   (42) // type-rational
#define TCOMPLEX                    (43) // type-complex
#define TINEXACT                    (44) // type-inexact, IEEE-754

#define TVPTR                       (49) // void*, only RAW
#define TCALLABLE                   (61) // type-callable, receives '(description . callable-lambda)
#define TDLSYM                      (62) // type-dlsym, temp name

//#define likely(x)                   __builtin_expect((x), 1)
//#define unlikely(x)                 __builtin_expect((x), 0)

#define is_enump(ob)                (is_value(ob)     && value_type (ob) == TENUMP)
#define is_enumn(ob)                (is_value(ob)     && value_type (ob) == TENUMN)
#define is_enum(ob)                 (is_enump(ob) || is_enumn(ob))
#define is_const(ob)                (is_value(ob)     && value_type (ob) == TCONST)
#define is_pair(ob)                 (is_reference(ob) && (*(word*) (ob)) == make_header(TPAIR,     3))
#define is_npairp(ob)               (is_reference(ob) && (*(word*) (ob)) == make_header(TINTP,     3))
#define is_npairn(ob)               (is_reference(ob) && (*(word*) (ob)) == make_header(TINTN,     3))
#define is_rational(ob)             (is_reference(ob) && (*(word*) (ob)) == make_header(TRATIONAL, 3))
#define is_complex(ob)              (is_reference(ob) && (*(word*) (ob)) == make_header(TCOMPLEX,  3))
#define is_inexact(ob)              (is_reference(ob) && (*(word*) (ob)) == make_header(TINEXACT,  1+WALIGN(sizeof(inexact_t)), WPADS(sizeof(inexact_t))))

#define is_string(ob)               (is_reference(ob) && reference_type (ob) == TSTRING)
#define is_vector(ob)               (is_reference(ob) && reference_type (ob) == TVECTOR)
#define is_thread(ob)               (is_reference(ob) && reference_type (ob) == TTHREAD)

#define is_vptr(ob)                 (is_reference(ob) && (*(word*) (ob)) == make_header(TVPTR,     2, 0))
#define is_callable(ob)             (is_reference(ob) && (*(word*) (ob)) == make_header(TCALLABLE, 2, 0))
#define is_dlsym(ob)                (is_reference(ob) && (*(word*) (ob)) == make_header(TDLSYM,    3, 0))

#define is_numberp(ob)              (is_enump(ob) || is_npairp(ob))
#define is_numbern(ob)              (is_enumn(ob) || is_npairn(ob))
#define is_number(ob)               (is_numberp(ob) || is_numbern(ob))
#define is_integer(ob)              (is_numberp(ob) || is_numbern(ob))

#define is_boolean(ob)              ((ob == ITRUE) || (ob == IFALSE))

// взять значение аргумента:
#define value(v)                    ({ word x = (word)(v); assert(is_value(x));     (((word)(x)) >> VPOS); })
#define deref(v)                    ({ word x = (word)(v); assert(is_reference(x)); *(word*)(x); })

#define ref(ob, n)                  ((reference(ob))[n])
#define car(ob)                     (ref(ob, 1))
#define cdr(ob)                     (ref(ob, 2))
#define payload(o)                  (&car(o))

#define caar(o)                     car(car (o))
#define cadr(o)                     car(cdr (o))
#define cdar(o)                     cdr(car (o))
#define cddr(o)                     cdr(cdr (o))

// constants:
#define IFALSE    make_value(TCONST, 0)  // #false, false value
#define ITRUE     make_value(TCONST, 1)  // #true, non false value
#define INULL     make_value(TCONST, 2)  // #null, empty list, '()
#define IEMPTY    make_value(TCONST, 3)  // #empty, empty ff, {}
#define IEOF      make_value(TCONST, 4)  // #eof, end of file
// olvm internal, not exposed to the ol
#define IEXIT     make_value(TCONST, 5)  // end of thread/program
#define IRETURN   make_value(TCONST, 6)  // end of thunk, return value

#define RFALSE    ((R)IFALSE)
#define RTRUE     ((R)ITRUE)
#define RNULL     ((R)INULL)
#define REMPTY    ((R)IEMPTY)
#define REOF      ((R)IEOF)

// ------- service functions ------------------
void E(char* format, ...);
#ifdef NDEBUG
#	define D(...)
#else
#	define D(...) E(__VA_ARGS__)
#endif

// --------- memory ---------------------------
// -----------------------------------------------------//--------------------
// -=( GC )=------------------------------------------------------------------

/*** Garbage Collector,
 * based on "Efficient Garbage Compaction Algorithm" by Johannes Martin (1982)
 **/
// "на почитать" по теме GC:
// shamil.free.fr/comp/ocaml/html/book011.html

// память машины, управляемая сборщиком мусора
// TODO?: усложнить, разделив на две - кучу больших объектов и кучу маленьких
struct heap_t
{
	// new (size) === { *(size*)fp; fp += size; }
	word *fp;        // allocation pointer

	// always: begin <= genstart <= end
	word *begin;     // begin of heap
	word *end;       // end of heap
	word *genstart;  // young generation begin pointer

	size_t padding;  // safe padding area

	// вызвать GC если в памяти мало места (в словах)
	// для безусловного вызова передать 0
	// возвращает 1, если была проведена сборка
	int (*gc)(struct olvm_t* ol, long ws);
};
typedef struct heap_t heap_t;


// -= new =--------------------------------------------
// выделить блок памяти, unsafe
// size is a payload size, not a size of whole object
// so in fact we'r allocating (size+1) words
#define NEW(size) ({ \
	word* _addr = fp;\
	fp += (size) + 1;\
	/*return*/ _addr;\
})

// аллоцировать новый объект (указанного типа)
#define NEW_OBJECT(type, size) ({\
word*_o = NEW (size);\
	*_o = make_header(type, 1+(size));\
	/*return*/ _o;\
})

// аллоцировать новый "бинарный" объект (указанного типа),
//  данные объекта не проверяются сборщиком мусора и не
//  могут содержать другие объекты!
#define NEW_BINARY(type, size, pads) ({\
word*_b = NEW (size);\
	*_b = make_header(type, 1+(size), pads);\
	/*return*/ _b;\
})

// new(size) - allocate memory, without type;
//             size is payload size, not whole object with header size
//             so, we can't create real 0-sized invalid objects
// new(type, size) - allocate object, with type
// new(type, size, pads) - allocate binary, with type, size in words and pads
#define NEW_MACRO(_1, _2, _3, NAME, ...) NAME
#define new(...) NEW_MACRO(__VA_ARGS__, NEW_BINARY, NEW_OBJECT, NEW, NOTHING)(__VA_ARGS__)

// allocate raw memory block
#define new_alloc(type, length) ({\
	int _size = (length);\
	int _words = WALIGN(_size);\
	int _pads = WPADS(_size);\
	\
word* p = new (type, _words, _pads);\
	/*return*/ p;\
})

// -= ports =-------------------------------------------
// создает порт, возможно аллоцирует память

// it's safe (posix uses int as io handles)
#define is_port(ob)  ((is_value(ob)     && value_type(ob)     == TPORT) ||\
                      (is_reference(ob) && reference_type(ob) == TPORT))
#define make_port(a) ({ word _p = (word)a; assert (((word)_p << VPOS) >> VPOS == (word)_p); make_value(TPORT, _p); })
#define port(o)      ({ word _p = (word)o; is_value(_p) ? value(_p) : car(_p); })

#define new_port(arg1) ({ \
	word _arg1 = (word) (arg1);\
	/* точка следования */\
word*_port = new (TPORT, 1, 0);\
	_port[1] = _arg1;\
	/*return*/ _port;\
})


// -= new_pair =----------------------------------------

// предвычисляем a1 и a2 перед тем, как выделим память, так
// как они в свою очередь могут быть аллоцируемыми объектами.
#define NEW_TYPED_PAIR(type, a1, a2) ({\
	word data1 = (word) a1;\
	word data2 = (word) a2;\
	/* точка следования */ \
word*p = NEW_OBJECT (type, 2);\
	p[1] = data1;\
	p[2] = data2;\
	/*return*/ p;\
})

#define NEW_PAIR(a1, a2) NEW_TYPED_PAIR(TPAIR, a1, a2)

#define NEW_PAIR_MACRO(_1, _2, _3, NAME, ...) NAME
#define new_pair(...) NEW_PAIR_MACRO(__VA_ARGS__, NEW_TYPED_PAIR, NEW_PAIR, NOTHING, NOTHING)(__VA_ARGS__)

#define cons(a, b) new_pair(a, b)

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


// -= vector =---------------------------------------

#define new_vector1(a1) ({\
	word data1 = (word) (a1);\
	/* точка следования */ \
word*p = new (TVECTOR, 1);\
	p[1] = data1;\
	/*return*/ p;\
})
#define new_vector2(a1,a2) ({\
	word data1 = (word) (a1);\
	word data2 = (word) (a2);\
	/* точка следования */ \
word*p = new (TVECTOR, 2);\
	p[1] = data1;\
	p[2] = data2;\
	/*return*/ p;\
})
#define new_vector3(a1,a2,a3) ({\
	word data1 = (word) (a1);\
	word data2 = (word) (a2);\
	word data3 = (word) (a3);\
	/* точка следования */ \
word*p = new (TVECTOR, 3);\
	p[1] = data1;\
	p[2] = data2;\
	p[3] = data3;\
	/*return*/ p;\
})
#define new_vector5(a1,a2,a3,a4,a5) ({\
	word data1 = (word) (a1);\
	word data2 = (word) (a2);\
	word data3 = (word) (a3);\
	word data4 = (word) (a4);\
	word data5 = (word) (a5);\
	/* точка следования */ \
word*p = new (TVECTOR, 5);\
	p[1] = data1;\
	p[2] = data2;\
	p[3] = data3;\
	p[4] = data4;\
	p[5] = data5;\
	/*return*/ p;\
})
#define new_vector9(a1,a2,a3,a4,a5,a6,a7,a8,a9) ({\
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
word*p = new (TVECTOR, 9);\
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
#define new_vector13(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) ({\
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
word*p = new (TVECTOR, 13);\
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
#define new_vector(...) NEW_TUPLE(__VA_ARGS__, new_vector13, new_vector12, new_vector11,\
			new_vector10, new_vector9, new_vector8, new_vector7, new_vector6, new_vector5,\
			new_vector4, new_vector3, new_vector2, new_vector1, NOTHING)(__VA_ARGS__)


// -= числа =------------------------
// todo: потом переделать в трюк
// ! трюк, в общем, не нужен. gcc вполне неплохо сам оптимизирует код (на x64, например, использует cmov)
// алгоритмические трюки:
// (-1)^t*x === (x xor t) - t, где t - y >>(s) 31 (все 1, или все 0)

// i - machine integer
// ui - unsigned, si - signed
// v - value number (internal, that fits in one register), type-enum
//  or small numbers,
//  or short numbers
// uv, sv - unsigned/signed respectively.
// Z - mножество целых чисел.

// работа с numeric value типами

// todo: check this automation - ((struct value)(v).sign) ? -uvtoi (v) : uvtoi (v);
#define enum(v) \
	({  word _x1 = (word)(v);    \
		assert(is_enum(_x1));     \
		int_t y1 = (_x1 >> VPOS);\
		value_type (_x1) == TENUMN ? -y1 : y1; \
	})//(x1 & 0x80) ? -y1 : y1;
#define make_enum(v) \
	(word)({ int_t x4 = (int_t)(v);  (x4 < 0) ? (-x4 << VPOS) | 0x82 : (x4 << VPOS) | 2/*make_value(-x4, TENUMN) : make_value(x4, TENUMP)*/; })
#define make_enump(v) I(v)
// todo: check this automation - ((struct value)(v).sign) ? -uvtoi (v) : uvtoi (v);

// MATH
// todo: потом переделать в трюк
// ! трюк, в общем, не нужен. gcc вполне неплохо сам оптимизирует код (на x64, например, использует cmov)
// алгоритмические трюки:
// x = (x xor t) - t, где t - y >>(s) 31 (все 1, или все 0)
// signed enum to int

// i - machine integer
// ui - unsigned, si - signed
// v - value number (internal, that fits in one register), type-enum
//  or small numbers,
//  or short numbers
// uv, sv - unsigned/signed respectively.
// Z - mножество целых чисел.

// работа с numeric value типами
#ifndef UVTOI_CHECK
#define UVTOI_CHECK(v)  assert (is_value(v) && value_type(v) == TENUMP);
#endif

// арифметика целых (возможно больших)
// прошу внимания!
// todo: rename to un2i (signed number /value or long number/ TO integer)
#define untoi(num)  ({\
	is_value(num) ? value(num)\
		: value(car(num)) | value(cadr(num)) << VBITS; \
	}) //(is_reference(cdr(num)) ? uftoi(cadr(num)) << VBITS : 0); })

// something wrong: looks like __builtin_choose_expr doesn't work as expected!
#ifndef __GNUC__
#define __builtin_choose_expr(const_exp, exp1, exp2) (const_exp) ? (exp1) : (exp2)
#endif

// olvm numbers management:
// if value size less than word size then we no need to alloc memory in heap
// if value size equal to word size then we need only two words in memory in worst case
// if value size greater to word size then we need only three words in memory in worst case

#define new_unumber(val)  ({ \
	__builtin_choose_expr(sizeof(val) < sizeof(word), \
		(word*)make_enump(val), \
	__builtin_choose_expr(sizeof(val) > sizeof(word), \
		(word*)({ \
			big_t _x = (val); \
			(_x < (big_t)HIGHBIT) ? \
					(word)make_enump(_x): \
			(_x < (big_t)HIGHBIT*(big_t)HIGHBIT) ? \
					(word)new_list(TINTP, \
							make_enump(_x & VMAX), \
							make_enump(_x >> VBITS)): \
					(word)new_list(TINTP, \
							make_enump(_x & VMAX), \
							make_enump((_x >> VBITS) & VMAX), \
							make_enump((_x >> VBITS) >> VBITS)); \
		}), \
	/* else: sizeof(val) == sizeof(word) */ \
		(word*)({ \
			word _x = (word)(val); \
			(_x < HIGHBIT) ? \
					(word)make_enump(_x): \
					(word)new_list(TINTP, \
							make_enump(_x & VMAX), \
							make_enump(_x >> VBITS)); \
		})\
	));})

// special case (_v == INT_T_MIN) if no OLVM_ANSI_INT_LIMITS:
//   val == minimal applicable integer for selected word width (INT_T_MIN value)
//   that is equal to -2147483648 for 32-bit and -9223372036854775808 for 64-bit
// in this case -val cenverts into "0" by machine math and we got invalid value
//   so we need to compare val with INT_T_MIN and use a longer converter
//
// Stupid clang generates warnings just after preprocessor and
//   before __builtin_types_compatible_p() / sizeof() preprocessor,
//   and founds a "-Wtautological-constant-out-of-range-compare" warnings
//   in places that definitely will not compile.
// I don't want to make a code completely unreadable to satisfy it.

#if OLVM_ANSI_INT_LIMITS
# define NOT_A_MIN_INT(i) (1)
#else
# define NOT_A_MIN_INT(i) (i != INT_T_MIN)
#endif

#define new_snumber(val)  ({ \
	__builtin_choose_expr(sizeof(val) < sizeof(word), \
		(word*)make_enum(val), \
	__builtin_choose_expr(sizeof(val) > sizeof(word), \
		(word*)({ \
			typeof(val) _v = (val); \
			big_t _x = _v < 0 ? (big_t)(-_v) : (big_t)_v; \
			(_x < (big_t)HIGHBIT) && NOT_A_MIN_INT(_v) ? \
					(word)make_value(_v < 0 ? TENUMN : TENUMP, (word)_x): \
			(_x < (big_t)HIGHBIT*(big_t)HIGHBIT) && NOT_A_MIN_INT(_v) ? \
					(word)new_list(_v < 0 ? TINTN : TINTP, \
							make_enump(_x & VMAX), \
							make_enump(_x >> VBITS)): \
					(word)new_list(_v < 0 ? TINTN : TINTP, \
							make_enump(_x & VMAX), \
							make_enump((_x >> VBITS) & VMAX), \
							make_enump((_x >> VBITS) >> VBITS)); \
		}), \
	/* else: sizeof(val) == sizeof(word) */ \
		(word*)({ \
			typeof(val) _v = (val); \
			word _x = (_v < 0) ? (word)(-_v) : (word)_v; \
			(_x < (word)HIGHBIT) && NOT_A_MIN_INT(_v) ? \
					(word)make_value(_v < 0 ? TENUMN : TENUMP, _x): \
					(word)new_list(_v < 0 ? TINTN : TINTP, \
							make_enump(_x & VMAX), \
							make_enump(_x >> VBITS)); \
		})\
	));})

#ifndef __CHAR_UNSIGNED__
#define CHAR_SIGNED 1
#define CHAR_UNSIGNED 0
#else
#define CHAR_SIGNED 0
#define CHAR_UNSIGNED 1
#endif

#define new_number(val) \
	__builtin_choose_expr (\
		 __builtin_types_compatible_p (typeof(val), int8_t) ||\
		 __builtin_types_compatible_p (typeof(val), int16_t) ||\
		 __builtin_types_compatible_p (typeof(val), int32_t) ||\
		 __builtin_types_compatible_p (typeof(val), int64_t) ||\
		 __builtin_types_compatible_p (typeof(val), signed) ||\
		(__builtin_types_compatible_p (typeof(val), char) && CHAR_SIGNED) ||\
		 __builtin_types_compatible_p (typeof(val), signed char) ||\
		 __builtin_types_compatible_p (typeof(val), signed short) ||\
		 __builtin_types_compatible_p (typeof(val), signed int) ||\
		 __builtin_types_compatible_p (typeof(val), signed long) ||\
		 __builtin_types_compatible_p (typeof(val), signed long long),\
			new_snumber(val),\
	__builtin_choose_expr (\
		 __builtin_types_compatible_p (typeof(val), uint8_t) ||\
		 __builtin_types_compatible_p (typeof(val), uint16_t) ||\
		 __builtin_types_compatible_p (typeof(val), uint32_t) ||\
		 __builtin_types_compatible_p (typeof(val), uint64_t) ||\
		 __builtin_types_compatible_p (typeof(val), unsigned) ||\
		(__builtin_types_compatible_p (typeof(val), char) && CHAR_UNSIGNED) ||\
		 __builtin_types_compatible_p (typeof(val), unsigned char) ||\
		 __builtin_types_compatible_p (typeof(val), unsigned short) ||\
		 __builtin_types_compatible_p (typeof(val), unsigned int) ||\
		 __builtin_types_compatible_p (typeof(val), unsigned long) ||\
		 __builtin_types_compatible_p (typeof(val), unsigned long long),\
			new_unumber(val),\
	({ assert(0); (word*)IFALSE; })))

// get unsigned/signed number
//  в числовой паре надо сначала положить старшую часть, и только потом младшую!
#define unumber(num)  ({ word* n = (word*) (num); is_value(n) ? value(n) : value(car(n)) | value(cadr(n)) << VBITS; })
#define numberp(num)  unumber(num) // deprecated
#define number(num)  ({\
	word* x = (word*) (num);\
	is_numberp(x) ?  unumber(x) :\
	is_numbern(x) ? -unumber(x) :\
	0; })

// -= остальные аллокаторы =----------------------------

#define new_bytevector(length) new_alloc(TBYTEVECTOR, length)

#define NEW_STRING2(string, length) ({\
	char* _str = string;\
	int _strln = length;\
word* p = new_alloc(TSTRING, _strln);\
	char* ptr = (char*)&p[1];\
	while (_strln--)\
		*ptr++ = *_str++;\
	/*return*/ p;\
})

#define NEW_STRING(string) ({\
	char* str = string;\
	int strln = strlen(str);\
	NEW_STRING2(str, strln);\
})

#define NEW_STRING_MACRO(_1, _2, NAME, ...) NAME
#define new_string(...) NEW_STRING_MACRO(__VA_ARGS__, NEW_STRING2, NEW_STRING, NOTHING)(__VA_ARGS__)

#define string(o)   ({ word p = (word)o; assert (is_string(p)); (char*) ((word*)p + 1); })


#define new_vptr(a) ({\
word _data = (word) a;\
	word* _me = new (TVPTR, 1, 0);\
	_me[1] = _data;\
	/*return*/ _me;\
})

#define new_callable(a) ({\
word _data = (word) a;\
	word* _me = new (TCALLABLE, 1, 0);\
	_me[1] = _data;\
	/*return*/ _me;\
})

// unused, but should be.
#define new_dlsym(a, b) ({\
	new_pair (TDLSYM, new_vptr(a), b);\
})

#if OLVM_INEXACTS
#define new_inexact(a) ({\
inexact_t f = (inexact_t) a;\
	word* me = new_alloc (TINEXACT, sizeof(f));\
	*(inexact_t*)&me[1] = f;\
	/*return*/me;\
})
#endif


/****************************************************************/
#endif//__OLVM_H__

// <!-- __OLVM_C__
/****************************************************************/

#define __OLVM_NAME__ "OL"
#ifndef __OLVM_VERSION__
#define __OLVM_VERSION__ "2.6.rc3"
#endif
#ifndef lint
__attribute__((used)) const char copyright[] = "@(#)(c) 2014-2024 Yuriy Chumak";
#endif//lint

#define unless(...) if (! (__VA_ARGS__))

// gcc profiling:
// 1) gcc --coverage
// 2) gcov -b olvm.c

// http://beefchunk.com/documentation/lang/c/pre-defined-c/precomp.html
#ifdef __GNUC__
#	define GCC_VERSION (__GNUC__ * 10000 \
	                  + __GNUC_MINOR__ * 100 \
	                  + __GNUC_PATCHLEVEL__)
#	if GCC_VERSION < 30200
#		error "Required gcc version 3.2+"
#	endif

#	if __STDC_VERSION__ < 199901L
#		error "Required gnu99 enabled (-std=gnu99)"
#	endif
#endif

#ifndef WARN_ALL
#	ifdef __clang__
		// Stupid clang generates warnings just after preprocessor and
		//   before __builtin_types_compatible_p() / sizeof() preprocessor,
		//   and founds a warnings in places that definitely will not compile.
		// I don't want to make a code completely unreadable to satisfy it.
#		pragma clang diagnostic ignored "-Wtautological-constant-out-of-range-compare"
#	endif
	// while (ch = *p++)
#	pragma GCC diagnostic ignored "-Wparentheses"
	// my unused labels will be used in future
#	pragma GCC diagnostic ignored "-Wunused-label"
#endif

// #ifndef __has_feature
// #   define __has_feature(x) 0
// #endif

// additional gcc staff
// http://www.pixelbeat.org/programming/gcc/static_assert.html
// https://gcc.gnu.org/wiki/C11Status
// #define XSTR(x) STR(x)
// #define STR(x) #x
// #pragma message "The value of GCC_VERSION: " XSTR(GCC_VERSION)
#ifndef static_assert
# if GCC_VERSION < 40600
#	define CONCATENATE_DETAIL(x, y) x##y
#	define CONCATENATE(x, y) CONCATENATE_DETAIL(x, y)
#	define MAKE_UNIQUE(x) CONCATENATE(x, __COUNTER__)
#	define static_assert(condition, comment) \
		typedef char MAKE_UNIQUE(static_assertion_)[2*(!!(condition)) - 1];
# else
#	define static_assert _Static_assert
# endif
#endif

#ifndef offsetof
# if GCC_VERSION < 40503
#	define offsetof(st, m) \
        ((size_t)((char *)&((st *)0)->m - (char *)0))
# else
#	define offsetof __builtin_offsetof
# endif
#endif

#if GCC_VERSION < 40500
#	define __builtin_unreachable() do { \
		char s[] = "I saw a dragon!\n"; \
		write(STDOUT_FILENO, s, sizeof(s)); abort(); \
	} while(0)
#endif

#ifdef __unix__

// FreeBSD, NetBSD, OpenBSD, macOS, etc.
# ifndef __linux__
#	define SYSCALL_PRCTL 0
#	define SYSCALL_SYSINFO 0
#	define SYSCALL_GETRUSAGE 0
#	define SYSCALL_GETRLIMIT 0
# endif

#endif

#ifdef __EMSCRIPTEN__
#	define SYSCALL_PRCTL 0

#	define HAVE_SOCKETS 0
#   define HAVE_MEMFD_CREATE 0
#endif

#ifdef __ANDROID__
// gdb for android: https://dan.drown.org/android/howto/gdb.html
// http://resources.infosecinstitute.com/android-hacking-and-security-part-20-debugging-apps-on-android-emulator-using-gdb/

// android supports seccomp only for Lollipop and Nougat
// https://security.googleblog.com/2016/07/protecting-android-with-more-linux.html
#	if __ANDROID_API__ < 15
#		define HAVE_SECCOMP 0
#	endif
#	if __mips__
#		define HAVE_SECCOMP 0
#	endif
#	define SYSCALL_SYSINFO 0
#	define SYSCALL_GETRLIMIT 0

#	if __ANDROID_API__ < 30
#		define HAVE_MEMFD_CREATE 0
#	endif
#endif

// https://msdn.microsoft.com/en-us/library/b0084kay.aspx
// WIN32: Defined for Win32 and Win64 applications, always defined.
#ifdef _WIN32
#	define SYSCALL_PRCTL 0    // no sandbox for windows yet, sorry
#	define SYSCALL_GETRLIMIT 0
// qemu for windows: https://qemu.weilnetz.de/
// images for qemu:  https://4pda.ru/forum/index.php?showtopic=318284

#   define HAVE_MEMFD_CREATE 0 // we have own win32 implementation!
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

#   define HAVE_MEMFD_CREATE 0
#endif

// ---------------------------------------------------------
// максимальные атомарные числа для элементарной математики:
//	для 32-bit: 16777215 (24 бита, 0xFFFFFF)
//  для 64-bit: 72057594037927935 (56 бит, 0xFFFFFFFFFFFFFF)

// математику считать так: (values-apply (vm:add (vm:maxvalue) 1) (lambda (n carry) (list carry n)))
//                   либо: (let* ((n carry (vm:add (vm:maxvalue) 1))) (...))
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

// Несколько замечаний по WIN32::ThreadProc
//  http://msdn.microsoft.com/en-us/library/windows/desktop/ms686736(v=vs.85).aspx
//  The return value should never be set to STILL_ACTIVE (259), as noted in GetExitCodeThread.

// todo: support ALL of this OS
// http://sourceforge.net/p/predef/wiki/OperatingSystems/
//    Linux-i386                                                                  +
//    Linux-x86_64 (amd64)                                                        +
//    Linux-powerpc                                                               +
//    Linux-sparc
//    Linux-ARM                                                                   +
//    Win32-i386 (2000/XP, WinNT or later)                                        +
//    Win64-x86_64 (XP or later)                                                  +
//    Wince-ARM (cross compiled from win32-i386)
//    FreeBSD-i386                                                                +
//    FreeBSD-x86_64                                                              +
//    NetBSD-i386                                                                 +
//    NetBSD-x86_64                                                               +
//    Mac OS X/Darwin for PowerPC (32 and 64 bit)                                 +
//    Mac OS X/Darwin for Intel (32 and 64 bit)                                   +
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

// DEFAULTS,
// please don't change! use -D{OPTION}={0|1} compiler command line instead

#ifndef HAVE_SOCKETS
#define HAVE_SOCKETS 1
#endif

#ifndef HAVE_SENDFILE
# if defined(__linux__) || defined(__APPLE__)
#  define HAVE_SENDFILE HAVE_SOCKETS
# else
#  define HAVE_SENDFILE 0
# endif
#endif

#ifndef HAVE_DLOPEN
#define HAVE_DLOPEN 1
#endif

#ifndef HAVE_SECCOMP
#define HAVE_SECCOMP 0
#endif
#ifndef HAVE_SANDBOX
// we support only seccomp as sendbox right now
// todo: add bsd sandboxes and, possibly, other
#define HAVE_SANDBOX HAVE_SECCOMP
#endif

#ifndef HAVE_STRFTIME
#define HAVE_STRFTIME 1
#endif

// builtin olvm math functions (vm:fp1)
#ifndef OLVM_BUILTIN_FMATH
#define OLVM_BUILTIN_FMATH 1
#endif

#ifndef OLVM_LIBRARY_SO_NAME
#define OLVM_LIBRARY_SO_NAME NULL
#endif

#ifndef OLVM_UNSAFES
#define OLVM_UNSAFES 0
#endif

#ifndef OLVM_TARVENV
#define OLVM_TARVENV 0
#endif

#if HAVE_UNISTD_H
#include <unistd.h> // posix, https://ru.wikipedia.org/wiki/C_POSIX_library
#endif

#ifdef __linux__
#include <features.h>
#endif


// check this for nested functions:
//	https://github.com/Leushenko/C99-Lambda

// posix or not:
//	http://stackoverflow.com/questions/11350878/how-can-i-determine-if-the-operating-system-is-posix-in-c
// https://web.archive.org/web/20150816133838/http://nadeausoftware.com/articles/2012/01/c_c_tip_how_use_compiler_predefined_macros_detect_operating_system

#ifdef __MINGW32__ /* forkaround for mingw bug */
#ifndef _cdecl
#define _cdecl __cdecl
#endif

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
//	quote, lambda, receive, values, bind, ifeq, setq, ifa, (env.scm)
//	все остальное - макросы или функции/процедуры

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <dirent.h>
#include <string.h>
#include <setjmp.h>

// alloca
#if defined(__linux__) || defined(__APPLE__)
#include <alloca.h>
#endif
#if defined(_WIN32)
#include <malloc.h>
#endif

#ifndef __GNUC__
#define __builtin_alloca alloca
#endif

#include <errno.h>
#include <stdio.h>
//#include <inttypes.h> // not exist under RISC-V (and not required, actually)
#include <fcntl.h>
#ifndef _WIN32
#include <termios.h>
#endif

#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef _WIN32
#include <sys/ioctl.h>
#endif

#if defined(__unix__) || defined(__APPLE__)
#include <sys/wait.h>
#endif

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#endif

#ifdef __ANDROID__
#include <android/log.h>
#endif

// some portability issues (mainly for freebsd)
#ifndef EWOULDBLOCK
#define EWOULDBLOCK EAGAIN
#endif

#include <time.h>
#include <math.h>
//	https://gcc.gnu.org/wiki/FloatingPointMath
#ifdef __TINYC__
#	define __builtin_sqrt  sqrt
#	define __builtin_sin   sin
#	define __builtin_cos   cos
#	define __builtin_tan   tan
#	define __builtin_atan  atan
#	define __builtin_log   log
#	define __builtin_exp   exp
#	define __builtin_asin  asin
#	define __builtin_acos  acos
#	define __builtin_floor floor
#	define __builtin_atan2 atan2
#	define __builtin_pow   pow
#endif

// memfd_create:
#include <sys/mman.h>
#if !HAVE_MEMFD_CREATE
#	if defined(__linux__) || defined(__APPLE__)
	// not a real memfd_create, but compatibility wrapper
	int memfd_create (const char* name, unsigned int flags)
	{
		(void) name;
		assert (flags == 0);

		char tmp_m[] = "/tmp/memfd_olvmXXXXXX";
		int fd = mkstemp(tmp_m); unlink(tmp_m);

		return fd;
	}
#	endif
#	if defined(_WIN32)
	int memfd_create (const char* name, unsigned int flags)
	{
		(void) name;
		assert (flags == 0);

		TCHAR path[MAX_PATH];
		GetTempPath(MAX_PATH, path);
		TCHAR file[MAX_PATH];
		GetTempFileName(path, "memfd_olvm", 0, file);

		HANDLE handle = CreateFile(file, GENERIC_READ | GENERIC_WRITE, 0,NULL, CREATE_ALWAYS,
			FILE_ATTRIBUTE_TEMPORARY | FILE_FLAG_DELETE_ON_CLOSE | FILE_FLAG_SEQUENTIAL_SCAN, NULL);
		return _open_osfhandle((intptr_t) handle, 0);
	}
#	endif
#endif

#include <sys/utsname.h> // we have own win32 implementation
#ifdef _WIN32
int (*puname)(struct utsname* out) = uname;
# define uname puname
#endif

#if HAVE_DLOPEN
#	include <dlfcn.h> // we have own win32 implementation
#endif

#if !defined(SYSCALL_SYSINFO) || (defined(SYSCALL_SYSINFO) && SYSCALL_SYSINFO != 0)
#	include <sys/sysinfo.h> // we have own win32 implementation
#endif

#ifdef __linux__
#	include <sys/resource.h> // getrusage
#	if HAVE_SECCOMP
#		include <sys/prctl.h>
#		include <linux/seccomp.h>
#	endif
#endif

#ifdef _WIN32
#	include <conio.h>
#	include <malloc.h>

#	if HAVE_SOCKETS
#		include <winsock2.h>
#	endif
#	include "unistd-ext.h"  // own win32 implementation
#	include "stdlib-ext.h"  // own win32 implementation

// we'r not using win32 ERROR (which is 0), we are using own macro
#	ifdef ERROR
#	undef ERROR
#	endif
#endif


// yield:
#ifdef __unix__
# if !defined (__EMSCRIPTEN__) && HAS_CDEFS
#	include <sys/cdefs.h>
# endif
# if !defined (__EMSCRIPTEN__)
#	include <sched.h> // yield
# endif
#endif

#ifdef __APPLE__
#	include <sched.h>
#endif

void yield()
{
#ifdef __EMSCRIPTEN__
	emscripten_sleep(1);
#else
# if defined(__unix__) || defined(__APPLE__)
	sched_yield();
# endif
# ifdef _WIN32
	Sleep(1);
# endif
#endif
}


// sockets/sendfile:
// sockets:
#if HAVE_SOCKETS

# ifdef __unix__
#	include <sys/socket.h>
#	include <netinet/in.h>
#  ifndef __linux__
#	include <sys/select.h>
#  endif

#	include <arpa/inet.h> // for inet_addr()
#	include <netdb.h>     // for gethostbyname()

#	ifndef PF_INET
#	define PF_INET AF_INET
#	endif

#	ifndef INADDR_NONE
#	define INADDR_NONE	0xffffffff
#	endif
# endif

# ifdef _WIN32
#	include <winsock2.h>
#	include <ws2tcpip.h>

	typedef unsigned long in_addr_t;
#	ifndef EWOULDBLOCK
#	define EWOULDBLOCK WSAEWOULDBLOCK
#	endif
# endif

# ifdef __APPLE__
#	undef _POSIX_C_SOURCE // macos sendfile workaround
#	include <sys/socket.h>
#	include <netinet/in.h>

#	include <arpa/inet.h> // for inet_addr()
#	include <netdb.h>     // for gethostbyname()
# endif

//#ifdef __ANDROID__
//	typedef unsigned long in_addr_t;
//#endif
#endif // HAVE_SOCKETS

// sendfile:
#if HAVE_SENDFILE
#	ifdef __linux__
#		include <sys/sendfile.h>
#	endif
#	ifdef __APPLE__
#		include <sys/types.h>
#		include <sys/uio.h>
#	endif
#else
# if HAVE_SOCKETS
#	undef HAVE_SENDFILE
#	define HAVE_SENDFILE 1
#	ifndef _WIN32
#		define SOCKET_ERROR -1
#		define SD_SEND SHUT_WR
#	endif
	size_t
	sendfile(int out_fd, int in_fd, off_t *offset, size_t count)
	{
		char buf[8*1024];
		size_t toRead, numRead, numSent, totSent;

		totSent = 0;
		while (count > 0) {
			toRead = (sizeof(buf) < count) ? sizeof(buf) : count;

			// read
			numRead = read(in_fd, buf, toRead);
			if (numRead == -1)
				return -1;
			if (numRead == 0) // EOF
				break;

			// write
			resend:
			numSent = send(out_fd, buf, numRead, 0);
			if (numSent == SOCKET_ERROR) {
			#ifdef _WIN32
				if (WSAGetLastError() != WSAEWOULDBLOCK)
			#else
				if (errno != EAGAIN && errno != EWOULDBLOCK)
			#endif
					return -1;

				yield();
				goto resend;
			}
			if (numSent == 0) // should never happen
				return 0;

			count -= numSent;
			totSent += numSent;
			offset += numSent;
		}
		return totSent;
	}
# endif
#endif

// FFI support:
#ifndef OLVM_FFI
// ffi have no sense without dlopen/dlsym
#define OLVM_FFI HAVE_DLOPEN
#endif

#if OLVM_FFI && defined(OLVM_NOPINS)
#error "FFI requires PINS support"
#endif


#ifndef OLVM_CALLABLES
#define OLVM_CALLABLES OLVM_FFI
#endif


// additional defines:
#ifndef O_BINARY
#	define O_BINARY 0
#endif

// https://gcc.gnu.org/onlinedocs/gcc/Diagnostic-Pragmas.html
//#pragma GCC diagnostic push
//#pragma GCC diagnostic error "-Wuninitialized"
//#pragma GCC diagnostic pop

#ifndef VMRAW_CHECK
#define VMRAW_CHECK 1
#endif

#ifndef OLVM_NOMAIN
#define OLVM_NOMAIN 0
#endif

//empty if not exist
#ifndef OLVM_SETOPEN
#define OLVM_SETOPEN
#endif

#if OLVM_TARVENV
# ifdef _WIN32

# endif
# ifdef __unix__
#	include <elf.h>
# endif
#endif

// ========================================
// -=( logger )=---------------------------
#ifndef OLVM_OWNLOGGER
#define OLVM_OWNLOGGER 1
#endif

#if OLVM_OWNLOGGER
#	pragma GCC diagnostic push
#	pragma GCC diagnostic ignored "-Wunused-result"
# ifdef __clang__
#	pragma GCC diagnostic ignored "-Wstring-plus-int"
# endif

void E(char* format, ...)
{
	va_list args;
	va_start(args, format);
	int fd = STDERR_FILENO;

	for(;;)
	switch (*format++) {
	case 0: { // done
		char newline[] = "\n";
		(void)write(fd, newline, sizeof(newline)-1);
		// fsync(fd);
		va_end(args);
		return;
	}
	case '%': { // mask
		switch (*format++) {
		case 0:
			--format;
			continue;
		case 's': {
			char* s = va_arg(args, char*);
			(void)write(fd, s, strlen(s));
			break;
		}
		case 'd': {
			int d = va_arg(args, int);
			if (d < 0) {
				(void)write(fd, "-", 1);
				d = -d;
			}
			int q = 1;
			for (int i = d; i != 0; i /= 10)
				q *= 10;
			q /= 10;
			for (int i = q; i != 0; i /= 10)
				(void)write(fd, "0123456789" + (d / i) % 10, 1);
			break;
		}
		case 'p': {
			word p = va_arg(args, word);
			(void)write(fd, "0x", 2);
#if defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
			for (int i = 0; i < sizeof(word)*8; i += 4)
#else
			for (int i = sizeof(word)*8 - 4; i >= 0; i -= 4)
#endif
				(void)write(fd, "0123456789abcdef" + ((p >> i) & 0xF), 1);
			break;
		}
		default:
			(void)write(fd, format-1, sizeof(char));
		}
		continue;
	}
	default:
		(void)write(fd, format-1, sizeof(char));
	}
}
#	pragma GCC diagnostic pop
#else
#	define E(...) fprintf(stderr, __VA_ARGS__)
#endif

// --------------------------------------------------------
// -=( i/o )=----------------------------------------------
// os independent i/o implementations
typedef int     (open_t) (const char *filename, int flags, int mode, void* userdata);
typedef int     (close_t)(int fd, void* userdata);
typedef ssize_t (read_t) (int fd, void *buf, size_t count, void* userdata);
typedef ssize_t (write_t)(int fd, void *buf, size_t count, void* userdata);
typedef int     (stat_t) (const char *filename, struct stat *st, void* userdata);

typedef void    (idle_t) (void* userdata);

// notes: 64-bit versions of Windows use 32-bit handles for
//	interoperability. When sharing a handle between 32-bit
//	and 64-bit applications, only the lower 32 bits are
//	significant, so it is safe to truncate the handle (when
//	passing it from 64-bit to 32-bit) or sign-extend the
//	handle (when passing it from 32-bit to 64-bit).

// iternal wrappers for open/close/read and write functions:
// (just skip userdata)
static inline
int os_open (const char *filename, int flags, int mode, void* userdata) {
	(void) userdata;
	return open(filename, flags, mode);
}
static inline
int os_close(int fd, void* userdata) {
	(void) userdata;
	return close(fd);
}
static inline
ssize_t os_read(int fd, void *buf, size_t size, void* userdata) {
	(void) userdata;
	return read(fd, buf, size);
}
static inline
ssize_t os_write(int fd, void *buf, size_t size, void* userdata) {
	(void) userdata;
	return write(fd, buf, size);
}
static inline
int os_stat(const char *filename, struct stat *st, void* userdata) {
	(void) userdata;
	// D("os_stat(%s, %p, %p)", filename, st, userdata);
	return stat(filename, st);
}

// --------------------------------------------------------
// -=( TAR Virtual ENVironment )=--------------------------
// take it all in
#if OLVM_TARVENV
static char* pvenv = NULL;

__attribute__((__used__))
static int pvenv_open (const char *filename, int flags, int mode, void* userdata) {
	(void) userdata;
	if (pvenv && filename) {
		char* ptr = pvenv;

		while (*ptr) {
			int len = 0;
			for (int i = 124; i < 124+11; i++)
				len = len * 8 + ptr[i] - '0';

			if (strcmp(ptr, filename) == 0) {
				int fd = memfd_create((char*)filename, 0);
				write(fd, ptr + 0x200, len);
				lseek(fd, 0, SEEK_SET);
				return fd;
			}

			ptr += 0x200; // skip header
			// tar format operates on 512-byte blocks.
			ptr += (len + 0x1FF) & -0x200;
		}
	}
	return open(filename, flags, mode);
}

__attribute__((__used__))
static char* pvenv_main() {
	// linux: `readlink /proc/self/exe`
	// solaris: `getexecname()` or `readlink("/proc/self/path/a.out", buf, bufsize)`
	// freebsd: `readlink("/proc/curproc/file", buf, bufsize)` or `sysctl CTL_KERN KERN_PROC KERN_PROC_PATHNAME -1`
	// netbsd: `readlink /proc/curproc/exe`
	// dragonflybsd: `readlink /proc/curproc/file`
	// windows: `GetModuleFileName()`
	char exe[PATH_MAX]; // TODO: cancel filename size limitations

# ifdef __unix__
#  if UINTPTR_MAX == UINT32_MAX
#   define Elf_Ehdr Elf32_Ehdr
#   define Elf_Shdr Elf32_Shdr
#  else
#   define Elf_Ehdr Elf64_Ehdr
#   define Elf_Shdr Elf64_Shdr
#  endif

#  ifdef __linux__
	int len = readlink("/proc/self/exe", exe, sizeof(exe));
#  else
	int len = 0;
#  endif
	if (len > 0 && len < sizeof(exe)) {
		exe[len] = 0;
		int fp = open(exe, O_RDONLY | O_BINARY, S_IRUSR);
		if (fp >= 0) {
			struct stat sb;
			fstat(fp, &sb);

			if (S_ISREG(sb.st_mode) || S_ISLNK(sb.st_mode)) {
				char* ptr = (char*) mmap(0, sb.st_size, PROT_READ, MAP_PRIVATE, fp, 0);

				Elf_Ehdr* elf = (Elf_Ehdr*) ptr;
				Elf_Shdr* sym_table;
				sym_table = (Elf_Shdr*) (ptr + elf->e_shoff);
				char* names = ptr + sym_table[elf->e_shstrndx].sh_offset;

				for (int i = 0; i < elf->e_shnum; i++) {
					if (!strcmp(".tar", (names + sym_table[i].sh_name))) {
						pvenv = ptr + sym_table[i].sh_offset;
						break;
					}
				}

				if (!pvenv) {
					munmap(ptr, sb.st_size);
				}
			}
			close(fp);
		}
	}
# endif
# ifdef _WIN32
	int len = GetModuleFileName(NULL, (LPSTR)&exe, sizeof(exe));
	if (len > 0 && len < sizeof(exe)) {
		HANDLE fh = CreateFile(exe, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
		if (fh != INVALID_HANDLE_VALUE) {
			HANDLE fm = CreateFileMapping(fh, NULL, PAGE_READONLY, 0, 0, NULL);
			if (fm != NULL) {
				char* ptr = MapViewOfFile(fm, FILE_MAP_READ, 0, 0, 0);
				char* end = ptr + GetFileSize(fh, NULL);
				IMAGE_NT_HEADERS* imageNTHeaders = (IMAGE_NT_HEADERS*)
					(ptr + ((IMAGE_DOS_HEADER*)ptr)->e_lfanew);
				size_t size = imageNTHeaders->OptionalHeader.SizeOfHeaders;

				char* sectionLocation = (char*)imageNTHeaders + sizeof(DWORD) + sizeof(IMAGE_FILE_HEADER) + imageNTHeaders->FileHeader.SizeOfOptionalHeader;
				for (int i = 0; i < imageNTHeaders->FileHeader.NumberOfSections; i++) {
					IMAGE_SECTION_HEADER* sectionHeader = (IMAGE_SECTION_HEADER*)sectionLocation;
					size += sectionHeader->SizeOfRawData;

					sectionLocation += sizeof(IMAGE_SECTION_HEADER);
				}
                // 0x200 - is a minimal non-empty tar file size
				if ((ptr + size + 0x200 < end) && (strncmp((ptr + size + 0x101), "ustar", 5) == 0))
					pvenv = ptr + size;
				else
					UnmapViewOfFile(ptr);
				CloseHandle(fm);
			}
			CloseHandle(fh);
		}
	}
# endif
	return pvenv;
}

#endif

// ----------
// -----------------------------------------------------------------------------
// ------------------------------
// -=( OL )=--------------------------------------------------------------------
// --

// internal declarations

// набор макросов - проверок для команд
// car, cdr:
#ifndef CAR_CHECK
#define CAR_CHECK 0
#endif

#if CAR_CHECK
#	define CHECKCAR(arg) is_pair(arg) || is_npairp(arg) || is_npairn(arg) || is_rational(arg) || is_complex(arg)
#else
#	define CHECKCAR(arg) is_reference(arg)
#endif

#ifndef CDR_CHECK
#define CDR_CHECK 0
#endif

#if CDR_CHECK
#	define CHECKCDR(arg) is_pair(arg) || is_npairp(arg) || is_npairn(arg) || is_rational(arg) || is_complex(arg)
#else
#	define CHECKCDR(arg) is_reference(arg)
#endif


#define NR                          256 // see n-registers in register.scm
#define CR                          128 // available initial callables, should be more than 4!

#define GCPAD(nr)                  (nr+3) // space after end of heap to guarantee the GC work
#define MEMPAD                     (1024) // space after end of heap to guarantee apply
#define FREESPACE                  (4096) // expected working memory buffer (in words)

// 1024 - некое магическое число, подразумевающее количество
// памяти, используемой между вызовами apply. мои тесты пока показывают максимальное число 96


// possible data models: LP64 ILP64 LLP64 ILP32 LP32
// http://www.unix.org/version2/whatsnew/lp64_wp.html
// http://stackoverflow.com/questions/384502/what-is-the-bit-size-of-long-on-64-bit-windows
// http://ru.cppreference.com/w/cpp/language/types

// LP32 or 2/4/4: int — 16 bits, long and pointer - 32 bits
//	Win16 API (not supported)
// ILP32 or 4/4/4: int, long and pointer — 32 bits
//	Win32 API (supported)
//	Unix and Unix-like systems (Linux, Mac OS X) (supported)
// LLP64 or 4/4/8: int and long — 32 bits, pointer — 64 bits
//	Win64 API (supported)
// LP64 or 4/8/8: int — 32 bits, long and pointer — 64 bits
//	Unix and Unix-like systems (Linux, Mac OS X) (supported)

// Other models are rare. For example, ILP64 or 8/8/8: int, long and pointer — 64 bits)
//  only in some older 64-bit Unix-systems (Unicos for Cray, etc.)

// win32/mingw fix for ILP32/LLP64
#ifdef _WIN32
# ifdef _WIN64
#	define __LLP64__ 1
# else
#	define __ILP32__ 1
# endif
#endif
// ubuntu 18.04 fix for LP64/ILP32
#ifndef __LP64__
# if (__x86_64__ && (__unix__ || __APPLE__)) || __aarch64__
#	define __LP64__ 1
# endif
#endif
// older unixes
#ifndef __ILP32__
# if (__i386__ && __unix__) || __ARMEL__
#	define __ILP32__ 1
# endif
#endif


#if SIZE_MAX == 0xffffffffffffffff
#	define MATH_64BIT 1
#	define MATH_32BIT 0
#elif SIZE_MAX == 0xffffffff
#	define MATH_64BIT 0
#	define MATH_32BIT 1
#else
#	error Unsupported math bit-count
#endif


// http://outflux.net/teach-seccomp/
// http://mirrors.neusoft.edu.cn/rpi-kernel/samples/seccomp/bpf-direct.c
// https://www.kernel.org/doc/Documentation/prctl/seccomp_filter.txt
#define SECCOMP                     10000 // todo: change to x1000 или что-то такое
static int sandboxp = 0;     /* are we in seccomp? а также дельта для оптимизации syscall's */
//static unsigned long seccomp_time; /* virtual time within seccomp sandbox in ms */

#if OLVM_UNSAFES
static int unsafesp = 1;
#endif

//static int breaked = 0;    /* set in signal handler, passed over to owl in thread switch */


// -= gc implementation =-----------
#define is_flagged(x) (((word)(x)) & 1)  // mark for GC

// cells - новый размер кучи (в словах)
static
ptrdiff_t resize_heap(heap_t *heap, int cells)
{
	if (sandboxp) /* realloc is not allowed within seccomp */
		return 0;

	word *old = heap->begin;
	size_t pads = heap->padding;
	heap->begin = realloc(heap->begin, (cells + pads) * sizeof(word));
    if (heap->begin == NULL) {
        E("Fatal: heap reallocation failed! (%ld -> %ld)", heap->end - old, (long)(cells + pads));
        exit(1); // todo: manage memory issues by longjmp
		// longjmp(heap->ret, IFALSE); todo: manage memory issues; todo: signal SIGGC
    }
	heap->end = heap->begin + cells; // so, heap->end is not actual heap end, we have a "safe" bytes after

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
			int sz = object_size(hdr);
			if (is_rawstream(pos))
				pos += sz; // no pointers in raw objects
			else {
				pos++, sz--;
				while (sz--) {
					word val = *pos;
					if (is_reference(val)) // TODO: optimize
						*pos = val + delta*W;
					pos++;
				}
			}
		}
		return delta;
	} else {
		E("Heap adjustment failed");
		// longjmp(heap->ret, IFALSE); todo: manage memory issues; todo: signal SIGGC
	}
	return 0;
}

/* input desired allocation size and (the only) pointer to root object
   return a pointer to the same object after heap compaction, possible heap size change and relocation */
#if DEBUG_GC
big_t marked;
#endif

// возвращается по цепочке "flagged" указателей назад
static __inline__
word* chase(word* pos) {
	//	assert(pos IS flagged)
	word* p_pos;
	while (1) {
		p_pos = *(word**) ((word)pos & ~1);      // p_pos = *pos; ~ = bitwise NOT, (корректное разименование указателя, без учета бита mark)
		if (!is_reference(p_pos) || !is_flagged(p_pos)) // p_pos & 0x3 == 0x1  // TODO: optimize!
			return (word*)((word)pos & ~1);
		pos = p_pos;
	}
}

// todo: ввести третий generation
// просматривает список справа налево
static
void mark(word *pos, word *end, heap_t* heap)
{
#if DEBUG_GC
	marked = 0;
#endif
//	assert(pos is NOT flagged)
	while (pos != end) {
		word val = ref(pos, 0); // pos header
		if (is_reference(val) && val >= (word)heap->genstart) { // genstart - начало молодого поколения
			if (is_flagged(val))
				pos = chase((word*) val);
			else {
				word hdr =*((word*) val);
#if DEBUG_GC
				marked++;
#endif

				word* ptr = (word*)val;
				*pos = *ptr;
				*ptr = ((word)pos | 1);

				unless (hdr & (RAWBIT|1))
					pos = ((word *) val) + object_size(hdr);
			}
		}
		--pos;
	}
}

// compact & sweep
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

			word h = object_size(val);
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
			old += object_size(*old);
	}
	return newobject;
}

//__attribute__ ((aligned(sizeof(word))))
// heap: heap
// query: sure to be able to alloc query words
// regs: registers
static
word gc(heap_t *heap, long query, word regs)
{
	word *fp;
	if (query == -1) { // do the full gc?
		query = 0;
		heap->genstart = heap->begin; // reset generations
	}
	else
		query += FREESPACE; // expected tmp objects buffer

	fp = heap->fp;
	{
		word *root = &fp[1];
	//	word *root = fp + 1; // same

		#if DEBUG_GC
			clock_t gctime;
			gctime = -(1000 * clock()) / CLOCKS_PER_SEC;
		#endif

		// непосредственно сам процесс сборки
		root[0] = regs;
		mark(root, fp, heap);        // assert (root > fp)
		fp = sweep(fp, heap);
		regs = root[0];

		// TODO: add diagnostic callback "if (heap->oncb) heap->oncb(heap, deltatime)"
#if DEBUG_GC
		static int tick = 0;
		typedef long long int lld;
		// show only full garbage collecting
		if ((heap->genstart == heap->begin) || (tick++ > 999)) {
			gctime += (1000 * clock()) / CLOCKS_PER_SEC;
			struct tm tm = *localtime(&(time_t){time(NULL)});
			char buff[70]; strftime(buff, sizeof buff, "%c", &tm);
			fprintf(stderr,
					"[%s], %sGC done in %d ms, used %2ld%% (%ld from %ld words), %lld live %s objects.\n", //marked %6d, moved %6d, pinned %2d, moved %8d bytes total\n",
					buff/*asctime(&tm)*/,
					heap->genstart == heap->begin ? "full " : "     ", (int)gctime,
					((fp - (word*) heap->begin) * 100)/((heap->end - heap->begin)),
					((fp - (word*) heap->begin)),      ((heap->end - heap->begin)),
					(lld)marked, heap->genstart == heap->begin ? "total" : "young"
				);
			tick = 0;
		}
#endif
	}
	heap->fp = fp;

	// кучу перетрясли и уплотнили, посмотрим надо ли ее увеличить/уменьшить

	ptrdiff_t hsize = heap->end - heap->begin; // вся куча в словах
	ptrdiff_t nfree = heap->end - (word*)regs; // свободно в словах
	ptrdiff_t nused = hsize - nfree;           // использовано слов

	nused += query; // сколько у нас должно скоро стать занятого места
	if (heap->genstart == heap->begin) {
		// сюда мы попадаем только после полной сборки (или первый раз)

		// Please grow your buffers exponentially:
		//  https://blog.mozilla.org/nnethercote/2014/11/04/please-grow-your-buffers-exponentially/
		//  ! https://habrahabr.ru/post/242279/

		// выделим на "старое" поколение не более 50% кучи, при этом кучу будем увеличивать на 33%
		// note!: множитель регулярного увеличения кучи должен быть меньше золотого сечения: 1.618
		if (nused > (hsize * 2 / 3)) {
			if (nused < hsize)
				nused = hsize;
#if DEBUG_GC
			fprintf(stderr, "Growing GC from %ld(%ld) to %ld(%ld), queried %ld\n",
					hsize, hsize*W, nused + nused / 3, (nused + nused / 3) * W, query);
#endif
			// TODO: ограничить рост 300 мегабайтами
			//       (после гигабайта растем линейно, а не экспоненциально)
			regs += resize_heap(heap, nused + nused / 3) * W;
		}
		// decrease heap size if more than 33% is free by 11% of the free space
		else if (nused < (hsize / 3)) {
#if DEBUG_GC
			fprintf(stderr, "Shrinking GC from %ld(%ld) to %ld(%ld), queried %ld\n",
					hsize, hsize*W, hsize - hsize / 9, (hsize - hsize / 9) * W, query);
#endif
			regs += resize_heap(heap, hsize - hsize / 9) * W;
		}
		heap->genstart = (word*)regs; // always start new generation
	}
	// полная сборка, если осталось меньше 20% свободного места в куче
	//  иначе, у нас слишком часто будет вызываться сборщик
	// TODO: посчитать математически, на каком именно числе
	//  стоит остановиться, чтобы ни слишком часто не проводить молодую сборку,
	//  ни слишком часто старую. мне по тестам кажется, что 20% это вполне ок.
	else if ((nfree - query) < hsize / 5) {
		heap->genstart = heap->begin; // force start full generation
		return gc(heap, query, regs);
	}
	else
		heap->genstart = (word*)regs; // simply start new generation

	return regs;
}



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

/* small functions defined locally after hitting some portability issues */
//static __inline__ void bytecopy(char *from, char *to, int n) { while (n--) *to++ = *from++; }
static __inline__ void wordcopy(word *from, word *to, int n) { while (n--) *to++ = *from++; }

#if !OLVM_NOMAIN
#	ifdef _WIN32

static
void set_signal_handlers() { }

#	else

static
void sigint_handler(int sig)
{
	(void) sig;
	set_blocking(STDOUT_FILENO, 1);
	set_blocking(STDERR_FILENO, 1);
	exit(1);
}

static
void set_signal_handlers()
{
	signal(SIGINT, sigint_handler);
	signal(SIGPIPE, SIG_IGN);	// do not break on sigpipe
	signal(SIGCHLD, SIG_IGN);   // avoid zombies
}

#	endif//_WIN32
#endif
/***********************************************************************************
 * OL
 */
struct olvm_t
{
	heap_t heap; // MUST be first!
	jmp_buf ret; // emergency exit

	// word max_heap_size; // max heap size in MiB

	// 0 - mcp, 1 - this, 2 - clos-this, 3 - a0, often cont, 4 - a1, ..., NR - pin0, ...
	// registers
    word reg[NR];

#ifndef OLVM_NOPINS
	// pinned objects support
    word* pin;
	size_t cr;  // pins count
	size_t ffpin; // first free pin
	size_t lfpin; // last free pin
#endif

	// текущий контекст с арностью
	word this;
	long arity; // arguments count + 1

	// i/o polymorphism
	open_t* open;
	close_t* close;
	read_t* read;
	write_t* write;
	stat_t* stat;

	// debug callbacks
	idle_t* idle;   // OL task ready to switch

	// user data
	void* userdata;
};

static_assert(offsetof(olvm_t, heap) == 0, "heap_t must be first field of olvm_t!");
// -=( ol ffi )--------------------------------------

// =================================================================
// machine floating point support, internal functions
#if OLVM_INEXACTS
// todo: add disabling ol2d (for machines without doubles), or better reusing as ol2f
static
double ol2d_convert(word p) {
	double v = 0;
	double m = 1;
	while (p != INULL) {
		v += value(car(p)) * m;
		m *= HIGHBIT;
		p = cdr(p);
	}
	return v;
}

OLVM_PUBLIC
double OL2D(word arg) {
	if (is_enum(arg))
		return enum(arg);

	assert (is_reference(arg));
	switch (reference_type(arg)) {
	case TINTP:
		return +ol2d_convert(arg);
	case TINTN:
		return -ol2d_convert(arg);
	case TRATIONAL:
		return OL2D(car(arg)) / OL2D(cdr(arg));
	case TCOMPLEX: // only real part of complex number
		return OL2D(car(arg));
#if OLVM_INEXACTS
	case TINEXACT:
		return *(inexact_t*)&car(arg);
#endif
	default:
		assert(0);
		return 0.;
	}
}

static
float ol2f_convert(word p) {
	float v = 0;
	float m = 1;
	while (p != INULL) {
		v += value(car(p)) * m;
		m *= HIGHBIT;
		p = cdr(p);
	}
	return v;
}
OLVM_PUBLIC
float OL2F(word arg) {
	if (is_enum(arg))
		return enum(arg);

	assert (is_reference(arg));
	switch (reference_type(arg)) {
	case TINTP:
		return +ol2f_convert(arg);
	case TINTN:
		return -ol2f_convert(arg);
	case TRATIONAL:
		return OL2F(car(arg)) / OL2F(cdr(arg));
	case TCOMPLEX: // use only real part of complex number
		return OL2F(car(arg));
#if OLVM_INEXACTS
	case TINEXACT:
		return *(inexact_t*)&car(arg);
#endif
	default:
		assert(0);
		return 0.;
	}
}

// TODO: add memory checking
word d2ol(struct heap_t* heap, double v) {
	word* fp;
	// check for non representable numbers:
	if (v == INFINITY || v == -INFINITY || v == NAN)
		return IFALSE; // todo: return +inf.0, -inf.0, +nan.0

	fp = heap->fp;

	word a, b = INULL;
	double i;
	if (modf(v, &i) != 0) {
		word* p = fp;

		word m = 1;
		for (int t = 0; t < 1024; t++) { // ограничим точность снизу
			double i, f = modf(v, &i);
			if (m & ~VMAX) {
				*++p = I(0);
				m >>= VBITS;
			}
			if (f == 0) {
				*++p = I(m);
				break;
			}
			v *= 2;
			m *= 2;
		}
		// если все-таки что-то после запятой было, то
		if (p != fp) {
			modf(v, &v); // отбросим все после запятой

			size_t len = (p - fp);
			p = new(TVECTOR, len) + len; // temp, will be destroyed during next gc()

			if (len == 1)
				b = *p--;
			else
				for (size_t i = 0; i < len; i++)
					b = (word) new_pair(TINTP, *p--, b);
		}
	}

	// word a = INULL;
	// число целое?
	// числа должны лежать в обратном порядке, как мы их и получаем
	// но в память то мы их кладем в обратном! так что нужен реверс
	if (1) {
		int negative = v < 0;  v = v < 0 ? -v : v;
		if (v < (double)HIGHBIT)
			a = negative ? make_value(TENUMN, v) : make_value(TENUMP, v);
		else {
			word* p = fp;
			do {
				*++p = I((long long)v & VMAX);
				modf(v / (double)HIGHBIT, &v);
			}
			while (v > 0);
			size_t len = (p - fp);

			//word* m = (word*) __builtin_alloca(len * sizeof(word)) + len;
			// allocation is safe during MEMPAD 
			new_bytevector(sizeof(word) * len); // dummy,
			               // will be destroyed during next gc()
			word* m = fp;
			p = (word*)INULL;
			for (size_t i = 0; i < len - 1; i++)
				p = new_pair(TINTP, *--m, p);
			a = (word)new_pair(negative ? TINTN : TINTP, *--m, p);
		}
	}
	word r;
	if (b == INULL)
		r = a;
	else
		r = (word)new_pair(TRATIONAL, a, b);

	heap->fp = fp;
	return r;
}

#if OLVM_INEXACTS
#define ol2f(num) __builtin_choose_expr( __builtin_types_compatible_p (inexact_t, double), OL2D(num), OL2F(num) )
#endif // OLVM_INEXACTS

#endif
// =================================================================


// проверить достаточно ли места в стеке, и если нет - вызвать сборщик мусора
static int OLVM_gc(struct olvm_t* ol, long ws) // ws - required size in words
{
	word *fp = ol->heap.fp; // memory allocation pointer

	// если места еще хватит, не будем ничего делать
	if ((ws >= 0) && ((fp + ws) < ol->heap.end))
		return 0;

	word* r = ol->reg;

	// попробуем освободить ненужные регистры
    // (но не пины)
	// !!! это неправильный код, так нельзя
	// TODO: найти лучшее решение по освобождению регистров
	// for (int i = 3 + ol->arity; i < NR; i++)
	// 	r[i] = IFALSE; // todo: use wmemset?

	// assert (fp + N + 3 < ol->heap.end);
	int p = 0, N = NR;

	// создадим в топе два временных объекта со значениями всех регистров и пинов
#ifndef OLVM_NOPINS
    word pins = (word)new(TVECTOR, ol->cr);
    memcpy(payload(pins), ol->pin, ol->cr * sizeof(word));
#else
    word pins = IFALSE;
#endif

	word *regs = new (TVECTOR, N + 2); // N for regs, 1 for this, 1 for pins
	while (++p <= N) regs[p] = r[p-1]; // todo: use memcpy?
	regs[p] = ol->this;
    regs[p + 1] = pins;

	ol->heap.fp = fp; // выполним сборку мусора
	regs = (word*)gc(&ol->heap, ws, (word)regs);

    pins = regs[p + 1];         // и восстановим все пины и регистры, уже подкорректированные сборщиком
	ol->this = regs[p];
	while (--p >= 1) r[p-1] = regs[p];

#ifndef OLVM_NOPINS
    memcpy(ol->pin, payload(pins), ol->cr * sizeof(word));
#endif

	// закончили, почистим за собой:
	ol->heap.fp = regs; // (вручную сразу удалим временный объект, это такая оптимизация)
	return 1;
}

// ff's get implementation
static
word get(word *ff, word key, word def, jmp_buf ret)
{
	while ((word) ff != IEMPTY) { // ff = [header key value [maybe left] [maybe right]]
		word this = ff[1], hdr;
		if (this == key)
			return ff[2];
		hdr = ff[0];
		switch (object_size(hdr)) {
		case 5: ff = (word *) ((key < this) ? ff[3] : ff[4]);
			continue;
		case 3: return def;
		case 4:
			if (key < this)
				ff = (word *) ((hdr & (1 << TPOS)) ? IEMPTY : ff[3]);
			else
				ff = (word *) ((hdr & (1 << TPOS)) ? ff[3] : IEMPTY);
			continue;
		default:
			E("assert! object_size(ff) == %d", (int)object_size(hdr));
			longjmp(ret, IFALSE); // todo: return error code
		}
	}
	return def;
}

#define R0  reg[0]
#define R1  reg[1]
#define R2  reg[2]
#define R3  reg[3]
#define R4  reg[4]
#define R5  reg[5]
#define R6  reg[6]

#define A0  reg[ip[0]]
#define A1  reg[ip[1]]
#define A2  reg[ip[2]]
#define A3  reg[ip[3]]
#define A4  reg[ip[4]]
#define A5  reg[ip[5]]

// generate errors and crashes
#define ERROR5(type,value, code,a,b) { \
	D("VM " type " AT %s:%d (%s) -> %d/%p/%p", __FILE__, __LINE__, __FUNCTION__, code,a,b); \
	R4 = I(code);    R3 = I(value);\
	R5 = (word) (a);\
	R6 = (word) (b);\
	goto error; \
}
#define ERROR_MACRO(_1, _2, _3, NAME, ...) NAME

// "ERROR" is an error (produces 'error, mcp #5)
#define ERROR3(code, a, b) ERROR5("ERROR", 5, code,a,b)
#define ERROR2(code, a) ERROR3(code, a, INULL)
#define ERROR(...) ERROR_MACRO(__VA_ARGS__, ERROR3, ERROR2,, NOTHING)(__VA_ARGS__)

// "CRASH" is a critical error (produces 'crash, mcp #3)
#define CRASH3(code, a, b) ERROR5("CRASH", 3, code,a,b)
#define CRASH2(code, a) CRASH3(code, a, INULL)
#define CRASH(...) ERROR_MACRO(__VA_ARGS__, CRASH3, CRASH2,, NOTHING)(__VA_ARGS__)

#define ASSERT(exp, code, a)        if (!(exp)) CRASH(code, a, INULL);
#define CHECK(exp, val, errorcode)  if (!(exp)) ERROR(errorcode, val, ITRUE);

// # of function calls in a thread quantum
#define TICKS  10000

static
void runtime_gc(struct olvm_t *ol, word words, unsigned char** ip, unsigned char** ip0, word** fp, word* this)
{
	ptrdiff_t dp;
	dp = *ip - *ip0;

	ol->heap.fp = *fp; ol->this = *this;
	ol->heap.gc(ol, words);
	*fp = ol->heap.fp; *this = ol->this;

	// (the bytecode of thread thunk is last element of)
	*ip0 = (unsigned char*) &car(
			reference_type(*this) == TTHREAD
				? (word) ref(*this, reference_size(*this))
				: *this);
	*ip = *ip0 + dp;
	return;
}

#ifdef DEBUG_COUNT_OPS
static unsigned long long ops[256];
#endif

#if HAVE_DLOPEN
# if defined(__ANDROID__) && !OLVM_NOMAIN
	static char* LIBRARY_FILENAME = 0;
# else
	#define LIBRARY_FILENAME OLVM_LIBRARY_SO_NAME
# endif
#endif

static //__attribute__((aligned(8)))
word runtime(struct olvm_t* ol)
{
	heap_t* heap = &ol->heap; // global vm heap
	word *fp = heap->fp; // memory allocation pointer
	word *reg = ol->reg; // virtual machine registers

//	int breaked = 0;
	int ticker = TICKS; // any initial value ok
	int bank = 0; // ticks deposited at interop

	word this = ol->this; // context
	long acc = ol->arity; // arity

	set_blocking(STDOUT_FILENO, 0);
	set_blocking(STDERR_FILENO, 0);

	// vm instruction pointer(s)
	unsigned char *ip = 0, *ip0;
	// internal gc call wrapper
#	define GC(size) runtime_gc(ol, (size), &ip, &ip0, &fp, &this)

#ifdef DEBUG_COUNT_OPS
    bzero(ops, sizeof(ops));
#endif

	// runtime loop
apply:;

	// if it's an allocated object, not a value:
	if (is_reference(this)) {
		word type = reference_type (this);
		if (type == TCLOSURE) { // (66% for "yes")
			R1 = this; this = car(this);
			R2 = this; this = car(this);
		}
		else
		if (type == TPROCEDURE) { // (58% for "yes")
			R1 = this; this = car(this);
		}
		else
		// low bits have special meaning
		if ((type & 0x3C) == TFF) { // (95% for "no")
			// ff assumed to be valid
			word continuation = R3;
            word key = R4;
			switch (acc) {
			case 2:
				R3 = get((word*)this, key,  0, ol->ret); // 0 is "not found"
				if (!R3)
					ERROR(260, this, key);
				break;
			case 3:
				R3 = get((word*)this, key, R5, ol->ret);
				break;
			default:
				ERROR(259, this, I(acc));
			}
			this = continuation;
			acc = 1;

			goto apply;
		}
		else
#if OLVM_FFI // unsafe must be enabled
		// running ffi function
		if (type == TVPTR) { // todo: change to special type or/and add a second word - function name (== [ptr function-name])
                             // todo: use same type by longer size, with function name
			word* args = (word*)INULL;
			for (int i = acc; i > 1; i--)
				args = cons(reg[i+2], args);

			word (*function)(struct olvm_t*, word*) = (word (*)(struct olvm_t*, word*)) car(this);  assert (function);

			heap->fp = fp;

			size_t c = OLVM_pin(ol, R3);
			word x = function(ol, args);
			reg = ol->reg; // pin can realloc registers!
			fp = heap->fp;

			this = OLVM_unpin(ol, c);

			R3 = x; acc = 1;
			goto apply;
		}
		else
#endif
		if (type == TCONSTRUCTOR) {
			this = car(this);
			goto apply;
		}
		else
		if (type != TBYTECODE)
			ERROR(258, this);

		// А не стоит ли нам переключить поток?
		if (--ticker < 0) {
			// время потока вышло, переключим на следующий
			ticker = TICKS;

			// if mcp present,
			if (R0 != IFALSE) {
				// save vm state and enter mcp cont at R0!
				bank = 0;
				acc += 4;

				word *thread;

				thread = new (TTHREAD, acc-1);
				for (ptrdiff_t pos = 1; pos < acc-1; pos++)
					thread[pos] = reg[pos];

				reg[acc] = this;
				thread[acc-1] = this;
				this = R0; // mcp

				R0 = IFALSE; // remove mcp cont
				// R3 marks the interop to perform
				// 1 - runnig and time slice exhausted
				// 10: breaked - call signal handler
				// 14: memory limit was exceeded
				R3 = I(1); // breaked ? ((breaked & 8) ? I(14) : I(10)) : I(1); // fixme - handle also different signals via one handler
				R4 = (word) thread; // thread state
				R5 = I(0); // I(breaked); // сюда можно передать userdata из потока
				R6 = IFALSE;
				acc = 4; // вот эти 4 аргумента, что возвращаются из (run) после его завершения
				// breaked = 0;

				if (ol->idle)
					ol->idle(ol->userdata);

				// reapply new thread
				goto apply;
			}
		}

		ol->arity = acc; // reflect possibly changed arity into vm state

		// теперь проверим доступную память

		// приблизительно сколько памяти может потребоваться для одного эпплая?
		// теоретически, это можно вычислить проанализировав текущий контекст
		// а практически, пока поюзаем количество доступных регистров

		// если места в буфере не хватает, то мы вызываем GC,
		//	а чтобы автоматически подкорректировались регистры,
		//	мы их складываем в память во временный объект.
		if (fp >= heap->end) {
			heap->fp = fp; ol->this = this;
			heap->gc(ol, 0);
			fp = heap->fp; this = ol->this;

			// temporary removed:
			//word heapsize = (word)heap->end - (word)heap->begin;
			//if ((heapsize / (1024 * 1024)) >= ol->max_heap_size)
			//	breaked |= 8; // will be passed over to mcp at thread switch
		}

		ip0 = ip = (unsigned char *) &ref(this, 1);
		goto mainloop; // let's execute
	}

	// ff call ({} key def) -> def
	if (this == IEMPTY) {
		word continuation = R3;
        word key = R4;
		switch (acc) {
		case 2:
			ERROR(260, this, key); // key
			break;
		case 3:
			R3 = R5;           // default value
			break;
		default:
			ERROR(259, this, I(acc));
		}
		this = continuation;
		acc = 1;
		goto apply;
	}

	// done ?
	if (this == IEXIT) {
		// a thread or mcp is calling the final continuation
		this = R0;
		if (!is_reference(this))
			goto done; // expected exit

		R0 = IFALSE; // set mcp yes?
		R4 = R3;
		R3 = I(2);   // 2 = thread finished, look at (mcp-syscalls) in lang/threading.scm
		R5 = IFALSE;
		R6 = IFALSE;
//		breaked = 0;
		ticker = TICKS;// ?
		bank = 0;
		acc = 4;

		goto apply;
	}

	if (this == IRETURN) {
		// в reg[3] находится код возврата
		goto done;       // колбек закончен! надо просто выйти наверх
	}
	
	ERROR(261, this); // not callable

mainloop:;
	// ip - счетчик команд (опкод - младшие 6 бит команды, старшие 2 бита - модификатор(если есть) опкода)
	#	define MODD(i, n) ((i) + ((n)<<6))
	// Rn - регистр машины (reg[n])
	// An - регистр, на который ссылается операнд N (записанный в параметре n команды, начиная с 0)
	// todo: добавить в комменты к команде теоретическое количество тактов на операцию
	// todo: exchange NOP and APPLY operation codes

	// todo: add "HLT" function (may be 0x0 ?)
	// список команд смотреть в assembly.scm

	// безусловный переход
	#	define GOTO   2       // jmp a, nargs
	#	define CLOS   3

	// управляющие команды
	#	define NOP   21
	#	define APPLY 20
	#	  define APPLYCONT MODD(APPLY, 1)
	#	define RET   24
	#	define RUN   50
	#	define ARITY_ERROR 17
	#	define VMEXIT 37

	#	define MCP   27

	#	define JAF   11
	#	define JAX   12
	#	define LDI   13      // LDE (13), LDN (77), LDT (141), LDF (205)
	#	  define LDE MODD(LDI, 0)
	#	  define LDN MODD(LDI, 1)
	#	  define LDT MODD(LDI, 2)
	#	  define LDF MODD(LDI, 3)
	#	define LD    14

	#	define REFI   1      // refi a, p, t:   Rt = Ra[p], p unsigned (indirect-ref from-reg offset to-reg)
	#	define MOVE   9      //
	#	define MOV2   5      // optimization for MOVE + MOVE

	#	define JEQ    8      // jeq
	#	define JP    16      // JZ (16), JN (80), JT (144), JF (208)

	// примитивы языка:
	#	define VMNEW 23     // make a typed object (fast and simple)
	#	define VMMAKE 18    // make a typed object (slow, but smart)
	#		define VMALLOC MODD(VMMAKE, 1)  // alloc a memory region
	#	define VMCAST 22
	#	define VMSETE 43

	#	define VMPIN   35
	#	define VMUNPIN 60
	#	define VMDEREF 25

	#	define CONS  51

	#	define TYPE  15
	#	define SIZE  36

	#	define CAR   52
	#	define CDR   53
	#	define REF   47

		// ?
	#	define SETREF 10
	#	  define SETREFE MODD(SETREF, 1) // set-ref!

		// ?
	#	define EQQ   54
	#	define LESSQ 44

	#	define FP1 33
	#	define FP2 34

		// tuples, trees
	#	define VECTORAPPLY 32
	#	define FFAPPLY 49

	#	define FFLEAF    42 // make ff leaf
	#	define FFBLACK   FFLEAF
	#	define FFRED     MODD(FFLEAF, 1)
	#	define FFTOGGLE  46 // toggle ff leaf color
	#	define FFREDQ    41 // is ff leaf read?
	#	define FFRIGHTQ  MODD(FFREDQ, 1) // if ff leaf right?

		// ALU
	#	define ADDITION       38
	#	define DIVISION       26
	#	define MULTIPLICATION 39
	#	define SUBTRACTION    40
	#	define BINARY_AND     55
	#	define BINARY_IOR     56
	#	define BINARY_XOR     57
	#	define SHIFT_RIGHT    58
	#	define SHIFT_LEFT     59

	#	define CLOCK 61 // todo: remove and change to SYSCALL_GETTIMEOFDATE

	// operation SYStem interaction CALLs
	#	define SYSCALL 63
	#		define SYSCALL_SYSCALL ? // TODO: https://linux.die.net/man/2/syscall and remove redundant calls
			// read, write, open, close must exist
	#		define SYSCALL_READ 0    // 
	#		define SYSCALL_WRITE 1   // 
	#		define SYSCALL_OPEN 2    // 
	#		define SYSCALL_CLOSE 3   // 
	#		define SYSCALL_STAT 4    // same for fstat and lstat
	#		define SYSCALL_POLL 7    // 
	#		define SYSCALL_LSEEK 8   // 
	#		define SYSCALL_MMAP 9    // 
	#		define SYSCALL_FSYNC 74  //
	#		define SYSCALL_EXECVP 59 //
	#		define SYSCALL_WAITPID 61//
	// 5, 6 - free
	// 12 - reserved for memory functions
	// #		define SYSCALL_BRK 12
	// 14 - todo: set signal handling

	#		ifndef SYSCALL_IOCTL
	#		define SYSCALL_IOCTL 16
	#		endif//

	#		define SYSCALL_IOCTL_TIOCGETA 19

	#		define SYSCALL_PIPE 22
	#		define SYSCALL_DUP 32

	#		define SYSCALL_YIELD 24

	#		ifndef SYSCALL_SLEEP
	#		define SYSCALL_SLEEP 35
	#		endif//

    #		define SYSCALL_ERRNO 60  // errno
	#		define SYSCALL_GETDENTS 78

	#		define SYSCALL_SENDFILE 40

	#		ifndef SYSCALL_MEMFD
    #        ifdef HAVE_MEMFD_CREATE
	#		  define SYSCALL_MEMFD ((HAVE_MEMFD_CREATE == 1) ? 85 : 0)
    #        else
	#		  define SYSCALL_MEMFD 85
    #        endif
	#		endif

	#		define SYSCALL_GETTIMEOFDAY 96

	#		ifndef SYSCALL_GETRLIMIT
	#		define SYSCALL_GETRLIMIT 97
	#		endif
	#		ifndef SYSCALL_GETRUSAGE
	#		define SYSCALL_GETRUSAGE 98
	#		endif
	#		ifndef SYSCALL_SYSINFO
	#		define SYSCALL_SYSINFO 99
	#		endif

	#		ifndef SYSCALL_PRCTL
	#		define SYSCALL_PRCTL 157
	#		endif
	#		define SYSCALL_ARCHPRCTL 158
	#		define SYSCALL_TIME 201

	#		define SYSCALL_DLOPEN 174
	#		define SYSCALL_DLCLOSE 176
	#		define SYSCALL_DLSYM 177
	#		define SYSCALL_DLERROR 178

	// ENTRY LOOP POINT
	int op; //operation to execute
loop:;
	/*! ### OLVM Codes
	 * 
	 * | #o/8 | o0         | o1        | o2      | o3    | o4      | o5      | o6       | o7     |
	 * |:-----|:----------:|:---------:|:-------:|:-----:|:-------:|:-------:|:--------:|:------:|
	 * |**0o**| JIT        | REFI      | GOTO    | CLOS  |  -----  | MOV2    |  ------  |  ----  |
	 * |**1o**| JEQ        | MOVE      | set-ref*| JAF   | JAX     | LD*     | LD       | TYPE   |
	 * |**2o**| JP         |ARITY-ERROR| vm:make*|  ---  | APPLY   | NOP     | CAST     | NEW    |
	 * |**3o**| RET        | DEREF     | DIV     | MCP   | VERSION | FEATURES| VMAX     | VSIZE  |
	 * |**4o**|VECTOR-APPLY| FP1       | FP2     | PIN   | SIZE    | EXIT    | ADD      | MUL    |
	 * |**5o**| SUB        | FF:RED?   | FF:BLACK| SET!  | LESS?   |  -----  | FF:TOGGLE| REF    |
	 * |**6o**|  --------  | FF:APPLY  | RUN     | CONS  | CAR     | CDR     | EQ?      | AND    |
	 * |**7o**| IOR        | XOR       | SHR     | SHL   | UNPIN   | CLOCK   |  ------  | SYSCALL|
	 * 
	 * * set-ref*: `set-ref`, `set-ref!`
	 * * vm:make*: `vm:make`, `vm:alloc`
	 * * LD*: `LDE`, `LDN`, `LDT`, `LDF`
	 */
#ifdef DEBUG_COUNT_OPS
    ops[*ip]++;
#endif

	switch ((op = *ip++) & 0x3F) {
	/*! #### JIT
	 * Reserved for feature use.
	 *
	 * Throws "Invalid opcode" error.
	 */
	case 0:
		op = (ip[0] << 8) | ip[1]; // big endian
		// super_dispatch: run user instructions
		switch (op) {
		/* AUTOGENERATED INSTRUCTIONS */
		// TODO: JIT!
		//	https://gcc.gnu.org/onlinedocs/gcc-5.1.0/jit/intro/tutorial04.html
		default:
			CRASH(0, I(op));
		}
		break;

	/*! #### Unused numbers
	 * Reserved for feature use.
	 * 
	 * Throws "Invalid opcode" error.
	 */
	default:
		CRASH(0, I(op));
		break;

	/*! #### NOP
	 * No OPeration
	 * `21`
	 */
	case NOP:
		break;

	/*! #### ARITY_ERROR
	 * Arity Error
	 */
	case ARITY_ERROR: // (0%)
		ERROR(ARITY_ERROR, this, I(acc));
		break;

	/*! #### GOTO
	 * GO TO procedure, arity
	 */
	case GOTO: { // (10%)
		this = A0;
		acc = ip[1];
		goto apply;
	}

	/*! #### RET
	 * RETurn from procedure
	 */
	case RET: { // (3%) return value
		this = R3;
		R3 = A0; acc = 1;
		goto apply;
	}

	/*! #### APPLY
	 */
	// todo:? include apply-tuple, apply-values? and apply-ff to the APPLY
	case APPLY: { // (0%)
		int r = 4;   // normal apply: cont=r3, func=r4, a0=r5,
		int arity = 1;
		acc -= 3; // ignore cont, function and stop before last one (the list)

		if (op == APPLYCONT) {    // (_sans_cps apply): func=r3, a0=r4
			r = 3;
			arity = 0;
			acc += 1;
		}

		if (acc < 0)
			ERROR(APPLY, I(0));
		this = reg[r];

		// copy args down
		while (acc--) { // move explicitly given arguments down by one to correct positions
			reg[r] = reg[r+1];
			r++;
			arity++;
		}
		word *lst = (word *) reg[r+1];

		while (is_pair(lst)) { // unwind argument list
			if (r > NR)
				ERROR(APPLY, I(r));
			reg[r++] = car (lst);
			lst = (word *) cdr(lst);
			arity++;
		}
		acc = arity;

		goto apply;
	}

	/*! #### MCP
	 */
	// do mcp operation with continuation
	case MCP: { // (1%) sys continuation op arg1 arg2
		this = R0;
		R0 = IFALSE; // let's call mcp
		R3 = A1; R4 = A0; R5 = A2; R6 = A3;
		acc = 4;
		if (ticker > 10)
			bank = ticker; // deposit remaining ticks for return to thread
		ticker = TICKS;

		goto apply;
	}

	/*! #### RUN
	 */
	case RUN: { // (1%) run thunk quantum
	// the type of quantum is ignored, for now.
	// todo: add quantum type checking
	//			if (ip[0] != 4 || ip[1] != 5)
	//				STDERR("run reg[%d], reg[%d]", ip[0], ip[1]);
		this = A0;
		R0 = R3;
		ticker = bank ? bank : (int) value(A1);
		bank = 0;
		if (!is_reference(this))
			CRASH(RUN, this);

		word hdr = ref(this, 0);
		if (value_type (hdr) == TTHREAD) {
			int pos = object_size(hdr) - 1;
			word code = ref(this, pos);
			acc = pos - 3;
			while (--pos)
				reg[pos] = ref(this, pos);
			ip0 = ip = (unsigned char *) &car(code);
			break;  // no apply, continue
		}
		// else call a thunk with terminal continuation
		R3 = IEXIT;  // exit via R0 when the time comes
		acc = 1;

		goto apply;
	}

	/************************************************************************************/
	// операции с данными
	//	смотреть "vm-instructions" в "lang/assembly.scm"

	/*! #### LDI r (LDE, LDN, LDT, LDF)
	 * - LDE Store `#empty` into register `r`
	 * - LDN Store `#null` into register `r`
	 * - LDT Store `#true` into register `r`
	 * - LDF Store `#false` into register `r`
	 */
	case LDI: {  // (1%) 13,  -> ldi(lde, ldn, ldt, ldf){2bit what} [to]
		static
		const word I[] = { IEMPTY, INULL, ITRUE, IFALSE };
		A0 = I[op>>6];
		ip += 1; break;
	}
	/*! #### LD b r
	 * Create `enum` from `b` binary value (0..255) and store it into register `r`
	 */
	case LD: // (5%)
		A1 = I(ip[0]); // I(ip[0]) -> reg[ip[1]]
		ip += 2; break;


	/*! #### REFI a p t
	 * Rt = (ref reg[a] reg[p]), p is unsinged
	 */
	case REFI: { // (24%)
		word* Ra = (word*)A0; A2 = Ra[ip[1]]; // A2 = A0[p]
		ip += 3; break;
	}
	/*! #### MOVE a t
	 * Rt = Ra
	 */
	case MOVE: // (3%)
		A1 = A0;
		ip += 2; break;
	/*! #### MOV2 a1 t1 a2 t2
	 * Rt1 = Ra1,
	 * Rt2 = Ra2
	 */
	case MOV2: // (6%) mov2 from1 to1 from2 to2
		A1 = A0;
		A3 = A2;
		ip += 4; break;
	// todo: add MOV3?

	// условные переходы
	/*! #### JEQ a b o
	 * Jump to ip+o if a == b, o is a two-byte binary value
	 */
	case JEQ: // (5%) jeq a b o, extended jump
		if (A0 == A1) // 30% for "yes"
			ip += (ip[3] << 8) + ip[2]; // little-endian
		ip += 4; break;

	/*! #### JP a o (JZ, JN, JT, JF)
	 * - JZ, jump if a == 0
	 * - JN, jump if a == #null
	 * - JE, jump if a == #empty
	 * - JF, jump if a == #false
	 */
	case JP: { // (10%) JZ, JN, JE, JF a hi lo
		static
		const word I[] = { I(0), INULL, IEMPTY, IFALSE };
		if (A0 == I[op>>6]) // 49% for "yes"
			ip += (ip[2] << 8) + ip[1]; // little-endian
		ip += 3; break;
	}

	// (13%) for JAF and JAX
	// NOTE: Don't combine JAF and JAX for the gods of speed
	case JAF: {
		long arity = ip[0];
		if (acc != arity)
			ip += (ip[1] << 8) | ip[2];

		ip += 3; break;
	}

	// JAF eXtended, additionally packs extra arguments list
	case JAX: {
		long arity = ip[0];
		if (acc >= arity) {
			word tail = INULL;
			while (acc > arity) {
				tail = (word) cons(reg[acc + 2], tail);
				acc--;
			}
			reg[acc + 3] = tail;
		}
		else
			ip += (ip[1] << 8) | ip[2];

		ip += 3; break;
	}

	case CLOS:
	{	// CLOS type size r i a1 a2 a3 a4 ...
		word type = *ip++;
		word size = *ip++;
		word *T = new (type, size-1);

		word vec = reg[*ip++];
		T[1] = ((word *) vec)[*ip++]; // (ref reg[r] i)

		for (size_t i = 2; i < size; )
			T[i++] = reg[*ip++];
		reg[*ip++] = (word) T; // reg[ret] = T
		break;
	}

	/************************************************************************************/
	// более высокоуровневые конструкции
	//	смотреть "owl/primop.scm" и "lang/assemble.scm"

	// make object (fastest choice)
	//	проверка размеров тут не нужна, так как в любом случае количество аргументов,
	//	передаваемых в функцию не превысит предусмотренных пределов
	case VMNEW: { // new t f1 .. fs r
		// vm:new is a SPECIAL operation with different arguments order
		word type = *ip++;
		word size = *ip++ + 1; // the argument is n-1 to allow making a 255-tuple with 255, and avoid 0 length objects
		word *p = new (type, size), i = 0; // s fields + header
		while (i < size) {
			p[i+1] = reg[*ip++];
			i++;
		}
		reg[*ip++] = (word) p;
		break;
	}

	// make typed reference from list
	// (vm:make type list-of-values) ; length is list length
	// (vm:make type length) ; default value is equal to #false
	// (vm:make type length default-value)
	case VMMAKE: { // and VMALLOC
	 	word size = *ip++;
		word type = value (A0) & 63; // maybe better add type checking? todo: add and measure time
		word value = A1;

		word el = IFALSE;
		switch (size) {
			case 3:
				el = A2;
				// fall through
			case 2: {
				unsigned len = 0;
				word list = value;
				if (is_numberp(value))
					len = numberp(value);
				else
				while (is_pair(list)) {
					++len;
					list = cdr(list);
				}

				// эта проверка необходима, так как действительно можно
				//	выйти за пределы кучи (репродюсится стабильно)
				int mult = (op == VMALLOC) ? sizeof(word) : 1;
				if (fp + (len / mult) > heap->end) {
					GC(len / mult);
					
					value = A1, el = A2; // reload values after possible gc

					// fail, no memory available:
					if (fp + (len / mult) > heap->end)
						CRASH(op, sandboxp ? ITRUE : IFALSE, I(size));
				}

				word *ptr = (op == VMMAKE)
					? new (type, len)
					: new_alloc(type, len);
				reg[ip[size]] = (word)ptr;

				if (is_numberp(value)) { // no list, just
					if (op == VMMAKE) {
						word* wp = ptr;
						for (unsigned i = 0; i < len; i++)
							*++wp = el;
					}
					else { // VMALLOC
						unsigned char* wp = (unsigned char*)&ptr[1];
						size_t elnum = (size_t) value(el);
						for (unsigned i = 0; i < len; i++)
							*wp++ = (unsigned char) elnum;
						// clear the padding bytes, don't remove!
						// actually not required, but sometimes very useful!
						while ((word)wp % sizeof(word))
							*wp++ = 0;
					}
				}
				else
				if ((is_pair(value)) || (value == INULL)) {
					if (op == VMMAKE) {
						word* wp = ptr;
						while (value != INULL) {
							*++wp = car (value);
							value = cdr (value);
						}
					}
					else { // VMALLOC
						unsigned char* wp = (unsigned char*)&ptr[1];
						while (is_pair(value) && len--) {
							*wp++ = value(car(value)) & 255;
							value = cdr (value);
						}
						// clear the padding bytes, don't remove!
						while (len-- > 0) *wp++ = 0;
						while ((word)wp % sizeof(word))
							*wp++ = 0;
					}
					assert (value == INULL);
				}
				else {
					// invalid parameters
					reg[ip[size]] = IFALSE;
				}
				break;
			}
			default: fail:
				ERROR(op, this, I(size));
		}

	 	ip += size + 1; break;
	}


	// операции посложнее
	case CONS:   // cons a b -> r : Rr = (cons Ra Rb)
		A2 = (word) cons(A0, A1); // видимо, вызывается очень часто, так как замена на макрос дает +10% к скорости
		ip += 3; break;


	case TYPE: { // type o -> r
		word T = A0;
		A1 = I(is_reference(T) ? reference_type(T) : value_type(T));
		ip += 2; break;
	}

	case SIZE: { // size o -> r
		word* T = (word*)A0;
		if (is_reference(T)) {
			if (is_rawstream(T))
				A1 = I(rawstream_size(T));
			else
				A1 = I(reference_size(T));
		}
		else
			A1 = IFALSE;
		ip += 2; break;
	}

	// todo: опасные вещи вроде int->port и int->void* спрятать под SAFE блок
	case VMCAST: { // cast obj type -> result
		word T = A0;
		word a1 = A1;
		A2 = IFALSE;

		if (!is_value(a1))
			ERROR(VMCAST, T, a1);
		word type = value(a1) & 0x3F;

		switch (type) {
		case TPORT:
			if (is_value(T)) {
				word val = value(T);
				// only stdin/stdout/stderr are supported
				switch (val) {
					case 0: A2 = make_port(STDIN_FILENO);
						break;
					case 1: A2 = make_port(STDOUT_FILENO);
						break;
					case 2: A2 = make_port(STDERR_FILENO);
						break;
					default:
						goto error_cast;
				}
			}
			else
				goto error_cast;
			break;
		case TVPTR:
#if OLVM_UNSAFES && OLVM_UNSAFE_TVPTR_CAST
			if (is_integer(T))
				A2 = (word) new_vptr(unumber(T));
#else
			if (T == I(0) || T == I(1))
				A2 = (word) new_vptr(value(T));
#endif
			// TODO: add macro to enable this check:
			else
				goto error_cast;
			break;

#if OLVM_INEXACTS
		case TINEXACT:
			// exact->inexact
			A2 = (word) new_inexact(ol2f(T));
			break;
		case TRATIONAL:
			// inexact->exact
			if (is_reference(T) && reference_type(T) == TINEXACT) {
				inexact_t v = *(inexact_t*)&car(T);

				ol->heap.fp = fp;
				A2 = d2ol(&ol->heap, v); // no GC expected
				fp = ol->heap.fp;
				break;
			}
			if (is_number(T)) // just copy a number to number
				type = is_value(T) ? value_type(T) : reference_type(T);
#endif
			// fall through
		default:
			if (is_value(T)) {
				word val = value(T);
				A2 = make_value(type, val);
			}
			else
			{	// make this code safe
				// make an object clone with new type
				word* ob = (word*)T;
				word hdr = *ob++;
				int size = object_size(hdr)-1; // (-1) for header
				word *newobj = new (size);
				word *res = newobj;
				*newobj++ = (hdr & (~252)) | (type << TPOS);
				wordcopy(ob, newobj, size);
				A2 = (word)res;
			}
			break;
		}
		ip += 3; break;
		error_cast:
			ERROR(VMCAST, T, I(type));
	}

	// todo: reference objects (with check for 'less?')
	// * experimental feature, do not use!
	// (vm:set! dst src)
	// (vm:set! dst to src start end)
	case VMSETE: {
		word argc = *ip++;

		char *p = (char*)A0; // dst
		word setok = IFALSE;

		// word to, start, end;
		if (is_reference(p)) {
			word to    = (argc == 5) ? value (A1) : 0;
			char* q    = (argc == 5) ?(char*) A2  : (argc == 2) ?(char*) A1 : 0; // src
			word start = (argc == 5) ? value (A3) : 0;
			word end   = (argc == 5) ? value (A4) : (argc == 2 && is_reference(q)) ?
													rawstream_size(q) : 0;

			if (is_reference(q)
				&& end <= rawstream_size(q)
				&& end - start + to <= rawstream_size(p))
			{
				if (end - start == W) // speedup for inexact numbers and simple objects
					*(word*)(&p[W + to]) = *(word*)(&q[W + start]);
				else
					memcpy(&p[W + to], &q[W + start], end - start);
				setok = (word) p;
			}
		}

		reg[ip[argc]] = setok;
		ip += argc + 1; break;
		// A5 = result;
		// ip += 6; break;
	}

	// speed version of (ref a 1)
	case CAR: {  // car a -> r
		word T = A0;
		ASSERT(CHECKCAR(T), CAR, T);
		A1 = car(T);//((word*)T)[1];
		ip += 2; break;
	}

	// speed version of (ref a 2)
	case CDR: {  // cdr a -> r
		word T = A0;
		ASSERT(CHECKCDR(T), CDR, T);
		A1 = cdr(T);//((word*)T)[2];
		ip += 2; break;
	}

	// general (ref a n)
	// works with objects and blobs both
	// objects referenced from 1, blobs from 0
	case REF: {  // ref t o -> r
		word *p = (word *) A0;
		if (is_reference(p) && is_enum(A1)) {
			word hdr = *p;
			if (is_rawstream(p)) {
				word size = rawstream_size(p);
				word pos = is_enump (A1) ? (value(A1)) : (size - value(A1));
				if (pos < size) // blobs are indexed from 0
					A2 = I(((unsigned char *) p)[pos+W]);
				else
					A2 = IFALSE;
			}
			else {
				word size = object_size(hdr);
				word pos = is_enump (A1) ? (value(A1)) : (size - value(A1));
				if (pos && pos < size) // objects are indexed from 1
					A2 = p[pos];
				else
					A2 = IFALSE;
			}
		}
		else
			A2 = IFALSE;
		ip += 3; break;
	}

	// if position out of bounds will return unchanged object
	case SETREF: {  // (set-ref object position value), position starts from 1 to objects and 0 to blobs
					// (set-ref! object position value), position starts from 1 to objects and 0 to blobs
		word *p = (word *)A0;
		word result = IFALSE;

		if (is_reference(p) && is_enum(A1)) {
			word hdr = *p;
			word size = object_size (hdr) - 1; // -1 for header
			word *newobj;
			if (op == SETREF) { // __builtin_expect((x),1)
				newobj = new (size);
				for (size_t i = 0; i <= size; i++)
					newobj[i] = p[i];
			}
			else
				newobj = p;
			result = (word)newobj;

			if (is_rawstream(p)) {
				ASSERT(is_enump(A2), A2, 10001) // ?
				size = size * sizeof(word) - header_pads(hdr);
				word pos = is_enump (A1) ? (value(A1)) : (size - value(A1));
				if (pos < size) // will add [0..255] numbers
					((unsigned char*)&car(newobj))[pos] = enum(A2) & 0xFF;
			}
			else {
				++size;
				word pos = is_enump (A1) ? (value(A1)) : (size - value(A1));
				if (pos && pos < size) // objects are indexed from 1
					newobj[pos] = A2;
			}
			A3 = result;
		}
		else
			A3 = IFALSE;
		ip += 4; break; }

	case EQQ: // (eq? a b)
		A2 = (A0 == A1) ? ITRUE : IFALSE;
		ip += 3; break;

	case LESSQ: { // (less? a b)
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
		A2 = I(r & VMAX); // speedup: A2 = I(r);
		A3 = (r & HIGHBIT) ? ITRUE : IFALSE;
		ip += 4; break; }
	case SUBTRACTION: { // vm:sub a b  r u, args prechecked, signs ignored
		word r = (value(A0) | HIGHBIT) - value(A1);
		A2 = I(r & VMAX); // speedup: A2 = I(r);
		A3 = (r & HIGHBIT) ? IFALSE : ITRUE;
		ip += 4; break; }

	case MULTIPLICATION: { // vm:mul a b l h
		big_t r = (big_t) value(A0) * (big_t) value(A1);
		A2 = I(r & VMAX);
		A3 = I(r>>VBITS);
		ip += 4; break; }
	case DIVISION: { // vm:div ah al b  qh ql r, b != 0, int64(32) / int32(16) -> int64(32), as enums
		big_t a = (big_t) value(A1) | (((big_t) value(A0)) << VBITS);
		big_t b = (big_t) value(A2);

		// http://stackoverflow.com/questions/7070346/c-best-way-to-get-integer-division-and-remainder
		big_t q = a / b;
		big_t r = a % b;

		A3 = I(q>>VBITS);
		A4 = I(q & VMAX);
		A5 = I(r);

		ip += 6; break; }


	case BINARY_AND: // vm:and a b r, prechecked
		A2 = (A0 & A1);
		ip += 3; break;
	case BINARY_IOR: // vm:ior a b r, prechecked
		A2 = (A0 | A1);
		ip += 3; break;
	case BINARY_XOR: // vm:xor a b r, prechecked
		A2 = (A0 ^ (A1 & (VMAX << VPOS))); // inherit a's type info
		ip += 3; break;

	case SHIFT_RIGHT: { // vm:shr a b hi lo
		big_t r = ((big_t) value(A0)) << (VBITS - value(A1));
		A2 = I(r>>VBITS);
		A3 = I(r & VMAX);
		ip += 4; break; }
	case SHIFT_LEFT: { // vm:shl a b hi lo
		big_t r = ((big_t) value(A0)) << (        value(A1));
		A2 = I(r>>VBITS);
		A3 = I(r & VMAX);
		ip += 4; break; }


	case 28: // (vm:version)
		A0 = (word) cons(
				new_string(__OLVM_NAME__,    sizeof(__OLVM_NAME__) - 1),
				new_string(__OLVM_VERSION__, sizeof(__OLVM_VERSION__) - 1));
		ip += 1; break;
	case 29: // (vm:features)
		A0 = I(0
		// general build options
		#ifndef REPL
			| 000000001
		#endif
		#if OLVM_NOMAIN
			| 000000002
		#endif
		#if OLVM_FFI
			| 000000004
		#endif
		#if OLVM_CALLABLES
			| 000000010
		#endif
		#if OLVM_INEXACTS
			| 000000020
		#endif
		#if OLVM_BUILTIN_FMATH
			| 000000040
		#endif
		#if OLVM_UNSAFES
			| 000000100
		#endif
		// syscalls
		#if SYSCALL_SYSINFO
			| 000000200
		#endif
        #if SYSCALL_MEMFD
			| 000000400
        #endif
		#if SYSCALL_GETRLIMIT
			| 000001000
		#endif
		#if SYSCALL_GETRUSAGE
			| 000002000
		#endif
		// os
		#if HAVE_DLOPEN
			| 000010000
		#endif
		#if HAVE_SOCKETS
			| 000020000
		#endif
		//	| 000040000 // reserved
		#if HAVE_SANDBOX
			| 000100000
		#endif
		#if HAVE_STRFTIME
			| 000200000
		#endif
		#if HAVE_SENDFILE
			| 000400000
		#endif
		// posix
		#if HAVE_UNISTD_H
			| 001000000
		#endif
		);
		ip += 1; break;
	case 30: // (vm:vmax)
		A0 = I(VMAX);
		ip += 1; break;
	case 31: // (vm:vsize)
		A0 = I(VBITS);
		ip += 1; break;

	// bind vector to registers
	case VECTORAPPLY: { /* bind <vector > <n> <r0> .. <rn> */
		word *vector = (word *) reg[*ip++];
		ASSERT(is_reference(vector), vector, I(10101));

		word pos = 1, n = *ip++;
		//word hdr = *tuple;
		//CHECK(!(is_raw(hdr) || object_size(hdr)-1 != n), vector, BIND);
		while (n--)
			reg[*ip++] = vector[pos++];

		break;
	}

	/* ff's ---------------------------------------------------
	 *
	 */
	// bind ff to registers
	case FFAPPLY: { // ff:bind <node >l k v r   - bind node left key val right, filling in #false when implicit
		word *ff = (word *) A0;
		word hdr = *ff++;
		A2 = *ff++; // key
		A3 = *ff++; // value
		switch (object_size(hdr)) {
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
	case FFLEAF: { // {ff:black|ff:red} l k v r t
		word t = (op == FFRED ? TRED : 0) |TFF;
		word l = A0;
		word r = A3;

		word *me;
		if (l == IEMPTY) {
			if (r == IEMPTY)
				me = new (t, 2);
			else {
				me = new (t|TRIGHT, 3);
				me[3] = r;
			}
		}
		else
		if (r == IEMPTY) {
			me = new (t, 3);
			me[3] = l;
		}
		else {
			me = new (t, 4);
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
		*p++ = (h ^ (TRED << TPOS));
		switch (object_size(h)) {
			case 5:  *p++ = *node++; // fall through
			case 4:  *p++ = *node++; // fall through
			default: *p++ = *node++;
					 *p++ = *node++; // fall through
		}
		fp = (word*) p;
		ip += 2; break;
	}

	// is node red/right predicate?
	case FFREDQ: { // ff:red?|ff:right? node r
		word t = (op == FFREDQ ? TRED : TRIGHT);
		word node = A0;
		if (is_reference(node)) // assert to IEMPTY || is_reference() ?
			node = *(word*)node;
		A1 = ((value_type (node) & (0x3C | t)) == (t|TFF)) ? ITRUE : IFALSE;
		ip += 2; break;
	}

	// ---------------------------------------------------------
	case CLOCK: { // clock <secs> <ticks>
		struct timeval tp;
		gettimeofday(&tp, NULL);

		A0 = (word) new_number (tp.tv_sec);
		A1 = (word) new_number (tp.tv_usec / 1000);
		ip += 2; break;
	}

	/*! - - -
	 */

	/*! ### OLVM Syscalls
	 * `(syscall number ...) -> val|ref`
	 *
	 * A system call is the way in which an Ol requests a service
	 * from the operating system on which it is executed.
	 *
	 */
	// http://docs.cs.up.ac.za/programming/asm/derick_tut/syscalls.html (32-bit)
	// https://filippo.io/linux-syscall-table/
	case SYSCALL: {
		word argc = *ip++;
		if (argc == 0)
			ERROR(62000, I(0), I(1));
		--argc; // skip syscall number
		word* r = (R) IFALSE;  // by default returning #false

		// -----------------------------------------------------------------------------------
		// arguments count checking macro
		#define CHECK_ARGC_EQ(n)  if (argc - n) ERROR(62000, I(argc), I(n)); // === (arg != n)
		#define CHECK_ARGC(a, b)  if (argc < a) ERROR(62000, I(argc), I(a))\
                             else if (argc > b) ERROR(62000, I(argc), I(b));
		// arguments type checking macro
		#define CHECK_TYPE(arg, type, error) if (argc >= arg) if (!is_##type(A##arg)) ERROR(error, I(arg), A##arg)
		#define CHECK_TYPE_OR_FALSE(arg, type, error) \
		                                     if (argc >= arg) if (!is_##type(A##arg) && !(A##arg == IFALSE)) ERROR(error, I(arg), A##arg)
		#define CHECK_TYPE_OR_TYPE2(arg, type, type2, error) \
		                                     if (argc >= arg) if (!is_##type(A##arg) && !is_##type2(A##arg)) ERROR(error, I(arg), A##arg)

		#define CHECK_TRUE_OR_FALSE(arg)     if (argc >= arg) if (A##arg != ITRUE && A##arg != IFALSE) ERROR(62008, I(arg), A##arg)

		#define CHECK_PORT(arg)      CHECK_TYPE(arg, port, 62001)
		#define CHECK_NUMBER(arg)    CHECK_TYPE(arg, number, 62002)
		#define CHECK_NUMBERP(arg)   CHECK_TYPE(arg, numberp, 62003)
		#define CHECK_REFERENCE(arg) CHECK_TYPE(arg, reference, 62004)
		#define CHECK_RAWSTREAM(arg) CHECK_TYPE(arg, rawstream, 62005)
		#define CHECK_STRING(arg)    CHECK_TYPE(arg, string, 62006)
		#define CHECK_VPTR(arg)      CHECK_TYPE(arg, vptr, 62007)
		#define CHECK_BOOLEAN(arg)   CHECK_TYPE(arg, boolean, 62008)

		#define CHECK_NUMBER_OR_FALSE(arg)  CHECK_TYPE_OR_FALSE(arg, number, 62020)
		#define CHECK_NUMBERP_OR_FALSE(arg) CHECK_TYPE_OR_FALSE(arg, numberp, 62030)
		#define CHECK_PORT_OR_STRING(arg)   CHECK_TYPE_OR_TYPE2(arg, port, string, 62016)
		#define CHECK_VPTR_OR_STRING(arg)   CHECK_TYPE_OR_TYPE2(arg, vptr, string, 62076)
		#define CHECK_NUMBER_OR_STRING(arg) CHECK_TYPE_OR_TYPE2(arg, number, string, 62026)
		#define CHECK_STRING_OR_FALSE(arg)  CHECK_TYPE_OR_FALSE(arg, string, 62060)

		// ------------------------------------------------------------------------------

		// continue syscall handler:
		CHECK_NUMBER(0);
		size_t op = value (A0);

		switch (op + sandboxp) {

			// I/O FUNCTIONS

			/*! #### READ
			* * `(syscall 0 port) -> bytevector | #t | #eof`
            * * `(syscall 0 port count) -> bytevector | #t | #eof`
			*
			* Attempts to read up to *count* bytes from input port *port*
			* into the bytevector.
			*
			* - *port*: input port
			* - *count*: count (negative value means "all available")
			*
			* Return:
			* - *bytevector*, if success
			* - *#true*, if file not ready
			* - *#eof*, if file was ended
			*
			* http://man7.org/linux/man-pages/man2/read.2.html
			*/
			case SYSCALL_READ + SECCOMP:
			case SYSCALL_READ: { //
				CHECK_ARGC(1, 2);
				CHECK_PORT(1);
				CHECK_NUMBER_OR_FALSE(2);

				int portfd = port(A1);
				int count = (argc > 1 && A2 != IFALSE)
									? (int) number(A2) : -1; // in bytes

				if (count < 0) {
#if defined(FIONREAD) && !defined(_WIN32)
					if (ioctl(portfd, FIONREAD, count) == -1)
#endif
						// сколько есть свободного места, столько читаем
						count = ((heap->end - fp) - 1) * sizeof(word);
				}

				unsigned words = WALIGN(count) + 1; // in words
				if (fp + words > heap->end) {
					GC(words);
				}

				int read;
				read = ol->read(portfd, (char *) &fp[1], count, ol->userdata);
				int err = errno;

				if (read > 0)
					r = new_bytevector(read);
				else if (read == 0)
					r = (R) IEOF;
				else if (err == EAGAIN) { // (may be the same value as EWOULDBLOCK) (POSIX.1)
					#ifdef __EMSCRIPTEN__
					emscripten_sleep(500);
					#endif
					r = (R) ITRUE;
				}

				break;
			}

			/*! #### WRITE
			* * `(syscall 1 port object) -> number | #false`
			* * `(syscall 1 port object count) -> number | #false`
			*
			* Writes up to *count* bytes from the binary *object* to the output port *port*.
			*
			* - *port*: output port,
			* - *object*: binary object data to write,
			* - *count*: count bytes to write (negative value means "whole object", default is object size)
			*
			* Return:
			* - *count of written data bytes*, if success
			* - *0*, if file busy
			* - *#false*, if error
			*
			* http://man7.org/linux/man-pages/man2/write.2.html
			*/
			case SYSCALL_WRITE + SECCOMP:
			case SYSCALL_WRITE: {
				CHECK_ARGC(2, 3);
				CHECK_PORT(1);
				CHECK_RAWSTREAM(2); // we write only binary objects
				CHECK_NUMBER_OR_FALSE(3);

				int portfd = port(A1);
				int count = (argc > 2 && A3 != IFALSE)
									? (int) number(A3) : -1; // в байтах

				word *buff = (word *) A2;
				int length = rawstream_size(buff);
				if (count > length || count < 0)
					count = length;

				int wrote;
				wrote = ol->write(portfd, (char*)&buff[1], count, ol->userdata);

				if (wrote > 0)
					r = new_number (wrote);
				else
				if (errno == EAGAIN || errno == EWOULDBLOCK)
					r = (R) I(0);

				break;
			}

			/*! #### OPEN
			* * `(syscall **2** pathname mode) -> port | #false`
			* * `(syscall **2** pathname mode blocking?) -> port | #false`
			* * `(syscall **2** pathname mode blocking? flags) -> port | #false`
			*
			* Open port to the file specified by *pathname* (and, possibly,
			*  create) using specified *mode*, *blocking?* and *flags*.
			*
			* - *pathname*: filename with/without path as c-like string(!)
			*   - Note: use `(c-string "filename.ext")` function or just `"filename.ext\0"`
			* - *mode*: open mode (0 for read, #o100 for write, for example)
			* - *blocking?*: blocking mode (default is non-blocking)
			* - *flags*: additional flags in POSIX sence
			*
			* Return:
			* - *port*, if success
			* - *#false*, if error
			*
			* http://man7.org/linux/man-pages/man2/open.2.html
			*/
			case SYSCALL_OPEN: {
				CHECK_ARGC(2, 4);
				CHECK_STRING(1); // pathname
				CHECK_NUMBER(2); // mode
				CHECK_TRUE_OR_FALSE(3);   // blocking
				CHECK_NUMBER_OR_FALSE(4); // flags

				char* s = string(A1);
				int f = number(A2);
				int flg = (f & 01000 ? O_CREAT : 0)
				        | (f & 00100 ? O_TRUNC : 0)
				        | (f & 00002 ? O_RDWR  : 0)
				        | (f & 0100000 ? O_BINARY : 0);
				int blocking = (argc > 2 && A3 == ITRUE) ? 1 : 0; // todo: A3 != IFALSE
				int mode =     (argc > 3 && A4 != IFALSE)
			 	    ? number(A4)
				    : S_IRUSR | S_IWUSR;

				int file = ol->open(s, flg, mode, ol);
				if (file == -1)
					break;

#ifndef __ANDROID__
				struct stat sb; // do not open directories
				if (fstat(file, &sb) < 0 || S_ISDIR(sb.st_mode)) {
					close(file);
					break;
				}
#endif

				// regular file? (id less than VMAX, then we return port as value)
				if ((unsigned long)file <= VMAX) {
					if (!blocking)
					    set_blocking(file, 0); // and set "non-blocking" mode
					r = (R) make_port(file);
				}
				else // port as reference
					r = new_port((unsigned long)file);

				break;
			}

			/*! #### CLOSE
			* `(syscall **3** port) -> #true | #false`
			*
			* Closes a port, so that it no longer refers to any file.
			*
			* - *port*, valid port
			*
			* Return:
			* - *#true*, if success
			* - *#false*, if error
			*
			* http://man7.org/linux/man-pages/man2/close.2.html
			*/
			case SYSCALL_CLOSE: {
				CHECK_ARGC_EQ(1);
				CHECK_PORT(1);

				int portfd = port(A1);

				if (ol->close(portfd, ol) == 0)
					r = (R)ITRUE;
#if defined(_WIN32) && HAVE_SOCKETS
				// Win32 socket workaround
				else if (errno == EBADF) {
					if (closesocket(portfd) == 0)
						r = (R)ITRUE;
				}
#endif
				break;
			}

			/*! #### LSEEK
			* `(syscall **8** port offset whence) -> offset | #f`
			*
			* Repositions the file offset of the *port* to the *offset* according to the directive *whence*.
			*
			* - *port*: valid port
			* - *offset*: offset
			* - *whence*:
			*   - 0 - from beginning of file,
			*   - 1 - from current file position,
			*   - 2 - from end of file (in negative direction)
			*
			* Return:
			* - *resulting offset location as measured in bytes
			*         from the beginning of the file*, if success
			* - *#false*, if error
			*
			* http://man7.org/linux/man-pages/man2/lseek.2.html
			*/
			case SYSCALL_LSEEK + SECCOMP:
			case SYSCALL_LSEEK: { // TODO: add to tests!
				CHECK_ARGC_EQ(3);
				CHECK_PORT(1);
				CHECK_NUMBER(2);
				CHECK_NUMBER(3);

				int whence = number(A3);
				off_t offset = lseek(port(A1), number(A2),
					whence == 0 ? SEEK_SET :
					whence == 1 ? SEEK_CUR :
					whence == 2 ? SEEK_END : 0);

				if (offset >= 0)
					r = new_number(offset);
				break;
			}

			/*! #### STAT
			* `(syscall **4** port/path) -> #(. stats .) | #false`
			*
			* Returns information about a file or port.
			*
			* - *port/path*: valid port of filename
			*
			* Return:
			* - *vector*, if success  
			*   (vector
            *     - ID of device containing file
			*     - Inode number
			*     - File type and mode
			*     - Number of hard links
			*     - User ID of owner
			*     - Group ID of owner
			*     - Device ID (if special file)
			*     - **Total size, in bytes**
			*     - #false, reserved
			*     - #false, reserved
			*     - Time of last access
			*     - Time of last modification
			*     - Time of last status change
			*   )
			* - *#false*, if error
			*
			* Note:
			* - Use `(ref (syscall 4 file) 8)` to get the file size
			*
			* http://man7.org/linux/man-pages/man2/stat.2.html
			*/
			case SYSCALL_STAT: {
				CHECK_ARGC(1,2);
				CHECK_PORT_OR_STRING(1);
				CHECK_BOOLEAN(2);

				struct stat st;

				if (is_port(A1)) {
					if (fstat(port(A1), &st) < 0)
						break;
				}
				else
				if (is_string(A1)) {
					// if (((argc == 2 && A2 == ITRUE) ? lstat : ol->stat)(string(A1), &st, this) < 0)
					// 	break;
					if (argc == 2 && A2 == ITRUE) {
						if (lstat(string(A1), &st) < 0)
							break;
					}
					else {
						 if (ol->stat(string(A1), &st, (void*)this) < 0)
						 	break;
					}
				}
				else
					break;

				r = new_vector(
						new_number(st.st_dev),    // устройство
						new_number(st.st_ino),    // inode
						new_number(st.st_mode),   // режим доступа
						new_number(st.st_nlink),  // количество жестких ссылок
						new_number(st.st_uid),    // идентификатор пользователя-владельца
						new_number(st.st_gid),    // идентификатор группы-владельца
						new_number(st.st_rdev),   // тип устройства (если это устройство)
						new_number(st.st_size),   // ** общий размер в байтах **
						IFALSE, // new_number(st.st_blksize),// размер блока ввода-вывода в файловой системе
						IFALSE, // new_number(st.st_blocks), // количество выделенных блоков
						// Since Linux 2.6, the kernel supports nanosecond
						//   precision for the following timestamp fields.
						new_number(st.st_atime),  // время последнего доступа (в секундах)
						new_number(st.st_mtime),  // время последней модификации (в секундах)
						new_number(st.st_ctime)   // время последнего изменения (в секундах)
				);
				break;
			}

			// TODO: rewrite for "poll" if supported
			case SYSCALL_POLL: {
				word a = A1;
				word b = A2;
				word c = A3;
				word ms = value(c);

				fd_set rs, ws, es;
				int nfds = 1;
				FD_ZERO(&rs); FD_ZERO(&ws); FD_ZERO(&es);
				for (word* cur = (word *)a; (word)cur != INULL; cur = (word *)cur[2]) {
					int fd = port(caar(cur));
					FD_SET(fd, &rs);
					FD_SET(fd, &es);
					if (fd >= nfds)
						nfds = fd + 1;
				}
				for (word* cur = (word *)b; (word)cur != INULL; cur = (word *)cur[2]) {
					int fd = port(caar(cur));
					FD_SET(fd, &ws);
					FD_SET(fd, &es);
					if (fd >= nfds)
						nfds = fd + 1;
				}

				int res;
				if (c == IFALSE) {
					res = select(nfds, &rs, &ws, &es, NULL);
				} else {
					struct timeval tv;
					tv.tv_sec = ms / 1000;
					tv.tv_usec = (ms % 1000) * 1000;
					res = select(nfds, &rs, &ws, &es, &tv);
				}

				word r1, r2;
				if (res < 1) {
					r1 = IFALSE; r2 = res < 0 ? ITRUE : IFALSE; /* 0 = timeout, otherwise error or signal */
				} else {
					int fd; /* something active, wake the first thing */
					for (fd = 0; ; ++fd) {
						if (FD_ISSET(fd, &rs)) {
							r1 = make_port(fd); r2 = I(1); break;
						} else if (FD_ISSET(fd, &ws)) {
							r1 = make_port(fd); r2 = I(2); break;
						} else if (FD_ISSET(fd, &es)) {
							r1 = make_port(fd); r2 = I(3); break;
						}
					}
				}

				r = cons(r1, r2);
				break;
			}

			case SYSCALL_FSYNC: {
				CHECK_ARGC_EQ(1);
				if (!is_port(A1))
					break;

				if (fsync(port (A1)) < 0)
					break;

				r = (word*)ITRUE;
				break;
			}

#if	HAVE_SENDFILE
			/*! \subsection sendfile
			* \brief (syscall **40** outp inp offset count) -> number | #f
			*
			* Transfer data between ports
			*
			* \param outp    output port
			* \param inp     input port
			* \param offset  offset inside input port
			* \param count   count of bytes to copy
			*
			* \return written bytes if success,
			*         #false if error
			*
			* http://man7.org/linux/man-pages/man2/sendfile.2.html
			*/
			case SYSCALL_SENDFILE: {
				CHECK_ARGC_EQ(4);
				CHECK_PORT(1);
				CHECK_PORT(2);
				CHECK_NUMBER(3);
				CHECK_NUMBER(4);

				int socket = port(A1); // to
				int filefd = port(A2); // from
				off_t offset = number(A3);
				int count = number(A4);// count

				ssize_t wrote= 0;
				while (count > 0) {
#if defined(__APPLE__)
					off_t len = count;
					wrote = sendfile(filefd, socket, offset, &len, NULL, 0);
					if (wrote == 0)
						wrote = len;
#else
					wrote = sendfile(socket, filefd, &offset, count);
#endif
					if (wrote < 0) {
						if (errno != EAGAIN)
							break;
						yield();
#if defined(__APPLE__)
						wrote = len;
						goto ok;
#endif
					}
					else
					if (wrote == 0)
						break;
					else {
						// "offset += wrote" is NOT REQUIRED
					ok:	count -= wrote;
					}
				}
				if (wrote < 0)
					break;

				r = (word*) ITRUE;
				break;
			}
#endif

#if SYSCALL_MEMFD
			case SYSCALL_MEMFD: {
				CHECK_ARGC_EQ(0);

				int port = memfd_create("T", 0);
				if (port >= 0)
					r = (word*) make_port(port);

				break;
			}
#endif

			// PIPES
			case SYSCALL_PIPE: {
				CHECK_ARGC(0,1);
				CHECK_NUMBERP(1);
				int flags = (argc > 0) ? (int) numberp(A1) : 0; // 0b00, 1 means synchronous

				int pipefd[2];
				if (pipe(pipefd) == 0) {
					r = cons(make_port(pipefd[0]), make_port(pipefd[1]));

					#ifndef _WIN32
					if (!(flags & 1)) fcntl(pipefd[0], F_SETFL, fcntl(pipefd[0], F_GETFL, 0) | O_NONBLOCK);
					if (!(flags & 2)) fcntl(pipefd[1], F_SETFL, fcntl(pipefd[1], F_GETFL, 0) | O_NONBLOCK);
					#endif
				}
				break;
			}

			case SYSCALL_DUP: {
				CHECK_ARGC(1,2);
				int portfd = port(A1);
				if (argc == 1) {
					int fd = dup(portfd);
					if (fd != -1)
						r = (R) make_port(fd);
				}
				else
				if (argc == 2) {
					int portfd2 = port(A2);
					int fd = dup2(portfd, portfd2);
					if (fd != -1)
						r = (R) make_port(fd);
				}
				break;
			}

            case SYSCALL_ERRNO: {
				CHECK_ARGC_EQ(0);
                r = (R) I(errno);
                break;
            }
			// FOLDERS

			// UNSAFES

#if OLVM_FFI
			/*! \subsection mmap
			* \brief 9: (mmap address length offset) -> bytevector
			*
			* \param address (type-vptr)
			* \param length (integer)
			* \param offset (integer)
			*
			* \return
			*/
			// TODO: disable when in safe mode
			case SYSCALL_MMAP: {
				CHECK_ARGC(2,3);
				CHECK_VPTR(1);
				CHECK_NUMBER(2);
				CHECK_NUMBER_OR_FALSE(3);

				char* address = (char*) car(A1);
				if (!address)
					break;

				ssize_t count = number(A2); // in bytes
				size_t offset = (argc > 2 && A3 != IFALSE)
					? number(A3) // in bytes
					: 0;

				if (count == -1) {
					char* p = address + offset;
					while (*p++);
					count = p - address;
				}

				unsigned words = WALIGN(count) + 1; // in words
				if (fp + words > heap->end) {
					GC(words);
				}

				r = new_bytevector(count);
				memcpy(&car(r), address + offset, count);
				break;
			}
#endif

			// INFO

			/*! \subsection syscall-16
			* \brief 16: (syscall 16 ...) -> ...|#f
			*
			* Syscall 16
			*
			* \param
			*
			* \return #false if error
			*
			*/
			case SYSCALL_IOCTL + SECCOMP:
			case SYSCALL_IOCTL: {
				CHECK_ARGC(2, 255);
				CHECK_PORT(1);
				CHECK_NUMBER(2);

				int portfd = port(A1);
				int ioctl = number(A2);

				switch (ioctl + sandboxp) {
					case SYSCALL_IOCTL_TIOCGETA: {
						#ifdef _WIN32
							if (_isatty(portfd))
						#else
							struct termios t;
							if (tcgetattr(portfd, &t) != -1)
						#endif
								r = (word*) ITRUE;
						break;
					}
					case SYSCALL_IOCTL_TIOCGETA + SECCOMP: {
						if ((portfd == STDIN_FILENO) || (portfd == STDOUT_FILENO) || (portfd == STDERR_FILENO))
							r = (word*) ITRUE;
						break;
					}
				}
				break;
			}

			// INTERPROCESS FUNCTIONS

			case SYSCALL_YIELD: {
				CHECK_ARGC_EQ(1);
				yield();
				r = (word*)ITRUE;
				break;
			}

			// todo: http://man7.org/linux/man-pages/man2/nanosleep.2.html
			// TODO: change to "select" call (?)
#if SYSCALL_SLEEP
			case SYSCALL_SLEEP: { // time in micro(!)seconds
				CHECK_ARGC_EQ(1);
				CHECK_NUMBERP(1);

				int_t us = numberp(A1);

				#ifdef __EMSCRIPTEN__
					//int_t ms = us / 1000;
					//emscripten_sleep(ms);
					r = (word*) ITRUE;
				#endif
				#ifdef _WIN32// for Windows
					int_t ms = us / 1000;
					Sleep(ms); // in ms
					r = (word*) ITRUE;
				#endif
				#if defined(__unix__) || defined(__APPLE__) // Linux, *BSD, MacOS, etc.
					struct timespec ts = { us / 1000000, (us % 1000000) * 1000 };
					struct timespec rem;
					if (nanosleep(&ts, &rem) != 0)
						r = new_number((rem.tv_sec * 1000000 + rem.tv_nsec / 1000));
				#endif
				break;
			}
#endif

			// (EXECVP program-or-function env (vector port port port))
			// http://linux.die.net/man/3/execvp
			case SYSCALL_EXECVP: {
				CHECK_ARGC_EQ(3);
				CHECK_STRING(1);

				// if a is string:
				if (is_string(A1)) {
					char* command = string(A1);
					word b = A2;
					word c = A3;
					#if defined(__unix__) || defined(__APPLE__)
					# ifdef __EMSCRIPTEN__
						emscripten_run_script(command);
						r = (word*) ITRUE;
					# else
						// todo: add case (cons program environment)
						int child = fork();
						if (child == 0) {
							D("forked %s", command);
							if (is_pair (c)) {
								const int in[3] = { STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO };
								for (size_t i = 0; i < sizeof(in) / sizeof(in[0]) && is_pair(c); i++) {
									if (is_port(car(c))) {
										dup2(port(car(c)), in[i]);
										close(port(car(c)));
									}
									c = cdr (c);
								}
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

							exit(execvp(command, args));
							assert (0); // should not be reached
						}
						else if (child > 0)
							r = (word*) new_number(child);
					# endif
					#endif
					#ifdef _WIN32
						STARTUPINFO si;
						ZeroMemory(&si, sizeof(STARTUPINFO));
						si.cb = sizeof(STARTUPINFO);
						si.dwFlags |= STARTF_USESTDHANDLES;
						si.hStdInput = (HANDLE) _get_osfhandle(STDIN_FILENO);
						if (is_pair(c)) {
							if (is_port(car(c))) // stdin in
								si.hStdInput = (HANDLE) _get_osfhandle(port(car(c)));
							c = cdr(c);
						}
						si.hStdOutput = (HANDLE) _get_osfhandle(STDOUT_FILENO);
						if (is_pair(c)) {
							if (is_port(car(c))) // stdout out
								si.hStdOutput = (HANDLE) _get_osfhandle(port(car(c)));
							c = cdr(c);
						}
						si.hStdError = (HANDLE) _get_osfhandle(STDERR_FILENO);
						if (is_pair(c)) {
							if (is_port(car(c))) // stderr out
								si.hStdError = (HANDLE) _get_osfhandle(port(car(c)));
							c = cdr(c);
						}

						char* args = (char*) &car (fp);
						// todo: add length check! TODO: add this feature under #define like HARDENING
						sprintf(args, "\"%s\"", command);

						if (is_pair(b)) {
							b = cdr(b); // skip command name
							word i = b;
							while (i != INULL) {
								strcat(args, " ");
								strcat(args, string(car(i))); i = cdr(i);
							}
						}

						PROCESS_INFORMATION pi;
						ZeroMemory(&pi, sizeof(PROCESS_INFORMATION));

						if (CreateProcess(NULL, args, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
							CloseHandle(pi.hThread);
							CloseHandle(pi.hProcess);
							r = new_number(pi.dwProcessId);
						}
					#endif
					break;
				}
				break;
			}

			case SYSCALL_WAITPID: {
				CHECK_ARGC(1, 2);
				CHECK_NUMBER(1);
				CHECK_BOOLEAN(2);

				#if defined(__unix__) || defined(__APPLE__)
					pid_t pid = (pid_t)number(A1);
					int flag = (argc > 1 && A2 != IFALSE) ? 0 : WNOHANG;
					int status = 0;
					int err = waitpid(pid, &status, flag);
					if (err >= 0)
						r = new_number(WEXITSTATUS(status));
				#endif
				#ifdef _WIN32
					DWORD pid = (DWORD)number(A1);
					DWORD exitCode = 0;
					HANDLE hProcess = OpenProcess(PROCESS_QUERY_INFORMATION, TRUE, pid);
					if (!hProcess) { // no process active
						r = (R)ITRUE;
						break;
					}
					if (GetExitCodeProcess(hProcess, &exitCode))
						if (exitCode != STILL_ACTIVE)
							r = new_number(exitCode);
					CloseHandle(hProcess);
				#endif
				break;
			}

#if HAVE_DLOPEN
			// -=( dlopen )=-------------------------------------------------
			case SYSCALL_DLOPEN: { // (dlopen filename mode)
				word a = A1;
				word b = A2;

				word *filename = (word*)a;
				int mode = (int) value(b);

				// android 2.2 segfaults on dlopen(NULL, ...)
				// http://code.google.com/p/android/issues/detail?id=5049
				char* library_name;
				if (filename == (R)IFALSE)
					library_name = LIBRARY_FILENAME;
				else
				if (is_string(filename))
					library_name = string(filename);
				else
					break; // invalid filename

				void* module = dlopen(library_name, mode);
				if (module)
					r = new_vptr(module);
				break;
			}

			case SYSCALL_DLCLOSE: { // (dlclose module)
				word a = A1;
				if (!is_vptr(a))
					ERROR(SYSCALL_DLCLOSE, a, INULL);
				void* module = (void*)car (a);

				if (dlclose(module) == 0)
					r = (word*) ITRUE;
				break;
			}

			case SYSCALL_DLSYM: { // (dlsym module function)
				// CHECK(is_vptr(a), a, SYSCALL);
				word a = A1;
				word b = A2;

				if (!is_reference(a))
					break;

				void* module = (void*) car(a);
				word* symbol = (word*) b;
				// http://www.symantec.com/connect/articles/dynamic-linking-linux-and-windows-part-one
				if (!(is_value(symbol) || reference_type (symbol) == TSTRING))
					break;

				char* name = is_value(symbol)
						? (char*) value((word)symbol)
						: (char*) &symbol[1];

				word function = (word)dlsym(module, name);
				if (function)
                    r = new_vptr(function);
					// todo: in DEBUG mode : r = new_vptr(function, b);
#ifdef OLVM_DLSYM_DEBUG
				else
					D("dlsym failed: %s", dlerror());
#endif
				break;
			}
			case SYSCALL_DLERROR: { // (dlerror)
				char* error = (char*)dlerror();
				if (error)
					r = new_string(error);
				break;
			}
#endif// HAVE_DLOPEN

			// TIME FUNCTIONS

			// (gettimeofday)
			// todo: change (clock) call to this one
			case SYSCALL_GETTIMEOFDAY: {
				CHECK_ARGC_EQ(0);

				struct timeval tv;
				if (gettimeofday(&tv, NULL) == 0)
					r = cons (new_number(tv.tv_sec), new_number(tv.tv_usec));
				break;
			}

			/**
			 * @brief (format seconds #f)
			 * @arg format return string, else seconds
			 * @arg if seconds == false, get current seconds
			 * @see http://man7.org/linux/man-pages/man2/time.2.html
			 */
			case SYSCALL_TIME: {
				CHECK_ARGC(0,2);
				CHECK_STRING(1);
				CHECK_NUMBERP_OR_FALSE(2);

				time_t seconds = (argc > 1 && A2 != IFALSE)
			 		? (time_t) numberp(A2)
					: time(0);

#if HAVE_STRFTIME // todo: check this code!
				word* A = (word*) A1;
				if (is_string(A)) {
					struct tm * timeinfo = localtime(&seconds);
					if (! timeinfo)
						break;
					// The environment variables TZ and LC_TIME are used!
					size_t len = strftime(
							(char*) &fp[1],
							(size_t) (heap->end - fp - 1) * sizeof(word),
							string (A), timeinfo);
					r = new_alloc(TSTRING, len);
				}
				else
#endif
					r = new_number (seconds);
				break;
			}

			// SOCKETS
		// http://www.kegel.com/c10k.html
#if HAVE_SOCKETS
			// todo: add getsockname() and getpeername() syscalls

			/*! #### SOCKET
			* * `(syscall 41)` -> tcp socket
			* * `(syscall 41 domain type)` -> typed socket
			* * `(syscall 41 domain type protocol)` -> typed socket
			*
			* - *type*, 1 - STREAM, 2 - DGRAM, 3 - RAW
			* - *protocol*, 
			*/
			case 41: {
				CHECK_ARGC(0,3);
				CHECK_NUMBER(1);
				CHECK_NUMBER(2);
				CHECK_NUMBER(3);

				int domain = (argc > 0) ? (int)number(A1) : AF_INET;
				int type = (argc > 1) ? (int)number(A2) : SOCK_STREAM;
				int protocol = (argc > 2) ? (int)number(A3) : IPPROTO_TCP;
#ifdef _WIN32
				int sock = socket(domain, type, protocol);
				ioctlsocket(sock, FIONBIO, &(unsigned long){1}); // enable non-blocking mode
#else
				int sock = socket(domain, type, protocol); // |SOCK_NONBLOCK
				setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &(int){1}, sizeof(int));
#endif
				if (sock != -1)
					r = (word*)make_port (sock);
				break;
			}

			// CONNECT
			case 42: { // (connect sockfd host port)
				word a = A1, b = A2, c = A3;

				if (!is_port(a))
					ERROR(SYSCALL, 42, a);
				int sockfd = port (a);
				char* host = string (b); // todo: check for string type
				int port = value (c);

				struct sockaddr_in addr;
				addr.sin_family = AF_INET;
				addr.sin_addr.s_addr = inet_addr(host);
				addr.sin_port = htons(port);

				if (addr.sin_addr.s_addr == INADDR_NONE) {
					struct hostent *he = gethostbyname(host);
					if (he != NULL)
						memcpy(&addr.sin_addr, he->h_addr_list[0], sizeof(addr.sin_addr));
				}

		//				ipfull = (ip[0]<<24) | (ip[1]<<16) | (ip[2]<<8) | ip[3];
		//				addr.sin_addr.s_addr = htonl(ipfull);
				if (connect(sockfd, (struct sockaddr *) &addr, sizeof(struct sockaddr_in)) < 0)
					break;
				// set_blocking(sock, 0):
#if _WIN32
				unsigned long mode = 1; // non blocking
				if (ioctlsocket(sockfd, FIONBIO, &mode) == 0)
#else
				int flags = fcntl(sockfd, F_GETFL, 0);
				if (flags < 0)
					break;
				flags = (flags | O_NONBLOCK);
				if (fcntl(sockfd, F_SETFL, flags) == 0)
#endif

					r = (R)ITRUE;

				break;
			}

			// SHUTDOWN
			// http://linux.die.net/man/2/shutdown
			case 48: { // (shutdown socket)
				word a = A1; //, b = A2, c = A3;
				if (!is_port(a))
					ERROR(SYSCALL, 48, a);

				int socket = port(a);

				// On error, -1 is returned
				if (shutdown(socket, 2) == 0) // both
					r = (word*)ITRUE;

				break;
			}

			// BIND
			// http://linux.die.net/man/2/bind
			case 49: { //  (socket, port, #false) // todo: c for options
				word a = A1, b = A2;

				// CHECK(is_port(a), a, SYSCALL);
				int sockfd = port(a);
				int port = value (b);

				// todo: assert on argument types
				struct sockaddr_in interface;
				interface.sin_family = AF_INET;
				interface.sin_port = htons(port);
				interface.sin_addr.s_addr = INADDR_ANY;

				// On success, zero is returned.
				if (bind(sockfd, (struct sockaddr *) &interface, sizeof(interface)) == 0)
					r = (word*)ITRUE;
				break;
			}

			// LISTEN (socket)
			// http://linux.die.net/man/2/listen
			// listen() marks the socket referred to by sockfd as a passive socket, that is,
			// as a socket that will be used to accept incoming connection requests using accept(2).
			case 50: {
				word a = A1;

				// CHECK(is_port(a), a, SYSCALL);
				int sockfd = port(a);

				// On success, zero is returned.
				if (listen(sockfd, 42) == 0) {
		//					set_blocking(sockfd, 0);
					r = (word*)ITRUE;
				}

				break;
			}

			// ACCEPT
			// http://linux.die.net/man/2/accept
			case 43: { // (accept sockfd)
				word a = A1;

				// CHECK(is_port(a), a, SYSCALL);
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
					r = (word*)make_port (sock);
				break;
			}

			// SELECT
			// http://linux.die.net/man/2/select
			case 23: { // (select sockfd)
				word a = A1, b = A2;

				// CHECK(is_port(a), a, SYSCALL);
				int sockfd = port(a);
				int timeus = is_numberp(b) ? untoi (b) : 100000; // default is 100 ms

				fd_set fds;
				FD_ZERO(&fds); FD_SET(sockfd, &fds);

				struct timeval timeout = { timeus / 1000000, timeus % 1000000 }; // µs
				if (select(sockfd + 1, &fds, NULL, NULL, &timeout) > 0
						&& FD_ISSET(sockfd, &fds))
					r = (word*)ITRUE;

				break;
			}

			// GETPEERNAME
			// http://linux.die.net/man/2/getpeername
			case 51: { // (getpeername sockfd)
				word a = A1;

				// CHECK(is_port(a), a, SYSCALL);
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

				r = cons (new_string(ipaddress), I(port));

		#else
				unsigned short port;

				if (peer.ss_family == AF_INET) {
					char ipaddress[INET_ADDRSTRLEN];

					struct sockaddr_in *s = (struct sockaddr_in *)&peer;
					port = ntohs(s->sin_port);
					inet_ntop(AF_INET, &s->sin_addr, ipaddress, sizeof ipaddress);
					r = cons (new_string(ipaddress), I(port));
				}
				/* temporary disable IP_v6, todo: return back
				else
				if (peer.ss_family == AF_INET6) {
					char ipaddress[INET6_ADDRSTRLEN];

					struct sockaddr_in6 *s = (struct sockaddr_in6 *)&peer;
					port = ntohs(s->sin6_port);
					inet_ntop(AF_INET6, &s->sin6_addr, ipaddress, sizeof ipaddress);
					result = new_pair(new_string(ipaddress), I(port));
				}*/
				else
					break;
		#endif

				break;
			}
#endif// HAVE_SOCKETS

			// SYSTEM INFO

			// UNAME (uname)
			// http://linux.die.net/man/2/uname
			case 63: {
				CHECK_ARGC_EQ(0);

				struct utsname name;
				if (uname(&name))
					break;

				r = new_vector(
				#ifdef __ANDROID__
					new_string("Android"),
				#else
					({*name.sysname ? (word)new_string(name.sysname) : IFALSE;}),
				#endif
					new_string(name.nodename),
					new_string(name.release),
					new_string(name.version),
					({*name.machine ? (word)new_string(name.machine) : IFALSE;}));

				break;
			}

			#if SYSCALL_GETRLIMIT
			/*! ##### getrlimit
			* * `(syscall 97 resource) --> bytevector | #false`
			*
			* Attempts to get resource limits into the bytevector.
			*
			* - *resource*: resource
			*   * 0, CPU time in sec
			*   * 1, Maximum filesize
			*   * 2, max data size
			*   * 9, address space limit
			*
			* Return:
			* - *[soft limit, hard limit]* if success,
			* - *#false* if error
			*
			* https://man7.org/linux/man-pages/man2/prlimit.2.html
			*/
			case SYSCALL_GETRLIMIT: {
				CHECK_ARGC_EQ(1);
				CHECK_NUMBERP(1);

				struct rlimit l;
				int resource = value(A1);
				// arguments currently ignored. used RUSAGE_SELF
				if (getrlimit(
						resource == 0 ? RLIMIT_CPU : // limit, in seconds, on the amount of CPU time that the process can consume
						resource == 1 ? RLIMIT_FSIZE : // maximum size in bytes of files that the process may create
						resource == 2 ? RLIMIT_DATA : // maximum size of the process's data segment (initialized data, uninitialized data, and heap)
						resource == 9 ? RLIMIT_AS : // maximum size of the process's virtual memory
						-1, &l) == 0)
					r = new_vector(
							new_number(l.rlim_cur),
							new_number(l.rlim_max));
				break;
			}
			#endif

			#if SYSCALL_GETRUSAGE
			// GETRUSAGE (getrusage)
			// @returns: (vector utime stime)
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
					r = new_vector(
							cons (new_number(u.ru_utime.tv_sec), new_number(u.ru_utime.tv_usec)),
							cons (new_number(u.ru_stime.tv_sec), new_number(u.ru_stime.tv_usec))
					);
				break;

			}
			#endif

			// SYSINFO (sysinfo)
			#if SYSCALL_SYSINFO
			case SYSCALL_SYSINFO: {
				struct sysinfo info;
				if (sysinfo(&info) == 0)
					r = new_vector(
							new_number(info.uptime),
							new_vector(new_number(info.loads[0]),
									new_number(info.loads[1]),
									new_number(info.loads[2])),
							new_number(info.totalram),
							new_number(info.freeram),
							new_number(info.sharedram),
							new_number(info.bufferram),
							new_number(info.totalswap),
							new_number(info.freeswap),
							new_number(info.procs)
					);
				break;
			}
			#endif

			// getenv/setenv/environ
			case 1014: { // (setenv "name" "value" overwrite?)
				CHECK_ARGC(1,3);
				CHECK_STRING(1);
				CHECK_STRING_OR_FALSE(2);
				CHECK_TRUE_OR_FALSE(3);

				char* name = string(A1);
				word* value = (argc > 1) ? (word*)A2 : (word*)IFALSE;
				int overwrite = (argc > 2 && A3 != IFALSE) ? 1 : 0;

				if (value == RFALSE) {
#ifndef _WIN32
					if (unsetenv(name) == 0)
						r = (word*)ITRUE;
#endif
				}
				else {
					if (setenv(name, string(value), overwrite) == 0)
						r = (word*)ITRUE;
				}
				break;
			}
			case 1015: { // environ
				CHECK_ARGC_EQ(0);

				extern char **environ;
				
				char** ptr = environ;
				word* strings = (word*)INULL;
				while (*ptr != NULL) {
					char* p = strchr(*ptr, '=');
					if (p)
						strings = new_pair(new_pair(new_string(*ptr, p - *ptr), new_string(p+1)), strings);
					else
						strings = new_pair(new_pair(new_string(*ptr), IFALSE), strings);
					ptr++;
				}

				r = strings;
				break;

			}
			// getenv
			case 1016: { // getenv <owl-raw-bvec-or-ascii-leaf-string>
				word *name = (word *)A1;
				if (is_string(name)) {
					char* env = getenv(string(name));
					if (env)
						r = new_string(env);
				}
				break;
			}
			// system
			case 1017: { // system (char*)
				int q = system(string(A1));
				if (q >= 0)
					r = new_number(q);
				break;
			}

			// ...
			case 1000: { // GC
				CHECK_ARGC_EQ(0);

				GC(-1);

				break;
			}
			case 1002: // return userdata
				CHECK_ARGC_EQ(0);
				r = new_vptr(ol->userdata);
				break;

			// case 1007: // set memory limit (in mb) / // todo: переделать на другой номер
			// 	r = new_number (ol->max_heap_size);
			// 	ol->max_heap_size = value(A1);
			// 	break;
			// case 1009: // get memory limit (in mb) / // todo: переделать на другой номер
			// 	r = new_number (ol->max_heap_size);
			// 	break;

			case 1022: // set ticker
				r = new_number(ticker);
				ticker = value(A1);
				break;
	//		case 1014: { /* set-ticks n _ _ -> old */
	//			result = new_number (ol->slice);
	//			ol->slice  = uvtoi (a);
	//			break;
	//		}
			case 1117 + SECCOMP:
			case 1117: { // get memory stats -> #[old-generation-size allocated-size heap-size], all in words
				int g = heap->genstart - heap->begin;
				int f = fp - heap->begin;
				int t = heap->end - heap->begin;
				r = new_vector(I(g), I(f), I(t));
				break;
			}

#ifdef __EMSCRIPTEN__
			case 1201: { // TODO: merge with system (1017)
				CHECK_ARGC_EQ(2);
				//CHECK(is_number(a), a, SYSCALL);
				//CHECK(is_string(b), b, SYSCALL);
				char* string = string(A2);

				switch (value(A1)) {
				case TSTRING: {
					char* v = emscripten_run_script_string(string);
					if (v)
						r = new_string(v);
					break;
				}
				case TINTP: {
					int v = emscripten_run_script_int(string);
					r = (word*)make_enum(v);
					break;
				}
				default:
					emscripten_run_script(string);
					r = (word*) ITRUE;
				}
				break;
			}
#endif

			// SANDBOX/UNSAFES

			// https://www.mindcollapse.com/blog/processes-isolation.html
			// http://outflux.net/teach-seccomp/
#if HAVE_SANDBOX && SYSCALL_PRCTL
			case SYSCALL_PRCTL:
				//seccomp_time = 1000 * time(NULL); /* no time calls are allowed from seccomp, so start emulating a time if success */
				/*struct sock_filter filter[] = {
					// http://outflux.net/teach-seccomp/
				};*/
			#ifdef SECCOMP_MODE_STRICT
				if (prctl(PR_SET_SECCOMP, SECCOMP_MODE_STRICT, 0, 0, 0) != -1) { /* true if no problem going seccomp */
					sandboxp = SECCOMP;
					r = (word*) ITRUE;
				}
			#endif
				break;
#endif

#if OLVM_UNSAFES
			case SYSCALL_ARCHPRCTL:
				unsafesp = 0;
				r = (word*) ITRUE;
				break;
#endif

			default:
				break;
		}

		++argc; // restore real arguments count
		reg[ip[argc]] = (word)r; // result
		ip += argc + 1; break;
	}// end of syscalls

	// FPU extensions
	case FP1: { // with 1 argument
	#if OLVM_INEXACTS && OLVM_BUILTIN_FMATH
		word fn = value (A0);
		inexact_t a = ol2f(A1);

		A2 = (word) new_alloc(TINEXACT, sizeof(inexact_t));
		switch (fn) {
		case 0xFA: // fsqrt
			*(inexact_t*)&car(A2) = __builtin_sqrt(a);
			break;
		case 0xFE: // fsin
			*(inexact_t*)&car(A2) = __builtin_sin(a);
			break;
		case 0xFF: // fcos
			*(inexact_t*)&car(A2) = __builtin_cos(a);
			break;
		case 0xF2: // ftan
			*(inexact_t*)&car(A2) = __builtin_tan(a);
			break;
		case 0xF3: // fatan
			*(inexact_t*)&car(A2) = __builtin_atan(a);
			break;
		case 0xF1: // flog
			*(inexact_t*)&car(A2) = __builtin_log(a);
			break;

		case 0x81: // fexp
			*(inexact_t*)&car(A2) = __builtin_exp(a);
			break;
		case 0x8E: // fasin
			*(inexact_t*)&car(A2) = __builtin_asin(a);
			break;
		case 0x8F: // facos
			*(inexact_t*)&car(A2) = __builtin_acos(a);
			break;

		case 0xFC: // ffloor
			*(inexact_t*)&car(A2) = __builtin_floor(a);
			break;

		default:
			A2 = IFALSE;
			break;
		}
	#else
		A2 = IFALSE;
	#endif
		ip += 3; break;
	}

	case FP2: { // with 2 arguments
	#if OLVM_INEXACTS
		word fn = value (A0);
		inexact_t a = ol2f(A1);
		inexact_t b = ol2f(A2);

		A3 = (word) new_alloc(TINEXACT, sizeof(inexact_t));
		switch (fn) {
		case 0xD9: // fless?
			A3 = (a < b) ? ITRUE : IFALSE;
			break;
		case 0xC1: // fadd
			*(inexact_t*)&car(A3) = a + b;
			break;
		case 0xE9: // fsub
			*(inexact_t*)&car(A3) = a - b;
			break;
		case 0xC9: // fmul
			*(inexact_t*)&car(A3) = a * b;
			break;
		case 0xF9: // fdiv
			*(inexact_t*)&car(A3) = a / b;
			break;
	#if OLVM_BUILTIN_FMATH
		case 0xF3: // fatan2
			*(inexact_t*)&car(A3) = __builtin_atan2(a, b);
			break;
		case 0xF1: // flog2
			*(inexact_t*)&car(A3) = __builtin_log(a) / __builtin_log(b);
			break;

		case 0x81: // fexpt
			*(inexact_t*)&car(A3) = __builtin_pow(a, b);
			break;
	#endif
		default:
			A3 = IFALSE;
			break;
		}
	#else
		A3 = IFALSE;
	#endif
		ip += 4; break;
	}

#ifndef OLVM_NOPINS
	case VMPIN: {  // (vm:pin object) /pin object/ => pin id
		word object = A0;

		int id = OLVM_pin(ol, object);
		reg = ol->reg; // pin can realloc registers!

        A1 = (id > 3) ? I(id) : IFALSE;
		ip += 2; break;
	}
	case VMUNPIN: { // vm:unpin => old pin value
		word pin = A0;
		ASSERT (is_value(pin), pin, VMUNPIN);

		int id = value(pin);
        word o = OLVM_unpin(ol, id);
		reg = ol->reg; // don't remove

		A1 = o;
		ip += 2; break;
	}

	case VMDEREF: {// vm:deref /get pinned object value/
		word pin = A0;
		ASSERT (is_value(pin), pin, VMDEREF);

		int id = value(pin);
        A1 = OLVM_deref(ol, id);
		ip += 2; break;
	}
#endif

	// (vm:exit value)
	case VMEXIT:
		this = R3;
		R3 = A0;
		goto done;
	}
	goto loop;


error:; // R4-R6 set, and call mcp (if any)
	this = R0;
	R0 = IFALSE;
	if (is_reference(this)) {
		acc = 4;
		goto apply;
	}
	E("invoke_mcp failed");
	goto done; // no mcp to handle error (fail in it?), so nonzero exit

done:;
	ol->this = this;
	ol->arity = acc;

	ol->heap.fp = fp;

	set_blocking(STDOUT_FILENO, 1);
	set_blocking(STDERR_FILENO, 1);
	return 1; // ok
} // end of runtime

// ======================================================================
//       загрузчик скомпилированного образа и его десериализатор
//

// fasl decoding
static __inline__
size_t get_nat(unsigned char** hp)
	// TODO: assert if overflow
{
	size_t nat = 0;
	int i = 0;
	unsigned char ch;

	do {
		ch = *(*hp)++;
		nat |= (ch & 0x7F) << i;
		i += 7;
	} while (ch & 0x80);
	return nat;
}

// возвращает новый топ стека
static
word* deserialize(word *ptrs, int nobjs, unsigned char *bootstrap, word* fp)
{
	// TODO: process both formats!
	unsigned char* hp = bootstrap;
	word* constructor;

	constructor = (word*) INULL;
	for (ptrdiff_t id = 0; id < nobjs; id++) {
		ptrs[id] = (word) fp;

		size_t type;
		size_t size;
		switch (*hp++) { // todo: adding type information here would reduce fasl and executable size
		case 1: {
			type = *hp++; assert (!(type & ~0x3F)); // type is 6 bits long
			size = get_nat(&hp);

			word* object = fp;
			*fp++ = make_header(type, size + 1); // +1 to include header in size
			while (size--) {
				if (*hp != 0) { // reference
					size_t diff = get_nat(&hp);
					*fp++ = ptrs[id - diff];
				}
				else { // value
					hp++;
					unsigned char type = *hp++;
					word* obj = object;

					word nat = 0;
					size_t i = 0;
					unsigned char ch;
					do {
						ch = *hp++;
						nat |= (word)(ch & 0x7F) << i;
						if (!(ch & 0x80) && !(nat >> VBITS))
							break; // early exit if number fits an 'enum'

						i += 7;
						if (i > VBITS) {
							// well, this value is too big. let's produce a number
							memmove(obj + 3, obj, (fp - obj) * W); fp += 3; // shift object
							object += 3;
							// сдвинуть всю цепочку вверх

							obj[0] = make_header(type == TENUMP ? TINTP : TINTN, 3);
							obj[1] = I(nat & VMAX);
							obj[2] = INULL;

							nat >>= VBITS;
							i -= VBITS;
						}
					} while (ch & 0x80);

					if (obj == object) { // мы ничего никуда не сдвигали, значит число оказалось маленьким (enum)
						word val = make_value(type, nat);
						*fp++ = val;
					}
					else { // мы сдвигали, надо сохранить оставшийся кусок числа
						if (nat != 0) {
							memmove(obj + 3, obj, (fp - obj) * W); fp += 3; // shift object, move fp
							object += 3;

							obj[0] = make_header(TPAIR, 3);
							obj[1] = I(nat); // assert (nat <= VMAX)
							obj[2] = INULL;
						}
						// а теперь заполним все указатели
						obj += 3;
						while (obj < object) {
							obj[2] = (word) (obj - 3);
							obj += 3;
						}
						*fp++ = (word) (obj - 3);
						ptrs[id] = (word) object; // обновим указатель
					}
				}
			}
			break;
		}
		case 2: {
			type = *hp++; assert (!(type & ~0x3F)); // type is 6 bits long
			size = get_nat(&hp);

			int words = WALIGN(size);
			int pads = WPADS(size);

			unsigned char *p = (unsigned char*)&car(new (type, words, pads));

			// inexact numbers
// is it a big-endian target architecture?
#if (defined(__BYTE_ORDER) && __BYTE_ORDER == __BIG_ENDIAN) || \
    (defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__) ||\
	(defined(__BIG_ENDIAN__)) || \
    (defined(__ARMEB__) || defined(__THUMBEB__) || defined(__AARCH64EB__) || defined(_MIBSEB) || defined(__MIBSEB) || defined(__MIBSEB__))
#	ifdef OLVM_INEXACTS
			if (type == TINEXACT) {
				int s = size;
				while (size--)
					p[size] = *hp++;
				p += s;
			}
			else
#	endif
// is it a little-endian target architecture?
#elif\
    (defined(__BYTE_ORDER) && __BYTE_ORDER == __LITTLE_ENDIAN) || \
    (defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__) ||\
	(defined(__LITTLE_ENDIAN__)) || \
    (defined(__ARMEL__) || defined(__THUMBEL__) || defined(__AARCH64EL__) || defined(_MIBSEL) || defined(__MIBSEL) || defined(__MIBSEL__))
			// nothing to do
#else
#	error "Unknown target endianness arcitecture"
#endif
			// обычные байтовые последовательности
				while (size--)
					*p++ = *hp++;
			// not required, but may be usefull
			while (pads--)
				*p++ = 0;
			break;
		}
		default:
			D("Bad object in heap at %d", (void*)(hp-bootstrap));
			return 0;
		}
		// если мы декодировали конструктор, надо его добавить в цепочку
		if (type == TCONSTRUCTOR) // special case: constructor
			constructor = cons(ptrs[id], constructor);
	}
	// return construction list
	ptrs[nobjs] = (word) constructor;
	return fp;
}

static
// функция подсчета количества объектов в загружаемом образе
int count_fasl_objects(word *words, unsigned char *lang) {
	// TODO: process both formats!
	unsigned char* hp;

	// count:
	int n = 0;
	hp = lang;

	int allocated = 0;
	while (*hp != 0) {
		switch (*hp++) {
		case 1: { // object
			hp++; ++allocated;
			size_t size = get_nat(&hp);
			while (size--) {
				//decode_field:
				if (*hp == 0) {
					hp++;
					unsigned char* op = hp++;
					get_nat(&hp); // нужна копия этой функции без проверки переполнения

					int type = *op++;
					if (type == TENUMP || type == TENUMN) {
						size_t size = hp - op;
						int words = (size < W) ? 1 : ((size + W-2) / (W-1)) * 3;
						allocated += words;
					}
				}
				else {
					get_nat(&hp); // simply skip word (a number of referenced object)
					++allocated;
				}
			}
			break;
		}
		case 2: { // raw object
			hp++;
			int size = get_nat(&hp);
			hp += size;

			int words = (size / W) + ((size % W) ? 2 : 1);
			allocated += words;

			break;
		}

		default:
			D("Ol: bad object in heap");
			return 0;
		}

		n++;
	}

	*words = allocated;
	return n;
}

// ----------------------------------------------------------------
// -=( virtual machine functions )=--------------------------------
//

typedef struct olvm_t OL;

void* OLVM_userdata(OL* ol, void* userdata)
{
	void* old_userdata = ol->userdata;
	ol->userdata = userdata;
	return old_userdata;
}

read_t*  OLVM_set_read (struct olvm_t* ol, read_t  read);
write_t* OLVM_set_write(struct olvm_t* ol, write_t write);
open_t*  OLVM_set_open (struct olvm_t* ol, open_t  open);
close_t* OLVM_set_close(struct olvm_t* ol, close_t close);
stat_t*  OLVM_set_stat (struct olvm_t* ol, stat_t  stat);

idle_t*  OLVM_set_idle (struct olvm_t* ol, idle_t  idle);

// i/o polymorphism
#define override(name) \
name##_t* OLVM_set_##name(struct olvm_t* ol, name##_t name) {\
	name##_t *old_##name = ol->name;\
	ol->name = name;\
	return old_##name;\
}

override(open)
override(read)
override(write)
override(close)
override(stat)

override(idle)

#undef override

void* OLVM_allocate(OL* ol, unsigned words)
{
	word* fp;

	fp = ol->heap.fp;
	word* r = new(words);
	ol->heap.fp = fp;

	return (void*)r;
}

// -=( main )=-----------------------------------------
#ifdef REPL
	extern unsigned char REPL[];
#	define language REPL
#else
#	define language NULL
#endif

#if !OLVM_NOMAIN
int main(int argc, char** argv)
{
	unsigned char* bootstrap = language;

#ifdef __ANDROID__
	LIBRARY_FILENAME = argv[0];
#endif

	//  vm special key: if command line is "--version" then print a version
#ifndef REPL
	if (argc == 2 && strcmp(argv[1], "-v") == 0) {
		E("olvm (Otus Lisp Virtual Machine) %s", __OLVM_VERSION__);
		return 0;
	}
	if (argc == 2 && strcmp(argv[1], "--version") == 0) {
		E("olvm (Otus Lisp Virtual Machine) %s", __OLVM_VERSION__);
		E("Copyright (c) 2014-2024 Yuriy Chumak");
		E("License LGPLv3+: GNU LGPL version 3 or later <http://gnu.org/licenses/>");
		E("License MIT: <https://en.wikipedia.org/wiki/MIT_License>");
		E("This is free software: you are free to change and redistribute it.");
		E("There is NO WARRANTY, to the extent permitted by law.");
		return 0;
	}
#endif

	int file = -1; // running program file
	char*name = 0; // running program name
	// int autoremove = 0;

#if OLVM_TARVENV
	char* argz[] = { "./main" }; // default command line
	if (pvenv_main()) {
		if (argc == 1) {
			// if pvenv main exists:
			int venv = pvenv_open(argz[0], O_RDONLY, 0, 0);
			if (venv >= 0) {
				// speedup: (instead of "close(main);")
				argv = argz; argc = 1;
				name = argv[0];
				file = venv; goto bom;
			}
		}
	}

#endif

	// in case of "ol -- --script", "--script" may be a binary code
	if ((name == 0) && (argc > 1) && (strcmp(argv[1], "--") == 0)) {
		argc -= 2; argv += 2;
		if (argc > 0) {
			name = argv[0];
			argc--; argv++;
		}
	}
	else
	// ./ol - если первая команда - не имя файла, то использовать repl
	if ((name == 0) && (argc > 1) && (strncmp(argv[1], "-", 1) != 0)) {
		name = argv[1];
		argv++, argc--;
	}

	int v = 0;
	if (name == 0) { // входной файл не указан
#ifndef REPL
		goto no_binary_script;
#else
		argc--; argv++;
#endif
	}
	else {
		// todo: possibly use mmap()
		char bom = 42;
		file =
#if OLVM_TARVENV
			pvenv ?
			pvenv_open(name, O_RDONLY | O_BINARY, S_IRUSR, 0) :
#endif
			      open(name, O_RDONLY | O_BINARY, S_IRUSR);
		if (file == -1)
			goto can_not_open_file; // не смогли файл открыть

	bom:;
		// check the file access
		struct stat st;
		if (fstat(file, &st))
			goto can_not_stat_file;

		// empty file, or pipe, or fifo...
		if (st.st_size != 0) {
			int pos = read(file, &bom, 1);  // прочитаем один байт
			if (pos != 1)
				goto can_not_read_file;

			// skip possible hashbang:
			// if (bom == '#') {
			// 	while (read(file, &bom, 1) == 1 && bom != '\n')
			// 		st.st_size--;
			// 	st.st_size--;
			// 	if (read(file, &bom, 1) < 0)
			// 		goto can_not_read_file;
			// 	st.st_size--;
			// }

			if (bom > 2) {	// это текстовая программа (скрипт)
#ifndef REPL
				goto invalid_binary_script;
#else
				close(file);
#endif
			}
			else {
				// иначе загрузим его
				unsigned char* ptr = (unsigned char*) malloc(st.st_size);
				if (ptr == NULL)
					goto can_not_allocate_memory;	// опа, не смогли выделить память...

				ptr[0] = bom;
				while (pos < st.st_size) {
					int n = read(file, &ptr[pos], st.st_size - pos);
					if (n < 0)
						goto can_not_read_file;		// не смогли прочитать
					pos += n;
				}
				close(file);

				bootstrap = ptr;
				argc--; argv++; // бинарный файл заменяет repl, скорректируем строку аргументов
			}
		}
#ifdef REPL
		// ol handle pipes/fifos as a text scripts.
#else
		// but olvm can't handle files with unknown size,
		//  because can't deserialize such things.
		else
			goto invalid_binary_script;
#endif
	}

	set_signal_handlers();

#if	HAVE_SOCKETS && defined(_WIN32)
	WSADATA wsaData;
	int sock_init = WSAStartup(MAKEWORD(2,2), &wsaData);
	if (sock_init  != 0) {
		E("WSAStartup failed with error: %d", sock_init);
		return 1;
	}
#endif

// larger uname
#if	HAVE_DLOPEN && defined(_WIN32)
	void* libuname = dlopen("ol-uname.dll", 0);
	if (libuname) {
		void* f = dlsym(libuname, "uname");
		if (f)
			uname = (int (*)(struct utsname*))f;
	}
#endif

#if defined(_WIN32)
	if (_isatty(STDIN_FILENO))
		win32_io_setup(); // <win32/unistd-ext.h>
#endif

	// configuring OL_HOME (if not exists):
#ifndef PREFIX
#define PREFIX "/usr"
#endif
	setenv("OL_HOME", PREFIX "/lib/ol", 0); // don't overwrite

	OL* olvm = OLVM_new(bootstrap);
	if (bootstrap != language) // was previously malloc'ed
		free(bootstrap);

	// so, let's rock?
	if (olvm) {
#if OLVM_TARVENV
		if (pvenv)
			OLVM_set_open(olvm, pvenv_open);
#else
		OLVM_SETOPEN
#endif
		word r = OLVM_run(olvm, argc, argv);
        // convert result to appropriate system value
        if (is_number(r))
            v = number(r);

		OLVM_delete(olvm);
	}

#if	HAVE_SOCKETS && defined(_WIN32)
	WSACleanup();
#endif

#ifdef DEBUG_COUNT_OPS
    for (int j = 0; j < 16; j++) {
        for (int i = 0; i < 16; i++) {
            printf("%8lld ", ops[i+j*16]);
        }
        printf("\n");
    }
#endif

ok:
	// if (autoremove) unlink(name);
	return (int) v;

// FAILS:
	char* message;

	can_not_stat_file:
	message = "File inaccessible or not found";
	goto fail;

	can_not_open_file:
	message = "Can't open file";
	goto fail;

	can_not_read_file:
	message = "Can't read file";
	goto fail;

#	ifndef REPL
	no_binary_script:
	message = "No binary script provided";
	errno = ENOENT;
	goto fail;

	invalid_binary_script:
	message = "Invalid binary script";
	errno = EILSEQ;
	goto fail;
#	endif

	can_not_allocate_memory:
	errno = ENOMEM;
	message = "Can't allocate memory";
	goto fail;

fail:;
	int e = errno;
	E("%s (errno: %d, %s)", message, errno, strerror(errno));
	// if (autoremove) unlink(name);
	return e;
}
#endif

#ifdef max
#undef max
#endif
#define max(a,b) ((a) > (b) ? (a) : (b))

// TODO: optional olvm without malloc
struct olvm_t*
OLVM_new(unsigned char* bootstrap)
{
	// если отсутствует исполнимый образ
	if (bootstrap == 0)
#ifdef REPL
		bootstrap = REPL;
#else
		return 0;
#endif

	// ===============================================================
	// создадим виртуальную машину:
	OL *handle = malloc(sizeof(OL));
	memset(handle, 0x0, sizeof(OL));

	word *fp;
	heap_t* heap = &handle->heap;

	// посчитаем сколько памяти нам надо для выполнения бинарника:
	word words = 0;
	word nobjs = count_fasl_objects(&words, bootstrap);
	if (nobjs == 0)
		goto fail;
	// words += (nobjs + 2); // add place for ptrs (уже не надо)

	// выделим память машине:
	// int max_heap_size = (W == 4) ? 4096 : 65535; // can be set at runtime

	// в соответствии со стратегией сборки 50*1.3-33*0.9 и так как данные в бинарнике
	// практически гарантированно "старое" поколение, выделим в два раза больше места.
	int required_memory_size = words + max(words/4, FREESPACE);
	size_t padding = GCPAD(NR + CR) + MEMPAD; // memory padding

	fp = heap->begin = (word*) malloc((required_memory_size + padding) * sizeof(word)); // at least one argument string always fits
	if (!heap->begin) {
		E("Error: can't allocate %d", (required_memory_size + padding) * sizeof(word));
		goto fail;
	}
	// память выделена, инициализируем кучу:
	heap->end = heap->begin + required_memory_size;
	heap->genstart = heap->begin;
	heap->padding = padding;

	// дефолтный сборщик мусора
	heap->gc = OLVM_gc;

	// handle->max_heap_size = max_heap_size;

	// Десериализация загруженного образа в объекты
	// обязательно выделить n+1 объектов,
	// последнее место зарезервировано для конструктора
	word *ptrs = alloca((nobjs + 1) * sizeof(word));

	// этот вектор содержит "неправильные" ссылки в смысле модели памяти ol,
	// которые указывают вперед по куче, а не назад. но так как на него никто
	// не указывает, то этот объект будет спокойно удален во время первой же
	// полной сборки кучи (которую стоило бы сделать в deserialize()).
	fp = deserialize(ptrs, nobjs, bootstrap, fp);
	if (!fp) {
		E("Error: invalid bootstrap");
		goto fail;
	}

	// подготовим регистры и закрепленные объекты (regs + pins):
#ifndef OLVM_NOPINS
	handle->cr = CR;
	handle->pin = malloc(CR * sizeof(word));
	if (!heap->begin) {
		E("Error: can't allocate %d", CR * sizeof(word));
		goto fail;
	}
    word* pin = handle->pin;
	for (ptrdiff_t i = 0; i < CR; i++)
		pin[i] = IFALSE;
#endif

	// регистры виртуальной машины
	word* reg = handle->reg;
	for (ptrdiff_t i = 0; i < NR; i++)
		reg[i] = IFALSE;
	reg[0] = IFALSE; // MCP - master control program (NO mcp for now)
	reg[3] = IEXIT;  // continuation, just finish job
	reg[4] = INULL;  // first argument

#ifndef OLVM_NOPINS
	handle->ffpin = 4; // first four pins are used internally
#endif

	// i/o overrides
	handle->open = os_open;
	handle->close = os_close;
	handle->read = os_read;
	handle->write = os_write;
	handle->stat = os_stat;

//	handle->exit = exit;

	// if no autorun points found, just run last one lambla (like old behavior)
	// точка входа в программу: последняя лямбда загруженного образа (λ (args))
	// TODO: make configurable
	if (ptrs[nobjs] == INULL) {
		handle->this = ptrs[nobjs-1]; // (construction constructors main args)
		handle->arity = 2; // 1 argument
	}
	// точка входа в программу - бутстрап, обрабатывающий список автовыполняемых функций
	else {
		// objects: список автостартующих функций (лежит в [nobjs]),
		//          заменяет в загрузчике последнюю лямбду
		// args: аргументы, которые предоставляет функция OLVM_run
		//       (а мы пока положим #null)
		//
		// (define (construction args objects)
		//    (unless (null? objects)
		//       (construction args (cdr objects))
		//       ((car objects) args)))
		//
		// (fasl-encode construction):
		unsigned char construction[] = { 2,16,12,11,3,0,7,1,1,2,6,2,6,4,17,2,16,23,11,1,
			0,18,1,1,2,4,52,4,5,1,1,4,3,1,1,3,4,2,5,2,17,2,16,31,11,4,0,26,80,5,18,0,53,
			5,7,3,17,5,1,2,5,4,3,3,9,7,5,2,6,4,205,7,24,7,17,1,17,2,1,2,1,17,2,4,1,0 };

		// подсчет количества слов и объектов в этом коде
		word wc = 0;
		word no = count_fasl_objects(&wc, construction);
		// assert (w < NUMPAD), etc.
		word *p = new(TBYTEVECTOR, no + 1, 0);
		fp = deserialize(&p[1], no, construction, fp);

		handle->this = p[no]; // (construction constructors main args)
		handle->reg[5] = ptrs[nobjs];
		handle->arity = 3; // two arguments
	}

	// теперь все готово для запуска главного цикла виртуальной машины
	heap->fp = fp;
	return handle;

fail:
	if (heap->begin)
		free (heap->begin);
	heap->begin = 0;
#ifndef OLVM_NOPINS
	if (handle->pin)
		free (handle->pin);
	handle->pin = 0;
#endif
	OLVM_delete(handle);
	return 0;
}

void OLVM_delete(OL* ol)
{
	if (sandboxp)
		return;

#ifndef OLVM_NOPINS
	free(ol->pin);
#endif
	free(ol->heap.begin);
	free(ol);
}

// ===============================================================
word
OLVM_run(OL* ol, int argc, char** argv)
{
#ifndef __EMSCRIPTEN__
	int r = setjmp(ol->ret);
	if (r != 0) {
		// TODO: restore old values
		// TODO: if IFALSE - it's error
		return ol->reg[3]; // returned value
	}
#endif

	// подготовим аргументы:
	word userdata = ol->reg[4];
	{
		word* fp = ol->heap.fp;

		argv += argc - 1;
		for (ptrdiff_t i = argc; i > 0; i--, argv--) {
			char *pos = (char*)(fp + 1);
			char *v = *argv;
			while ((*pos = *v++) != 0)
				pos++;
			int length = pos - (char*)(fp + 1);
			// todo: check the memory!
			if (length > 0) // если есть что добавить
				userdata = (word) cons (new_alloc(TSTRING, length), userdata);
		}

		ol->heap.fp = fp;
	}
	ol->reg[4] = userdata;

	sandboxp = 0;  // static variable

#ifndef __EMSCRIPTEN__
	longjmp(ol->ret,
		(int)runtime(ol));
#else
	runtime(ol);
	return ol->reg[3];
#endif
}

word
OLVM_evaluate(OL* ol, word function, int argc, word* argv)
{
#ifndef __EMSCRIPTEN__
	int r = setjmp(ol->ret);
	if (r != 0) {
		return ol->reg[3];
	}
#endif
	if (argc + 3 > NR) {
		E("arguments count exceeds the maximum value (%d)", NR);
		return IFALSE;
	}

	// функция к выполнению
	word this = function;

	// подготовим аргументы
	unsigned short acc = 1;
	for (ptrdiff_t i = 0; i < argc; i++)
		ol->reg[acc++ + 3] = (word)argv[i];

	ol->reg[3] = IRETURN; // continuation

	// теперь все готово для запуска главного цикла виртуальной машины
	ol->this = this;
	ol->arity = acc;

#ifndef __EMSCRIPTEN__
	longjmp(ol->ret,
		(int)runtime(ol));
#else
	runtime(ol);
	return ol->reg[3];
#endif
}

#ifndef OLVM_NOPINS
// [0..3] - errors
// 0 means "no space left"
// 1 means "not a pinnable object"
OLVM_PUBLIC
size_t OLVM_pin(struct olvm_t* ol, word ref)
{
	if (ref == IFALSE)
		return 1; // #false is not a pinnable object
	size_t id = ol->ffpin;
	size_t cr = ol->cr;
	for (; id < cr; id++) {
		if (ol->pin[id] == IFALSE)
			goto ok;
	}

	// нету места, попробуем увеличить
	size_t ncr = cr + cr / 3 + 1;
    ol->heap.gc(ol, ncr - cr);

	word* p = realloc(ol->pin, ncr * sizeof(word));
	if (!p)
		return 0; // no space left
	ol->pin = p;
	ol->cr = ncr;

    ol->heap.end -= ncr - cr;
	ol->heap.padding += ncr - cr;

	for (size_t i = id; i < ncr; i++)
		p[i] = IFALSE;

ok:
	ol->pin[id] = ref;
	ol->ffpin = id + 1;
	return id;
}

OLVM_PUBLIC
word OLVM_deref(struct olvm_t* ol, size_t p)
{
	size_t id = p;
	if (id > 3 && id < ol->cr)
		return ol->pin[id];
	else
		return IFALSE;
}

OLVM_PUBLIC
word OLVM_unpin(struct olvm_t* ol, size_t p)
{
    word re = IFALSE;
    size_t id = p;
    if (id > 3 && id < ol->cr) {
        re = ol->pin[id];
        ol->pin[id] = IFALSE;

		if (ol->ffpin > id)
			ol->ffpin = id;
    }

    return re;
}
#endif

// ffi callbacks support
word OLVM_apply(struct olvm_t* ol, word object, word args)
{
	ol->this = object; // lambda для обратного вызова
//	ol->ticker = ol->bank ? ol->bank : 999;
//	ol->bank = 0;
	// assert (is_reference(ol->this));
	// assert (reference_type(ol->this) != TTHREAD);

	// надо сохранить значения, иначе их уничтожит GC
	// todo: складывать их в память! и восстанавливать оттуда же
	word* reg = ol->reg;

//	reg[NR + 0] = reg[0]; // не надо, mcp
//	reg[NR + 1] = reg[1]; // не надо
//	reg[NR + 2] = reg[2]; // не надо
	reg[NR + 3] = reg[3]; // continuation/result

	// вызовем колбек:
//	reg[0] = IFALSE;  // не надо, продолжаем использовать mcp
	reg[3] = IRETURN; // команда выхода из колбека
	ol->arity = 1;

	size_t a = 4;
	while (args != INULL) {
		reg[a] = car(args);
		a++,
		ol->arity++;
		args = cdr(args);
	}

	runtime(ol);

	word r = reg[3]; // callback result
	// возврат из колбека,
	// reg, NR могли измениться
	reg[3] = reg[NR + 3];
//	reg[2] = reg[NR + 2]; // не надо
//	reg[1] = reg[NR + 1]; // не надо
//	reg[0] = reg[NR + 0]; // не надо, продолжаем использовать MCP

	return r;
}


/*!
 *
 * ## Related links:
 * - http://people.csail.mit.edu/jaffer/Scheme
 * - http://srfi.schemers.org/
 * - http://groups.csail.mit.edu/mac/projects/scheme/
 * - http://www.s48.org/
 * - http://www.call-cc.org/
 * - http://www.scheme.com/tspl4/
 */
// -->
