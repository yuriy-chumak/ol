/*!
 * ## FFI - Foreign Function Interface
 *
 * A ForeignFunctionInterface (FFI) is an interface that allows calling code written
 * in one programming language, from another that is neither a superset nor a subset.
 *
 * Тут у нас реализация ffi механизма. Примеры в lib/opengl.scm, lib/sqlite.scm, etc.
 *
 * btw, FFI is also Fatal Familial Insomnia. Hmmm...
 */

/*!
 * ### Source file: extensions/ffi.c
 */

// Libc and Unicode: http://www.tldp.org/HOWTO/Unicode-HOWTO-6.html
//
// Design Issues for Foreign Function Interfaces
// http://autocad.xarch.at/lisp/ffis.html
//
// Utf-8 https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt

#ifndef OLVM_FFI
// ffi have no sense without dlopen/dlsym
#define OLVM_FFI HAVE_DLOPEN
#endif

#if OLVM_FFI

// defaults:
#ifndef OLVM_CALLABLES
#define OLVM_CALLABLES 1
#endif

#ifndef OLVM_FFI_VECTORS
#define OLVM_FFI_VECTORS 1
#endif


// use virtual machine declaration from the olvm source code
#ifndef __OLVM_H__
#include <ol/vm.h>
#endif

#define unless(...) if (! (__VA_ARGS__))
#define max(a,b) ((a) > (b) ? (a) : (b))

#include <string.h>
#include <unistd.h>

// alloca
#if defined(__linux__) || defined(__APPLE__)
#include <alloca.h>
#endif
#if defined(_WIN32)
#include <malloc.h>
#endif

#ifndef __GNUC__
#define __builtin_alloca alloca
#define __builtin_memcpy memcpy
#endif

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#ifdef __EMSCRIPTEN__
#ifndef __WAJIC__
#include <emscripten.h>
#endif
#endif

#include <sys/mman.h> // we have own win32 implementation

#ifndef WARN_ALL
#	ifdef __clang__
		// clang is not a primary compiler and clang have no ability to remove
		// only one warning instance. I don't want to add SEVEN lines of code
		// to disable only ONE warning that in fact is not a warning but fully
		// planned behavior. so disable all same warnings to the release build.
#		pragma clang diagnostic ignored "-Wtautological-constant-out-of-range-compare"
#	endif
#	pragma GCC diagnostic ignored "-Wparentheses"
#	pragma GCC diagnostic ignored "-Wunused-label"
#endif

// platform filters
#if __x86_64__ && __unix__
#	define IFx86_64_unix(...) __VA_ARGS__
#else
#	define IFx86_64_unix(...)
#endif

#if __x86_64__
#	define IFx86_64(...) __VA_ARGS__
#else
#	define IFx86_64(...)
#endif

#ifdef __aarch64__
#	define IFaarch64(...) __VA_ARGS__
#else
#	define IFaarch64(...)
#endif

#ifdef __aarch64__
#	define ONLYaarch64
#else
#	define ONLYaarch64 __attribute__((unused))
#endif

#ifdef __mips__
#	define IFmips32(...) __VA_ARGS__
#else
#	define IFmips32(...)
#endif


// ffi type system
#define TVOID         (48)

#define TBOOL         (60) // 0 is #false, everything else is #true
#define TUNKNOWN      (62) // direct sending argument without any processing
#define TANY          (63) // automatic conversion based on actual argument type, can be overriden

#define TFLOAT        (46)
#define TDOUBLE       (47)

#define TINT8         (50)
#define TINT16        (51)
#define TINT32        (52)
#define TINT64        (53)
// tbd: 54 for int128_t

#define TUINT8        (55)
#define TUINT16       (56)
#define TUINT32       (57)
#define TUINT64       (58)
// tbd: 59 for uint128_t

#define TMASK      0x00FFF

#define TCDECL     0x01000
//efine TSYSCALL    // OS/2, not supported/required
//efine TOPTLINK    // VisualAge, not supported/required
//efine TPASCAL     // Pascal, not supported/required
#define TSTDCALL   0x02000
#define TFASTCALL  0x03000
//efine TVECTORCALL // VS2013, not supported/required
//efine TDELPHI     // Delphi, not supported/required
//efine TWATCOM     // Watcom, not supported/required
//efine TSAFECALL   // Delphi/F-Pascal, not supported
//efine TTHISCALL   // c++, use in toplevel code

#define FFT_PTR    0x10000 // todo: change to 0x100 ? или другой уникальный небольшой тип
#define FFT_REF    0x20000 // todo: change to 0x200 ? или другой уникальный небольшой тип
// possible speedup - FFT_REF = 0x30000 (include FFT_PTR)

// -----------------
// sizeof(intmax_t): // same as long long
//	arm, armv7a, armv8a, arm64: 8
//	avr: 8
//	risc-v 32, risc-v 64: 8
//	wasm: 8
//	x86, x86_64 (incl msvc): 8
//	kvx: 8
//	mips, mips64: 8
//	msp430: 8
//	ppc, ppc64, ppc64le: 8
//	raspbian, arduino: 8

#if defined(_WIN32)
#	define EXPORT __declspec(dllexport)
#elif defined(__EMSCRIPTEN__)
#	define EXPORT EMSCRIPTEN_KEEPALIVE
#else
#	define EXPORT __attribute__ ((__visibility__("default")))
#endif

#ifdef _WIN32
	// Windows uses UTF-16LE encoding internally as the memory storage
	// format for Unicode strings, it considers this to be the natural
	// encoding of Unicode text. In the Windows world, there are ANSI
	// strings (the system codepage on the current machine, subject to
	// total unportability) and there are Unicode strings (stored
	// internally as UTF-16LE).
	// We just use UTF-16LE subset for now.
	typedef WCHAR widechar;
#else
	typedef wchar_t widechar;
#endif

// ------------------
// maximal applicable return type of called functions
typedef int64_t ret_t;

#define IDF(...)   (__VA_ARGS__) // Identity function
#define TALIGN(ptr, type) \
	__builtin_choose_expr(sizeof(type) > 1, ptr = ((ptr + sizeof(type) - 1) & -sizeof(type)), (void)0)

// ------------------------------------------------------------------------------------
// -- assembly part -------------------------------------------------------------------

// C preprocessor trick, a MAP
// http://jhnet.co.uk/articles/cpp_magic
// http://stackoverflow.com/questions/319328/writing-a-while-loop-in-the-c-preprocessor
// http://stackoverflow.com/questions/6707148/foreach-macro-on-macros-arguments
#define EVAL0(...) __VA_ARGS__
#define EVAL1(...) EVAL0 (EVAL0 (EVAL0 (__VA_ARGS__)))
#define EVAL2(...) EVAL1 (EVAL1 (EVAL1 (__VA_ARGS__)))
#define EVAL3(...) EVAL2 (EVAL2 (EVAL2 (__VA_ARGS__)))
#define EVAL4(...) EVAL3 (EVAL3 (EVAL3 (__VA_ARGS__)))
#define EVAL5(...) EVAL4 (EVAL4 (EVAL4 (__VA_ARGS__)))
#define EVAL(...)  EVAL5 (EVAL5 (EVAL5 (__VA_ARGS__)))

#define MAP_END(...)
#define MAP_OUT

#define MAP_GET_END() 0, MAP_END
#define MAP_NEXT0(test, next, ...) next MAP_OUT
#define MAP_NEXT1(test, next) MAP_NEXT0 (test, next, 0)
#define MAP_NEXT(test, next)  MAP_NEXT1 (MAP_GET_END test, next)

#define MAP0(f, x, peek, ...) f(x) MAP_NEXT (peek, MAP1) (f, peek, __VA_ARGS__)
#define MAP1(f, x, peek, ...) f(x) MAP_NEXT (peek, MAP0) (f, peek, __VA_ARGS__)
#define MAP(f, ...) EVAL (MAP1 (f, __VA_ARGS__, (), 0))
// end of C preprocessor trick

#define NEWLINE(x) x "\n\t"
#define __ASM__(...) __asm__(MAP(NEWLINE, __VA_ARGS__))

#ifndef __GNUC__
#define __asm__ asm
#endif

// platform defines:
// https://sourceforge.net/p/predef/wiki/Architectures/
//
// builtin assembly:
// http://locklessinc.com/articles/gcc_asm/
// http://www.agner.org/optimize/calling_conventions.pdf
// https://en.wikibooks.org/wiki/Embedded_Systems/Mixed_C_and_Assembly_Programming

// http://www.angelcode.com/dev/callconv/callconv.html

// ------------------------------------------
#if __x86_64__ // x86-64 (LP64/LLP64)

# if _WIN64 // Windows
// The x64 Application Binary Interface (ABI) uses a four register fast-call
// calling convention by default. Space is allocated on the call stack as a
// shadow store for callees to save those registers. There is a strict one-to-one
// correspondence between the arguments to a function call and the registers
// used for those arguments. Any argument that doesn’t fit in 8 bytes, or is
// not 1, 2, 4, or 8 bytes, must be passed by reference. There is no attempt
// to spread a single argument across multiple registers. The x87 register
// stack is unused. It may be used by the callee, but must be considered volatile
// across function calls. All floating point operations are done using the 16
// XMM registers. Integer arguments are passed in registers RCX, RDX, R8, and
// R9. Floating point arguments are passed in XMM0L, XMM1L, XMM2L, and XMM3L.
// 16-byte arguments are passed by reference. Parameter passing is described
// in detail in Parameter Passing. In addition to these registers, RAX, R10,
// R11, XMM4, and XMM5 are considered volatile. All other registers are non-volatile.
//
// The caller is responsible for allocating space for parameters to the callee,
// and must always allocate sufficient space to store four register parameters,
// even if the callee doesn’t take that many parameters.

// https://msdn.microsoft.com/en-us/library/9z1stfyw.aspx
// rcx, rdx, r8, r9 (xmm0, xmm1, ..., xmm3) are arguments,
// RAX, R10, R11, XMM4, and XMM5 are considered volatile

// http://www.gamasutra.com/view/news/171088/x64_ABI_Intro_to_the_Windows_x64_calling_convention.php

ret_t win64_call(word argv[], long argc, void* function, long type);
	// rcx - argv
	// edx - argc
	// r8  - function
	// r9d - type
__ASM__("win64_call:", "_win64_call:",  // "int $3",
	"pushq %rbp",
	"movq  %rsp, %rbp",

	"pushq %r9",
	"andq  $-16, %rsp", // выравняем стек по 16-байтовой границе

	// get count of arguments
	"xor   %rax, %rax",
	"movl  %edx, %eax",

	// get last parameter
	"leaq  -8(%rcx, %rax,8), %r10",
	"subq  $4, %rax",
	"jbe   4f",

	// довыравняем стек, так как:
	//  The stack pointer must be aligned to 16 bytes in any region of code
	//  that isn’t part of an epilog or prolog, except within leaf functions.
	"movq  %rax, %rdx",
	"andq  $1, %rdx",
	"leaq  -16(%rsp,%rdx,8), %rsp",

"1:",
	"pushq (%r10)",
	"subq  $8, %r10",
	"decq  %rax",
	"jnz   1b",

	// 4. заполним обычные rcx, rdx, ... не проверяя количество аргументов и их
	// тип, так будет быстрее
"4:",
	"movq  %r8, %rax",
	"movsd 24(%rcx), %xmm3", "mov  24(%rcx), %r9",
	"movsd 16(%rcx), %xmm2", "mov  16(%rcx), %r8",
	"movsd  8(%rcx), %xmm1", "mov  8(%rcx), %rdx",
	"movsd  0(%rcx), %xmm0", "mov  0(%rcx), %rcx",
	"subq $32, %rsp", // extra free space for call
	"call *%rax",
	// вернем результат
	"cmpl $46, -8(%rbp)", // TFLOAT
	"je   52f",
	"cmpl $47, -8(%rbp)", // TDOUBLE
	"je   52f",
"9:",
	"leave",
	"ret",

// "51:",
// 	"cvtss2sd %xmm0, %xmm0", // float->double
"52:",
	"movsd %xmm0, (%rsp)",
	"pop   %rax", // можно попать, так как уже пушнули один r9 вверху (оптимизация)
	"jmp   9b");
# endif

# if __unix__ || __linux__ || __APPLE__ // System V (unix, linux, osx)
static __attribute((__naked__))
ret_t nix64_call(word argv[], double ad[], long i, long d, long mask, void* function, long type)
	            //    rdi            rsi      edx     ecx       r8          r9           16(rbp)
{
__ASM__(//"int $3",
	"pushq %rbp",
	"movq  %rsp, %rbp",

	"movq  %rcx, %rax", // vararg support (rbp-8)
	"addq  %rdx, %rax",
	"pushq %rax",

	"pushq %r9", // function (rbp-16)
	"andq  $-16, %rsp", // stack align

	// 1. если есть флоаты, то заполним их
"1:",
	"testq %rcx, %rcx",
	"jz    2f",
	"movsd  0(%rsi), %xmm0",
	"movsd  8(%rsi), %xmm1",
	"movsd 16(%rsi), %xmm2",
	"movsd 24(%rsi), %xmm3",
	// "cmpq  $4, %rcx", // speedup (todo: need to proof)
	// "jb    2f",
	"movsd 32(%rsi), %xmm4",
	"movsd 40(%rsi), %xmm5",
	"movsd 48(%rsi), %xmm6",
	"movsd 56(%rsi), %xmm7",
//	"cmpq  $9,%rcx", // temp for debug purposes
//	"jne   2f",      // temp
//	"int   $3",      // temp
	// 2. проверим на "стековые" аргументы (todo: сделать быструю проверку как у arm64)
"2:",
	"xorq  %rax, %rax",
	"subq  $8, %rcx",
	"cmovs %rax, %rcx", // = max(rcx-8, 0)
	"cmpl  $6, %edx",
	"cmova %rdx, %rax", //
	"addq  %rcx, %rax", // = max(rdx-6, 0) + max(rcx-8, 0)
	"testq %rax, %rax",
	"jz    4f", // no agruments for push to stack
	// забросим весь оверхед в стек с учетом очередности и маски
"3:",
	"andq  $1, %rax",
	"leaq  -16(%rsp,%rax,8),%rsp", // выравнивание стека по 16-байтной границе

	"leaq  -8(%rdi,%rdx,8), %rax",
	"leaq  56(%rsi,%rcx,8), %rsi", // last float argument  (if any) 56=8*8-8

	"subq  $6, %rdx",
"31:",
	"shrq  %r8",
	"jc    33f", // bit 0 was set, so push float value
"32:", // push fixed
	"cmpq  $0, %rdx",
	"jle   31b", // нету больше аргументов для fixed пуша
	"pushq (%rax)",
	"subq  $8, %rax",
	"decq  %rdx",
	"jnz   31b",
	"cmpq  $0, %rcx",
	"jle   4f",
	"jmp   31b",
"33:", // push float
	"cmpq  $0, %rcx",
	"jle   31b", // нету больше аргументов для float пуша
	"pushq (%rsi)",
	"subq  $8, %rsi",
	"decq  %rcx",
	"jnz   31b",
	"cmpq  $0, %rdx",
	"jle   4f",
	"jmp   31b",

	// 4. не проверяем количество аргументов, так будет быстрее
"4:",
	"movq  40(%rdi), %r9",
	"movq  32(%rdi), %r8",
	"movq  24(%rdi), %rcx",
	"movq  16(%rdi), %rdx",
	"movq   8(%rdi), %rsi",
	"movq   0(%rdi), %rdi",

	"movq  -8(%rbp), %rax",
	"call *-16(%rbp)",

	// вернем результат
	"cmpl $46, 16(%rbp)", // TFLOAT
	"je   6f",
	"cmpl $47, 16(%rbp)", // TDOUBLE
	"je   6f",
"9:",
	"leave",
	"ret",

// "5:",
// 	"cvtss2sd %xmm0, %xmm0", // float->double
"6:",
	"movsd %xmm0, (%rsp)",
	"popq  %rax", // corresponded push not required (we already pushed %r9)
	"jmp   9b");
}

// RDI, RSI, RDX, RCX (R10 in the Linux kernel interface[17]:124), R8, and R9
// while XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6 and XMM7 are used for floats
# endif

// ------------------------------------------
#elif __i386__ // ILP32
// value returned in the edx:eax

// CDECL / STDCALL
// The cdecl (which stands for C declaration) is a calling convention
// that originates from the C programming language and is used by many
// C compilers for the x86 architecture.[1] In cdecl, subroutine
// arguments are passed on the stack. Integer values and memory
// addresses are returned in the EAX register, floating point values in
// the ST0 x87 register. Registers EAX, ECX, and EDX are caller-saved,
// and the rest are callee-saved. The x87 floating point registers ST0
// to ST7 must be empty (popped or freed) when calling a new function,
// and ST1 to ST7 must be empty on exiting a function. ST0 must also be
// empty when not used for returning a value.
//
// В нашем случае мы так или иначе восстанавливаем указатель стека, так что
// функция x86_call у нас будет универсальная cdecl/stdcall
// 
ret_t x86_call(word argv[], long i, void* function, long type);
	// all variables are in stack
__ASM__("x86_call:", "_x86_call:", //"int $3",
	"pushl %ebp",
	"movl  %esp, %ebp",
	"andl  $-16, %esp", // need to align stack

	"movl  12(%ebp), %ecx", // i
	"test  %ecx, %ecx",
	"jz    1f",

	// gcc may use aligned SSE instructions,
	"movl  %ecx, %eax",
	"andl  $3, %eax",
	"leal  -16(%esp,%eax,4),%esp",

	"movl  8(%ebp), %eax",  // argv
	"leal  -4(%eax,%ecx,4),%eax",
"0:",
	"pushl (%eax)",
	"subl  $4, %eax",
	"decl  %ecx",
	"jnz   0b",
"1:",
	"call  *16(%ebp)",      // function

	"movl  20(%ebp), %ecx", // проверка возвращаемого типа
	"cmpl  $46, %ecx",      // TFLOAT
	"je    3f",
	"cmpl  $47, %ecx",      // TDOUBLE
	"je    4f",
"9:",
	"leave", // == "movl  %ebp, %esp", "popl %ebp",
	"ret",

// с плавающей точкой float
"3:", // float
	"pushl %eax",
	"fstps (%esp)",
	"popl  %eax",
	"jmp   9b",
// с плавающей точкой double
"4:", // double
	"pushl %edx",
	"pushl %eax",
	"fstpl (%esp)",
	"popl  %eax",
	"popl  %edx",
	"jmp   9b");


// ------------------------------------------
#elif __aarch64__

// https://developer.arm.com/documentation/102374/0102/Procedure-Call-Standard
// calling convention: IHI0055B_aapcs64.pdf
// r0-r7 arguments, integer
// x8 - Indirect result location register, TODO
// v0-v7 arguments, floating
// r9-r15: scratch registers
// v16-v31: scratch registers
// stack is 16-byte aligned at all times
// notes:
//   don't use r16, r17, r18
//   don't forget "sudo DevToolsSecurity -enable" when debugging with lldb
// https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst#parameter-passing

// "static __attribute((__naked__))"" is unsupported, so
ret_t arm64_call(word argv[], double ad[], long i, long d, char* extra, void* function, long type, long e);
				//    x0             x1        x2      x3        x4           x5             x6        x7
__ASM__(
".global arm64_call");
__ASM__("arm64_call:", "_arm64_call:", // "brk #0",
	"stp  x29, x30, [sp, -16]!", // fp + lr
	"stp  x10, x26, [sp, -16]!",
	"mov  x29, sp",
	"mov  x26, x6",

	"mov x9, x5",
	//"brk #0",

	// будем заполнять регистры с плавающей запятой по 4 или 8 (в целях оптимизации)
	"cbz x3, 0f",  // is x3 == 0 goto Lno_more_floats
	"ldr d0, [x1]",
	"ldr d1, [x1, #8]",
	"ldr d2, [x1, #16]",
	"ldr d3, [x1, #24]",

	"cmp x3, #4", // question: is this a speedup or better just copy regs without jump?
	"ble 0f", // goto Lno_more_floats
	"ldr d4, [x1, #32]",
	"ldr d5, [x1, #40]",
	"ldr d6, [x1, #48]",
	"ldr d7, [x1, #56]",

"0:", // Lno_more_floats
	"cmp x2, #8", // у нас есть что складывать в стек?
	"blt 4f",
	"mov x8, #8",

	"add x4, x0, #64", // &argv[8]
	"sub x7, x2, #8",  // (i - 8)

	// выравняем стек (по 16-байтовой границе!)
	"orr x10, x7, #1", // точно сделаем +8, который сложится с x8 в +16
	"madd x10, x10, x8, x8", // +8 или +16 (не +0 или +8, так проще. а стек, он резиновый)
	"sub sp, sp, x10", // выделим в стеке место под аргументы

	"mov x4, x4", // x4 - первый элемент стековых данных (todo: can be removed)
	"mov x3, sp",
"6:", // Lpush
	"cbz x7, 4f", // больше нечего пушить, goto Lgo
	"ldr x5, [x4], #+8",
	"str x5, [x3], #+8",
	"sub x7, x7, #1",
	"b 6b",// goto Lpush

// done. go
"4:", // Lgo
	// а теперь обычные целочисленные аргументы, не стековые
	"cbz x2, 7f", // Lcall
	// "cmp x2, #4", // оптимизация (возможно не нужна, надо тестить)
	// "ble 8f", // Lless2
	"ldr x7, [x0, #56]",
	"ldr x6, [x0, #48]",
	"ldr x5, [x0, #40]",
	"ldr x4, [x0, #32]",
"8:", // Lless2
	"ldr x3, [x0, #24]",
	"ldr x2, [x0, #16]",
	"ldr x1, [x0, #8]",
	"ldr x0, [x0]",

"7:", // Lcall
	"blr x9", // assert (SP mod 16 == 0). The stack must be quad-word aligned.
	"mov sp, x29",
	"str x0, [sp]",

	"cmp x26, #46",      // TFLOAT
	"beq 6f",
	"cmp x26, #47",      // TDOUBLE
	"beq 7f",
"0:",
	"ldp  x0, x26, [sp], 16",
	"ldp x29, x30, [sp], 16",
	"ret",
	
"6:", // float (4)
	"str s0, [sp]",
	"b 0b",
"7:", // double (8)
	"str d0, [sp]",
	"b 0b");

// ------------------------------------------
#elif __arm__
// http://ru.osdev.wikia.com/wiki/Категория:Архитектура_ARM
// https://msdn.microsoft.com/ru-ru/library/dn736986.aspx - Обзор соглашений ABI ARM (Windows)
// Procedure Call Standard for the ARM®  Architecture
//  http://infocenter.arm.com/help/topic/com.arm.doc.ihi0042f/IHI0042F_aapcs.pdf

# ifndef __ARM_PCS_VFP
// gcc-arm-linux-gnueabi: -mfloat-abi=softfp options (and -mfloat-abi=soft ?)

static __attribute((__naked__)) // do not remove "naked", arm32 require this form of function!
ret_t arm32_call(word argv[], long i, void* function)
{
__ASM__(//"arm32_call:_arm32_call:",
	// "BKPT",
	// !!! stack must be 8-bytes aligned, so let's push even arguments count
	"push {r4, r5, r6, lr}",
	"mov r4, sp", // save sp

	// note: at public interface stack must be double-word aligned (SP mod 8 = 0).
	// finally, sending regular (integer) arguments
	"cmp r1, #4",  // if (i > 4)
	"ble .Lnoextraregs",      // todo: do the trick -> jmp to corresponded "ldrsh" instruction based on r3 value

	// "add r5, r0, r1, asl #2",
	"lsl r5, r1, #2",
	"add r5, r0, r5",
	"sub r5, r5, #4",

	// stack alignment
	"and r3, r1, #1",         // попадет ли новый стек на границу двойного слова?
	"sub sp, sp, r3, asl #2", // если да, то скорректируем стек на 4 байта вниз.

".Lextraregs:", // push argv[i]
	"ldr r3, [r5]",
	//"rev16 r3, r3",
	"push {r3}",
	"sub r1, r1, #1",
	"sub r5, r5, #4",
	"cmp r1, #4",
	"bgt .Lextraregs",

// 	"mov r5, r0",
// ".Lextraregs:", // push argv[i]
// 	"mov r3, 0",
// 	"push {r3}",
// 	"sub r1, r1, #1",
// 	"add r5, r5, #4",
// 	"cmp r1, #4",
// 	"bgt .Lextraregs",

".Lnoextraregs:",
	"mov r5, r2", // function

	"ldr r3, [r0,#12]", // save all 4 registers without checking
	"ldr r2, [r0, #8]",
	"ldr r1, [r0, #4]",
	"ldr r0, [r0, #0]",
	// call the function

#ifdef __ARM_ARCH_4T__ // armv4t
	"mov lr, pc",
	"bx r5",
#else
	"blx r5",
#endif
	"mov sp, r4", // restore sp

	// all values: int, long, float and double returning in r0+r1
	"pop {r4, r5, r6, pc}");
//	"bx lr");
}

# else
// gcc-arm-linux-gnueabihf: -mfloat-abi=hard and -D__ARM_PCS_VFP options

// __ARM_PCS_VFP, __ARM_PCS
// __ARM_ARCH_7A__
//
static __attribute((__naked__)) // do not remove "naked", arm32 require this form of function!
ret_t arm32_call(word argv[], float af[], long i, long f, void* function, long type) {
__ASM__(// "arm32_call:_arm32_call:",
	// "BKPT",
	// r0: argv, r1: af, r2: i, r3: ad, f: [sp, #12], g: [sp, #16]
	// r4: saved sp
	// r5: temporary
	"stmfd   sp!, {r4, r5, lr}",

	// check floats
#ifdef __ARM_PCS_VFP // gnueabihf, -mfloat-abi=hard
	"cmp r3, #0",  // f (count of floats)
	"beq .Lnofloats",
	// будем заполнять регистры с плавающей запятой по 4 или 8 (в целях оптимизации)
	// https://developer.arm.com/technologies/floating-point
	"vldr.32 s0, [r1]",
	"vldr.32 s1, [r1, #4]",
	"vldr.32 s2, [r1, #8]",
	"vldr.32 s3, [r1, #12]",
	"cmp r3, #4",
	"ble .Lnofloats",
	"vldr.32 s4, [r1, #16]",
	"vldr.32 s5, [r1, #20]",
	"vldr.32 s6, [r1, #24]",
	"vldr.32 s7, [r1, #28]",
	"cmp r3, #8",
	"ble .Lnofloats",
	"vldr.32 s8, [r1, #32]",
	"vldr.32 s9, [r1, #36]",
	"vldr.32 s10, [r1, #40]",
	"vldr.32 s11, [r1, #44]",
	"vldr.32 s12, [r1, #48]",
	"vldr.32 s13, [r1, #52]",
	"vldr.32 s14, [r1, #56]",
	"vldr.32 s15, [r1, #60]",
	// todo: s16-s32 must be preserved across subroutine calls (ARM ABI)!
	//  save this situation as flag and restore this regs after function call if so
	// "cmp r3, 16",
	// "ble .Lnofloats",
	// "vldr.32 s16, [r1, #48]",
	// "vldr.32 s17, [r1, #52]",
	// "vldr.32 s18, [r1, #56]",
	// "vldr.32 s19, [r1, #60]",
	// "vldr.32 s20, [r1, #48]",
	// "vldr.32 s21, [r1, #52]",
	// "vldr.32 s22, [r1, #56]",
	// "vldr.32 s23, [r1, #60]",
	// "vldr.32 s24, [r1, #48]",
	// "vldr.32 s25, [r1, #52]",
	// "vldr.32 s26, [r1, #56]",
	// "vldr.32 s27, [r1, #60]",
	// "vldr.32 s28, [r1, #48]",
	// "vldr.32 s29, [r1, #52]",
	// "vldr.32 s30, [r1, #56]",
	// "vldr.32 s31, [r1, #60]",
".Lnofloats:",
#endif

	"mov r4, sp", // save sp
	// note: at public interface stack must be double-word aligned (SP mod 8 = 0).
	"sub r1, sp, r2, asl #2",// try to predict stack alighnment (к текущему стеку прибавим количество аргументов * размер слова)
	"and r1, r1, #4", // попадает ли стек на границу слова?
	"sub sp, sp, r1", // если да, то на слово его и опустим
	// finally, sending regular (integer) arguments
	"cmp r2, #4",  // if (i > 4)
	"ble .Lnoextraregs",      // todo: do the trick -> jmp to corrsponded "ldrsh" instruction based on r3 value
	"add r1, r0, r2, asl #2",
	"sub r1, r1, #4",
".Lextraregs:", // push argv[i]
	"ldr r3, [r1]",
	"push {r3}",
	"sub r2, r2, #1",
	"sub r1, r1, #4",
	"cmp r2, #4",
	"bgt .Lextraregs",
	// todo: arrange stack pointer to dword

".Lnoextraregs:",
	"ldr r3, [r0,#12]", // save all 4 registers without checking
	"ldr r2, [r0, #8]",
	"ldr r1, [r0, #4]",
	"ldr r0, [r0, #0]",
	// call the function
	"ldr r5, [r4,#12]", // function
#ifdef __ARM_ARCH_4T__ // armv4t
	"mov lr, pc",
	"bx r5",
#else
	"blx r5", // call this function
#endif
	"mov sp, r4", // restore sp

#ifdef __ARM_PCS_VFP
	"ldr r5, [r4,#16]", // return type
	"cmp r5, #46",          // TFLOAT
	"beq .Lfconv",
	"cmp r5, #47",          // TDOUBLE
	"beq .Lfconv",
".Lfconv:",
	"vmov r0, s0",
	"vmov r1, s1",
#endif

".Lret:",
	// all values: int, long, float and double returns in r0+r1
	"ldmfd   sp!, {r4, r5, pc}");
}
# endif

// ------------------------------------------
#elif __mips__ // 32-bit

// https://refspecs.linuxfoundation.org/elf/mipsabi.pdf
__attribute__((__interrupt__))
ret_t mips32_call(word argv[], long i, void* function, long type);
	// $a0: argv
	// $a1: i
	// $a2: function
	// $a3: type
__ASM__(".globl mips32_call", // inline assembly should start with .globl <func_name>.
		"mips32_call:",
	"addiu  $sp, $sp,-16",
	"sw     $fp, 4($sp)",
	"sw     $ra, 8($sp)",
	// "sw     $a3, 0($sp)",
	"move   $fp, $sp",

	"ble  $a1, 4, $run",

	// align stack
	"addi $t2, $a1, 1", // +1
	"andi $t2, $t2, 1", // &1
	"sll  $t2, $t2, 2", // *4
	"sub  $sp, $sp, $t2",

	// let's push data to the stack
	"li   $t1, 3", // current index
	"mul  $t2, $a1, 4",
	"add  $t0, $a0, $t2", // move t0 up to end of arguments list
"$loop:"
	"sub  $sp, $sp, 4",
	"lw   $t2, 0($t0)",
	"sw   $t2, 0($sp)",
	"sub  $t0, $t0, 4",
	"add  $t1, $t1, 1",
	"bne  $t1, $a1, $loop",

"$run:",
	"move $t0, $a0", // t0 <- argv
	"move $t9, $a2", // a2 - function
	"lw   $a0, 0($t0)",
	"lw   $a1, 4($t0)",
	"lw   $a2, 8($t0)",
	"lw   $a3, 12($t0)",
	"sub  $sp, $sp, 16", // reserved space for $a0..$a3
	"jalr $t9",
	"nop",
// 	"lw   $a3, 0($sp)",
// 	"beq  $a3, 46, $conv", // TFLOAT
// 	"bne  $a3, 47, $ret",  // TDOUBLE
// "$conv:",
// 	// "move $v0, $f0",
// 	// "move $v1, $f1",
// 	// return
"$ret:",
	"move   $sp, $fp",
	"lw     $ra, 8($sp)",
	"lw     $fp, 4($sp)",
	"addiu  $sp, $sp, 16",
	"jr     $ra",
	"nop");

// ------------------------------------------
#elif __EMSCRIPTEN__

// 	fmask is two-bits width with high bit 1 as a mark
static
ret_t wasm32_call(word arg[], int fmask, void* function, int type)
{
	assert(sizeof(int_t) == sizeof(word));
	assert(sizeof(float) == sizeof(word));

	ret_t r;
	const int_t* argi = (int_t*)arg;
	const float* argf = (float*)arg;

	int rtype = type;
	switch (type) {
		case TINT8: case TINT16: case TINT32:
		case TUINT8: case TUINT16: case TUINT32:
		case TVPTR:   // assert (sizeof(void*) == sizeof(uint32_t))
		case TSTRING: // type-string == void*
			rtype = TINT32;
			break;
	}

	#define CALL(variables, values) \
		switch (rtype) { \
			case TVOID: \
				((void (*) variables) function) values;\
				return 1;\
			case TINT32:\
				return (ret_t)\
				((word (*) variables) function) values;\
			case TFLOAT:\
				*(float*)&r =\
				((float(*) variables) function) values;\
				return r;\
			case TDOUBLE:\
				*(double*)&r =\
				((double(*)variables) function) values;\
				return r;\
		}

	switch (fmask) {
	case 0b1: CALL((), ());
	// 1
	case 0b100:
		CALL((int_t), (argi[ 0]));
	case 0b110:
		CALL((float), (argf[ 0]));
	case 0b111:
		CALL((double),(*(double*)&arg[ 0]));

	// 2
	case 0b10000:
		CALL((int_t, int_t),
		     (argi[ 0], argi[ 1]));
	case 0b11010:
		CALL((float, float),
		     (argf[ 0], argf[ 1]));
	case 0b11111:
		CALL((double,double),
		     (*(double*)&arg[ 0], *(double*)&arg[ 2]));

	// 3
	case 0b1000000:
		CALL((int_t, int_t, int_t),
		     (argi[ 0], argi[ 1], argi[ 2]));
	case 0b1101010:
		CALL((float, float, float),
		     (argf[ 0], argf[ 1], argf[ 2]));
	case 0b1111111:
		CALL((double, double, double),
		     (*(double*)&arg[ 0], *(double*)&arg[ 2],
		      *(double*)&arg[ 4]));

	// 4
	case 0b100000000:
		CALL((int_t, int_t, int_t, int_t),
		     (argi[ 0], argi[ 1], argi[ 2], argi[ 3]));
	case 0b110101010:
		CALL((float, float, float, float),
		     (argf[ 0], argf[ 1], argf[ 2], argf[ 3]));
	case 0b111111111:
		CALL((double, double, double, double),
		     (*(double*)&arg[ 0], *(double*)&arg[ 2],
		      *(double*)&arg[ 4], *(double*)&arg[ 6]));

	// 5

	// 6
	case 0b1111111111111:
		CALL((double, double, double, double, double, double),
		     (*(double*)&arg[ 0], *(double*)&arg[ 2],
		      *(double*)&arg[ 4], *(double*)&arg[ 6],
		      *(double*)&arg[ 8], *(double*)&arg[10]));


// 	case 0b10:
// 		return (ret_t)(word)((word (*)  (int_t))
// 					 function) (args[ 0]);

// 	case 0b100:
// 		return (ret_t)(word)((word (*)  (int_t, int_t))
// 		             function) (args[ 0], args[ 1]);
// 	case 0b111:
// 		return (ret_t)(word)((word (*)  (float, float))
// 		             function) ((float)args[ 0], (float)args[ 1]);

// 	case 0b1000:
// 		return (ret_t)(word)((word (*)  (int_t, int_t, int_t))
// 		             function) (args[ 0], args[ 1], args[ 2]);
// 	case 0b1111:
// 		return (ret_t)(word)((word (*)  (float, float, float))
// 		             function) ((float)args[ 0], (float)args[ 1], (float)args[ 2]);

// 	case 0b10000:
// 		return (ret_t)(word)((word (*)  (int_t, int_t, int_t, int_t))
// 		             function) (args[ 0], args[ 1], args[ 2], args[ 3]);
// 	case 0b11111:
// //		printf("%f/%f/%f/%f\n", *(float*)&args[ 0], *(float*)&args[ 1], *(float*)&args[ 2], *(float*)&args[ 3]);
// 		return (ret_t)(word)((word (*)  (float, float, float, float))
// 		             function) (*(float*)&args[ 0], *(float*)&args[ 1], *(float*)&args[ 2], *(float*)&args[ 3]);
// 	case 0b100000:
// 		return (ret_t)(word)((word (*)  (int_t, int_t, int_t, int_t, int_t))
// 		             function) (args[ 0], args[ 1], args[ 2], args[ 3],
// 		                        args[ 4]);
// 	case 0b1000000:
// 		return (ret_t)(word)((word (*)  (int_t, int_t, int_t, int_t, int_t, int_t))
// 		             function) (args[ 0], args[ 1], args[ 2], args[ 3],
// 		                        args[ 4], args[ 5]);
// 	case 0b10000000:
// 		return (ret_t)(word)((word (*)  (int_t, int_t, int_t, int_t, int_t, int_t,
// 	                                  int_t))
// 	                 function) (args[ 0], args[ 1], args[ 2], args[ 3],
// 	                            args[ 4], args[ 5], args[ 6]);
// 	case 0b100000000:
// 		return (ret_t)(word)((word (*)  (int_t, int_t, int_t, int_t, int_t, int_t,
// 	                                  int_t, int_t))
// 	                 function) (args[ 0], args[ 1], args[ 2], args[ 3],
// 	                            args[ 4], args[ 5], args[ 6], args[ 7]);
// 	case 0b1000000000:
// 		return (ret_t)(word)((word (*)  (int_t, int_t, int_t, int_t, int_t, int_t,
// 	                                  int_t, int_t, int_t))
// 	                 function) (args[ 0], args[ 1], args[ 2], args[ 3],
// 	                            args[ 4], args[ 5], args[ 6], args[ 7],
// 	                            args[ 8]);
// 	// 10:
// 	case 0b10000000000:
// 		return (ret_t)(word)((word (*)  (int_t, int_t, int_t, int_t, int_t, int_t,
// 	                                  int_t, int_t, int_t, int_t))
// 	                 function) (args[ 0], args[ 1], args[ 2], args[ 3],
// 	                            args[ 4], args[ 5], args[ 6], args[ 7],
// 	                            args[ 8], args[ 9]);
// 	case 0b100000000000:
// 		return (ret_t)(word)((word (*)  (int_t, int_t, int_t, int_t, int_t, int_t,
// 	                                  int_t, int_t, int_t, int_t, int_t))
// 	                 function) (args[ 0], args[ 1], args[ 2], args[ 3],
// 	                            args[ 4], args[ 5], args[ 6], args[ 7],
// 	                            args[ 8], args[ 9], args[10]);
// 	case 0b1000000000000:
// 		return (ret_t)(word)((word (*)  (int_t, int_t, int_t, int_t, int_t, int_t,
// 	                                  int_t, int_t, int_t, int_t, int_t, int_t))
// 	                 function) (args[ 0], args[ 1], args[ 2], args[ 3],
// 	                            args[ 4], args[ 5], args[ 6], args[ 7],
// 	                            args[ 8], args[ 9], args[10], args[11]);
	default: E("Unsupported parameters count for ffi function: %d", fmask >> 2);
		return 0;
	};
}

// ------------------------------------------
#else // other platforms

// http://byteworm.com/2010/10/12/container/ (lambdas in c)

// https://en.wikipedia.org/wiki/X86_calling_conventions
// x86 conventions: cdecl, syscall(OS/2), optlink(IBM)
// pascal(OS/2, MsWin 3.x, Delphi), stdcall(Win32),
// fastcall(ms), vectorcall(ms), safecall(delphi),
// thiscall(ms)

	// todo: ограничиться количеством функций поменьше
	//	а можно сделать все в одной switch:
	// todo: а можно лямбдой оформить и засунуть эту лябмду в функцию еще в get-proc-address
	// todo: проанализировать частоту количества аргументов и переделать все в
	//   бинарный if

	// __attribute__((stdcall))
/*						__stdcall // gcc style for lambdas in pure C
	int (*stdcall[])(char*) = {
			({ int $(char *str){ printf("Test: %s\n", str); } $; })
	};*/
	// http://www.agner.org/optimize/calling_conventions.pdf
	#define CALL(conv) \
		switch (i) {\
		case  0: return (ret_t)(word)((conv word (*)  ())\
						 function) ();\
		case  1: return (ret_t)(word)((conv word (*)  (word))\
						 function) (args[ 0]);\
		case  2: return (ret_t)(word)((conv word (*)  (word, word))\
		                 function) (args[ 0], args[ 1]);\
		case  3: return (ret_t)(word)((conv word (*)  (word, word, word))\
		                 function) (args[ 0], args[ 1], args[ 2]);\
		case  4: return (ret_t)(word)((conv word (*)  (word, word, word, word))\
		                 function) (args[ 0], args[ 1], args[ 2], args[ 3]);\
		case  5: return (ret_t)(word)((conv word (*)  (word, word, word, word, word))\
		                 function) (args[ 0], args[ 1], args[ 2], args[ 3],\
		                            args[ 4]);\
		case  6: return (ret_t)(word)((conv word (*)  (word, word, word, word, word, word))\
		                 function) (args[ 0], args[ 1], args[ 2], args[ 3],\
		                            args[ 4], args[ 5]);\
		case  7: return (ret_t)(word)((conv word (*)  (word, word, word, word, word, word, \
		                                  word))\
		                 function) (args[ 0], args[ 1], args[ 2], args[ 3],\
		                            args[ 4], args[ 5], args[ 6]);\
		case  8: return (ret_t)(word)((conv word (*)  (word, word, word, word, word, word, \
		                                  word, word))\
		                 function) (args[ 0], args[ 1], args[ 2], args[ 3],\
		                            args[ 4], args[ 5], args[ 6], args[ 7]);\
		case  9: return (ret_t)(word)((conv word (*)  (word, word, word, word, word, word, \
		                                  word, word, word))\
		                 function) (args[ 0], args[ 1], args[ 2], args[ 3],\
		                            args[ 4], args[ 5], args[ 6], args[ 7],\
		                            args[ 8]);\
		case 10: return (ret_t)(word)((conv word (*)  (word, word, word, word, word, word, \
		                                  word, word, word, word))\
		                 function) (args[ 0], args[ 1], args[ 2], args[ 3],\
		                            args[ 4], args[ 5], args[ 6], args[ 7],\
		                            args[ 8], args[ 9]);\
		case 11: return (ret_t)(word)((conv word (*)  (word, word, word, word, word, word, \
		                                  word, word, word, word, word))\
		                 function) (args[ 0], args[ 1], args[ 2], args[ 3],\
		                            args[ 4], args[ 5], args[ 6], args[ 7],\
		                            args[ 8], args[ 9], args[10]);\
		case 12: return (ret_t)(word)((conv word (*)  (word, word, word, word, word, word, \
		                                  word, word, word, word, word, word))\
		                 function) (args[ 0], args[ 1], args[ 2], args[ 3], \
		                            args[ 4], args[ 5], args[ 6], args[ 7], \
		                            args[ 8], args[ 9], args[10], args[11]);\
		default: E("Unsupported parameters count for ffi function: %d", i);\
			return 0;\
		};
static
inline ret_t call_x(word args[], int i, void* function, int type) {
	CALL();
}
#endif
// -- end of assembly part ------------------------------------------------------------
// ------------------------------------------------------------------------------------

// ------------------
// tools
#define max2(a,b) ({ \
	typeof (a) _a = (a); \
	typeof (b) _b = (b); \
    _a > _b ? _a : _b; \
})
#define max3(a,b,c) max2(a, max2(b, c))

#ifdef max
#undef max
#endif
#define MAX_MACRO(_1, _2, _3, NAME, ...) NAME
#define max(...) MAX_MACRO(__VA_ARGS__, max3, max2, NOTHING, NOTHING)(__VA_ARGS__)

static
int llen(word list)  {
	int i = 0;
	while (list != INULL)
		list = cdr(list), i++;
	return i;
}

// ------------------
// return types convertors
// todo: check the word size!! if sizeof(word) == 4!
#if defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
	#define DECLARE_TYPE_CONVERTER(type, name) \
	static __inline__ \
	type name(ret_t* got) { \
		return *(type*)(((unsigned char*)got) + sizeof(word) - sizeof(type));\
	}
#else // __ORDER_LITTLE_ENDIAN__
	#define DECLARE_TYPE_CONVERTER(type, name) \
	static __inline__ \
	type name(ret_t* got) { \
		return *(type*)(((unsigned char*)got) + 0);\
	}
#endif

// lower types
DECLARE_TYPE_CONVERTER(int8_t, CV_INT8)
DECLARE_TYPE_CONVERTER(uint8_t, CV_UINT8)
DECLARE_TYPE_CONVERTER(int16_t, CV_INT16)
DECLARE_TYPE_CONVERTER(uint16_t, CV_UINT16)
DECLARE_TYPE_CONVERTER(int32_t, CV_INT32)
DECLARE_TYPE_CONVERTER(uint32_t, CV_UINT32)
DECLARE_TYPE_CONVERTER(void*, CV_VOIDP)
DECLARE_TYPE_CONVERTER(long, CV_LONG)
DECLARE_TYPE_CONVERTER(float, CV_FLOAT)

// 64bit types
static __inline__
int64_t  CV_INT64 (ret_t* got) { return *(int64_t *)got; }
static __inline__
uint64_t CV_UINT64(ret_t* got) { return *(uint64_t*)got; }
static __inline__
double   CV_DOUBLE(ret_t* got) { return *(double*)  got; }

// https://en.wikipedia.org/wiki/Double-precision_floating-point_format
// However, on modern standard computers (i.e., implementing IEEE 754), one may
// in practice safely assume that the endianness is the same for floating-point
// numbers as for integers, making the conversion straightforward regardless of
// data type.
// (Small embedded systems using special floating-point formats may be another
// matter however.)
word d2ol(struct heap_t* ol, double v);      // implemented in olvm.c
double OL2D(word arg); float OL2F(word arg); // implemented in olvm.c

word* string2ol(olvm_t* this, char* vptr);   // implemented in olvm.c

// -=( PTR and REF )=------------------------------------------------------
// TODOs:
//  * make memory allocator configurable new_bytevector/builtin_alloca
//    (because in case of callback GC will move data)
//	* handle TVECTORLEAF
#define SERIALIZE_LIST(memory, type, convert)\
	case TPAIR: {\
		int c = list_length(arg);\
		type* p = (type*) &new_bytevector(c * sizeof(type))[1];\
		*memory = (word)p;\
		\
		word l = arg;\
		while (c--)\
			*p++ = (type)convert(car(l)), l = cdr(l);\
		break;\
	}
#if OLVM_FFI_VECTORS
#	define SERIALIZE_VECTOR(memory, type, convert)\
	case TVECTOR: {\
		int c = vector_length(arg);\
		type* p = (type*) &new_bytevector(c * sizeof(type))[1];\
		*memory = (word)p;\
		\
		word* l = &car(arg);\
		while (c--)\
			*p++ = (type)convert(*l++);\
		break;\
	}
#else
#	define SERIALIZE_VECTOR(type, convert)
#endif
#define SERIALIZE(memory, type, convert)\
	{\
		assert (arg == INULL || is_reference(arg));\
		\
		switch (reference_type(arg)) {\
			SERIALIZE_LIST(memory, type, convert)\
			SERIALIZE_VECTOR(memory, type, convert)\
			default:\
				E("unsupported type %d", reference_type(arg));\
		}\
	}

// -=( DESERIALIZE )=------------------------------------------
#if OLVM_INEXACTS
#	define DESERIALIZE_INEXACT()\
				case TINEXACT:\
					*(inexact_t*)&car(num) = (inexact_t)value;\
					break;
#else
#	define DESERIALIZE_INEXACT()
#endif

// todo: modify DESERIALIZE_INT for different types
#define DESERIALIZE_INT()\
				case TINTP:\
				case TINTN: {\
					int_t ivalue = (int_t)value;\
					if (ivalue > VMAX || ivalue < -VMAX) {\
						if (ivalue < 0) {\
							*(word*)num = make_header(value < 0 ? TINTN : TINTP, 3);\
							value = -value;\
						}\
						else\
							*(word*)num = make_header(TINTP, 3);\
						*(word*)&car(num) = I(ivalue & VMAX);\
						*(word*)&cadr(num) = I(ivalue >> VBITS);\
					}\
					else\
						*(word*)l = make_enum(value);\
					break;\
				}

// TODO: DESERIALIZE_RATIONAL()
// case TRATIONAL: {
// 	// максимальная читабельность (todo: change like fto..)
// 	long n = value * 10000;
// 	long d = 10000;
// 	car(num) = make_enum(n);
// 	cdr(num) = make_enum(d);
// 	// максимальная точность (fixme: пока не работает как надо)
// 	//car(num) = make_enum(value * VMAX);
// 	//cdr(num) = I(VMAX);
// 	break;
// }

#define DESERIALIZE_TYPED(type, convert)\
	type value = *p++;\
	word num = convert(l);\
	if (is_value(num))\
		convert(l) = make_enum(value);\
	else\
	switch (reference_type(num)) {\
		DESERIALIZE_INT()\
		DESERIALIZE_INEXACT()\
		default:\
			(void) value;\
			assert (0 && "Invalid return variables.");\
			break;\
	}\

#define DESERIALIZE_LIST(type)\
	case TPAIR: {\
		int c = list_length(arg);\
		type* p = (type*) args[i];\
		\
		word l = arg;\
		while (c--) {\
			DESERIALIZE_TYPED(type, car)\
			l = cdr(l);\
		}\
		break;\
	}
#if OLVM_FFI_VECTORS
#	define DESERIALIZE_VECTOR(type)\
	case TVECTOR: {\
		int c = vector_length(arg);\
		type* p = (type*) args[i];\
		\
		word* l = &car(arg);\
		while (c--) {\
			DESERIALIZE_TYPED(type, *)\
			l++;\
		}\
		break;\
	}
#else
#	define DESERIALIZE_VECTOR(type)
#endif

#define DESERIALIZE(type)\
	if (is_reference(arg))\
	switch (reference_type(arg)) {\
		DESERIALIZE_LIST(type)\
		DESERIALIZE_VECTOR(type)\
		default:\
			E("unsupported type %d", reference_type(arg));\
	}

// strings

// note: invalid codepoint will be encoded as "?", so len is 1
#define codepoint_len(x) ({ int cp = x; \
	cp < 0x80 ? 1 : \
	cp < 0x0800 ? 2 : \
	cp < 0x10000 ? 3 : \
	cp < 0x110000 ? 4 : 1; })

static // length of wide string in utf-8 encoded bytes
__attribute__((unused))
size_t utf8_len(word widestr)
{
	size_t len = 0;
	for (size_t i = 1; i <= reference_size(widestr); i++)
		len += codepoint_len(value(ref(widestr, i)));
	return len;
}

#define list_length(x) llen(x)
#define vector_length(x) reference_size(x)
#define string_length(x) ({ \
    word t = reference_type(x); \
    t == TSTRING ? rawstream_size(x) :\
    t == TSTRINGWIDE ? utf8_len(x) :\
    t == TSTRINGDISPATCH ? number(ref(x, 1)) :\
    0; })


// ol->c convertors
static
int_t from_uint(word arg) {
	assert (is_reference(arg));
	// так как в стек мы все равно большое число сложить не сможем,
	// то возьмем только то, что влазит (первые два члена)
	return (car(arg) >> 8) | ((car(cdr(arg)) >> 8) << VBITS);
}

#if UINTPTR_MAX == UINT32_MAX // 32-bit platform math
static
int64_t from_ulong(word arg) {
	assert (is_reference(arg));
	int64_t v = car(arg) >> 8;
	int shift = VBITS;
	while (cdr(arg) != INULL) {
		v |= ((int64_t)(car(cdr(arg)) >> 8) << shift);
		shift += VBITS;
		arg = cdr(arg);
	}
	return v;
};
#endif

static
int_t from_rational(word arg) {
	word* pa = (word*)car(arg);
	word* pb = (word*)cdr(arg);

	int64_t a = 0;
	if (is_value(pa))
		a = enum(pa);
	else {
		switch (reference_type(pa)) {
		case TINTP:
			a = +unumber(pa);
			break;
		case TINTN:
			a = -unumber(pa);
			break;
		}
	}

	int64_t b = 1;
	if (is_value(pb))
		b = enum(pb);
	else {
		switch (reference_type(pb)) {
		case TINTP:
			b = +unumber(pb);
			break;
		case TINTN:
			b = -unumber(pb);
			break;
		}
	}

	return (a / b);
}

static
int_t to_int(word arg) {
	if (is_enum(arg))
		return enum(arg);

	switch (reference_type(arg)) {
	case TINTP:
		return (int_t)+from_uint(arg);
	case TINTN:
		return (int_t)-from_uint(arg);
	case TRATIONAL:
		return (int_t) from_rational(arg);
	case TCOMPLEX: // use Re part
		return to_int(car(arg));
	case TINEXACT: // truncate, not round
		return (int_t) *(inexact_t*)&car(arg);
	default:
		E("can't cast type %d to (u)int", reference_type(arg));
	}

	return 0;
}

#if UINT32_MAX == UINTPTR_MAX // 32-bit machines
static
int64_t to_int64(word arg) {
	if (is_enum(arg))
		return enum(arg);

	switch (reference_type(arg)) {
	case TINTP:
		return (int64_t)+from_ulong(arg);
	case TINTN:
		return (int64_t)-from_ulong(arg);
	case TRATIONAL:
		return (int64_t) from_rational(arg);
	case TCOMPLEX: // use Re part
		return to_int64(car(arg));
	case TINEXACT: // truncate, not round
		return (int64_t) *(inexact_t*)&car(arg);
	default:
		E("can't cast type %d to (u)int64", reference_type(arg));
	}

	return 0;
}
#else
#	define to_int64 to_int
#endif

static
char* chars2ol(char* ptr, word string)
{
	assert (is_string(string));
	int size = rawstream_size(string);
	memcpy(ptr, &car(string), size);

	return ptr + size;
}

static
char* wchars2utf8(char* ptr, word widestring)
{
	//assert (is_stringwide(widestring));

	// utf-8 encoding
	// we don't know actual length of encoded string
	for (size_t i = 1; i <= reference_size(widestring); i++) {
		int cp = value(ref(widestring, i));
		if (cp < 0x80)
			*ptr++ = cp;
		else
		if (cp < 0x0800) {
			*ptr++ = 0xC0 | (cp >> 6);
			*ptr++ = 0x80 | (cp & 0x3F);
		}
		else
		if (cp < 0x10000) {
			*ptr++ = 0xE0 | (cp >> 12);
			*ptr++ = 0x80 | ((cp >> 6) & 0x3F);
			*ptr++ = 0x80 | (cp & 0x3F);
		}
		else
		if (cp < 0x110000) {
			*ptr++ = 0xF0 | (cp >> 18);
			*ptr++ = 0x80 | ((cp >> 12) & 0x3F);
			*ptr++ = 0x80 | ((cp >> 6) & 0x3F);
			*ptr++ = 0x80 | (cp & 0x3F);
		}
		else {
			E("ffi error: invalid codepoint %x found.\n", cp);
			*ptr++ = 0x7F;
		}
	}
	return ptr;
}

static
char* stringleaf2ol(char* ptr, word stringleaf)
{
	// assert (is_stringleaf(stringleaf));
	int parts = reference_size(stringleaf) - 1;
	for (int i = 0; i < parts; i++) {
		word substr = ref(stringleaf, i + 2);
		switch (reference_type(substr)) {
		case TSTRING:
			ptr = chars2ol(ptr, substr);
			break;
		case TSTRINGWIDE:
			ptr = wchars2utf8(ptr, substr);
			break;
		case TSTRINGDISPATCH:
			ptr = stringleaf2ol(ptr, substr);
			break;
		default:
			E("ffi error: invalid stringdispatch element type %d.\n", reference_type(substr));
			break;
		}
	}
	return ptr;
}

static __inline__
size_t copychars(char* ptr, word string, char* (cpy)(char*,word))
{
	char* end = cpy(ptr, string);
	*end++ = 0;
	return end - ptr;
}

static
void not_a_type(char* tname)
{
	E("invalid parameter value (requested %s)", tname);
}

static
char* not_a_string(char* ptr, word string)
{
	(void) string;
	not_a_type("string");
	return ptr;
}

// static // decode native ptr into ol string

#define PTR FFT_PTR // just pointer
#define REF FFT_REF // pointer with drawback

// 		switch (value(tty)) {
// 		case TSTRING+PTR: // todo: remove PTR?
// 			while (arg != INULL) {
// 				*total += WORDS(rawstream_size(arg)+1); // 1 for '\0'
// 				arg = cdr(arg);
// 			}
// 			break;
// 		case TSTRINGWIDE+PTR:
// 			while (arg != INULL) {
// 				*total += WORDS(utf8_len(arg)+1);
// 				arg = cdr(arg);
// 			}
// 			break;
// 		case TSTRINGDISPATCH+PTR:
// 			while (arg != INULL) {
// 				*total += WORDS(number(ref(arg, 1))+1); // todo: decode all as utf8_len and use FAST_STRING_CALC macro
// 				arg = cdr(arg);
// 			}
// 			break;
// #if UINT64_MAX > UINTPTR_MAX
// 		case TINT64: case TUINT64:
// 		case TDOUBLE:
// 			while (arg != INULL) { //?
// 				*total += 2;
// 				arg = cdr(arg);
// 			}
// 			break;
// #endif
// 		default:
// 			// handle proper lists and '(vptr . offset) // ???
// 			// todo: обосновать
// 			while (is_reference(arg)) { // until #null or value
// 				(*total)++;
// 				arg = cdr(arg);
// 			}
// 			break;
// 		}

	// 	break;
	// case TVECTOR: // list of arguments
	// 	for (int i = 1; i <= reference_size(arg); i++) {
	// 		// TODO: handle TSTRING+PTR, etc.
	// 		(*total)++;
	// 	}
	// 	break;
	// }
// }

#define is_pointer(ptr) (is_value(ptr) && (value(ptr) & (FFT_PTR | FFT_REF)))
static size_t structure_calc(word args, word rtty, size_t* total);
static size_t pointer_calc(word args, word rtty, size_t* total);


static
size_t structure_calc(word args, word rtty, size_t* total)
{
	// http://www.catb.org/esr/structure-packing/
	while (rtty != INULL) {
		word arg = car(args);
		word tty = car(rtty); // структуры должны совпадать по количеству аргументов с типами

		if (is_pair(tty)) // pointer or substructure?
			if (is_pointer(car(tty)))
				pointer_calc(arg, cdr(tty), total);
			else
				structure_calc(arg, tty, total);
		else { // TODO: handle strings, for example!
			assert (is_value(tty));
			(*total)++; // очень грубо - вместо точного подсчета просто берем пословно
						// структуры у нас не настолько могут быть гигантские (в отличие от
						// массивов, например), чтобы надо было экономить память.
			#if UINT64_MAX > UINTPTR_MAX
			int type = value(tty);
			if (type == TINT64 || type == TUINT64 || type == TDOUBLE)
				(*total)++;
			#endif
		}
		args = cdr(args);
		rtty = cdr(rtty);
	}
	return 0;
}

static // structure or array of structures
size_t deep_array_calc(word args, word rtty, size_t* total)
{
	switch (reference_type(args)) {
		case TPAIR:
			// TODO: handle strings as elements (use arg_size())
			while (args != INULL) {
				(*total) += 0; // calc real len
				args = cdr(args);
			}
			break;
		case TVECTOR:
			// TODO: handle strings as elements (use arg_size())
			(*total) += 0; //
			(void)reference_size(args); // TODO
			break;
	}
	return 0;
}

// грубый подсчет нужного размера памяти (в словах)
// каждый элемент структуры принимаем равный word
static // structure or array of structures
size_t pointer_calc(word args, word rtty, size_t* total)
{
	// указатель на массив простых типов
	if (is_value(rtty)) { // просто тип, 
		if (is_reference(args)) {
			size_t size = 0;
			size_t subsize;
			switch (value(rtty)) {
				case TINT8:  case TUINT8:  subsize = sizeof(int8_t);  break;
				case TINT16: case TUINT16: subsize = sizeof(int16_t); break;
				case TINT32: case TUINT32: subsize = sizeof(int32_t); break;
				case TINT64: case TUINT64: subsize = sizeof(int64_t); break;
				case TFLOAT:               subsize = sizeof(float);   break;
				case TDOUBLE:              subsize = sizeof(double);  break;
				case TSTRING:
				    subsize = sizeof(char*);
					deep_array_calc(args, rtty, total);
					break;
				default:
					subsize = sizeof(word);
			}
			switch (reference_type(args)) {
				case TPAIR:
					while (args != INULL) {
						size++;
						args = cdr(args);
					}
					break;
				case TVECTOR:
					size = reference_size(args);
					break;
			}
			*total += WORDS(size * subsize);
		}
	}
	else
		structure_calc(args, rtty, total);
	return 0;
}

static size_t structure_by_value(word args, word rtty, size_t* total)
{
	return 0;
}

// todo: добавить структуры by-value
// todo: добавить вложенные указатели
// todo: добавить вложенные указатели на структуры
static
size_t arguments_size(word args, word rtty, size_t* total)
{
	size_t words = 0;
	while (args != INULL) { // пока есть аргументы
		word arg = car(args);
		word tty = is_pair(rtty) ? car(rtty) : I(TANY);

		// type override?
		again:
		if (is_reference(arg)) {
			if (tty == I(TANY) && is_pair(arg)) {
				tty = car(arg); arg = cdr(arg);
				goto again; // does it needed?
			}
			switch (reference_type(arg)) {
				// pointer or structure-by-value
				case TPAIR:
				case TVECTOR: {
					if (is_pair(tty)) {
						int retype = car(tty);
						// pointer (to array or structure)?
						if (is_pointer(retype))
							pointer_calc(arg, cdr(tty), total);
						else {
							// structure-by-value
							size_t local = 0;
							words += structure_by_value(arg, tty, &local) + local;
						}
					}
					else
						; // nothing special
					break;
				}
				case TBYTEVECTOR:
					*total += reference_size(arg); // in words
					break;

				case TSYMBOL:
					arg = car(arg);
					// fall through
				case TSTRING:
					*total += reference_size(arg); // in words
					break;
				case TSTRINGWIDE:
					*total += reference_size(arg) * 4; // worst case (all of chars are unicode-32)
					break;
				case TSTRINGDISPATCH:
					// todo: if (string-len > 10000), use precise string calculation ?
					*total += number(ref(arg, 1)) * 4; // worst case (all of chars are unicode-32)
					break;
			}
		}
#if UINT64_MAX > UINTPTR_MAX
		// 32-bit machines
		else
		switch (value(tty)) {
			case TINT64: case TUINT64:
			case TDOUBLE:
#	if __mips__ // 32-bit mips,
				words = (words+1)&-2; // dword align
#	else
				words += 1;
#	endif
		}
#endif
		words += 1;
		args = cdr(args);
		rtty = is_pair(rtty) ? cdr(rtty) : INULL;
	}
	if (rtty != INULL) {
		E("Not enough arguments");
		// return IFALSE; todo: do runtime-error
	}
	return words;
}

// ---------------------------------------------
// storing complex arguments
// memory: is address to store
// str: internal string
static
void store_string(word** ffp, char** memory, word str)
{
	word* fp;
again:;
	// todo: add check to not copy the zero-ended string?
	// note: no heap size checking required (done before)
	int type = reference_type(str);
	if (type == TSYMBOL) {
		str = car(str); goto again;
	}
	else {
		fp = *ffp;
		char* ptr = *memory = (char*) &fp[1];
		new_bytevector(copychars(ptr, str,
			type == TSTRING ? chars2ol :
			type == TSTRINGWIDE ? wchars2utf8 :
			type == TSTRINGDISPATCH ? stringleaf2ol :
			not_a_string));
		*ffp = fp;
	}
}

static
void store_string_array(word** ffp, char*** memory, word array)
{
	word* fp;
	if (is_pair(array) || array == INULL) {
		int size = list_length(array);
		fp = *ffp;
		char** p = *memory = (char**) &fp[1];
		new (TBYTEVECTOR, size, 0);
		*ffp = fp;

		while (array != INULL) {
			store_string(ffp, p++, car(array));
			array = cdr(array);
		}
		return;
	}
	if (is_vector(array)) {

		return;
	}
	assert (array == IFALSE);
	*memory = NULL;
}

static
void store_stringwide(word** ffp, word* memory, word str)
{
	word* fp;
again:;
	int type = reference_type(str);
	if (type == TSYMBOL) {
		str = car(str); goto again;
	}
	else {
		switch (type) {
		// case TBYTEVECTOR: // Deprecated(?)
		case TSTRING: {
			int len = rawstream_size(str);
			fp = *ffp;
			widechar* unicode = (widechar*) &car(new_bytevector ((len + 1) * sizeof(widechar))); // 1 for '\0'
			*ffp = fp;

			widechar* p = unicode;
			char* s = (char*)&car(str);
			for (int i = 0; i < len; i++)
				*p++ = (widechar)(*s++);
			*p = 0;

			*memory = (word) unicode;
			break;
		}
		case TSTRINGWIDE: {
			int len = reference_size(str);
			fp = *ffp;
			widechar* unicode = (widechar*) &car(new_bytevector ((len + 1) * sizeof(widechar))); // 1 for '\0'
			*ffp = fp;

			widechar* p = unicode;
			word* s = (word*)&car(str);
			for (int i = 0; i < len; i++)
				*p++ = (widechar)value(*s++);
			*p = 0;

			*memory = (word) unicode;
			break;
		}
		default:
			E("unsupported type-string-wide source.");
		}
	}
}

// struct*
// Structure Sending by Reference:
// https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf
static
size_t structure_size(size_t size, word t)
{
	// http://www.catb.org/esr/structure-packing/
	while (t != INULL) {
		word p = car(t);
		if (is_pair(p)) {
			if (is_pointer(car(p))) {
				size_t subsize = sizeof(void*);
				size = ((size + subsize - 1) & -subsize) + subsize; // align + size
			}
			else
				size = structure_size(size, p);
		}
		else {
			assert (is_value(p));
			int type = value(p);
			if (type == TINT8 || type == TUINT8 || type == TBOOL)
				size++;
			else {
				size_t subsize;
				switch (type) {
					case TINT16: case TUINT16: subsize = sizeof(int16_t); break;
					case TINT32: case TUINT32: subsize = sizeof(int32_t); break;
					case TINT64: case TUINT64: subsize = sizeof(int64_t); break;
					case TFLOAT:               subsize = sizeof(float);   break;
					case TDOUBLE:              subsize = sizeof(double);  break;
					default:                   subsize = sizeof( word );
				}
				size = ((size + subsize - 1) & -subsize) + subsize; // align + size
			}
		}
		t = cdr(t);
	}
	return size;
}

// Notes:
//	* C standard guarantees that struct members always appear in memory in the
//	  exact same order in which they are declared in code,
//	* The first member must always start at the same memory address as the structure itself,
//	* There may be padding at the end of the structure (?),
// Btw:
//	https://abstractexpr.com/2023/06/29/structures-in-c-from-basics-to-memory-alignment
static
size_t store_structure(word** ffp, char* memory, size_t ptr, word t, word a)
{
	word *fp;
	#define SAVE(type, conv) {\
		TALIGN(ptr, type);\
		*(type*)(memory+ptr) = (type)conv(v);\
		ptr += sizeof(type); \
	}

	while (t != INULL && a != INULL) {
		word p = car(t); word v = car(a);
		if (is_pair(p)) {
			// pointers
			if (is_pointer(car(p))) {
				p = cdr(p);
				switch (value(p)) {
					case TSTRING:
						TALIGN(ptr, char**);
						fp = *ffp;
						store_string_array(&fp, (char***)(memory+ptr), v);
						*ffp = fp;
						ptr += sizeof(char**);
						break;
					// todo: other pointer types
				}
			}
			else
				ptr = store_structure(ffp, memory, ptr, p, v); // assert (is_pair v)
		}
		else {
			if (v == IFALSE) v = I(0); // we accept "false" as "empty value, 0"!
			switch (value(p)) {
			case TINT8: case TUINT8:
				*(int8_t*)(memory+ptr) = (int8_t)value(v); ptr += sizeof(int8_t);
				break;
			case TINT16: case TUINT16:
				SAVE(int16_t, to_int);
				break;
			case TINT32: case TUINT32:
				SAVE(int32_t, to_int);
				break;
			case TINT64: case TUINT64:
				SAVE(int64_t, to_int64);
				break;
			case TFLOAT:
				SAVE(float, OL2F);
				break;
			case TDOUBLE:
				SAVE(double, OL2D);
				break;

			case TVPTR:
				SAVE(void*, car);
				break;

			case TSTRING:
				TALIGN(ptr, char*);
				fp = *ffp;
				store_string(&fp, (char**)(memory+ptr), v);
				*ffp = fp;
				break;
			// todo: type-string-wide, etc.
			default:
				E("unhandled");
				break;
			}
			//ptr += ((ptr + subsize - 1) & -subsize) + subsize;
		}
		t = cdr(t); a = cdr(a);
	}
	return ptr;
}

static
size_t restore_structure(void* memory, size_t ptr, word t, word a)
{
	while (t != INULL && a != INULL) {
		word p = car(t); word v = car(a);
		if (is_pair(p)) {
			// pointers
			if (is_pointer(car(p))) {
				p = cdr(p);
				switch (value(p)) {
					case TSTRING:
						// TALIGN(ptr, char**);
						// fp = *ffp;
						// store_string_array(&fp, (char***)(memory+ptr), v);
						// *ffp = fp;
						// ptr += sizeof(char**);
						assert (0);
						break;
					// todo: other pointer types
				}
			}
			else
				ptr = restore_structure(memory, ptr, p, v); // assert (is_pair v)
		}
		else {
			if (v != IFALSE) // don't restore unspecified data
			switch (value(p)) {
				#define LOAD(type)\
					TALIGN(ptr, type);\
					type value = *(type*)(memory+ptr);\
					ptr += sizeof(type);
				#define WRITEBACK(type) { \
					LOAD(type);\
					if (is_value(v))\
						*(R)&car(a) = make_enum(value);\
					else {\
						word num = v;\
						word *l = &car(v);\
						switch (reference_type(v)) {\
							DESERIALIZE_INT()\
							DESERIALIZE_INEXACT()\
						}\
					}}
			// reflect integer types
			case TINT8:  WRITEBACK(int8_t);  break;
			case TUINT8: WRITEBACK(uint8_t); break;
			case TINT16: WRITEBACK(int16_t); break;
			case TUINT16:WRITEBACK(uint16_t);break;
			case TINT32: WRITEBACK(int32_t); break;
			case TUINT32:WRITEBACK(uint32_t);break;
			case TINT64: WRITEBACK(int64_t); break;
			case TUINT64:WRITEBACK(uint64_t);break;
			case TFLOAT: WRITEBACK(float);   break;
			case TDOUBLE:WRITEBACK(double);  break;

			case TVPTR:
				LOAD(void*);
				if (is_vptr(v)) {
					*(void**)&car(v) = value;
				}
				break;

			case TSTRING:
				//printf("s");
				//TALIGN(ptr, char*);
				//fp = *ffp;
				//store_string(&fp, (char**)(memory+ptr), v);
				//*ffp = fp;
				break;
			// todo: type-string-wide, etc.
			default:
				E("unhandled restore");
				break;
			}
			//ptr += ((ptr + subsize - 1) & -subsize) + subsize;
		}
		t = cdr(t); a = cdr(a);
	}
	return ptr; // todo: word align?
}

/////////////////////////////////////////////////////////////////////////////////////
// Главная функция механизма ffi:
EXPORT
__attribute__((used))
word* OLVM_ffi(olvm_t* const this, word arguments)
{
	word* fp;
	heap_t* const heap = (heap_t*)this;

	// a - function address
	// b - arguments (may be a pair with type in car and argument in cdr - not yet done)
	// c - '(return-type . argument-types-list)
	register word ABC = arguments;
	word A = car(ABC); ABC = cdr(ABC); // function
	word B = car(ABC); ABC = cdr(ABC); // rtty, (cons type prototype)
	word C = car(ABC); ABC = cdr(ABC); // args, (list arg1 arg2 .. argN)

	assert (is_vptr(A));
	assert (B != INULL && (is_reference(B) && reference_type(B) == TPAIR));
	assert (C == INULL || (is_reference(C) && reference_type(C) == TPAIR));

	ret_t got = 0; // результат вызова функции (* internal)
	void *function = (void*)car(A); // "NULL" function means IDF function
	int returntype = is_value(car(B))
		? value(car(B))             // normal return type
		: reference_type(car(B));   // fft& & fft* return types

	if ((cdr(B)|C) == INULL) {      // no argument types and no arguments (speedup)
		got = ((ret_t (*)())function)();
		fp = heap->fp; // update fp
		goto handle_got_value;
	}

	// note: not working under netbsd. should be fixed.
	// static_assert(sizeof(float) <= sizeof(word), "float size should not exceed the word size");

#if _WIN32
	// nothing special for Windows (both x32 and x64)

#elif __x86_64__ // && (__unix__ || __APPLE__)), but not windows
	// *nix x64 содержит отдельный массив чисел с плавающей запятой
	double ad[18]; // 18? почему? это некий максимум?
	int d = 0;     // количество аргументов для ad
	long fpmask = 0; // маска для типа аргументов, (0-int, 1-float point), (63 maximum?)

#elif __aarch64__
	double ad[8];
	int d = 0;

#elif __ARM_EABI__ && __ARM_PCS_VFP // -mfloat-abi=hard (?)
	// арм int и float складывает в разные регистры (r?, s?), если сопроцессор есть
	float af[18]; // для флоатов отдельный массив (почему не 16?)
	int f = 0;     // количество аргументов для af

#elif __EMSCRIPTEN__
	// двухбитная маска типа аргументов, 16 arguments maximum
	//	00 - int
	//	01 - reserved
	//	10 - float
	//	11 - double
	// + старший бит-маркер,
	long fpmask = 1;
#endif

	// переменные для промежуточных результатов

	// ----------------------------------------------------------
	// 1. do memory precalculations and count number of arguments

	// TODO: добавить в общую структуру типов поле, куда кешировать
	// все вот эти размеры. например, как [types cached-size]
	// и детектировать по vector? со временем насовсем перейти

	// words for args, total + words for GC
	size_t total = 0;
	size_t words = arguments_size(C, cdr (B), &total);
	(void) words; // disable warning

	// special case of returning a structure by value:
	// http://www.sco.com/developers/devspecs/abi386-4.pdf
	//
	// Functions Returning Structures or Unions
	//
	// If a function returns a structure or union, then the caller provides space for the
	// return value and places its address on the stack as argument word zero. In effect,
	// this address becomes a ‘‘hidden’’ first argument. Having the caller supply the
	// return object’s space allows re-entrancy.
	if (is_pair(car(B)) && // ???
		value(caar(B)) == TBYTEVECTOR)
		// && value(cdar(B)) > sizeof(ret_t) // commented for speedup
	{
		total += WORDS(value(cdar(B))) + 1;
	}

	// ------------------------------------------------
	// 1.1  ensure that all arguments will fit in heap
	if (heap->fp + total > heap->end) {
		size_t pin = OLVM_pin(this, arguments);
		heap->gc(this, total);
		arguments = OLVM_unpin(this, pin);

		register word ABC = arguments;
		A = car(ABC); ABC = cdr(ABC); // function
		B = car(ABC); ABC = cdr(ABC); // rtty
		C = car(ABC); ABC = cdr(ABC); // args
	}
	fp = heap->fp;

	// todo: add configuring api for next "32" etc.
	// something like "set minimal stack size" or make dynamic
#if __aarch64__
	int args_len = max(words, 32) * sizeof(word);
	word* args = __builtin_alloca(args_len); // GRNC + SA (stacked arguments)
	#define GRNC 8 // General-purpose Register Number Count
	#define FRNC 8 // Floating-point Register Number Count

# if __APPLE__
	char * extra = (char*) &args[GRNC];
	int e = 0; // указатель на стековые данные
# endif
#else
	word* args = __builtin_alloca(max(words, 16) * sizeof(word)); // minimum - 16 words for arguments
	#define GRNC 6
#endif
	// todo: clear args array
	// todo: clear ad array
	

	// ==============================================
	// 2. prepare arguments to push
	word* p = (word*)C;   // ol arguments
	word* t = (word*)cdr (B); // rtty
	int writeback = 0; // has write-back in arguments (code speedup)

	int i = 0;  // актуальное количество аргументов
#if (__x86_64__ && (__unix__ || __APPLE__))
	int l = 0;  // нижняя граница для параметров больших структур (linux and inttel mac)
#endif

	// special case of returning a structure by value
	// allocate a space for the return value
	if (is_pair(car(B)) &&
		value(caar(B)) == TBYTEVECTOR &&
		value(cdar(B)) > sizeof(ret_t))
	{
		int len = value(cdar(B));
		word* result = new_bytevector(len);
		args[i++] = (word) &car(result);
	}

	//__ASM__("brk #0");
	// ==============================================
	// PUSH
	while ((word)p != INULL) { // пока есть аргументы
		assert (reference_type(p) == TPAIR); // assert(list)
		assert (t == RNULL || reference_type(t) == TPAIR); // assert(list)

		// speedup: all except primitive types hide under fft-any
		word tty = is_pair(t) ? car(t) : I(TANY);
		word arg = (word) car(p);

#if (__x86_64__ && (__unix__ || __APPLE__))		// linux x64 and mac intel, struct-by-value-passing (check this)
		if (i == GRNC && l) { // ??
			i = l; // check is l needed?
		}
#endif

		// подготовим маску к следующему аргументу
#if (__x86_64__ && (__unix__ || __APPLE__)) // LP64, but without arm64
		fpmask <<= 1;
#elif __EMSCRIPTEN__
		fpmask <<= 2;
#endif

#if __aarch64__
		// TODO: add api to extend "extra" array or dynamically resize
		// #define SAVE_ARM64(type, conv, arg) {
		// 	ALIGN_NSAA(e, type);
		// 	if (__builtin_expect((e > extra_len - sizeof(type)), 0)) { /* unlikely */
		// 		/* possible optimization (depends on compiler) - alloca only delta
		// 		   and still use old extra. maybe create own assembly functions.*/
		// 		int new_extra_len = extra_len + 8 * sizeof(word);
		// 		char * new_extra = alloca(new_extra_len);
		// 		memcpy(new_extra, extra, extra_len);
		// 		extra = new_extra;
		// 		extra_len = new_extra_len;
		// 	}
		// 	*(type*)&extra[e] = (type) conv(arg);
		// 	i-- /* adjust i (because later we have i++) */, e += sizeof(type);
		// }

#	if __APPLE__ /* m1 */
		#define STORE_STCK(type, conv, arg) {\
			TALIGN(e, type); \
			*(type*)&extra[e] = (type) conv(arg);\
			i-- /* adjust i (because later we have i++) */, e += sizeof(type);\
		}
		#define STORE(conv, type, arg) ({\
			if (__builtin_expect((i >= GRNC), 0)) \
				STORE_STCK(type, conv, arg) \
			else \
				args[i] = (word) conv (arg);\
		})
		#define STORE_F(conv, type, arg) ({\
			if (__builtin_expect((d >= FRNC), 0)) {\
                i = max(i, GRNC); \
				STORE_STCK(type, conv, arg) \
            } else \
				*(type*)&ad[d++] = conv(arg), --i;\
		})
#	else
		#define STORE(conv, type, arg) ({\
			args[i] = (word) conv (arg);\
		})
		#define STORE_F(conv, type, arg) ({\
			if (d < FRNC) \
				*(type*)&ad[d++] = conv(arg), --i;\
			else { \
				i = max(i, GRNC); \
				*(type*)&args[i] = conv(arg);\
			} \
		})
#	endif

		#define STORE_SERIALIZE(type, conv) {\
				word ptr = 0; \
				SERIALIZE(&ptr, type, conv); \
				STORE(IDF, void*, ptr); \
			}
		#define STORE_D(conv, type, arg) \
				STORE_F(conv, type, arg)

#else
		// todo: fix 32-bit STORE?
		// todo: change to args[i++] ?
		#define STORE(conv, type, arg) \
			args[i] = (type) conv(arg)
		#define STORE_SERIALIZE(type, conv) \
			SERIALIZE(&args[i], type, conv) // todo: change to args[i++] and i+=2 for 32-bit

		// floating point handlers
#	if (__x86_64__ && (__unix__ || __APPLE__))
		#define STORE_F(conv, type, arg) ({\
				*(type*)&ad[d++] = conv(arg); --i;\
				fpmask |= 1;\
		})
		#define STORE_D(conv, type, arg) \
				STORE_F(conv, type, arg)
#	elif __ARM_EABI__ && __ARM_PCS_VFP // only for -mfloat-abi=hard (?)
		#define STORE_F(conv, type, arg) ({\
				*(type*)&af[f++] = conv(arg); --i;\
		})
		#define STORE_D(conv, type, arg) ({\
				STORE_F(conv, type, arg);\
				++i; f++;\
		})
		// WebAssembly
#	elif __EMSCRIPTEN__
		#define STORE_F(conv, type, arg) ({\
				fpmask |= 0b10;\
				*(type*)&args[i] = conv(arg);\
		})
		#define STORE_D(conv, type, arg) ({\
				fpmask |= 0b11;\
				*(type*)&args[i] = conv(arg);\
				++i;\
		})
#	elif __mips__ /* 32-bit mips */
		#define STORE_F(conv, type, arg) ({\
				i = (i+1)&-2; /*dword align*/\
				*(type*)&args[i] = conv(arg);\
		})
		#define STORE_D(conv, type, arg) ({\
				STORE_F(conv, type, arg);\
				++i;\
		})
#	else
		#define STORE_F(conv, type, arg) ({\
				*(type*)&args[i] = conv(arg);\
		})
#		if __SIZEOF_DOUBLE__ > __SIZEOF_PTRDIFF_T__
		// 32-bits: doubles fills two words
		#define STORE_D(conv, type, arg) ({\
			STORE_F(conv, type, arg);\
			++i;\
		})
#		else
		#define STORE_D(conv, type, arg) ({\
				STORE_F(conv, type, arg);\
		})
#		endif
#	endif
#endif

		push:
		if (arg == IFALSE) { // #false is a universal "0" value
		//	- 0 -----------------------------------------------
			if (is_value(tty))
				switch (value(tty)) {
				case TINT8:  case TUINT8:
				case TINT16: case TUINT16:
				case TINT32: case TUINT32:
				case TINT64: case TUINT64:
					arg = I(0); goto push;

				case TFLOAT:
					STORE_F(IDF, float, 0);
					break;
				case TDOUBLE:
					IFmips32(i = (i+1)&-2); // 32-bit mips dword align
					STORE_D(IDF, double, 0);
					break;
				default:
					STORE(IDF, word, 0);
				}
			else
			if (is_pointer(car(tty)))
				STORE(IDF, word, 0);
			else
				E("structure can't be #f");
		}
		else
		if (is_value(tty)) {
		//	- V -----------------------------------------------
			switch (value(tty)) {
			// -------------------
			// целочисленные типы:

	#if __aarch64__
			case TINT8:  case TUINT8:
				STORE(to_int, int8_t, arg);
				break;
			case TINT16: case TUINT16:
				STORE(to_int, int16_t, arg);
				break;
			case TINT32: case TUINT32:
				STORE(to_int, int32_t, arg);
				break;
			case TINT64: case TUINT64:
				IFmips32(i = (i+1)&-2); // 32-bit mips dword align
				STORE(to_int, int64_t, arg);
				break;
	#else
			case TINT8:  case TUINT8:
			case TINT16: case TUINT16:
			case TINT32: case TUINT32:
				STORE(to_int, int32_t, arg);
				break;

			case TINT64: case TUINT64:
	# if UINT64_MAX > UINTPTR_MAX
				// 32-bit machines
				IFmips32(i = (i+1)&-2); // 32-bit mips dword align
				*(int64_t*)&args[i++] = to_int64(arg);
	# else
				// 64-bit machines
				STORE(to_int, int64_t, arg);
	# endif
				break;
	#endif

			// -------------------
			// с плавающей запятой:
			case TFLOAT:
				STORE_F(OL2F, float, arg);
				break;
			case TDOUBLE:
			tdouble:
				IFmips32(i = (i+1)&-2); // 32-bit mips dword align
				STORE_D(OL2D, double, arg);
				break;

			// bool
			case TBOOL:
				STORE(IDF, _Bool, (arg == IFALSE) ? 0 : 1);
				break;

			// поинтер на данные
			case TUNKNOWN:
				STORE(IDF, word, &car(arg));
			//	args[i] = (word) &car(arg);
				break;

			// free variables, pointers and structures
			case TANY: {
				// аргументы закончились, началась "..."
				#if defined(__aarch64__) && defined(__APPLE__)
					// M1 specific:
					//  https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms#Update-code-that-passes-arguments-to-variadic-functions
					if (t == RNULL) {
						i = GRNC; d = FRNC; // теперь аргументы падают в отдельный стек
					}
				#endif
				// automatic types:
				if (is_value(arg))
					STORE(to_int, int32_t, arg);
				else
				switch (reference_type(arg)) {
				case TVPTR: // value of vptr
				case TCALLABLE: // same as ^
					STORE(IDF, word, car(arg));
					break;
				case TBYTEVECTOR: // address of bytevector data (no copying to stack)
					STORE(IDF, word, (word)&car(arg));
					break;
				// any strings acts as "char*"
				case TSTRING:
				case TSTRINGWIDE:
				case TSTRINGDISPATCH:
				case TSYMBOL: {
					char* str;
					store_string(&fp, &str, arg);
					STORE(IDF, word, str);
					break;
				}
				case TINEXACT:
					goto tdouble;

				// type override:
				// '(type . arg)
				case TPAIR:
					tty = car(arg); arg = cdr(arg);
					goto push;
				}
				break;
			}

			// void*
			case TVPTR:
			tvptr:
				if (is_reference(arg))
					switch (reference_type(arg)) {
					case TVPTR:
						STORE(IDF, word, car(arg));
						break;
					// can be used instead of vptr, dangerous!
					case TBYTEVECTOR:
						STORE(IDF, word, &car(arg));
						break;
					// '(bytevector . offset)
					case TPAIR: {
						word bytevector = car(arg);
						unsigned offset = unumber(cdr(arg));
						if ((is_reference(bytevector)) &&
							(reference_type(bytevector) == TBYTEVECTOR) &&
							(offset < rawstream_size(bytevector)))
						{
							STORE(IDF, word, ((char*)&car(bytevector)) + offset);
						}
						break;
					}
					default:
						not_a_type("vptr");
					}
				else
					not_a_type("vptr");
				break;


			case TBYTEVECTOR:
			tbytevector:
				switch (reference_type(arg)) {
				case TBYTEVECTOR:
				case TSTRING: // ansi strings marshaling to bytevector data "as is" without conversion
					STORE(IDF, word, &car(arg));
					break;

				default:
					not_a_type("bytevector");
				}
				break;

			case TSTRING:
			tstring:
				store_string(&fp, (char**)&args[i], arg);
				break;

			case TSTRINGWIDE:
			tstringwide:
				store_stringwide(&fp, &args[i], arg);
				break;

			case TCALLABLE: {
				if (is_callable(arg))
					STORE(IDF, word, car(arg));
				else
					not_a_type("callable");
				break;
			}

			case TPORT: {
				if (arg == make_enum(-1)) { // ?
					STORE(IDF, word, -1);
					break;
				}
				int portfd = port(arg);
				switch (portfd) {
				case 0: // stdin
					STORE(IDF, word, STDIN_FILENO);
					break;
				case 1: // stdout
					STORE(IDF, word, STDOUT_FILENO);
					break;
				case 2: // stderr
					STORE(IDF, word, STDERR_FILENO);
					break;
				default:
					STORE(IDF, word, portfd);
					break;
				}
				break;
			}
			case TVOID:
				// do nothing, just for better readability
				STORE(IDF, word, 0);
				break;
			default:
				E("can't recognize %d type", value(tty));
			}
		}
		else
		if (is_pointer(car(tty))) {
		//	- P -----------------------------------------------
			writeback |= (value(car(tty)) & FFT_REF);

			if (is_value(cdr(tty))) {
				// pointer to array
				switch (value(cdr(tty))) {
					// целочисленные ссылочные типы
					case TINT8: case TUINT8:
						STORE_SERIALIZE(int8_t, to_int); break;
					case TINT16: case TUINT16:
						STORE_SERIALIZE(int16_t, to_int); break;
					case TINT32: case TUINT32:
						STORE_SERIALIZE(int32_t, to_int); break;
					case TINT64: case TUINT64:
						STORE_SERIALIZE(int64_t, to_int); break;
					// с плавающей запятой:
					case TFLOAT:
						STORE_SERIALIZE(float, OL2F); break;
					case TDOUBLE:
						STORE_SERIALIZE(double, OL2D); break;
					case TVOID: // same as type-vptr
						goto tvptr;
					// raw pointers to pointers
					case TVPTR: {
						if (arg == INULL) // empty list will be sent as nullptr
							break;
						if (reference_type(arg) == TVPTR || reference_type(arg) == TBYTEVECTOR) // single vptr value or bytevector (todo: add bytevector size check)
							STORE(IDF, word, &car(arg));
						// pointer to structure, instant structure declaration
						else if (is_pair(arg)) {
							size_t size = structure_size(0, car(arg));
							void* payload = alloca(size);
							STORE(IDF, word, payload);
							store_structure(&fp, payload, 0, car(arg), cdr(arg));
						}
						else
							not_a_type("vptr*");
						break;
					}
					case TSTRING: {
						int size = list_length(arg);

						word* p = new (TBYTEVECTOR, size, 0);
						STORE(IDF, word, ++p);

						size++;

						word src = arg;
						while (--size) {
							word str = car(src);

							int stype = reference_type(str);
							word ptr = *p++ = (word) &fp[1];
							new_bytevector(copychars((char*)ptr, str,
								stype == TSTRING ? chars2ol :
								stype == TSTRINGWIDE ? wchars2utf8 :
								stype == TSTRINGDISPATCH ? stringleaf2ol :
								not_a_string));

							src = cdr(src);
						}
						break;
					}
					// case TBYTEVECTOR+PTR:
					// case TBYTEVECTOR+REF:

				}
			}
			else {
				// pointer to structure
				assert (is_pair(cdr(tty)));
				size_t size = structure_size(0, cdr(tty));
				TALIGN(size, int); // structure's padding
				void* payload = alloca(size);
				STORE(IDF, word, payload);
				store_structure(&fp, payload, 0, cdr(tty), arg);
			}
		}
		else {
		//	- S -----------------------------------------------
			assert(is_pair(arg)); // structure by value, must be defined
			// pack structure into bytevector
			// todo: move to the standalone function named `struct2ol`
			// https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf

			// note the aarch64 special case:
			//  if struct has 1..4 only floats or has 1..4 only doubles
			//  then special parameter passing rule must be used
			// https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst#parameter-passing-rules
			ONLYaarch64
			unsigned count = 0, fs = 0, ds = 0; // ONLY aarch64

			// 1. calculate size
			// todo: change to structure_size call (?)
			size_t size = 0;
			// http://www.catb.org/esr/structure-packing/
			for (word p = car(t); p != INULL; p = cdr(p)) {
				word subtype = car(p);

				assert (is_value(subtype));
				word stv = value(subtype);
				// внутриструктурное выравнивание (по умолчанию)
				switch (stv) {
					case TINT8: case TUINT8:
						size += sizeof(int8_t);  break;
					case TINT16: case TUINT16:
						TALIGN(size, int16_t); size += sizeof(int16_t);  break;
					case TFLOAT:
						fs++; // fall through
					case TINT32: case TUINT32:
						TALIGN(size, int32_t); size += sizeof(int32_t);  break;
					case TDOUBLE:
						ds++; // fall through
					case TINT64: case TUINT64:
	#if	__i386__ && __unix__
						TALIGN(size, int32_t); size += sizeof(int64_t);  break;
	#else // _WIN32, _WIN64, aarch64, etc.
						TALIGN(size, int64_t); size += sizeof(int64_t);  break;
	#endif
				}
				count++;
			}
			// todo: align for aarch64 for 8 bytes
			TALIGN(size, word); // total size should be word aligned

			int j = i;
			char* ptr;
#if __aarch64__
			// two special cases for HFA structures
			if (count == fs && fs < 5 && d + fs <= FRNC) {
				for (word a = arg, c = 0; c < fs; c++) {
					if (a == INULL)
						break;
					*(float*)&
					ad[d++] = OL2F(car(a)); a = cdr(a);
				}
				goto next_argument;
			}
			if (count == ds && ds < 5 && d + ds <= FRNC) {
				for (word a = arg, c = 0; c < ds; c++) {
					if (a == INULL)
						break;
					ad[d++] = OL2D(car(a)); a = cdr(a);
				}
				goto next_argument;
			}

			// well, not an HFA, let's check the size
			// if (size > 16) { // should send using stack //?
			// 	j = max(i, GRNC);
			// 	//d = max(d, FRNC);
			// }

			// If the argument type is a Composite Type that is larger than 16 bytes,
			// then the argument is copied to memory allocated by the caller and the
			// argument is replaced by a pointer to the copy.
			if (size > 16) {
				ptr = (char*) alloca(size);
				args[i] = (word) ptr;
			}
			else
#elif __x86_64__
			int integer = 0; // 8-bit block should go to general register(s)
#	if __unix__ || __APPLE__
			if (size > 16) // should send using stack //?
				j = max(i, GRNC, l);
#	endif
#	if _WIN64
			if (size >= 16) {
				// the caller-allocated temporary memory must be 16-byte aligned (?)
				// int size16 = (size + 15) & -16;
				// ptr = (char*)alloca(size16 + 16);
				// ptr = (char*)(((word)ptr + 15) & -16);
				ptr = (char*) alloca(size);
				args[i] = (word) ptr;
			}
			else
#	endif
#endif
			ptr = (char*)&args[j];

			size_t offset = 0;
			for (word p = car(t), a = arg; ; p = cdr(p), a = cdr(a)) {
				word subtype = (p != INULL) ? car(p) : I(0);

				assert (is_value(subtype));
				word stv = value(subtype);

				switch (stv) {
					case TINT8:  case TUINT8:
						// offset = offset;
						break;
					case TINT16: case TUINT16:
						TALIGN(offset, int16_t);
						break;
					case TINT32: case TUINT32:
					case TFLOAT:
						TALIGN(offset, int32_t);
						break;
					case TINT64: case TUINT64:
					case TDOUBLE:
	#if	__i386__ && __unix__
						TALIGN(offset, int32_t);
	#else // _WIN32, _WIN64, aarch64, etc.
						TALIGN(offset, int64_t);
	#endif
						break;
					case 0: // 0 means "no more arguments"
						TALIGN(offset, word);
						break;
				}

#if __aarch64__
				if (size <= 16)
#endif
#if _WIN64
				if (size < 16)
#endif
				if (offset >= sizeof(word)) { // пришло время "сложить" данные в регистр
					assert (offset % sizeof(word) == 0);

#if __x86_64__ && (__unix__ || __APPLE__)
					if (integer || (size > 16)) { // в целочисленный регистр
						fpmask <<= 1;
						j++; ptr += 8;

						// если добрались до стека, а он уже что-то содержит
						if (j == 6 && l)
							j = l;
					}
					else { // в регистр с плавающей запятой
						// move from ptr to the ad
						*(int64_t*)&ad[d++] = args[j];
						fpmask |= 1;
					}
					integer = 0;
#else
					j += offset / sizeof(word);
					ptr += offset / sizeof(word) * sizeof(word);
#endif
					offset %= sizeof(word);
				}

				// аргументы закончились?
				if (p == INULL)
					break;
				assert (a != INULL);
				// если аргументов недостаточно запушим 0?
				// word arg = (a != INULL) ? car(a) : I(0);

				switch (stv) {
					case TINT8:  case TUINT8: {
						*(int8_t *)&ptr[offset] = (int8_t )to_int(car(a));
						offset += sizeof(int8_t); IFx86_64(integer = 1);
						break;
					}
					case TINT16: case TUINT16: {
						*(int16_t*)&ptr[offset] = (int16_t)to_int(car(a));
						offset += sizeof(int16_t); IFx86_64(integer = 1);
						break;
					}
					case TINT32: case TUINT32: {
						*(int32_t*)&ptr[offset] = (int32_t)to_int(car(a));
						offset += sizeof(int32_t); IFx86_64(integer = 1);
						break;
					}
					case TINT64: case TUINT64: {
						*(int64_t*)&ptr[offset] = (int64_t)to_int64(car(a));
						offset += sizeof(int64_t); IFx86_64(integer = 1);
						break;
					}

					case TFLOAT: {
						*(float *)&ptr[offset] = OL2F(car(a));
						offset += sizeof(float);
						break;
					}
					case TDOUBLE: {
						*(double*)&ptr[offset] = OL2D(car(a));
						offset += sizeof(double);
						break;
					}
				}
			}

#if __x86_64__ && (__unix__ || __APPLE__)
			if (size > 16 && i < 6)
				l = j;
			else
#elif _WIN64
			if (size < 16)
#endif
				i = j;

			if (offset == 0)
				i--; // no data left, adjust i (because later we have i++);
		}
		i++;
next_argument:
		p = (word*)cdr(p);
		t = (t == RNULL) ? t : (word*)cdr(t);
	}
	assert ((word)t == INULL); // количество аргументов должно совпадать

//	if (fpmask == 15)
//		__asm__("int $3");

	if (function) {
		size_t pin = OLVM_pin(this, arguments);
		heap->fp = fp; // сохраним, так как в xxx_call() могут быть вызваны коллейблы, и они попортят fp

#if  __x86_64__
# if _WIN64
		got = win64_call(args, i, function, returntype & 0x3F);
# endif
# if __unix__ || __linux__ || __APPLE__ // System V (unix, linux, osx)
		got = nix64_call(args, ad, max(i, l), d, fpmask, function, returntype & 0x3F);
# endif

#elif __i386__
		// cdecl and stdcall in our case are same, so...
		got = x86_call(args, i, function, returntype & 0x3F);

#elif __aarch64__
# if __APPLE__ // M1 .. MN
		i += WORDS(e);
# endif
		got = arm64_call(args, ad, i, d, NULL, function, returntype & 0x3F, 0);

#elif __arm__
	// arm calling abi http://infocenter.arm.com/help/topic/com.arm.doc.ihi0042f/IHI0042F_aapcs.pdf
#	ifndef __ARM_PCS_VFP
		got = arm32_call(args, i, function);
#	else // (?)
		got = arm32_call(args, NULL,
	        i, 0,
	        function, returntype & 0x3F); //(?)
#	endif

#elif __mips__
		got = mips32_call(args, i, function, returntype & 0x3F);

#elif __EMSCRIPTEN__
		// todo: handle 64-bit calls
		got = wasm32_call(args, fpmask, function, returntype & 0x3F);

#elif __sparc64__
		got = call_x(args, i, function, returntype & 0x3F);

#else // ALL other
		got = call_x(args, i, function, returntype & 0x3F);

/*	inline ret_t call_cdecl(word args[], int i, void* function, int type) {
		CALL(__cdecl);
	}
	inline ret_t call_stdcall(word args[], int i, void* function, int type) {
		CALL(__stdcall);
	}

	switch (returntype >> 6) {
	case 0:
	case 1:
		got = call_cdecl(args, i, function, returntype & 0x3F);
		break;
	case 2:
		got = call_stdcall(args, i, function, returntype & 0x3F);
		break;
	// FASTCALL: (3)
	case 3:
	// THISCALL: (4)
	// ...
	default:
		E("Unsupported calling convention %d", returntype >> 6);
		break;
	}*/
#endif
		// где гарантия, что C и B не поменялись? нет такой
		fp = heap->fp;
		arguments = OLVM_unpin(this, pin);

		// todo: optimize for "if writeback && returntype&0x3F == TVOID"
		register word ABC = arguments;
					  ABC = cdr(ABC); // skip A
		B = car(ABC); ABC = cdr(ABC); // rtty
		C = car(ABC); ABC = cdr(ABC); // args
	}
	else // just mirror argument
		got = *(ret_t *)args;

	// флажок, что среди параметров есть те, что надо заполнить (fft&) назад
	if (writeback) {
		// пробежимся по аргументам, найдем нужные
		p = (word*)C;   // arguments
		t = (word*)cdr (B); // rtti

		i = 0;
		while ((word)p != INULL) { // пока есть аргументы
			// assert (reference_type(p) == TPAIR); // assert (list)
    		// assert (t == RNULL || reference_type(t) == TPAIR); // assert (list)
	
			//int type = (is_pair(t) && is_value(car(t))) ? value(car(t)) : TANY;
			word tty = is_pair(t) ? car(t) : I(TANY);
			word arg = (word) car(p);

			wbloop: // writeback loop
			if (arg == IFALSE)
				; // nothing
			else
			if (is_value(tty)) {
			//	- V -----------------------------------------------
				if (value(tty) == TANY) {
					if (is_reference(arg))
					if (reference_type(arg) == TPAIR) {
						tty = car(arg); arg = cdr(arg);
						goto wbloop;
					}
				}
			}
			else
			if (is_pointer(car(tty)) && (value(car(tty)) & FFT_REF)) {
			//	- P -----------------------------------------------
				if (is_value(cdr(tty))) {
					// pointer to array
					switch (value(cdr(tty))) {
						case TINT8:   DESERIALIZE(signed char); break;
						case TUINT8:  DESERIALIZE(unsigned char); break;
						case TINT16:  DESERIALIZE(signed short); break;
						case TUINT16: DESERIALIZE(unsigned short); break;
						case TINT32:  DESERIALIZE(signed int); break;
						case TUINT32: DESERIALIZE(unsigned int); break;
						case TINT64:  DESERIALIZE(signed long long); break;
						case TUINT64: DESERIALIZE(unsigned long long); break;
						case TFLOAT:  DESERIALIZE(float); break;
						case TDOUBLE: DESERIALIZE(double); break;

						case TVPTR: {
							if (arg == INULL) // empty array will be sent as nullptr
								break;
							// TODO: handle vectors!
							if (reference_type(arg) == TVPTR || reference_type(arg) == TBYTEVECTOR) // single vptr value or bytevector (todo: add bytevector size check)
								break; // nothing to do

							int c = list_length(arg);
							void** f = (void**)args[i];

							word l = arg;
							while (c--) {
								void* value = *f++;
								word num = car(l);
								assert (reference_type(num) == TVPTR);
								*(void**)&car(num) = value;

								l = cdr(l);
							}
						}
						break;
					}
				}
				else {
					// pointer to structure
					assert (is_pair(cdr(tty)));

					word structure = cdr(tty);
					void* payload = (void*) args[i];
					restore_structure(payload, 0, structure, arg);
				}
			}

			p = (word*)cdr(p);
			t = (t == RNULL) ? t : (word*)cdr(t);
			i++;
		}
	}

	// RETURN
handle_got_value:
	returntype &= 0x3F;
	word* result = (word*)IFALSE;

	// // special case of returning a structure:
	// // allocate a space for the return value
	// if (is_pair(car(B)) && value(caar(B)) == TBYTEVECTOR) {
	// 	if (value(cdar(B)) > sizeof(ret_t))
	// 		result = (word*)args[0] - 1;
	// 	else {
	// 		int len = value(cdar(B));
	// 		result = new_bytevector(len);
	// 		memcpy((void*) &car(result), &got, len);
	// 	}
	// }
	// else // usual case
	switch (returntype) {
		// TENUMP - deprecated, TODO: remove
		case TENUMP: // type-enum+ - если я уверен, что число заведомо меньше 0x00FFFFFF! (или сколько там в x64)
			result = (word*) make_enum (CV_LONG(&got));
			break;

		case TINT8:
			result = (word*) new_number (CV_INT8(&got));
			break;
		case TINT16:
			result = (word*) new_number (CV_INT16(&got));
			break;
		case TINT32:
			result = (word*) new_number (CV_INT32(&got));
			break;
		case TINT64: {
			result = (word*) new_number (CV_INT64(&got));
			break;
		}

		case TUINT8:
			result = (word*) new_number (CV_UINT8(&got));
			break;
		case TUINT16:
			result = (word*) new_number (CV_UINT16(&got));
			break;
		case TUINT32:
			result = (word*) new_number (CV_UINT32(&got));
			break;
		case TUINT64: {
			result = (word*) new_number (CV_UINT64(&got));
			break;
		}


		case TPORT:
			result = (word*) make_port (CV_LONG(&got)); // todo: make port like in SYSCALL_OPEN
			break;
		case TVOID:
			result = (word*) ITRUE;
			// special case: returning a structure (?)
			if (is_pair(car(B)) && value(caar(B)) == TBYTEVECTOR) {
				size_t len = value(cdar(B));
				void* vptr = CV_VOIDP(&got);

				if (len > sizeof(ret_t)) // returning a big structure
					result = ((word*)vptr) - 1; //? TODO check this!!
				else { // returning a small structure
					result = new_bytevector(len);
					__builtin_memcpy(&car(result), vptr, len);
				}
			}
			break;

		case TBOOL:
			result = (CV_INT8(&got) == 0) ? (R)IFALSE : (R)ITRUE;
			break;

		case TVPTR: {
			void* vptr = CV_VOIDP(&got);
			if (vptr)
				result = new_vptr(vptr);
			break;
		}

		case TSTRING: { // assume that input string is a valid utf-8
			void* vptr = CV_VOIDP(&got);
			if (vptr) {
				heap->fp = fp;
				result = string2ol(this, vptr);
				fp = heap->fp;
			}
			break;
		}
		case TSTRINGWIDE: {
			void* vptr = CV_VOIDP(&got);
			if (vptr) {
				widechar* p = (widechar*) vptr;
				unsigned words = 0;
				while (i++ <= VMAX && *p++) words++;

				if (fp + words > heap->end) {
					heap->fp = fp;
					heap->gc(this, words);
					fp = heap->fp;
				}

				word* str = result = new (TSTRINGWIDE, words);
				p = (widechar*) vptr;
				while (*p)
					*++str = I(*p++);
			}
			break;
		}

		// возвращаемый тип не может быть TRATIONAL, так как непонятна будет точность
		case TFLOAT:
		case TDOUBLE: {
			inexact_t value =
				(returntype == TFLOAT)
					? CV_FLOAT(&got)
					: CV_DOUBLE(&got);

#if OLVM_INEXACTS
			result = new_inexact(value);
#else
			heap->fp = fp;
			result = (word*) d2ol(heap, value);
			fp = heap->fp;
#endif
			break;
		}

		// returning data value by pointer or structures
		case TPAIR: {
			// data value by pointer
			word b = car(B);
			if (is_value(car(b)) && (value(car(b)) & FFT_REF)) {
				switch (value(cdr(b))) {
				// integers
				case TINT8:
					result = (word*) new_number (CV_INT8((void*)got));
					break;
				case TUINT8:
					result = (word*) new_number (CV_UINT8((void*)got));
					break;
				case TINT16:
					result = (word*) new_number (CV_INT16((void*)got));
					break;
				case TUINT16:
					result = (word*) new_number (CV_UINT16((void*)got));
					break;
				case TINT32:
					result = (word*) new_number (CV_INT32((void*)got));
					break;
				case TUINT32:
					result = (word*) new_number (CV_UINT32((void*)got));
					break;
				case TINT64:
					result = (word*) new_number (CV_INT64((void*)got));
					break;
				case TUINT64:
					result = (word*) new_number (CV_UINT64((void*)got));
					break;

				// float/double
				case TFLOAT:
					result = (word*) new_inexact (CV_FLOAT((void*)got));
					break;
				case TDOUBLE:
					result = (word*) new_inexact (CV_DOUBLE((void*)got));
					break;
				}
			}
			else {
				// currest simplest case
				float* floats = (float*)&got;
				inexact_t a = floats[0];
				inexact_t b = floats[1];
				result = new_list(TPAIR,
					new_inexact(a),
					new_inexact(b));
			}
			break;
		}
	}

	heap->fp = fp;
	return result;
}

/** This function returns size of basic types:
 *  1 .. 32 - primitive types
 * 48 .. 63 - ffi types
 */
static
int c_sizeof(int type)
{
	switch (type) {
		// ansi c types
		case 1: return sizeof(char);
		case 2: return sizeof(short);
		case 3: return sizeof(int);
		case 4: return sizeof(long);
		case 5: return sizeof(long long);
		case 6: return sizeof(size_t);
		// floating point types
		case 10: return sizeof(float);
		case 11: return sizeof(double);
		// etc.
		case 20: return sizeof(void*);
	}
	return 0;
}
static
int ffi_sizeof(int type)
{
	switch (type) {
		// ffi types
		case TVOID: return sizeof(void*);
		case TBOOL: return sizeof(_Bool);

		case TFLOAT: return sizeof(float);
		case TDOUBLE: return sizeof(double);

		case TINT8:  return sizeof(int8_t);
		case TINT16: return sizeof(int16_t);
		case TINT32: return sizeof(int32_t);
		case TINT64: return sizeof(int64_t);

		case TUINT8:  return sizeof(uint8_t);
		case TUINT16: return sizeof(uint16_t);
		case TUINT32: return sizeof(uint32_t);
		case TUINT64: return sizeof(uint64_t);
	}
	return 0;
}

EXPORT
word OLVM_sizeof(olvm_t* self, word* arguments)
{
	(void) self;

	word A = car(arguments);
	if (is_value(A)) {
		int type = value(A);
		// ansi c types
		int size = c_sizeof(type);
		if (size) return I(size);
		// ffi types
		if (type & (FFT_PTR|FFT_REF)) {
			size = ffi_sizeof(type & ~(FFT_PTR|FFT_REF));
			if (size)
				size = sizeof(int*); // typed pointer
		}
		else
			size = ffi_sizeof(type);
		if (size) return I(size);
	}
	else if (is_pair(A))
		return I(structure_size(0, A));

	return IFALSE;
}

#endif//OLVM_FFI


// --=( CALLABLES support )=-----------------------------
// --
#if OLVM_CALLABLES

// http://man7.org/tlpi/code/faq.html#use_default_source
//  glibc version 6+ uses __GLIBC__/__GLIBC_MINOR__
// #ifndef _DEFAULT_SOURCE
// # ifndef __APPLE__
// #	error "Required -std=gnu11 (we use anonymous mmap)"
// # endif
// #endif

// todo: добавить возможность вызова колбека как сопрограммы (так и назвать - сопрограмма)
//       который будет запускать отдельный поток и в контексте колбека ВМ сможет выполнять
//       все остальные свои сопрограммы.
// ret is ret address to the caller function
static
int64_t callback(olvm_t* ol, size_t id, int_t* argi
	#if __x86_64__ || __aarch64__
		, inexact_t* argf, int_t* rest
	#endif
	);


// todo: удалить userdata api за ненадобностью (?) и использовать пин-api
// JIT howto: http://eli.thegreenplace.net/2013/11/05/how-to-jit-an-introduction
EXPORT
word OLVM_mkcb(olvm_t* self, word* arguments)
{
	word* A = (word*)car(arguments);
	unless (is_value(A))
		return IFALSE;

	word* fp;
	char* ptr = 0;

	int pin = unumber(A);

#ifdef __i386__ // x86 linux/windows
	// long callback(olvm_t* ol, int id, int* argv)
	static char bytecode[] =
			"\x90" // nop
			"\x8D\x44\x24\x04" // lea eax, [esp+4]
			"\x50"     // push eax
	/* 6*/	"\x68-id-" // push id
	/*11*/	"\x68-ol-" // push self
	/*16*/	"\xB8-cb-" // mov eax, &callback
			"\xFF\xD0" // call eax
			"\x83\xC4\x0C" // add esp, 3*4
			"\xC3"; // ret
# ifdef _WIN32
		HANDLE mh = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_EXECUTE_READWRITE,
				0, sizeof(bytecode), NULL);
		if (!mh)
			return IFALSE;
		ptr = MapViewOfFile(mh, FILE_MAP_ALL_ACCESS,
				0, 0, sizeof(bytecode));
		CloseHandle(mh);
		if (!ptr)
			return IFALSE;
# else
		ptr = mmap(0, sizeof(bytecode), PROT_READ | PROT_WRITE | PROT_EXEC,
				MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
		if (ptr == (char*) -1)
			return IFALSE;
# endif

	memcpy(ptr, &bytecode, sizeof(bytecode));
	*(int32_t*)&ptr[7] = pin;
	*(uint32_t*)&ptr[12] = (uint32_t)self;
	*(uint32_t*)&ptr[17] = (uint32_t)&callback;
#elif __x86_64__
	// Windows x64
# ifdef _WIN32
	//long long callback(olvm_t* ol, int id, long long* argi, double* argf, long long* rest)
	static char bytecode[] =
			"\x90" // nop
			"\x48\x8D\x44\x24\x28"  // lea rax, [rsp+40] (rest)
			"\x55"                  // push rbp
			"\x48\x89\xE5"          // mov rbp, rsp
			"\x41\x51"              // push r9
			"\x41\x50"              // push r8
			"\x52"                  // push rdx
			"\x51"                  // push rcx
			"\x49\x89\xE0"          // mov r8, rsp        // argi
			"\x48\x83\xEC\x20"      // sub rsp, 32
			"\xF2\x0F\x11\x44\x24\x00"    // movsd [rsp+ 0], xmm0
			"\xF2\x0F\x11\x4C\x24\x08"    // movsd [rsp+ 8], xmm1
			"\xF2\x0F\x11\x54\x24\x10"    // movsd [rsp+16], xmm2
			"\xF2\x0F\x11\x5C\x24\x18"    // movsd [rsp+24], xmm3
			"\x49\x89\xE1"          // mov r9, rsp         // argf
			// 50
			"\x48\xBA---id---"      // mov rdx, 0          // id
			"\x48\xB9---ol---"      // mov rcx, 0          // ol
			// 70
			"\x50"	                // push rax // dummy
			"\x50"                  // push rax            // rest
			"\x48\x83\xEC\x20"      // sub rsp, 32         // free space
			// 76
			"\x48\xB8--call--"      // mov rax, callback
			"\xFF\xD0"              // call rax
			"\xC9"                  // leave
			"\xC3";                 // ret

	HANDLE mh = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_EXECUTE_READWRITE,
			0, sizeof(bytecode), NULL);
	if (!mh)
		return IFALSE;
	ptr = MapViewOfFile(mh, FILE_MAP_ALL_ACCESS | FILE_MAP_EXECUTE,
			0, 0, sizeof(bytecode));
	CloseHandle(mh);
	if (!ptr)
		return IFALSE;

	memcpy(ptr, &bytecode, sizeof(bytecode));
	*(int64_t*)&ptr[52] = pin; // todo: change to uint64_t
	*(uint64_t*)&ptr[62] = (uint64_t)self;
	*(uint64_t*)&ptr[78] = (uint64_t)callback;

# else // System V (linux, unix, macos, android, ...)
	//long long callback(olvm_t* ol, int id, long long* argi, double* argf, long long* rest)
	// rdi: ol, rsi: id, rdx: argi, rcx: argf, r8: rest
	// not used: r9

	static char bytecode[] =
			"\x90" // nop
			"\x55"                  // push rbp
			"\x48\x89\xE5"          // mov rbp, rsp
			"\x41\x51"              // push r9
			"\x41\x50"              // push r8
			"\x51"                  // push rcx
			"\x52"                  // push rdx
			"\x56"                  // push rsi
			"\x57"                  // push rdi
			"\x48\x89\xE2"          // mov rdx, rsp         // argi
			"\x4C\x8D\x44\x24\x40"  // lea r8, [rsp+64]     // rest
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
			"\x48\x89\xE1"          // mov rcx, rsp        // argf
			// pin
	/*76*/	"\x48\xBE---id---"      // mov rsi, 0          // id
			// self
	/*86*/	"\x48\xBF---ol---"      // mov rdi, 0          // ol
			// callback
	/*96*/	"\x48\xB8--call--"      // mov rax, callback
			"\xFF\xD0"              // call rax
			"\xC9"                  // leave
			"\xC3";                 // ret
	ptr = mmap(0, sizeof(bytecode), PROT_WRITE,
			MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
	if (ptr == (char*) -1)
		return IFALSE;

	memcpy(ptr, &bytecode, sizeof(bytecode));
	*(int64_t*)&ptr[78] = pin;
	*(uint64_t*)&ptr[88] = (uint64_t)self;
	*(uint64_t*)&ptr[98] = (uint64_t)&callback;
	
	int res = mprotect(ptr, sizeof(bytecode), PROT_EXEC);
	if (res == -1)
		return IFALSE;
# endif
#elif __aarch64__

	//long long callback(olvm_t* ol, int id, long long* argi, double* argf, long long* rest);
	static char bytecode[] =
		"\xee\x03\x00\x91" //mov  x14, sp
		"\xfd\x7b\xbf\xa9" //stp  x29, x30, [sp,#-16]!
		"\xfd\x03\x00\x91" //mov  x29, sp
		// 
		"\xe6\x1f\xbf\xa9" //stp  x6, x7, [sp,#-16]!
		"\xe4\x17\xbf\xa9" //stp  x4, x5, [sp,#-16]!
		"\xe2\x0f\xbf\xa9" //stp  x2, x3, [sp,#-16]!
		"\xe0\x07\xbf\xa9" //stp  x0, x1, [sp,#-16]!
		"\xe2\x03\x00\x91" //mov  x2, sp              // argi
		"\xff\x03\x01\xd1" //sub  sp, sp, #0x40
		"\xe0\x07\x00\x6d" //stp  d0, d1, [sp]
		"\xe2\x0f\x01\x6d" //stp  d2, d3, [sp,#16]
		"\xe4\x17\x02\x6d" //stp  d4, d5, [sp,#32]
		"\xe6\x1f\x03\x6d" //stp  d6, d7, [sp,#48]
		"\xe3\x03\x00\x91" //mov  x3, sp              // argf
		"\xe4\x03\x0e\xaa" //mov  x4, x14             // rest
		// self
/*15*/	"\x00\x00\x80\xd2" //mov  x0, #0x0000
		"\x00\x00\xa0\xf2" //movk x0, #0x0000, lsl #16
		"\x00\x00\xc0\xf2" //movk x0, #0x0000, lsl #32
		"\x00\x00\xe0\xf2" //movk x0, #0x0000, lsl #48  // ol
		// id
/*19*/	"\x01\x00\x00\xd2" //mov  x1, #0x0000           // id
		// callback
/*20*/	"\x09\x00\x80\xd2" //mov  x0, #0x0000
		"\x09\x00\xa0\xf2" //movk x0, #0x0000, lsl #16
		"\x09\x00\xc0\xf2" //movk x0, #0x0000, lsl #32
		"\x09\x00\xe0\xf2" //movk x0, #0x0000, lsl #48  // ol

		"\x20\x01\x3f\xd6" //blr  x9
		"\xbf\x03\x00\x91" //mov  sp, x29
		"\xfd\x7b\xc1\xa8" //ldp  x29, x30, [sp],#16
		"\xc0\x03\x5f\xd6";//ret
	ptr = mmap(0, sizeof(bytecode), PROT_WRITE,
			MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
	if (ptr == (char*) -1)
		return IFALSE;

	memcpy(ptr, &bytecode, sizeof(bytecode));
	// self
	((uint32_t*)ptr)[15] = 0xD2800000 | ((((word)self      ) & 0xFFFF) << 5); // movz x0, #....
	((uint32_t*)ptr)[16] = 0xF2A00000 | ((((word)self >> 16) & 0xFFFF) << 5); // movk x0, #...., lsl #16
	((uint32_t*)ptr)[17] = 0xF2C00000 | ((((word)self >> 32) & 0xFFFF) << 5); // movk x0, #...., lsl #32
	((uint32_t*)ptr)[18] = 0xF2E00000 | ((((word)self >> 48) & 0xFFFF) << 5); // movk x0, #...., lsl #48
	// id
	assert (pin < 0x10000);
	((uint32_t*)ptr)[19] = 0xD2800001 | ((((word)pin) & 0xFFFF) << 5); // mov x1, pin
	// callback
	((uint32_t*)ptr)[20] = 0xD2800009 | ((((word)callback      ) & 0xFFFF) << 5); // movz x0, #....
	((uint32_t*)ptr)[21] = 0xF2A00009 | ((((word)callback >> 16) & 0xFFFF) << 5); // movk x0, #...., lsl #16
	((uint32_t*)ptr)[22] = 0xF2C00009 | ((((word)callback >> 32) & 0xFFFF) << 5); // movk x0, #...., lsl #32
	((uint32_t*)ptr)[23] = 0xF2E00009 | ((((word)callback >> 48) & 0xFFFF) << 5); // movk x0, #...., lsl #48

	int res = mprotect(ptr, sizeof(bytecode), PROT_EXEC);
	if (res == -1)
		return IFALSE;

#endif

	heap_t* heap = (heap_t*)self;
	fp = heap->fp;
	word result = (word) new_callable(ptr);
	heap->fp = fp;
	return result;
}


//long long callback(olvm_t* ol, int id, word* args) // win32
//long long callback(olvm_t* ol, int id, long long* argi, double* argf, long long* others) // linux
// notes:
//	http://stackoverflow.com/questions/11270588/variadic-function-va-arg-doesnt-work-with-float

static
__attribute__((used))
int64_t callback(olvm_t* ol, size_t id, int_t* argi // TODO: change "ol" to "this"
#if __x86_64__ || __aarch64__
		, inexact_t* argf, int_t* rest //win64
#endif
		)
{
	word* fp;
//	__asm("int $3");

	word cb = OLVM_deref(ol, id);
	word types = car(cb);
	word function = cdr(cb);

	word returntype = car(types); // returning type of lambda
	assert (is_enum(returntype));

	types = cdr(types);           // argument types

	// let's count of arguments
	word args = types;
	size_t a = 0;
	while (args != INULL) {
		a++;
		args = cdr(args);
	}

	word A[a]; // сложим все в стек, потом заполним оттуда список
	size_t count = a;

	heap_t* heap = (heap_t*)ol;
	fp = heap->fp;

	int i = 0;
#if (__x86_64__ || __aarch64__) && (__unix__ || __APPLE__)
	int j = 0;
#endif
/*#if __x86_64__  // !!!
	#if _WIN64
//	rest -= 4;
	#else
	rest -= 6;
	#endif
#endif*/
//	int f = 0; // linux
	while (types != INULL) {
		--a;
		// шаблон транслятора аргументов C -> OL
		#if __x86_64__
			#if _WIN64
			# define c2ol_value(type) \
			type value = i < 5 \
					? *(type*) &argi[i] \
					: *(type*) &rest[i-5];\
			i++;
			#else
			# define c2ol_value(type) \
			type value = i < 6 \
					? *(type*) &argi[i] \
					: *(type*) &rest[i-6]; \
			i++;
			#endif
		#elif __aarch64__
			# define c2ol_value(type) \
			type value = i < 8 \
					? *(type*) &argi[i] \
					: *(type*) &rest[i-8]; \
			i++;
		#else
			# define c2ol_value(type) \
			type value = *(type*) &argi[i++];
		#endif


		switch (car(types)) {
		case I(TVPTR): {
			c2ol_value(void*);
			A[a] = (word) new_vptr(value);
			break;
		}
		case I(TUINT8): {
			c2ol_value(unsigned char);
			A[a] = I(value);
			break; }
		case I(TUINT16): {
			c2ol_value(unsigned short);
			A[a] = I(value);
			break; }
		case I(TUINT32): {
			c2ol_value(unsigned int);
			A[a] = (word) new_number(value);
			break;
		}
		case I(TUINT64): {
			c2ol_value(unsigned long long);
			A[a] = (word) new_number(value);
			break;
		}

		case I(TINT8): {
			c2ol_value(signed char);
			A[a] = (word) new_number(value);
			break; }
		case I(TINT16): {
			c2ol_value(signed short);
			A[a] = (word) new_number(value);
			break; }
		case I(TINT32): {
			c2ol_value(signed int);
			A[a] = (word) new_number(value);
			break;
		}
		case I(TINT64): {
			c2ol_value(signed long long);
			A[a] = (word) new_number(value);
			break;
		}

		case I(TBOOL): {
			c2ol_value(signed char);
			A[a] = value == 0 ? IFALSE : ITRUE;
			break;
		}

		case I(TFLOAT): {
			float
			#if __x86_64__ || __aarch64__
				#if _WIN64
				value = i < 4
				        ? *(float*) &argf[i]
				        : *(float*) &rest[i-4];
				i++;
				#else
				value = j < 8
						? *(float*) &argf[j]
						: *(float*) &rest[j-8];
				j++;
				#endif
			#else
				value =   *(float*) &argi[i++];
			#endif
		#if OLVM_INEXACTS
			A[a] = (word) new_inexact(value);
		#else
			heap->fp = fp;
			A[a] = d2ol(heap, value);
			fp = heap->fp;
		#endif
			break;
		}
		case I(TDOUBLE): {
			double
			#if __x86_64__ || __aarch64__
				#if _WIN64
				value = i <= 4
				        ? *(double*) &argf[i]
				        : *(double*) &rest[i-4];
				i++;
				#else
				value = i < 8
						? *(double*) &argf[j]
						: *(double*) &rest[j-8];
				j++;
				#endif
			#else
				value =   *(double*) &argi[i++]; i++;
			#endif
		#if OLVM_INEXACTS
			A[a] = (word) new_inexact(value);
		#else
			heap->fp = fp;
			A[a] = d2ol(heap, value);
			fp = heap->fp;
		#endif
			break;
		}
		case I(TSTRING): {
			void*
			#if __x86_64__
			# if _WIN64
				value = i <= 4
				        ? *(void**) &argi[i]
				        : *(void**) &rest[i-4]; //?
				i++;
			# else
				value = i <= 6
						? *(void**) &argi[i]
						: *(void**) &rest[i-6]; // ???
				i++;
			# endif
			#elif __aarch64__
				value = i < 8
						? *(void**) &argi[i]
						: *(void**) &rest[i-8];
				i++;
			#else
				value =   *(void**) &argi[i++];
			#endif
			heap->fp = fp;
			A[a] = value ? (word)string2ol(ol, value) : IFALSE;
			fp = heap->fp;
			break;
		}
//		case I(TVOID):
//			A[a] = IFALSE;
//			i++;
//			break;
		default: {
			void*
			#if __x86_64__
			# if _WIN64
				value = i <= 4
				        ? *(void**) &argi[i]
				        : *(void**) &rest[i-4];
				i++;
			# else
				value = i <= 6
						? *(void**) &argi[i]
						: *(void**) &rest[i-6]; // ???
				i++;
			# endif
			#elif __aarch64__
				value = i < 8
						? *(void**) &argi[i]
						: *(void**) &rest[i-8];
				i++;
			#else
				value =   *(void**) &argi[i++];
			#endif

			if (is_pair(car(types))) {
				//int l = list_length(car(types));
				word tail = INULL;
				void** values = (void**)value;

				word argv = car(types);
				while (argv != INULL) {
					switch (car (argv)) {
					case I(TSTRING):
						tail = (word) new_pair(new_string(*values++), tail);
						break;
					case I(TVPTR):
						tail = (word) new_pair(new_vptr(*values++), tail);
						break;
					}
					argv = cdr (argv);
				}
				A[a] = tail;
				break;
			}

			// else error
			E("unknown argument type");
			break; }
		}
		types = cdr(types);
	}
	assert (a == 0);

	// let's put all arguments into list
	args = INULL;
	for (size_t i = 0; i < count; i++)
		args = (word) new_pair(A[i], args);

	heap->fp = fp; // well, done

	word r = OLVM_apply(ol, function, args);

	switch (enum(returntype)) {
		case TINT8: case TUINT8:
		case TINT16: case TUINT16:
		case TINT32: case TUINT32:
		case TINT64: case TUINT64:
			return number(r);
		case TFLOAT: {
			float f = OL2F(r);
#if __x86_64__ || __i386__  // x86/64
			__asm__("flds %0" :: "m" (f));
#elif __aarch64__
			__asm__("fmov s0, %w0" :: "r" (f));
// #elif __arm__
// # ifndef __ARM_PCS_VFP
// 			__asm__("mov r0, %0" :: "r" (f));
// # else 
// 			__asm__("vmov s0, %0" :: "r" (f));
// # endif
#endif
			return 0; // actually we return st(0)/s0
		}
		case TDOUBLE: {
			double d = OL2D(r);
#if __x86_64__ || __i386__  // x86/64
			__asm__("fldl %0" :: "m" (d));
#elif __aarch64__
			__asm__("fmov d0, %0" :: "r" (d));
// #elif __arm__
// # ifndef __ARM_PCS_VFP
// 			__asm__("ldm {r0, r1}, %0" :: "r" (d)); // todo: I'm not sure
// # else 
// 			__asm__("vmov d0, %0" :: "m" (d)); // todo: I'm not sure
// # endif
#endif
			return 0; // actually we return st(0)/s0
		}
		case TBOOL:
			return (r == IFALSE) ? 0 : 1;
		case TVOID:
			return 0;
		default:
			E("uknown return lambda type"); assert (0);
			return 0;
	}

	// if (is_enum (r))
	// 	return enum(r);
	// else
	// switch (reference_type (r)) {
	// 	case TVPTR:
	// 		return r;
	// 	case TINTP:
	// 		return unumber(r);
	// 	case TINTN:
	// 		return -unumber(r);
	// 	// return type override
	// 	case TFLOAT: {
	// 		float f[2] = { *(float*)&car(r), 0 };
	// 		return *(int64_t*)f;
	// 	}
	// 	case TPAIR: ; // TODO
	// 		// if expected float or double result,
	// 		// TODO: do the __ASM__ with loading the result into fpu/xmm register
	// 		//switch (value_type (car(r))) {
	// 		//	case TFLOAT:
	// 		//	case TDOUBLE:
	// 		//}
	// }
	// default:
	return 0;
}

#endif//OLVM_CALLABLES
