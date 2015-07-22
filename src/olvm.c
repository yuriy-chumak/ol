// максимальные атомарные числа для элементарной математики:
//	для 32-bit: 16777215 (24 бита, 0xFFFFFF)
//  для 64-bit: 72057594037927935 (56 бит, 0xFFFFFFFFFFFFFF)
// математику считать так: (receive (fx+ 16777214 1) (lambda (hi lo) (list hi lo)))
//                   либо: (let* ((hi lo (fx+ 16777214 1))) (...))
// при превышении выдает мусор
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

#include "olvm.h"

#define _POSIX_C_SOURCE 200112L

// На данный момент поддерживаются две операционные системы:
//  Windows, Linux
// Обратите внимание на проект http://sourceforge.net/p/predef/wiki/OperatingSystems/

// https://gcc.gnu.org/onlinedocs/cpp/Common-Predefined-Macros.html
#ifndef __GNUC__
#	warning "This code tested only under Gnu C compiler"
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

// check this for nested functions:
//	https://github.com/Leushenko/C99-Lambda

// posix or not:
//	http://stackoverflow.com/questions/11350878/how-can-i-determine-if-the-operating-system-is-posix-in-c

//
// PORT: равка, с типом type-port и размером 2
// todo: переименовать tuple в array. array же неизменяемый, все равно. (??? - seems to not needed)
//  а изменяемые у нас вектора

// http://joeq.sourceforge.net/about/other_os_java.html
// call/cc - http://fprog.ru/lib/ferguson-dwight-call-cc-patterns/

// компилятор lisp поддерживает только несколько специальных форм:
//	lambda, quote, rlambda (recursive lambda), receive, _branch, _define, _case-lambda, values (смотреть env.scm)
//	все остальное - макросы

#define  _BSD_SOURCE
#include <features.h>

#include <assert.h>
#include <unistd.h> // posix, https://ru.wikipedia.org/wiki/C_POSIX_library
#include <stddef.h>
#include <stdlib.h>
#include <signal.h>
#include <dirent.h>
#include <string.h>

#include <errno.h>
#include <stdio.h>
#include <inttypes.h>
#include <fcntl.h>
#include <time.h>
#include <termios.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <sys/utsname.h> // uname

// ?
#ifndef O_BINARY
#	define O_BINARY 0
#endif

// ========================================
//  HAS_SOCKETS 1
#if HAS_SOCKETS

// headers
#ifdef _WIN32
#	define WIN32_LEAN_AND_MEAN
#	define VC_EXTRALEAN
#	include <windows.h>
#	include <winsock2.h>
#	include <ws2tcpip.h>
#	include <conio.h>
	typedef unsigned long in_addr_t;
#	define EWOULDBLOCK WSAEWOULDBLOCK
#	undef ERROR // due to macro redefinition
#else

#	include <sys/socket.h>
#	include <netinet/in.h>
#	include <netdb.h>     // for gethostbyname()
#	include <arpa/inet.h> // for inet_addr()

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

// -----------------------------
// Threading (pthread for win32)
#if 0//EMBEDDED_VM

#ifdef _WIN32
//	http://mirrors.kernel.org/sourceware/pthreads-win32/
#define PTW32_VERSION 2,9,1,0
//#define ESRCH 3
typedef HANDLE pthread_t;
typedef struct pthread_attr_t {} pthread_attr_t;

static int
pthread_create(pthread_t * thread, const pthread_attr_t * attributes,
               void *(*function)(void *), void * argument)
{
	pthread_t th = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)function, argument, 0, NULL);
	if (thread != NULL)
		*thread = th;
	return (th == NULL);
}

static int
pthread_yield(void)
{
	Sleep(1);
	return 0;
}

/*static int
pthread_join(pthread_t thread, void **value_ptr)
{
	return WaitForSingleObject(thread, INFINITE);
}*/

static int
pthread_kill(pthread_t thread, int sig)
{
	assert(sig == 0);
	if (sig == 0)
		return (WaitForSingleObject(thread, 0) == WAIT_OBJECT_0) ? ESRCH : 0;
	else
		fprintf(stderr, "Invalid pthread_kill parameter signal %d", sig);
//	TerminateThread(thread, sig);
	return 0;
}

static void
pthread_exit(void *value_ptr)
{
	ExitThread((DWORD)value_ptr);
}
/*static unsigned sleep(unsigned seconds)
{
	Sleep(seconds * 1000);
}*/
#endif
#ifdef __APPLE__ // __MACH__
#	include <sched.h>
static int pthread_yield(void)
{
	return sched_yield();
}
#endif
#ifdef __linux__
#	define _GNU_SOURCE
#	include <pthread.h>
#endif

#endif

// -----------------------------------------------------------
// -=( fifo )=------------------------------------------------
#if EMBEDDED_VM_FIFO
// кольцевой текстовый буфер для общения с виртуальной машиной

#define FIFOLENGTH (1 << 14) // 4 * 4096 for now // was << 14


// todo: (?) когда приходит запрос на "после конца" входного буфера даных,
//	значит, машина сделала все, что надо было и теперь ждет новых даных.
//	похоже, пора возвращать сигнал "я все сделала"

struct fifo
{
	volatile char eof;
	volatile
	unsigned int putp, getp;
	char buffer[FIFOLENGTH];
};
//static __tlocal__ *fi, *fo;
//   fi = {0, 0}, fo = {0, 0}; // input/output
typedef struct fifo fifo;

// несколько основных ассертов:
// assert (f->putp >= f->getp)

static volatile __inline__
char fifo_empty(struct fifo* f)
{
	return ((f->putp - f->getp) == 0);
}
static volatile __inline__
char fifo_full(struct fifo* f)
{
	return ((f->putp - f->getp) == sizeof(f->buffer));
}

static __inline__
char fifo_put(struct fifo* f, char c)
{
	assert (! fifo_full(f));
	f->buffer[f->putp++ % FIFOLENGTH] = c;
	return c;
}
static __inline__
char fifo_get(struct fifo* f)
{
	assert (! fifo_empty(f));
	char
	c = f->buffer[f->getp++ % FIFOLENGTH];
	return c;
}

/*static __inline__ // must be called for VALID fifo
void fifo_clear(struct fifo* f)
{
	f->getp = f->putp = 0;
}*/

// utility fifo functions
static
int fifo_puts(struct fifo* f, char *message, int n)
{
	char *ptr = message;
	while (n--) {
		while (fifo_full(f))
			pthread_yield();
		fifo_put(f, *ptr++);
	}
	return ptr - message;
}
static
int fifo_gets(struct fifo* f, char *message, int n)
{
	assert (n > 0);
	char *ptr = message;
	while (--n) {
		char c;
		while (fifo_empty(f))
			pthread_yield( );
		c = fifo_get(f);
		if (c == EOF) {
			f->eof = 1;
			break;
		}

		if ((*ptr++ = c) == '\n')
			break;
	}
	*ptr = '\0'; // удалим крайний символ
	return ptr - message;
}
static
int fifo_feof(struct fifo* f)
{
	return f->eof;
}

#endif//EMBEDDED_VM


// --------------------------------------------------------
// -=( dl )=-----------------------------------------------
#ifdef HAS_DLOPEN
// интерфейс к динамическому связыванию системных библиотек

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
// seen at https://github.com/dlfcn-win32/dlfcn-win32/blob/master/dlfcn.c

//static thread_local char *dlerrno = 0;
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
	return hModule;
}
/*static
int dlclose(void *handle)
{
	return FreeLibrary((HMODULE)handle);
}*/

static
void *dlsym(void *handle, const char *name)
{
	FARPROC function;

	function = GetProcAddress((HANDLE)handle, name);
	return function;
}

static
char* dlerror() {
	return "description unavailable";

}

#else
#	include <dlfcn.h>
#endif

#endif//EMBEDDED_VM


// ----------
// -=( OL )=----------------------------------------------------------------------
// --
//
// виртуальная машина

// unsigned int that is capable of storing a pointer
// основной тип даных, зависит от разрядности машины
// based on C99 standard, <stdint.h>
typedef uintptr_t word;

typedef struct OL
{
	word *fp; // allocation pointer (top of allocated heap)

#if 0//EMBEDDED_VM
	pthread_t tid;
	struct fifo i; // обе очереди придется держать здесь, так как данные должны быть доступны даже после того, как vm остановится.
	struct fifo o;
#endif
} OL;

// descriptor format:
// заголовок объекта, то, что лежит у него в ob[0] (*ob)
// object headers are further
//  [... ssssssss ????rppp tttttt10] // bit "immediate" у заголовков всегда(!) выставлен в 1
//   '----------| '--||'-| '----|
//              |    ||  |      '-----> object type
//              |    ||  '------------> number of padding (unused) bytes at end of object if raw (0-(wordsize-1))
//              |    |'---------------> rawness bit (raw objects have no decriptors(pointers) in them)
//              |    '----------------> your tags here! e.g. tag for closing file descriptors in gc
//              '---------------------> object size in words
//  первый бит тага я заберу, наверное, для объектров, которые указывают слева направо, нарушая
//	общий порядок. чтобы можно было их корректно перемещать в памяти при gc()
//
// а это то, что лежит в объектах - либо непосредственное значение, либо указатель на объект
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

// удалю я пока эту высокоуровневую хрень :)
/*
#pragma pack(push, 0)
typedef struct object
{
	union {
		word header;
		word ref[1];
	};
} __attribute__ ((aligned(sizeof(word)), packed)) object; // or  ?
#pragma pack(pop)*/

#define IPOS                        8  // offset of immediate payload
#define SPOS                        16 // offset of size bits in header
#define TPOS                        2  // offset of type bits in header
#define RPOS                        11 // offset of RAW bit in header (IPOS+3)

#define V(ob)                       *((word *) (ob)) // *ob, ob[0]
#define W                           sizeof (word)

//#define NWORDS                    1024*1024*8    /* static malloc'd heap size if used as a library */
//#define FBITS                       24             /* bits in fixnum, on the way to 24 and beyond */
#define FBITS                       ((__SIZEOF_LONG__ * 8) - 8) // bits in fixnum
#define HIGHBIT                     ((unsigned long)1 << FBITS) // high long bit set
#define FMAX                        (((long)1 << FBITS)-1) // maximum fixnum (and most negative fixnum)
#define MAXOBJ                      0xffff         /* max words in tuple including header */
#if __amd64__
#define big                         __int128
#else
#define big                         long long //__int64
#endif

#define RAWBIT                      ((1 << RPOS))
#define RAWH(t)                     (t | (RAWBIT >> TPOS))
#define make_immediate(value, type)    ((((word)value) << IPOS) | ((type) << TPOS)                         | 2)
#define make_header(size, type)        (( (word)(size) << SPOS) | ((type) << TPOS)                         | 2)
#define make_raw_header(size, type, p) (( (word)(size) << SPOS) | ((type) << TPOS) | (RAWBIT) | ((p) << 8) | 2)
// p is padding

#define F(val)                      (((word)(val) << IPOS) | 2)

#define TRUEFALSE(cval)             ((cval) ? ITRUE : IFALSE)
#define fixval(desc)                ((desc) >> IPOS) // unsigned shift!!!
#define fliptag(ptr)                ((word)ptr ^ 2) /* make a pointer look like some (usually bad) immediate object */
// fliptag used in dir sys-prims

//#define header(x)                   *(word *x)
//#define imm_type(x)                 ((((word)x) >> TPOS) & 0x3F)
#define imm_val(x)                   (((word)x) >> IPOS)
#define hdrsize(x)                  ((((word)x) >> SPOS) & MAXOBJ)
#define padsize(x)                  ((((word)x) >> IPOS) & 7)
#define hdrtype(x)                  ((((word)x) >> TPOS) & 0x3F) // 0xFF from (p) << 8) in make_raw_header

#define typeof(x) hdrtype(x)

#define immediatep(x)               (((word)x) & 2)
#define allocp(x)                   (!immediatep(x))
#define is_raw(hdr)                 ((hdr) & RAWBIT)

#define is_pointer(x)               (!immediatep(x))
#define is_flagged(x)               (((word)x) & 1) // flag - mark for GC
#define is_fixed(x)                 (((word)x) & 2) // immediate value

// встроенные типы (смотреть defmac.scm по "ALLOCATED")
// todo: объединить типы TFIX и TINT, TFIXN и TINTN, так как они различаются битом I
#define TFIX                         (0)      // type-fix+
#define TFIXN                        (0 + 32) // type-fix-
#define TPAIR                        (1)
#define TTUPLE                       (2)
#define TSTRING                      (3)

#define TPORT                       (12)
#define TCONST                      (13)
#define TFF                         24
#define TBVEC                       19
#define TBYTECODE                   16
#define TPROC                       17
#define TCLOS                       18
#define TSTRINGWIDE                 22

#define TTHREAD                     31 // type-thread-state

#define TVOID                       48 // type-void
#define TINT                        40 // type-int+
#define TINTN                       41 // type-int-
#define TRATIONAL                   42
#define TCOMPLEX                    43

// special pinvoke types
#define TFLOAT                      46
#define TDOUBLE                     47
#define TTHIS                       44
#define TRAWP                       45

#define INULL                       make_immediate(0, TCONST)
#define IFALSE                      make_immediate(1, TCONST)
#define ITRUE                       make_immediate(2, TCONST)
#define IEMPTY                      make_immediate(3, TCONST) /* empty ff */
#define IEOF                        make_immediate(4, TCONST)
#define IHALT                       INULL /* FIXME: adde a distinct IHALT */

static const word I[]               = { F(0), INULL, ITRUE, IFALSE };  /* for ldi and jv */

#define HPAIR                       make_header(3, TPAIR)
#define HINT                        make_header(3, TINT) // <- on the way to 40, see type-int+ in defmac.scm
#define HPORT                       make_header(2, RAWH(TPORT))

#define FFRIGHT                     1
#define FFRED                       2

#define flagged_or_raw(hdr)         (hdr & (RAWBIT|1))
#define likely(x)                   __builtin_expect((x), 1)
#define unlikely(x)                 __builtin_expect((x), 0)

#define is_pair(ob)                 (is_pointer(ob) &&         *(word*)(ob)  == HPAIR)
#define is_string(ob)               (is_pointer(ob) && hdrtype(*(word*)(ob)) == TSTRING)
#define is_port(ob)                 (is_pointer(ob) && hdrtype(*(word*)(ob)) == TPORT) // todo: maybe need to check port rawness?

#define ref(ob, n)                  (((word*)(ob))[n])
#define car(ob)                     ref(ob, 1)
#define cdr(ob)                     ref(ob, 2)

#define cadr(ob)                    car(cdr(ob))

// todo: потом переделать в трюк
// алгоритмические трюки:
// x = (x xor t) - t, где t - y >>(s) 31 (все 1, или все 0)
// signed fix to int
#define uftoi(fix)  ({ ((word)fix >> IPOS); })
#define sftoi(fix)  ({ ((word)fix & 0x80) ? -uftoi (fix) : uftoi (fix); })
#define itosf(val)  ({ val < 0 ? (F(-val) | 0x80) : F(val); })


#define NR                          128 // see n-registers in register.scm

#define MEMPAD                      ((NR + 2) * sizeof(word)) // space at end of heap for starting GC
#define MINGEN                      (1024 * 32)  /* minimum generation size before doing full GC  */
#define INITCELLS                   1000

static int breaked = 0;      /* set in signal handler, passed over to owl in thread switch */
//static int seccompp;     /* are we in seccomp? */
//static unsigned long seccomp_time; /* virtual time within seccomp sandbox in ms */

//void exit(int rval);
//void *realloc(void *ptr, size_t size);
//void *malloc(size_t size);
//char *getenv(const char *name);
DIR *opendir(const char *name);
DIR *fdopendir(int fd);
pid_t fork(void);
pid_t waitpid(pid_t pid, int *status, int options);
int chdir(const char *path);
#ifndef _WIN32
int execv(const char *path, char *const argv[]);
#endif



// --------------------------------------------------------
// -=( gc )=-----------------------------------------------

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

	word *fp;        // allocation pointer
} heap_t;


// выделить сырой блок памяти
#define NEW(size) ({\
	word* addr = fp;\
	fp += size;\
	/*return*/ addr;\
})

// аллоцировать новый объект (указанного типа)
#define NEW_OBJECT(size, type) ({\
word*p = NEW (size);\
	*p = make_header(size, type);\
	/*return*/ p;\
})

#define NEW_RAW_OBJECT(size, type, pads) ({\
word*p = NEW (size);\
	*p = make_raw_header(size, type, pads);\
	/*return*/ p;\
})


// хитрый макрос агрегирующий макросы-аллокаторы памяти
//	http://stackoverflow.com/questions/11761703/overloading-macro-on-number-of-arguments
#define NEW_MACRO(_1, _2, NAME, ...) NAME
#define new(...) NEW_MACRO(__VA_ARGS__, NEW_OBJECT, NEW)(__VA_ARGS__)

// a1 и a2 надо предвычислить перед тем, как выделим память,
// так как они в свою очередь могут быть аллоцируемыми объектами.
#define NEW_TYPED_PAIR(type, a1, a2) ({\
	word data1 = (word) a1;\
	word data2 = (word) a2;\
	/* точка следования */ \
word*p = NEW_OBJECT (3, type);\
	p[1] = data1;\
	p[2] = data2;\
	/*return*/ p;\
})

#define NEW_PAIR(a1, a2) NEW_TYPED_PAIR(TPAIR, a1, a2)
// по факту эта функция сводится к простому:
/*static __inline__ word* new_pair (word* a1, word* a2)
{
	word *object = fp;

	fp[0] = PAIRHDR;
	fp[1] = (word) a1;
	fp[2] = (word) a2;

	fp += 3;
	return object;
}*/


#define NEW_PAIRX(_1, _2, _3, NAME, ...) NAME
#define new_pair(...) NEW_PAIRX(__VA_ARGS__, NEW_TYPED_PAIR, NEW_PAIR, NOTHING, NOTHING)(__VA_ARGS__)


// аллокаторы списоков (todo: что ставить в качестве типа частей, вместо TPAIR?)
#define new_list1(type, a1) \
	new_pair (type, a1, INULL)
#define new_list2(type, a1, a2) \
	new_pair (type, a1,\
	                new_pair (TPAIR, a2, INULL))
#define new_list3(type, a1, a2, a3) \
	new_pair (type, a1,\
	                new_pair (TPAIR, a2,\
	                                 new_pair (TPAIR, a3, INULL)))
#define new_list4(type, a1, a2, a3, a4) \
	new_pair (type, a1,\
	                new_pair (TPAIR, a2,\
	                                 new_pair (TPAIR, a3,\
	                                                  new_pair (TPAIR, a4, INULL))))
#define new_list5(type, a1, a2, a3, a4, a5) \
	new_pair (type, a1,\
	                new_pair (TPAIR, a2,\
	                                 new_pair (TPAIR, a3,\
	    	                                          new_pair (TPAIR, a4,\
	                                                                   new_pair (TPAIR, a5, INULL)))))

#define NEW_LIST(_1, _2, _3, _4, _5, _6, NAME, ...) NAME
#define new_list(...) NEW_LIST(__VA_ARGS__, new_list5, new_list4, new_list3, new_list2, new_list1, NOTHING)(__VA_ARGS__)


// кортеж
#define new_tuple(length)  new ((length)+1, TTUPLE)


// остальные аллокаторы
#define new_raw_object(size, type, pads) ({\
word*p = new (size);\
	*p = make_raw_header(size, type, pads);\
	/*return*/ p;\
})

/* make a byte vector object to hold len bytes (compute size, advance fp, set padding count) */
#define new_bytevector(length, type) ({\
	int size = (length);\
	int words = (size / W) + ((size % W) ? 2 : 1);\
	int pads = (words - 1) * W - size;\
	\
word* p = new_raw_object(words, type, pads);\
	/*return*/ p;\
})

#define new_string(length, string) ({\
word* p = new_bytevector(length, TSTRING);\
	char* ptr = (char*)&p[1];\
	int size = length;\
	char* data = string;\
	while (size--) *ptr++ = *data++;\
	*ptr = '\0'; \
	/*return*/ p;\
})


// создать новый порт
#define new_port(a) ({\
word value = (word) a;\
	word* me = new (2, RAWH(TPORT));\
	me[1] = value;\
	/*return*/ me;\
})


#define cont(n)                 V((word)n & ~1)  // ~ - bitwise NOT (корректное разименование указателя, без учета бита mark)

// возвращается по цепочке "flagged" указателей назад
static __inline__
word *chase(word* pos) {
//	assert(pos IS flagged)
//	word xpos = *(word*) ((word)pos & ~1);
	word ppos = cont(pos);                       // ppos = *pos;
	while (is_pointer(ppos) && is_flagged(ppos)) {  // ? ppos & 0x3 == 0x1
		pos = (word *) ppos;                     // pos = ppos
		ppos = cont(pos);                        // ppos = *pos;
	}
//	assert(pos IS flagged)
	return (word*)((word)pos & ~1);
}


#ifdef _LP64
typedef int64_t   wdiff;
#else
typedef int32_t   wdiff;
#endif

static __inline__
void fix_pointers(word *pos, ptrdiff_t delta, word *end)
{
	while (1) {
		word hdr = *pos;
		if (hdr == 0) return; // end marker reached. only dragons beyond this point.
		int n = hdrsize(hdr);
		if (is_raw(hdr))
			pos += n; // no pointers in raw objects
		else {
			pos++, n--;
			while (n--) {
				word val = *pos;
				if (allocp(val))
					*pos = val + delta;
				pos++;
			}
		}
	}
}

/* n-cells-wanted → heap-delta (to be added to pointers), updates memstart and memend  */
static __inline__
wdiff adjust_heap(heap_t *heap, int cells)
{
//	if (seccompp) /* realloc is not allowed within seccomp */
//		return 0;

	// add newobj realloc + heap fixer here later
	word nwords = heap->end - heap->begin + MEMPAD; // MEMPAD is after memend
	word new_words = nwords + ((cells > 0xffffff) ? 0xffffff : cells); // limit heap growth speed
	if (((cells > 0) && (new_words*W < nwords*W)) || ((cells < 0) && (new_words*W > nwords*W)))
		return 0; // don't try to adjust heap if the size_t would overflow in realloc

	word *old = heap->begin;
	heap->begin = realloc(heap->begin, new_words*W);
	if (heap->begin == old) { // whee, no heap slide \o/
		heap->end = heap->begin + new_words - MEMPAD; // leave MEMPAD words alone
		return 0;
	} else if (heap->begin) { // d'oh! we need to O(n) all the pointers...
		wdiff delta = (word)heap->begin - (word)old;
		heap->end = heap->begin + new_words - MEMPAD; // leave MEMPAD words alone
		fix_pointers(heap->begin, delta, heap->end);
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
//__attribute__ ((aligned(sizeof(int))))
static word gc(heap_t *heap, int size, word regs) {
	// просматривает список справа налево
	void mark(word *pos, word *end)
	{
	//	marked = 0;
	//	assert(pos is NOT flagged)
		while (pos != end) {
			word val = pos[0]; // pos header
			if (is_pointer(val) && val >= ((word) heap->genstart)) { // genstart - начало молодой генерации
				if (is_flagged(val)) {
					pos = chase((word*) val);
					pos--;
				}
				else {
					word hdr = *(word *) val;
	//				//if (immediatep(hdr))
	//					*(word *) val |= 1; // flag this ? (таки надо, иначе часть объектов не распознается как pinned!)
	//				marked++;

					word* ptr = (word*)val;
					*pos = *ptr;
					*ptr = ((word)pos | 1);

					if (flagged_or_raw(hdr))
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
					while (--h) *++newobject = *++old;
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
	word *fp = heap->fp;

	*fp = make_header(2, TTUPLE); // fyi: в *fp спокойно можно оставить мусор
	word *root = &fp[1]; // skip header
//	word *root = fp + 1; // same

	// непосредственно сам GC
	clock_t uptime;
	uptime = -(1000 * clock()) / CLOCKS_PER_SEC;
	root[0] = regs;
	mark(root, fp);        // assert (root > fp)
	fp = sweep(fp);
	regs = root[0];
	uptime += (1000 * clock()) / CLOCKS_PER_SEC;

	heap->fp = fp;
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
			((word*)regs)[hdrsize(*(word*)regs)] = 0; /* use an invalid descriptor to denote end live heap data  */
			regs += adjust_heap(heap, size*W + nused/10 + 4096);
			nfree = (word)heap->end - (word)regs;
//			if (nfree <= size)
//				breaked |= 8; /* will be passed over to mcp at thread switch. may cause owl<->gc loop if handled poorly on lisp side! */
		}
		else if (nfree > (heapsize/5)) {
			/* decrease heap size if more than 20% is free by 10% of the free space */
			int dec = -(nfree/10);
			int newobj = nfree - dec;
			if (newobj > size*W*2 + MEMPAD) {
				//regs[hdrsize(*regs)] = 0; /* as above */
				((word*)regs)[hdrsize(*(word*)regs)] = 0;
				regs += adjust_heap(heap, dec + MEMPAD*W);
				heapsize = (word) heap->end - (word) heap->begin;
				nfree = (word) heap->end - regs;
			}
		}
		heap->genstart = (word*)regs; /* always start newobj generation */
	} else if (nfree < MINGEN || nfree < size*W*2) {
		heap->genstart = heap->begin; /* start full generation */
		return gc(heap, size, regs);
	} else {
		heap->genstart = (word*)regs; /* start newobj generation */
	}
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

#ifndef _WIN32
static
void signal_handler(int signal) {
	fprintf(stderr, "signal %d!\n", signal);
   switch(signal) {
      case SIGINT:
         breaked |= 2; break;
      case SIGPIPE: break; // can cause loop when reporting errors
      default:
         // printf("vm: signal %d\n", signal);
         breaked |= 4;
   }
}
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

void set_signal_handler() {

#ifndef _WIN32
//   struct sigaction sa;
//   sa.sa_handler = signal_handler;
//   sigemptyset(&sa.sa_mask);
// sa.sa_flags = SA_RESTART;
   signal(SIGINT, signal_handler);
   signal(SIGPIPE, signal_handler);
#endif
}


#define OCLOSE(proctype)            { \
	word size = *ip++, tmp; word *T = new (size); tmp = R[*ip++]; tmp = ((word *) tmp)[*ip++]; \
	*T = make_header(size, proctype); T[1] = tmp; tmp = 2; \
	while (tmp != size) { T[tmp++] = R[*ip++]; } R[*ip++] = (word) T; }
#define CLOSE1(proctype)            { \
	word size = *ip++, tmp; word *T = new (size); tmp = R[  1  ]; tmp = ((word *) tmp)[*ip++]; \
	*T = make_header(size, proctype); T[1] = tmp; tmp = 2; \
	while (tmp != size) { T[tmp++] = R[*ip++]; } R[*ip++] = (word) T; }
#define TICKS                       10000 /* # of function calls in a thread quantum  */
#define ERROR(opcode, a, b)         { \
	R[4] = F(opcode);\
	R[5] = (word) a; \
	R[6] = (word) b; \
	goto invoke_mcp; }
#define CHECK(exp,val,code)         if (!(exp)) ERROR(code, val, ITRUE);

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
#define R5                          R[5]
#define R6                          R[6]

// структура с параметрами для запуска виртуальной машины
struct args
{
	OL *vm;	// виртуальная машина (из нее нам нужны буфера ввода/вывода)

	// структура памяти VM. распределяется еще до запуска самой машины
	word max_heap_size; // max heap size in MB
	struct heap_t heap;

	void *userdata;
	volatile char signal;// сигнал, что машина запустилась
};

// args + 0 = (list "arg0" "arg 1")
// args + 3 = objects list
// Несколько замечаний по этой функции:
//  http://msdn.microsoft.com/en-us/library/windows/desktop/ms686736(v=vs.85).aspx
//  The return value should never be set to STILL_ACTIVE (259), as noted in GetExitCodeThread.

static //__attribute__((aligned(8)))
void* runtime(void *args) // heap top
{
	heap_t heap;
	register word *fp; // memory allocation pointer
	int slice = TICKS; // default thread slice (n calls per slice)

	int max_heap_size;

	// регистры виртуальной машины
	word R[NR];
	int breaked = 0;

//	int seccompp = 0;

	// инициализируем локальную память
	heap.begin    = ((struct args*)args)->heap.begin;
	heap.end      = ((struct args*)args)->heap.end;
	heap.genstart = ((struct args*)args)->heap.genstart; // разделитель Old Generation и New Generation

	max_heap_size = ((struct args*)args)->max_heap_size; // max heap size in MB

	// allocation pointer (top of allocated heap)
	fp            = ((struct args*)args)->vm->fp;
	OL* ol        = ((struct args*)args)->vm;


#	if 0//EMBEDDED_VM
	// подсистема взаимодействия с виртуальной машиной посредством ввода/вывода
	fifo *fi =&((struct args*)args)->vm->o;
	fifo *fo =&((struct args*)args)->vm->i;
#	endif

	void *userdata = ((struct args*)args)->userdata; // command line

	// все, машина инициализирована, отсигналимся
	((struct args*)args)->signal = 1;

	// thinkme: может стоит искать и загружать какой-нибудь main() ?
	word* ptrs = (word*)userdata + 3;
	int nobjs = hdrsize(ptrs[0]) - 1;

	// точка входа в программу - это последняя лямбда загруженного образа (λ (args))
	word* this = (word*) ptrs[nobjs];

	// обязательно почистим регистры! иначе gc() сбойнет, пытаясь работать с мусором
	for (int i = i; i < NR; i++)
		R[i] = INULL;
	R[0] = IFALSE; // mcp (пока нету)
	R[3] = IHALT;  // continuation
	R[4] = (word) userdata; // command line as '(script arg0 arg1 arg2 ...)
	unsigned short acc = 2; // boot always calls with 1+1 args, no support for >255arg functions

	int bank = 0; // ticks deposited at interop
	int ticker = slice; // any initial value ok

	// instruction pointer
	unsigned char *ip = 0;

apply: // apply something at "this" to values in regs, or maybe switch context
	while (1) {
		if ((word)this == IEMPTY && acc > 1) { /* ff application: (False key def) -> def */
			this = (word *) R[3]; /* call cont */
			R[3] = (acc > 2) ? R[5] : IFALSE; /* default arg or false if none */
			acc = 1;
			continue;
		}
		if ((word)this == IHALT) {
			// a tread or mcp is calling the final continuation
			this = (word *) R[0];
			if (!allocp(this)) {
				// fprintf(stderr, "Unexpected virtual machine exit\n");
#				if 0//EMBEDDED_VM
				// подождем, пока освободится место в консоли
				while (fifo_full(fo)) pthread_yield();
				fifo_put(fo, EOF); // и положим туда EOF
#				endif
				return (void*)fixval(R[3]);
			}

			R[0] = IFALSE; // set no mcp
			R[4] = R[3];
			R[3] = F(2);   // 2 = thread finished, look at (mcp-syscalls-during-profiling) in lang/thread.scm
			R[5] = IFALSE;
			R[6] = IFALSE;
			breaked = 0;
			ticker = 0xffffff;
			bank = 0;
			acc = 4;
			continue;
		} /* <- add a way to call the newobj vm prim table also here? */

		if (allocp(this)) { // если это аллоцированный объект
			//word hdr = *this & 0x0FFF; // cut size out, take just header info
			word type = hdrtype(*this);
			if (type == TPROC) { //hdr == make_header(0, TPROC)) { // proc
				R[1] = (word) this; this = (word *) this[1]; // ob = car(ob)
			}
			else
			if (type == TCLOS) { //hdr == make_header(0, TCLOS)) { // clos
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
						hdr = *ff;
						switch (hdrsize(hdr)) {
						case 3: return def;
						case 4:
							if (key < this)
								ff = (word *) ((hdr & (1 << TPOS)) ? IEMPTY : ff[3]);
							else
								ff = (word *) ((hdr & (1 << TPOS)) ? ff[3] : IEMPTY);
							break;
						default:
							ff = (word *) ((key < this) ? ff[3] : ff[4]);
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
			if (!ticker--) {
				// время потока вышло, переключим на следующий
				if (R[0] == IFALSE) // no mcp, ignore
					ticker = TICKS;
				else {
					// save vm state and enter mcp cont at R0
					ticker = 0xffffff; // what the magic?
					bank = 0;
					acc += 4; //
					R[acc] = (word) this;

					word *state;
					state = (word*) new (acc, TTHREAD);
					state[acc-1] = R[acc];
					for (int pos = 1; pos < acc-1; pos++)
						state[pos] = R[pos];
//					while (pos < acc-1) {
//						state[pos] = R[pos];
//						pos++;
//					}
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

invoke:; // nargs and regs ready, maybe gc and execute ob
	void dogc(int size)
	{
		int p = 0, N = NR;
		// создадим в топе временный объект со значениями всех регистров
		word *regs = (word*) new_tuple (N + 1);
		while (++p <= N) regs[p] = R[p-1];
		regs[p] = (word) this;
		// выполним сборку мусора
		heap.fp = fp;
		regs = (word*)gc(&heap, size, (word)regs); // GC занимает 0-15 ms
		fp = heap.fp;
		// и восстановим все регистры, уже подкорректированные сборщиком
		this = (word *) regs[p];
		while (--p >= 1) R[p-1] = regs[p];

		// закончили, почистим за собой:
		fp = regs; // (вручную сразу удалим временный объект, это такая оптимизация)
	}
	// если места в буфере не хватает, то мы вызываем GC, а чтобы автоматически подкорректировались
	//  регистры, мы их складываем в память во временный кортеж.
	if (/*forcegc || */(fp >= heap.end - 16 * 1024)) { // (((word)fp) + 1024*64 >= ((word) memend))
		dogc (16 * 1024 * sizeof(word));
		ip = (unsigned char *) &this[1];

		// проверим, не слишком ли мы зажрались
		word heapsize = (word) heap.end - (word) heap.begin;
		if ((heapsize / (1024*1024)) > max_heap_size)
			breaked |= 8; // will be passed over to mcp at thread switch

	}

	// todo: add "NOP" function (may be 0x0 ?)
	// todo: add "HLT" function (may be 0x0 ?)

	// управляющие команды:
#	define APPLY 20 // apply-cont = 20+64
#	define RET   24
#	define SYS   27
#	define RUN   50
	// безусловные переходы
#	define GOTO   2       // jmp a, nargs
#	define GOTO_CODE 18   //
#	define GOTO_PROC 19   //
#	define GOTO_CLOS 21   //

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
//sys-prim 63

#	define CONS  51

#	define TYPE  15
#	define SIZE  36
#	define CAST  22

#	define CAR   52
#	define CDR   53
#	define REF   47

#	define NCONS 29
#	define NCAR  30
#	define NCDR  31

#	define SIZEB 28
#	define REFB  48

	//
#	define SET   45

	// АЛУ
#	define EQ    54

#	define CLOCK 61
#	define SYSCALL 63


	// tuples, trees
#	define MKT      23   // make tuple
#	define BIND     32
#	define LISTUPLE 35
#	define FFBIND   49

#	define MKRED    43
#	define MKBLACK  42
#	define FFTOGGLE 46
#	define FFREDQ   41

#	define ADDITION 38
#	define DIVISION 26
#	define MULTIPLICATION 39
#	define SUBTRACTION 40

	// free numbers: 34, 37, 62 (was _connect)

	// ip - счетчик команд (опкод - младшие 6 бит команды, старшие 2 бита - модификатор(если есть) опкода)
	// Rn - регистр машины (R[n])
	// An - регистр, на который ссылается операнд N (записанный в параметре n команды, начиная с 0)
	// todo: добавить в комменты к команде теоретическое количество тактов на операцию
	while (1) { // todo: добавить условие выхода из цикла
		int op; // operation to execute
		switch ((op = *ip++) & 0x3F) {
		case 0:
			op = (ip[0] << 8) | ip[1]; // big endian
			// super_dispatch: run user instructions
			switch (op) {
			/* AUTOGENERATED INSTRUCTIONS */
			default:
				ERROR(258, F(op), ITRUE);
			}
			goto apply;


		case GOTO:
			this = (word *)A0; acc = ip[1];
			goto apply;

		case GOTO_CODE:
			this = (word *)A0; acc = ip[1];
			ip = (unsigned char*) &this[1];
			goto invoke;
		case GOTO_PROC:
			this = (word *)A0; acc = ip[1];
			R1 = (word) this;
			this = (word *) this[1];
			ip = (unsigned char*) &this[1];
			goto invoke;
		case GOTO_CLOS:
			this = (word *)A0; acc = ip[1];
			R1 = (word) this;
			this = (word *) this[1];
			R2 = (word) this;
			this = (word *) this[1];
			ip = (unsigned char*) &this[1];
			goto invoke;

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

			while (allocp(lst) && *lst == HPAIR) { // unwind argument list
				// FIXME: unwind only up to last register and add limited rewinding to arity check
				if (reg > NR) { // dummy handling for now
					fprintf(stderr, "TOO LARGE APPLY\n");
					exit(3);
				}
				R[reg++] = lst[1];
				lst = (word *) lst[2];
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
			goto apply;

		case RUN: { // run thunk quantum
//			if (ip[0] != 4 || ip[1] != 5)
//				fprintf(stderr, "run R[%d], R[%d]\n", ip[0], ip[1]);
			this = (word *) A0;
			R[0] = R[3];
			ticker = bank ? bank : fixval(A1);
			bank = 0;
			CHECK(allocp(this), this, RUN);

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
		case LDI:    // 13,  -> ldi(ldn, ldt, ldf){2bit what} [to]
			A0 = I[op>>6];
			ip += 1; break;
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

		case JP:    // JZ, JN, JT, JF a hi lo
			// was: FIXME, convert this to jump-const <n> comparing to make_immediate(<n>,TCONST),
			//  но я считаю, что надо просто добавить еще одну команду, а эти так и оставить
			if (A0 == I[op>>6])
				ip += (ip[2] << 8) + ip[1]; // little-endian
			ip += 3; break;

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

		case RAW: { // raw type lst (fixme, alloc amount testing compiler pass not in place yet!) (?)
			word *lst = (word *) A1;
			int len = 0;
			word* p = lst;
			while (is_pair(p)) { // allocp(p) && *p == HPAIR) {
				len++;
				p = (word *) p[2];
			}

			if ((word) p == INULL && len <= MAXOBJ) {
				int type = uftoi (A0);
				word *raw = new_bytevector (len, type);

				unsigned char *pos;
				pos = (unsigned char *) &raw[1];
				p = lst;
				while ((word) p != INULL) {
					*pos++ = uftoi(car(p)) & 255;
					p = (word*)cdr(p);
				}

				while ((word)pos % sizeof(word)) // clear the padding bytes
					*pos++ = 0;
				A2 = (word)raw;
			}
			else
				A2 = IFALSE;

			ip += 3; break;
		}

		// операции посложнее
		case CONS:   // cons a b r:   Rr = (cons Ra Rb)
			A2 = (word) new_pair(A0, A1); // видимо, вызывается очень часто, так как замена на макрос дает +10% к скорости
			ip += 3; break;


		case TYPE: { // type o r <- actually sixtet
			word T = A0;
			if (is_pointer(T))
				T = V(T);
			A1 = F(typeof (T)); // was: F((T >> TPOS) & 63);
			ip += 2; break;
		}

		case SIZE: { // size o r
			word T = A0;
			A1 = (immediatep(T)) ? IFALSE : F(hdrsize(*(word*)T) - 1);
			ip += 2; break;
		}

		case CAST: { // cast o t r
			word T = A0;
			word type = fixval(A1) & 63;

			// todo: добавить каст с конверсией. например, из большого целого числа в handle или float
			// это лучше сделать тут, наверное, а не отдельной командой
			if (immediatep(T))
				A2 = make_immediate(imm_val(T), type);
			else
			{ // make a clone of more desired type
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


		case CAR: {  // car a r:
			word T = A0;
			CHECK(is_pair(T), T, CAR);
			A1 = car(T); //((word*)T)[1];
			ip += 2; break;
		}

		case CDR: {  // car a r:
			word T = A0;
			CHECK(is_pair(T), T, CDR);
			A1 = cdr(T); //((word*)T)[2];
			ip += 2; break;
		}

		case REF: {  /* ref t o r */ /* fixme: deprecate this later */
			word *p = (word *) A0;
			if (immediatep(p))
				A2 = IFALSE;
			else {
				word hdr = *p;
				if (is_raw(hdr)) { // raw data is #[hdrbyte{W} b0 .. bn 0{0,W-1}]
					word pos = fixval(A1);
					word size = ((hdrsize(hdr)-1)*W) - padsize(hdr);
					if (pos >= size)
						A2 = IFALSE;
					else
						A2 = F(((unsigned char *) p)[pos+W]);
				}
				else {
					word pos = fixval(A1);
					word size = hdrsize(hdr);
					if (!pos || size <= pos) // tuples are indexed from 1
						A2 = IFALSE;
					else
						A2 = p[pos];
				}
			}
			ip += 3; break;
		}


		// то же самое, но для числовых пар
		case NCONS:  // ncons a b r
			A2 = (word) new_pair (TINT, A0, A1);
			ip += 3; break;

		case NCAR: {  // ncar a r
			word T = A0;
			CHECK(is_pointer(T), T, NCAR);
			A1 = car(T);
			ip += 2; break;
		}

		case NCDR: {  // ncdr a r
			word T = A0;
			CHECK(is_pointer(T), T, NCDR);
			A1 = cdr(T);
			ip += 2; break;
		}


		case SET: { // set t o v r
			word *p = (word *)A0;
			if (immediatep(p))
				A3 = IFALSE;
			else {
				word hdr = *p;
				word pos = fixval(A1);
				if (is_raw(hdr) || hdrsize(hdr) < pos)
					A3 = IFALSE;
				else {
					word size = hdrsize (hdr);
					word *newobj = new (size);
					word val = A2;
					for (int i = 0; i <= size; i++)
						newobj[i] = (pos == i && i) ? val : p[i];
					A3 = (word) newobj;
				}
			}
			ip += 4; break;
		}

		case EQ: // eq a b r
			A2 = (A0 == A1) ? ITRUE : IFALSE;
			ip += 3; break;

		// АЛУ (арифметическо-логическое устройство)
		case ADDITION: { // fx+ a b  r o, types prechecked, signs ignored, assume fixnumbits+1 fits to machine word
			word r = uftoi(A0) + uftoi(A1);
			A2 = F(r & FMAX);
			A3 = (r & HIGHBIT) ? ITRUE : IFALSE; // overflow?
			ip += 4; break; }
		case SUBTRACTION: { // fx- a b  r u, args prechecked, signs ignored
			word r = (uftoi(A0) | HIGHBIT) - uftoi(A1);
			A2 = F(r & FMAX);
			A3 = (r & HIGHBIT) ? IFALSE : ITRUE; // unsigned?
			ip += 4; break; }

		case MULTIPLICATION: { // fx* a b l h
			big r = (big) uftoi(A0) * (big) uftoi(A1);
			A2 = F(r & FMAX);
			A3 = F(r>>FBITS); //  & FMAX)
			ip += 4; break; }
		case DIVISION: { // fx/ ah al b  qh ql r, b != 0, int64(32) / int32(16) -> int64(32), as fixnums
			big a = (big) uftoi(A1) | (((big) uftoi(A0)) << FBITS);
			big b = (big) uftoi(A2);

			// http://stackoverflow.com/questions/7070346/c-best-way-to-get-integer-division-and-remainder
			big q = a / b;
			big r = a % b;

			A3 = F(q>>FBITS);
			A4 = F(q & FMAX);
			A5 = F(r);

			ip += 6; break; }


		case 44: {/* less a b r */
			word a = A0;
			word b = A1;
			if (immediatep(a))
				A2 = immediatep(b) ? TRUEFALSE(a < b) : ITRUE;  /* imm < alloc */
			else
				A2 = immediatep(b) ? IFALSE : TRUEFALSE(a < b); /* alloc > imm */
			ip += 3; break; }


		case 55: { /* band a b r, prechecked */
	      word a = A0;
	      word b = A1;
	      A2 = a & b;
	      ip += 3; break; }
		case 56: { /* bor a b r, prechecked */
	      word a = A0;
	      word b = A1;
	      A2 = a | b;
	      ip += 3; break; }
		case 57: { /* bxor a b r, prechecked */
	      word a = A0;
	      word b = A1;
	      A2 = a ^ (b & (FMAX << IPOS)); /* inherit a's type info */
	      ip += 3; break; }

		case 58: { /* fx>> a b hi lo */
			big r = ((big) fixval(A0)) << (FBITS - fixval(A1));
			A2 = F(r>>FBITS);
			A3 = F(r & FMAX);
			ip += 4; break; }
		case 59: { /* fx<< a b hi lo */
			big r = ((big) fixval(A0)) << (fixval(A1));
			A2 = F(r>>FBITS);
			A3 = F(r & FMAX);
			ip += 4; break; }


		case REFB: { /* refb t o r */ /* todo: merge with ref, though 0-based  */
			word *p = (word *) A0;
			if (immediatep(p))
				A2 = F(-1); // todo: return IFALSE
			else {
				word hdr = *p;
				word hsize = ((hdrsize(hdr)-1)*W) - padsize(hdr); /* bytes - pads */
				int pos = fixval(A1);
				if (pos >= hsize)
					A2 = IFALSE;
				else
					A2 = F(((unsigned char *) p)[pos+W]);
			}
			ip += 3; break;
		}

		case SIZEB: { // sizeb obj to
			word* T = (word*) A0;
			if (immediatep(T))
				A1 = IFALSE;
			else {
				word hdr = *T;
				if (is_raw(hdr))
					A1 = F((hdrsize(hdr)-1)*W - padsize(hdr));
				else
					A1 = IFALSE;
			}
			ip += 2; break;
		}


		// ошибки!
		case 17: /* arity error */
			ERROR(17, this, F(acc));
			break;

		// неиспользуемые коды (историческое наследие, при желании можно реюзать)
		case 33:
		case 34:
		case 37:
		case 62:
			ERROR(op, IFALSE, IFALSE);
			break;

		// мутатор (нерабочий !)
		case 10: { // set! o t r
			word T = IFALSE;
			if (allocp(A0) && immediatep(A1) && immediatep(A2)) {
				word *obj = (word *)A0;
				word offset = fixval(A1);
				word value = fixval(A2);

				switch (hdrtype(obj[0]))
				{
				case TPAIR:
					while (offset-- && ((word)obj != INULL))
						obj = (word*)obj[2];
					if (offset == -1)
						obj[1] = T = value;
					break;
				case TTUPLE:
					if (offset < hdrsize(*obj))
						obj[offset+1] = T = value;
					break;
				}
			}
			A3 = T;
			ip += 4; break;
		}
		case 11: { // (set-car! pair value)
			word *pair = (word *)A0;
			CHECK(is_pointer(pair) && pair[0] == HPAIR, pair, 11);

			car(pair) = (word) A1;

			A2 = A0;
			ip += 3; break;
		}
		case 12: { // (set-cdr! pair value)
			word *pair = (word *)A0;
			CHECK(is_pointer(pair) && pair[0] == HPAIR, pair, 12);

			cdr(pair) = (word) A1;

			A2 = A0;
			ip += 3; break;
		}


		// make-tuple
		case MKT: { // mkt t s f1 .. fs r
			word type = *ip++;
			word size = *ip++ + 1; /* the argument is n-1 to allow making a 256-tuple with 255, and avoid 0-tuples */
			word *p = new (size+1, type), i = 0; // s fields + header
			while (i < size) {
				p[i+1] = R[ip[i]];
				i++;
			}
			R[ip[i]] = (word) p;
			ip += size+1; break;
		}

		// bind tuple to registers
		case BIND: { /* bind <tuple > <n> <r0> .. <rn> */ // todo: move to sys-prim?
			word *tuple = (word *) R[*ip++];
			CHECK(is_pointer(tuple), tuple, BIND);

			word pos = 1, n = *ip++;
			word hdr = *tuple;
			CHECK(!(is_raw(hdr) || hdrsize(hdr)-1 != n), tuple, BIND);
			while (n--)
				R[*ip++] = tuple[pos++];

			break;
		}

		// bind ff to registers
		case FFBIND: { // with-ff <node >l k v r */ // bindff - bind node left key val right, filling in #false when implicit
			word *T = (word *) A0;
			word hdr = *T++;
			A2 = *T++; // key
			A3 = *T++; // value
			switch (hdrsize(hdr)) {
			case 3: A1 = A4 = IEMPTY; break;
			case 4:
				if (hdr & (1 << TPOS)) // has right?
					A1 = IEMPTY, A4 = *T;
				else
					A1 = *T, A4 = IEMPTY;
				break;
			default:
				A1 = *T++;
				A4 = *T;
			}
			ip += 5; break;
		}

		// make tuple from list
		case LISTUPLE: { // listuple type size lst to
			word type = fixval(A0);
			word size = fixval(A1);
			word *lst = (word *)A2;
			word *p = new (size+1);
			A3 = (word) p;
			*p++ = make_header(size+1, type);
			while (size--) {
				CHECK((is_pointer(lst) && lst[0] == HPAIR), lst, LISTUPLE);
				*p++ = lst[1];
				lst = (word *) lst[2];
			}
			ip += 4; break;
		}

		/** ff's ---------------------------------------------------
		 *
		 */
		// create red/black node
		case MKBLACK: // mkblack l k v r t
		case MKRED: { // mkred l k v r t
			word t = op == MKBLACK ? TFF : TFF|FFRED;
			word l = A0;
			word r = A3;

			word *me;
			if (l == IEMPTY) {
				if (r == IEMPTY)
					me = new (3, t);
				else {
					me = new (4, t|FFRIGHT);
					me[3] = r;
				}
			}
			else
			if (r == IEMPTY) {
				me = new (4, t);
				me[3] = l;
			}
			else {
				me = new (5, t);
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
			CHECK(is_pointer(node), node, FFTOGGLE);

			word *p = fp;
			A1 = (word) p;

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

		case FFREDQ: { // red? node r (has highest type bit?) */
			word *node = (word *) A0;
			if (is_pointer(node) && ((node[0]) & (FFRED << TPOS)))
				A1 = ITRUE;
			else
				A1 = IFALSE;
			ip += 2; break;
		}


		case CLOCK: { // clock <secs> <ticks>
//			if (seccompp) {
//				unsigned long secs = seccomp_time / 1000;
//				A1 = F(seccomp_time - (secs * 1000));
//				ob[1] = F(secs >> FBITS);
//				ob[4] = F(secs & FMAX);
//				seccomp_time += ((seccomp_time + 10) > seccomp_time) ? 10 : 0; // virtual 10ms passes
//			}
//			else {
				struct timeval tp;
				gettimeofday(&tp, NULL);

				A1 = F(tp.tv_usec / 1000);

				if (tp.tv_sec < FMAX) // mainly for x64
					A0 = F(tp.tv_sec);
				else
					A0 = (word) new_list (TINT, F(tp.tv_sec & FMAX), F(tp.tv_sec >> FBITS));
//			}
			ip += 2; break;
		}

		// этот case должен остаться тут - как последний из кейсов
		// http://docs.cs.up.ac.za/programming/asm/derick_tut/syscalls.html (32-bit)
		// https://filippo.io/linux-syscall-table/
		case SYSCALL: { // sys-call (was sys-prim) op arg1 arg2 arg3  r1
			// linux syscall list: http://blog.rchapman.org/post/36801038863/linux-system-call-table-for-x86-64
			//                     http://www.x86-64.org/documentation/abi.pdf
			word op = uftoi (A0);
			word a = A1, b = A2, c = A3;
			word result = IFALSE;  // default returned value is #false
//			fprintf(stderr, "SYSCALL(%d, %d, %d, %d)\n", op, a, b, c);

			switch (op) {

			// (READ fd count) -> buf
			// http://linux.die.net/man/2/read
			// count<0 means read all
			case 0: {
				CHECK(is_port(a), a, SYSCALL);
				int portfd = car (a);
				int size = sftoi (b);

				if (size < 0)
					size = (heap.end - fp) * W - MEMPAD;
				else
				if (size > (heap.end - fp) * W - MEMPAD)
					dogc(size);

				int got;
#if 0//EMBEDDED_VM
				if (fd == 0) { // stdin reads from fi
					if (fifo_empty(fi)) {
						// todo: process EOF, please!
						n = -1;
						errno = EAGAIN;
					}
					else {
						char *d = ((char*) res) + W;
						while (!fifo_empty(fi))
							*d++ = fifo_get(fi);
						n = d - (((char*) res) + W);
					}
				}
				else
#endif
				{
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
				}

				if (got > 0) {
					// todo: обработать когда приняли не все,
					//	     вызвать gc() и допринять. и т.д.
					result = (word) new_bytevector (got, TBVEC);
				}
				else if (got == 0)
					result = IEOF;
				else if (errno == EAGAIN) // (may be the same value as EWOULDBLOCK) (POSIX.1)
					result = ITRUE;

				break;
			}

			// (WRITE fd buffer size) -> wrote
			// http://linux.die.net/man/2/write
			// size<0 means write all
			// n if wrote, 0 if busy, #false if error (argument or write)
			case 1: {
				CHECK(is_port(a), a, SYSCALL);
				int portfd = car (a);
				int size = sftoi (c);

				word *buff = (word *) b;
				if (immediatep(buff))
					break;

				int length = (hdrsize(*buff) - 1) * sizeof(word);
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
					result = F(wrote);
				else if (errno == EAGAIN || errno == EWOULDBLOCK)
					result = F(0);

				break;
			}

			// OPEN
			case 2: {
				CHECK(is_string(a), a, SYSCALL);
				word* path = (word*) a; // todo: check for string type
				int mode = fixval(b);
				mode |= O_BINARY | ((mode > 0) ? O_CREAT | O_TRUNC : 0);
				int file = open((char *) &path[1], mode, (S_IRUSR | S_IWUSR));

				struct stat sb;
				if (file < 0 || fstat(file, &sb) == -1 || (S_ISDIR(sb.st_mode))) {
					close(file);
					break;
				}
				set_blocking(file, 0);
				result = (word) new_port(file);

				break;
			}

			// CLOSE
			case 3: {
				CHECK(is_port(a), a, SYSCALL);
				int portfd = car (a);

				if (close(portfd) == 0)
					result = ITRUE;

				break;
			}

			// IOCTL (syscall 16 fd request #f)
			case 16: {
				CHECK(is_port(a), a, SYSCALL);
				int portfd = car (a);
				int ioctl = uftoi(b);

				switch (ioctl) {
				case 19: { // TIOCGETA
					struct termios t;
					if (tcgetattr(portfd, &t) != -1) {
//					if (isatty(portfd)) {
						result = ITRUE;
						fprintf(stderr, "TIOCGETA: ok!\n");
					}
					else
						fprintf(stderr, "TIOCGETA: --.\n");
					break;
				}
				default:
					;// do nothing
				}
				break;
			}

			// PIPE
			case 22: {
				// TBD.
				break;
			}


			// ==================================================
			//  network part:

#if HAS_SOCKETS
			// SOCKET
			case 41: { // socket (todo: options: STREAM or DGRAM)
				int sock = socket(PF_INET, SOCK_STREAM, 0);
				if (sock != -1)
					result = (word) new_port (sock);
				break;
			}

			// CONNECT
			case 42: { // (connect sockfd host port)
				CHECK(is_port(a), a, SYSCALL);
				int sockfd = car (a);
				word* host = (word*) b; // todo: check for string type
				int port = uftoi (c);

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
					result = ITRUE;
//				set_blocking(sock, 0);
				break;
			}

			// SHUTDOWN
			// http://linux.die.net/man/2/shutdown
			case 48: { // (shutdown socket #f #f)
				CHECK(is_port(a), a, SYSCALL);
				int socket = car (a);

				// On error, -1 is returned
				if (shutdown(socket, 2) != 0) // both
					break;

				result = ITRUE;
				break;
			}

			// BIND
			// http://linux.die.net/man/2/bind
			case 49: { //  (socket, port, #false) // todo: c for options
				CHECK(is_port(a), a, SYSCALL);
				int sockfd = car (a);
				int port = uftoi (b);

				// todo: assert on argument types
				struct sockaddr_in interface;
				interface.sin_family = AF_INET;
				interface.sin_port = htons(port);
				interface.sin_addr.s_addr = INADDR_ANY;

				// On success, zero is returned.
				if (bind(sockfd, (struct sockaddr *) &interface, sizeof(interface)) == 0)
					result = ITRUE;
				break;
			}

			// LISTEN (socket)
			// http://linux.die.net/man/2/listen
			// listen() marks the socket referred to by sockfd as a passive socket, that is,
			// as a socket that will be used to accept incoming connection requests using accept(2).
			case 50: {
				CHECK(is_port(a), a, SYSCALL);
				int sockfd = car (a);

				// On success, zero is returned.
				if (listen(sockfd, 42) == 0) {
		//					set_blocking(sockfd, 0);
					result = ITRUE;
				}

				break;
			}

			// ACCEPT
			// http://linux.die.net/man/2/accept
			case 43: { // (accept sockfd)
				CHECK(is_port(a), a, SYSCALL);
				int sockfd = car (a);

				struct sockaddr_in addr;
				socklen_t len = sizeof(addr);
				int sock = accept(sockfd, (struct sockaddr *)&addr, &len);
				// On error, -1 is returned
				if (sock >= 0)
					result = (word) new_port (sock);
				break;
			}

#endif
			// ==================================================


			// todo: http://man7.org/linux/man-pages/man2/nanosleep.2.html
			// NANOSLEEP
			case 35: {
				CHECK(immediatep(a), a, 35);
#ifdef _WIN32
				Sleep(uftoi(a));
#else
				struct timeval ts = { uftoi(a) / 1000, (uftoi(a) % 1000) * 1000000 };
				if (select(0, NULL, NULL, NULL, &ts) == 0)
					result = ITRUE;
//				poll(0, 0, uftoi(a));
#endif
/*				for Linux:
//				if (!seccompp)
#	if	_POSIX_C_SOURCE >= 199309L
				struct timespec ts = { fixval(a) / 1000, (fixval(a) % 1000) * 1000000 };
				struct timespec rem;
				if (nanosleep(&ts, &rem) == 0)
					result = ITRUE;
				else
					result = F(rem.tv_sec * 1000 + rem.tv_nsec / 1000000);
#	else
#	if _POSIX_C_SOURCE < 200809L // POSIX.1-2008 removes the specification of usleep(), use nanosleep instead
				if (usleep((useconds_t)uftoi (a) * 1000) == 0)
					result = ITRUE;
#	endif*/
				// or
				// poll(0, 0, uftoi(a));*/

//#endif
				break;
			}

			// (EXECVE program-or-function)
			// http://linux.die.net/man/3/execve
			case 59: {
#if HAS_DLOPEN
				// if a is port (result of dlsym)
				if (is_port(a)) {
					// a - function address (port)
					// b - arguments (may be pair with req type in car and arg in cdr - not yet done)
					word* A = (word*)a;
					word* B = (word*)b;
					word* C = (word*)c; // reserved, assert #false

				//	CHECK(hdrtype(A[0]) == TPORT, A, 59);
					assert ((word)B == INULL || hdrtype(B[0]) == TPAIR);
					assert ((word)C == IFALSE);

					void *function = (void*) car(A);  assert (function);

					ol->fp = fp;
					result = ((word (*)  (OL*, word*))function) (ol, B);
					fp = ol->fp;
					break;
				}
#endif
				// if a is string:
				if (is_string(a)) {
#if 1
					FILE* pipe = popen((const char*)car(a), "r");

					char* p = (char*) &car(fp);
					while (!feof(pipe)) {
						// todo: check for available memory, gc()
						p += fread(p, 1, 1024, pipe);
					}
					pclose(pipe);

					int count = p - (char*)&car(fp) - 1;
					result = (word) new_bytevector(count, TSTRING);
#else	// popen by syscall
					int stdin[2];
					if (pipe(stdin) < 0) {
						fprintf(stderr, "pipe for child input redirect failed.\n");
						break;
					}

					int stdout[2];
					if (pipe(stdout) < 0) {
						fprintf(stderr, "pipe for child output redirect failed.\n");
						close(stdin[0]);
						close(stdin[1]);
						break;
					}

					int child = fork();
					if (child == 0) {
						fprintf(stderr, "fork.1\n");
						dup2(stdin[0], STDIN_FILENO);
						dup2(stdout[1], STDOUT_FILENO);
						dup2(stdout[1], STDERR_FILENO);

						fprintf(stderr, "fork.2\n");
						close(stdin[0]);
						close(stdin[1]);
						close(stdout[0]);
						close(stdout[1]);

						fprintf(stderr, "fork.3\n");

						char* command = (char*)&car(a);
						char* args = 0; // temp, b
						char* envp = 0;

						fprintf(stderr, "command: %s\n", command);

						exit(execve(command, 0, 0));
					}
					else {
						close(stdin[0]);
						close(stdout[1]);

						if (is_string(c))
							write(stdin[1], &car(c), lenn(&car(c), -1));

						// read
						char* p = &car(fp);
						while (read(stdout[0], p++, 1) == 1)
							continue;
						int count = p - (char*)&car(fp) - 1;
						result = new_bytevector(count, TSTRING);

						close(stdin[1]);
						close(stdout[0]);
					}
#endif
					break;
				}
				break;
			}

			// EXIT errorcode
			// http://linux.die.net/man/2/exit
			// exit - cause normal process termination, function does not return.
			case 1006:
			case 60: {
				free(heap.begin); // освободим занятую память
#if 0//EMBEDDED_VM
				// подождем, пока освободится место в консоли
				while (fifo_full(fo)) pthread_yield();
				fifo_put(fo, EOF); // и положим туда EOF

				pthread_exit((void*)fixval(a));
#else
				exit(sftoi(a));
#endif
				assert(0);   // сюда мы уже не должны попасть
				break;
			}

			// UNAME (uname)
			// http://linux.die.net/man/2/uname
			case 63: {
				struct utsname name;
				if (uname(&name))
					break;

				result = (word)new_list (TPAIR,
						new_string(strlen((char*)name.sysname), name.sysname),
						new_string(strlen((char*)name.nodename), name.nodename),
						new_string(strlen((char*)name.release), name.release),
						new_string(strlen((char*)name.version), name.version),
						new_string(strlen((char*)name.machine), name.machine)
				);

				break;
			}


			// other commands

			// todo: сюда надо перенести все prim_sys операции, что зависят от глобальных переменных
			//  остальное можно спокойно оформлять отдельными функциями

				case 1007: // set memory limit (in mb) / // todo: переделать на другой номер
					result = F(max_heap_size);
					max_heap_size = fixval(a);
					break;
				case 1009: // get memory limit (in mb) / // todo: переделать на другой номер
					result = F(max_heap_size);
					break;

				case 1008: /* get machine word size (in bytes) */ // todo: переделать на другой номер
					  result = F(W);
					  break;

				case 1022:
					result = (ticker & FMAX);
					ticker = fixval(a);
					break;


#if HAS_DLOPEN
				// -=( dlopen )=-------------------------------------------------
				case 1030: { // (dlopen filename mode #false)
					word *filename = (word*)a;
					int mode = (int) uftoi(b);

					void* module;
					if ((word) filename == INULL)
						module = dlopen(NULL, mode); // If filename is NULL, then the returned handle is for the main program.
					else if (is_pointer(filename) && hdrtype(*filename) == TSTRING)
						module = dlopen((char*) &filename[1], mode);
					else
						break; // invalid filename, return #false

					if (module)
						result = (word) new_port(module);
//					else
//						fprintf(stderr, "dlopen failed: %s\n", dlerror());
					break;
				}
				case 1031: { // (dlsym module function #false)
					word* A = (word*)a;

					CHECK(is_port(A), A, SYSCALL);
					void* module = (void*)car (A);

					word* symbol = (word*) b;
					// http://www.symantec.com/connect/articles/dynamic-linking-linux-and-windows-part-one
					if (!(immediatep(symbol) || hdrtype(*symbol) == TSTRING))
						break;

					word function = (word)dlsym(module, immediatep(symbol)
							? (char*) imm_val((word)symbol)
							: (char*) &symbol[1]);
					if (function) // тут сразу создаем длинный port, так как адреса скорее всего более 24 бит
						result = (word)new_port(function);
					else
						fprintf(stderr, "dlsym failed: %s\n", dlerror());
					break;
				}

				// вызвать функцию (special case - отправить this)
				case 1032: { // (pinvoke function args #false)
					// a - function address (port)
					// b - arguments (may be pair with req type in car and arg in cdr - not yet done)
					word* A = (word*)a;
					word* B = (word*)b;
					word* C = (word*)c; // reserved

					CHECK(hdrtype(A[0]) == TPORT, A, 1032);
					assert ((word)B == INULL || hdrtype(B[0]) == TPAIR);
					assert ((word)C == IFALSE);

					void *function = (void*) car(A);  assert (function);

					ol->fp = fp;
					result = ((word (*)  (OL*, word*))function) (ol, B);
					fp = ol->fp;
					break;
				}
#endif

				// временный тестовый вызов
				case 1033: { // temp, todo: (dlclose module)
					//forcegc = 1;
/*					printf("opengl version: %s\n", glGetString(GL_VERSION));
					int glVersion[2] = {-1, -1}; // Set some default values for the version
					glGetIntegerv(GL_MAJOR_VERSION, &glVersion[0]); // Get back the OpenGL MAJOR version we are using
					glGetIntegerv(GL_MINOR_VERSION, &glVersion[1]); // Get back the OpenGL MAJOR version we are using

					GLint status;*/
//					PFNGLGETSHADERIVPROC  glGetShaderiv  = (PFNGLGETSHADERIVPROC)wglGetProcAddress("glGetShaderiv");
//					glGetShaderiv(3, GL_COMPILE_STATUS, &status);

//					PFNGLGETPROGRAMIVPROC glGetProgramiv = (PFNGLGETSHADERIVPROC)wglGetProcAddress("glGetProgramiv");
//					glGetProgramiv(1, GL_LINK_STATUS, &status);

					result = IFALSE;
/*
					word* A = a;
					char* B = (word*)b + 1;
					if (A[1] == 0) {
						printf("\n\n\n\n\n\n\n\n\n\nB = %s\n\n\n", B);
						result = F(0);
						exit(123);
					}

*/
					break;
				}



				case 1016: { // getenv <owl-raw-bvec-or-ascii-leaf-string>
					word *name = (word *)a;
					if (is_string(name)) {
						char* value = getenv((char*)&name[1]);
						if (value)
							result = (word) new_string(lenn(value, FMAX), value);
					}
					break;
				}

				default:
					if (op >= 1000) {
						word prim_sys(int op, word a, word b, word c) {
						   switch(op) {
						      case 1010: /* enter linux seccomp mode */
						#ifdef __gnu_linux__
						#ifndef NO_SECCOMP
						         if (seccompp) /* a true value, but different to signal that we're already in seccomp */
						            return INULL;
						         seccomp_time = 1000 * time(NULL); /* no time calls are allowed from seccomp, so start emulating a time if success */
						#ifdef PR_SET_SECCOMP
						         if (prctl(PR_SET_SECCOMP,1) != -1) { /* true if no problem going seccomp */
						            seccompp = 1;
						            return ITRUE;
						         }
						#endif
						#endif
						#endif
						         return IFALSE; /* seccomp not supported in current repl */
						      /* dirops only to be used via exposed functions */
						      case 1011: { /* sys-opendir path _ _ -> False | dirobjptr */
						         char *path = W + (char *) a; /* skip header */
						         DIR *dirp = opendir(path);
						         if(!dirp) return IFALSE;
						         return fliptag(dirp); }
						      case 1012: { /* sys-readdir dirp _ _ -> bvec | eof | False */
						         DIR *dirp = (DIR *)fliptag(a);
						         word *res;
						         unsigned int len;
						         struct dirent *dire = readdir(dirp);
						         if (!dire) return IEOF; /* eof at end of dir stream */
						         len = lenn(dire->d_name, FMAX+1);
						         if (len == FMAX+1) return IFALSE; /* false for errors, like too long file names */
						         res = new_bytevector(len, TSTRING); /* make a fake raw string (OS may not use valid UTF-8) */
						         bytecopy((char *)&dire->d_name, (char *) (res + 1), len); /* *no* terminating null, this is an owl bvec */
						         return (word)res; }
						      case 1013: /* sys-closedir dirp _ _ -> ITRUE */
						         closedir((DIR *)fliptag(a));
						         return ITRUE;
						      case 1014: { /* set-ticks n _ _ -> old */
						         word old = F(slice);
						         slice = fixval(a);
						         return old; }
						      case 1017: { // system (char*)
						    	  int result = system((char*)a + W);
						    	  return F(result);
						      }

/*						#ifndef _WIN32
						          char *path = ((char *) a) + W;
						          int nargs = llen((word *)b);
						         char **args = malloc((nargs+1) * sizeof(char *)); // potential memory leak
						         char **argp = args;
						         if (args == NULL)
						            return IFALSE;
						         while(nargs--) {
						            *argp++ = ((char *) ((word *) b)[1]) + W;
						            b = ((word *) b)[2];
						         }
						         *argp = NULL;
						         set_blocking(0,1); // try to return stdio to blocking mode
						         set_blocking(1,1); // warning, other file descriptors will stay in nonblocking mode
						         set_blocking(2,1);
						         execv(path, args); // may return -1 and set errno
						         set_blocking(0,0); // exec failed, back to nonblocking io for owl
						         set_blocking(1,0);
						         set_blocking(2,0);
						#endif
						         return IFALSE; }*/
						      case 1020: { /* chdir path res */
						         char *path = ((char *)a) + W;
						         if (chdir(path) < 0)
						            return IFALSE;
						         return ITRUE; }
						#if 0 // ndef _WIN32
						      case 1019: { /* wait <pid> <respair> _ */
						         pid_t pid = (a == IFALSE) ? -1 : fixval(a);
						         int status;
						         word *r = (word *) b;
						         pid = waitpid(pid, &status, WNOHANG|WUNTRACED|WCONTINUED);
						         if (pid == -1)
						            return IFALSE; /* error */
						         if (pid == 0)
						            return ITRUE; /* no changes, would block */
						         if (WIFEXITED(status)) {
						            r[1] = F(1);
						            r[2] = F(WEXITSTATUS(status));
						         } else if (WIFSIGNALED(status)) {
						            r[1] = F(2);
						            r[2] = F(WTERMSIG(status));
						         } else if (WIFSTOPPED(status)) {
						            r[1] = F(3);
						            r[2] = F(WSTOPSIG(status));
						         } else if (WIFCONTINUED(status)) {
						            r[1] = F(4);
						            r[2] = F(1);
						         } else {
						            fprintf(stderr, "vm: unexpected process exit status: %d\n", status);
						            r = (word *)IFALSE;
						         }
						         return (word)r; }
						      case 1018: { /* fork ret → #false=failed, fixnum=ok we're in parent process, #true=ok we're in child process */
						         pid_t pid = fork();
						         if (pid == -1) /* fork failed */
						            return IFALSE;
						         if (pid == 0) /* we're in child, return true */
						            return ITRUE;
						         if ((int)pid > FMAX)
						            fprintf(stderr, "vm: child pid larger than max fixnum: %d\n", pid);
						         return F(pid&FMAX); }
						      case 1021: /* kill pid signal → fixnum */
						         return (kill(fixval(a), fixval(b)) < 0) ? IFALSE : ITRUE;
						#endif
						      default:
						         return IFALSE;
						   }
						}

						result = prim_sys(op, a, b, c);
					}
					else {
						word syscall(word op, word a, word b, word c)
						{
							switch (op)
							{
								default:
									break;
							}

							return IFALSE;
						}

						result = syscall(op, a, b, c);
					}
					break;
				}

				A4 = result;
				ip += 5; break;
			}
		}
		main_dispatch: continue; // временная замена вызову "break" в свиче, пока не закончу рефакторинг
	}
	// while(1);

invoke_mcp: /* R4-R6 set, set R3=cont and R4=interop and call mcp */
	this = (word *) R[0];
	R[0] = IFALSE;
	R[3] = F(3);
	if (allocp(this)) {
		acc = 4;
		goto apply;
	}
	return (void*) 1; /* no mcp to handle error (fail in it?), so nonzero exit  */
}


// ======================================================================
//       загрузчик скомпилированного образа и его десериализатор
//

// fasl decoding
// возвращает новый топ стека
static __inline__
word* deserialize(word *ptrs, int nobjs, unsigned char *bootstrap, word* fp)
{
	register
	unsigned char* hp = bootstrap;
//	if (*hp == '#') // этот код не нужен, так как сюда приходит уже без шабанга
//		while (*hp++ != '\n') continue;

	// tbd: comment
	// todo: есть неприятный момент - 64-битный код иногда вставляет в fasl последовательность 0x7FFFFFFFFFFFFFFF (самое большое число)
	//	а в 32-битном коде это число должно быть другим. что делать? пока х.з.
	word get_nat() {
		long long result = 0;// word
		long long newobj, i; // word
		do {
			i = *hp++;
			newobj = result << 7;
			if (result != (newobj >> 7))
				exit(9); // overflow kills
			result = newobj + (i & 127);
		} while (i & 128);
#if __amd64__
		return result;
#else
		return result == 0x7FFFFFFFFFFFFFFF ? 0x7FFFFFFF : (word)result;
#endif
	}

	// tbd: comment
	word *get_field(word *ptrs, int pos) {
		if (*hp == 0) { // fixnum
			hp++;
			unsigned char type = *hp++;
			word val = make_immediate(get_nat(), type);
			*fp++ = val;
		} else {
			word diff = get_nat();
			*fp++ = ptrs[pos-diff];
		}
		return fp;
	}

	// function entry:
	for (int me = 0; me < nobjs; me++) {
		ptrs[me] = (word) fp;

		switch (*hp++) { // todo: adding type information here would reduce fasl and executable size
		case 1: {
			int type = *hp++;
			int size = get_nat();
			*fp++ = make_header(size+1, type); // +1 to include header in size
			while (size--)
				get_field(ptrs, me);
			break;
		}
		case 2: {
			int type = *hp++ & 31; /* low 5 bits, the others are pads */
			int bytes = get_nat();
			int size = ((bytes % W) == 0) ? (bytes/W)+1 : (bytes/W) + 2;
			int pads = (size-1)*W - bytes;

			*fp++ = make_raw_header(size, type, pads);
			unsigned char *wp = (unsigned char *) fp;
			while (bytes--)
				*wp++ = *hp++;
			while (pads--)
				*wp++ = 0;
			fp = (word *) wp;
			break;
		}
		default:
			puts("bad object in heap");
			exit(42);
		}
	}
	return fp;
}

static
// функция подсчета количества объектов в загружаемом образе
int count_fasl_objects(word *words, unsigned char *lang) {
	unsigned char* hp;

	word decode_word() {
		word result = 0;
		word newobj, i;
		do {
			i = *hp++;
			newobj = result << 7;
			//assert (result == (newobj >> 7)); // temp
	//			if (result != (newobj >> 7)) exit(9); // overflow kills
			result = newobj + (i & 127);
		}
		while (i & 128);
		return result;
	}

	// count:
	int n = 0;
	hp = lang;

	int allocated = 0;
	while (*hp != 0) {
		switch (*hp++) {
		case 1: {
			hp++; ++allocated;
			int size = decode_word();
			while (size--) {
				if (*hp == 0) {
					hp++; ++allocated;
					hp++;
				}
				decode_word(&hp); // simply skip word
				++allocated;
			}
			break;
		}
		case 2: {
			hp++;
			int size = decode_word();
			hp += size;

			int words = (size/W) + (((size % W) == 0) ? 1 : 2);
			allocated += words;

			break;
		}

		default:
			puts("bad object in heap");
			exit(42);
		}

		n++;
	}

	*words = allocated;
	return n;
}

/*static word* new_string2 (size_t length, char* string)
{
	word* object = fp;

	int size = (length / W) + ((length % W) ? 2 : 1);
	int pads = (size-1) * W - length;

	*fp = make_raw_header(size, TSTRING, pads);
	char* p = ((char *) fp) + W;
	while (length--) *p++ = *string++;

	fp += size;
	return object;
}*/

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
	if (argc > 1 && strcmp(argv[1], "-") != 0) {
		// todo: use mmap()
		struct stat st;
		if (stat(argv[1], &st) || st.st_size == 0)
			exit(errno);	// не найден файл или он пустой

		char bom;
		int bin = open(argv[1], O_RDONLY, (S_IRUSR | S_IWUSR));
		if (!bin)
			exit(errno);	// не смогли файл открыть

		int pos = read(bin, &bom, 1); // прочитаем один байт
		if (pos < 1)
			exit(errno);	// не смогли файл прочитать

		// переделать
		if (bom == '#') { // skip possible hashbang
			while (read(bin, &bom, 1) == 1 && bom != '\n')
				st.st_size--;
			st.st_size--;
			if (read(bin, &bom, 1) < 0)
				exit(errno);
			st.st_size--;
		}

		if (bom > 3) {	// ха, это текстовая программа (скрипт)!
			// а значит что? что файл надо замапить вместо stdin
			// rollback назад, на 1 прочитанный символ
#ifndef NAKED_VM
			lseek(bin, -1, SEEK_CUR);
			dup2(bin, STDIN_FILENO);
			close(bin);
#else
			exit(6); // некому проинтерпретировать скрипт
#endif
		}
		else {
			// иначе загрузим его
			unsigned char* ptr = (unsigned char*)malloc(st.st_size);
			if (ptr == NULL)
				exit(3);	// опа, не смогли выделить память...

			ptr[0] = bom;
			while (pos < st.st_size) {
				int n = read(bin, &ptr[pos], st.st_size - pos);
				if (n < 0) exit(5); // не смогли прочитать
				pos += n;
			}
			close(bin);

			bootstrap = ptr;
		}
	}

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
	int r = olvm(bootstrap, bootstrap != language ? free : NULL);

#if	HAS_SOCKETS && defined(_WIN32)
	WSACleanup();
#endif

	return r;
}
#endif

int    // this is NOT thread safe function
olvm(unsigned char* bootstrap, void (*release)(void*))
{
	// если это текстовый скрипт, замапим его на stdin, а сами используем встроенный (если) язык
	if (bootstrap[0] > 3) {
		char filename[16]; // lenght of above string
		strncpy(filename, "/tmp/olvmXXXXXX", sizeof(filename));

		int f = mkstemp(filename); // временный файл
		if (!write(f, bootstrap, strlen(bootstrap)))
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

	heap_t heap;
//	static //__tlocal__
	word *fp;

	void* result = 0; // результат выполнения скрипта

	// выделим память машине:
	int max_heap_size = (W == 4) ? 4096 : 65535; // can be set at runtime
	int required_memory_size = (INITCELLS + MEMPAD + nwords + 64 * 1024); // 64k objects for memory
	heap.begin = (word*) malloc(required_memory_size * sizeof(word)); // at least one argument string always fits
	if (!heap.begin) {
		fprintf(stderr, "Failed to allocate %d words for vm memory\n", required_memory_size);
		goto done;
	}
	heap.end = heap.begin + required_memory_size;
	heap.genstart = heap.begin;

	// подготовим в памяти машины параметры командной строки:

	// create '("some string" . NIL) as parameter for the start lambda
	// а вообще, от этого блока надо избавится.
	//  но пока оставлю как пример того, как можно предварительно
	//  загрузить в память аргументы перед вызовом образа
	// по совместительству, это еще и корневой объект
	fp = heap.begin;
	word oargs; // аргументы
	{
		oargs = INULL;
#if !EMBEDDED_VM
		// аргументы
		// todo: for win32 do ::GetCommandLine()
		int f = open("/proc/self/cmdline", O_RDONLY, S_IRUSR);
		if (f) {
			int r;
			do {
				char *pos = (char*)(fp + 1);
				while ((r = read(f, pos, 1)) > 0 && (*pos != 0)) {
					pos++;
				}
				int length = pos - (char*)fp - W;
				if (r > 0 && length > 0) // если есть что добавить
					oargs = (word)new_pair (new_bytevector(length, TSTRING), oargs);
			}
			while (r > 0);
			close(f);
		}
		else
#endif
		{
			char* filename = "-";
			char *pos = filename;

			int len = 0;
			while (*pos++) len++;

			oargs = (word)new_pair (new_string (len, filename), oargs);
		}
#if 0
		word *a, *b, *c, *d, *e;

		a = new_string(28, "aaaaaaaaaaaaaaaaaaaaaaaaaaaa");
		b = new_pair(a, INULL);
			new_string(16, "bbbbbbbbbbbbbbbb");
		c = new_pair(a, INULL);
			new_string(16, "cccccccccccccccc");
		d = new_pair(b, c);
			new_string(16, "dddddddddddddddd");
		e = new_pair(d, c);
			new_string(16, "eeeeeeeeeeeeeeee");

		gc(0, e, &fp);
#endif
	}

//	heap.fp = fp;
//	oargs = gc(&heap, nwords + (16*1024), oargs); // get enough space to load the heap without triggering gc
//	fp = heap.fp;

	// Десериализация загруженного образа в объекты
	word *ptrs = new_raw_object (nobjs+1, TCONST, 0);
	fp = deserialize(&ptrs[1], nobjs, bootstrap, fp);

	// все, программа в памяти, можно освобождать исходник
	if (release)
		release(bootstrap);

	// ===============================================================

	struct args args; // аргументы для запуска
	args.vm = handle; // виртуальной машины OL
	args.vm->fp = fp;

	// а это инициализационные аргументы для памяти виртуальной машины
	args.heap.begin    = heap.begin;
	args.heap.end      = heap.end;
	args.heap.genstart = heap.genstart;
	args.max_heap_size = max_heap_size; // max heap size in MB
	args.userdata      = (word*) oargs;

#if 0 //EMBEDDED_VM
	args.signal = 0;
	if (pthread_create(&handle->tid, NULL, &runtime, &args) == 0) {
		while (!args.signal)
			pthread_yield();
		return handle;
	}
	fprintf(stderr, "Can't create thread for vm");
#else
#	ifndef _WIN32
	setvbuf(stderr, (void*)0, _IONBF, 0);
//	setvbuf(stdout, (void*)0, _IONBF, 0);
	set_blocking(1, 0);
	set_blocking(2, 0);
#	endif

	result = runtime(&args);
#endif

	// ===============================================================

done:
	free(heap.begin);
	free(handle);

exit:
	return (int)(long)result;
}

#if 0 //EMBEDDED_VM

/*void eval(char* message, char* response, int length)
{
	fifo_puts(&fi, message, strlen(message) + 1);
	fifo_gets(&fo, response, length);
}
void eval2(char* message)
{
	fifo_puts(&fi, message, strlen(message) + 1);
}*/

int vm_alive(OL* vm)
{
	return (pthread_kill(vm->tid, 0) != ESRCH);
}


int vm_puts(OL* vm, char *message, int n)
{
	if (!vm_alive(vm))
		return 0; // если машина уже умерла - нет смысла ей еще что-то передавать
	return fifo_puts(&vm->o, message, n);
}
int vm_gets(OL* vm, char *message, int n)
{
	if (!vm_alive(vm) && fifo_empty(&vm->i)) // если там уже ничего нет, иначе заберем
		return *message = 0;
	return fifo_gets(&vm->i, message, n);
}
int vm_feof(OL* vm)
{
	return fifo_feof(&vm->i);
}
#endif//EMBEDDED_VM


//   а тут у нас реализация pinvoke механизма. пример в opengl.scm
#if HAS_PINVOKE
__attribute__
	((__visibility__("default")))
word pinvoke(OL* self, word arguments)
{
	word *fp, result;

	// get memory pointer
	fp = self->fp;

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
			case 2 + 0x0200:\
			         return ((conv word (*)  (word, float))\
			                 function) (argv[0], *(float*)&argv[1]);\
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
			         return ((conv word (*)  (double, double, double, double, double, double))\
			                 function) (*(double*)&argv[0], *(double*)&argv[1],\
			                            *(double*)&argv[2], *(double*)&argv[3],\
			                            *(double*)&argv[4], *(double*)&argv[5]);
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
		// default calling convention - cdecl
		CALL();
#else
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
			fprintf(stderr, "Unsupported calling convention %d", convention >> 6);
			break;
		}
#endif
		return 0; // if no call have made
	}
	long from_int(word* arg) {
		// так как в стек мы все равно большое сложить не сможем, то возьмем
		// только то, что влазит (первые два члена)
		assert (immediatep(arg[1]));
		assert (allocp(arg[2]));

		return (car(arg) >> 8) | ((car(cdr(arg)) >> 8) << FBITS);
	}
	float from_int_to_float(word* arg) {
		// читаем длинное число в float формат
		assert (immediatep(car(arg)));
		float f = (unsigned long)uftoi(car(arg));
		float mul = 0x1000000; // 1 << 24
		while (is_pointer(cdr(arg))) {
			arg = (word*)cdr(arg);
			f += (unsigned long)uftoi(cdr(arg)) * mul;
			mul *= 0x1000000;
		}
		assert (cdr(arg) == INULL);

		return f;
	}
	float from_rational(word* arg) {
		word* pa = (word*)car(arg);
		word* pb = (word*)cdr(arg);

		float a, b;
		if (immediatep(pa))
			a = sftoi(pa);
		else {
			switch (hdrtype(pa[0])) {
			case TINT:
				a = +from_int_to_float(pa);
				break;
			case TINTN:
				a = -from_int_to_float(pa);
				break;
			}
		}
		if (immediatep(pb))
			b = sftoi(pb);
		else {
			switch (hdrtype(pb[0])) {
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


	// a - function address
	// b - arguments (may be pair with req type in car and arg in cdr - not yet done)
	// c - '(return-type . argument-types-list)
	word* A = (word*)car(arguments); arguments = cdr(arguments); // function
	word* B = (word*)car(arguments); arguments = cdr(arguments); // rtty
	word* C = (word*)car(arguments); arguments = cdr(arguments); // args

//	assert(hdrtype(A[0]) == TPORT, A, 1032);
	assert ((word)B != INULL && hdrtype(B[0]) == TPAIR);
	assert ((word)C == INULL || hdrtype(C[0]) == TPAIR);
	// C[1] = return-type
	// C[2] = argument-types

	word args[18]; // пока только 12 аргумента максимум (18 - специально для gluLookAt)
	void *function = (void*)car(A);  assert (function);
	int returntype = uftoi(car(B));
	int floats = 0; // для amd64
	int doubles = 0; // temp

	long got;   // результат вызова функции
	int i = 0;     // количество аргументов
	word* p = (word*)C;   // сами аргументы
	word* t = (word*)cdr(B); // rtty

	while ((word)p != INULL) { // пока есть аргументы
		assert (hdrtype(*p) == TPAIR); // assert list
		assert (hdrtype(*t) == TPAIR); // assert list

		int type = uftoi(car(t));
		word* arg = (word*)car(p);

/*		// todo: add argument overriding as PAIR as argument value
		if (hdrtype(p[1]) == TPAIR) {
			type = imm_val (((word*)p[1])[1]);
			arg = ((word*)p[1])[2];
		}*/

//		args[i] = 0; // обнулим (так как потом можем записать только часть)
		// может и не надо.

		// destination type
		switch (type) {
		case TFIX:
		case TINT:
			if (immediatep(arg))
				args[i] = sftoi(arg);
			else
			switch (hdrtype(arg[0])) {
			case TINT: // source type
				args[i] = +from_int(arg);
				break;
			case TINTN:
				args[i] = -from_int(arg);
				break;
			case TRATIONAL:
				args[i] =  (int)from_rational(arg);
				break;
			// временное решение специально для sqlite3, потом я заведу отдельный тип type-int+-ref (такой, как type-handle)
			case TPORT:
			case TBVEC:
				args[i] = arg[1];
				break;
			default:
				args[i] = 0; // todo: error
			}
			break;

		case TFLOAT:
			if (immediatep(arg))
				*(float*)&args[i] = (float) (int)sftoi(arg);
			else
			switch (hdrtype(arg[0])) {
			case TINT: // source type
				*(float*)&args[i] = (float)+from_int(arg);
				break;
			case TINTN:
				*(float*)&args[i] = (float)-from_int(arg);
				break;
			case TRATIONAL:
				*(float*)&args[i] = (float) from_rational(arg);
				break;
			default:
				*(float*)&args[i] = (float) 0.0; // todo: error, return infinity, maybe, or NaN
			}
			floats |= (0x100 << i);
			break;
		case TDOUBLE:
			if (immediatep(arg))
				*(double*)&args[i] = (double) (int)sftoi(arg);
			else
			switch (hdrtype(arg[0])) {
			case TINT: // source type
				*(double*)&args[i] = (double)+from_int(arg);
				break;
			case TINTN:
				*(double*)&args[i] = (double)-from_int(arg);
				break;
			case TRATIONAL:
				*(double*)&args[i] = (double) from_rational(arg);
				break;
			default:
				*(double*)&args[i] = (double) 0.0; // todo: error, same as float
			}
#if __amd64__
			doubles |= (0x10000 << i);
#else
			i++; // for x86 double fills two words
#endif
			break;

		// запрос порта - это запрос значения порта
		// todo: добавить тип "указатель на порт"
		case TPORT:
			if ((word)arg == INULL)
				args[i] = (word) (void*)0;
			else
			switch (hdrtype(arg[0])) {
			case TPORT:
				args[i] = arg[1];
				break;
			default:
				args[i] = 0; // todo: error
			}
			break;

		case TBVEC:
		case TSTRING:
			if ((word)arg == INULL)
				args[i] = (word) (void*)0;
//#if sizeof(void*) = 8
//								args[++i] = (word) (void*)0;
//#endif
			else
			switch (hdrtype(arg[0])) {
			case TBVEC:
			case TSTRING:
			case TPORT:
//			case TCONST:
				// in arg[0] size got size of string
				args[i] = (word)(&arg[1]);
				break;
			default:
				args[i] = 0; // todo: error
			}
			break;
		case TTUPLE:
			if ((word)arg == INULL)
				args[i] = (word) (void*)0;
			else
			switch (hdrtype(arg[0])) {
/*			case TCONST:
				switch ((word)arg) {
				case INULL:
					args[i] = (word) (void*)0;
				}*/
			case TTUPLE: { // ?
				// tuple, это последовательность, а не список!
				// todo: сделать функцию cast или что-то такое
				// что-бы возвращать список, какой мне нужен

				// аллоцировать массив и сложить в него указатели на элементы кортежа
				int size = hdrsize(arg[0]);
				*fp++ = make_raw_header(size, TBVEC, 0);
				args[i] = (word)fp; // ссылка на массив указателей на элементы

				word* src = &arg[1];
				while (--size)
					*fp++ = (word)((word*)*src++ + 1);
//				int j;
//				for (j = 1; j < size; j++)
				}
				break;
			default:
				args[i] = INULL; // todo: error
			}
			break;
		case TRAWP:
			args[i] = (word)arg;
			break;
		default:
			args[i] = 0;
			break;
		}

		p = (word*)p[2]; // (cdr p)
		t = (word*)t[2]; // (cdr t)
		i++;
	}
	assert ((word)t == INULL); // количество аргументов совпало!

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

	got = call(function, args, i + floats + doubles);
#else
	got = call(returntype >> 8, args, i);
#endif

	switch (returntype & 0x3F) {
		case TINT:
			if (got > FMAX) {
				// прошу внимания!
				//  в числовой паре надо сначала положить старшую часть,
				//  и только потом младшую!
				result = (word) new_pair(TINT, F(got & 0xFFFFFF),
				                               new_pair(TINT, F(got >> 24),
				                                              INULL));
				break;
			}
			// no break
		case TFIX: // type-fix+ - если я уверен, что число заведомо меньше 0x00FFFFFF! (или сколько там в x64)
			result = itosf (got);
			break;
			// else goto case 0 (иначе вернем type-fx+)
		case TPORT:
			result = (word) new_port (got);
			break;
		case TRAWP:
			result = got;
			break;

		case TSTRING:
			if (got != 0)
				result = (word) new_string (lenn((char*)got, FMAX+1), (char*)got);
			break;
		case TVOID:
			result = INULL;
			break;
//                      todo: TRATIONAL
	}

	self->fp = fp;

	return result;
}
#endif//HAS_PINVOKE
