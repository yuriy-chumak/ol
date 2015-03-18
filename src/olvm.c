// максимальное число для элементарной математики: 16777215 (24 бита, 0xFFFFFF)
// считать так: (receive (fx+ 16777214 1) (lambda (hi lo) (list hi lo)))
// при превышении выдает мусор
// Z80: http://www.emuverse.ru/wiki/Zilog_Z80/%D0%A1%D0%B8%D1%81%D1%82%D0%B5%D0%BC%D0%B0_%D0%BA%D0%BE%D0%BC%D0%B0%D0%BD%D0%B4
//      http://igorkov.org/pdf/Z80-Central-Processor-Unit.pdf
//      https://ru.wikipedia.org/wiki/Zilog_Z80
// Всякие LISP иджеи и примеры:
//      http://habrahabr.ru/post/204992/
//      http://habrahabr.ru/post/211100/
// Книга http://ilammy.github.io/lisp/
// https://www.cs.utah.edu/flux/oskit/html/oskit-wwwch14.html
//
// GC в чикенлиспе: http://en.wikipedia.org/wiki/Cheney%27s_algorithm
//
// кастомные типы: https://www.gnu.org/software/guile/manual/html_node/Describing-a-New-Type.html#Describing-a-New-Type
// список функций: http://jscheme.sourceforge.net/jscheme/doc/R4RSprimitives.html

// pinned objects - если это будут просто какие-то равки, то можно из размещать ДО основной памяти,
//	при этом основную память при переполнении pinned размера можно сдвигать вверх.

#include "olvm.h"

// На данный момент поддерживаются четыре операционные системы:
//  Windows, Linux, Android, MacOS
// Обратите внимание на проект http://sourceforge.net/p/predef/wiki/OperatingSystems/
#ifndef __GNUC__
#	warning "This code tested only under Gnu C compiler"
#endif

// posix or not: http://stackoverflow.com/questions/11350878/how-can-i-determine-if-the-operating-system-is-posix-in-c


// STANDALONE - самостоятельный бинарник без потоков (виртуальная машина, короче) и т.д.

// PORT: либо равка, с типом type-port; либо raw-объект со словом port и размером 2

// todo: проверить, что все работает в 64-битном коде
// todo: переименовать tuple в array. array же неизменяемый, все равно. (???)
//  а изменяемые у нас вектора

// http://joeq.sourceforge.net/about/other_os_java.html
// call/cc - http://fprog.ru/lib/ferguson-dwight-call-cc-patterns/

// компилятор поддерживает только несколько специальных форм:
//	lambda, quote, rlambda (recursive lambda), receive, _branch, _define, _case-lambda, values (смотреть env.scm)
//	все остальное - макросы (?)

#include <stddef.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h> // posix
#include <assert.h>
#include <dirent.h>
#include <string.h>

#include <errno.h>
#include <time.h>
#include <inttypes.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>

// thread local storage modifier for virtual machine private variables -
// виртуальная машина у нас работает в отдельном потоке, соответственно ее
// локальные переменные можно держать в TLS, а не в какой-то VM структуре
#ifdef STANDALONE
#  define __tlocal__
#else
#	ifndef __tlocal__
#		if __STDC_VERSION__ >= 201112 && !defined __STDC_NO_THREADS__
#			define thread_local _Thread_local
#		elif defined _WIN32 && ( \
		     defined _MSC_VER || \
		     defined __ICL || \
		     defined __DMC__ || \
		     defined __BORLANDC__ )
#			define __tlocal__ __declspec(thread)
/* note that ICC (linux) and Clang are covered by __GNUC__ */
#		elif defined __GNUC__ || defined __SUNPRO_C || defined __xlC__
#			define __tlocal__ __thread
#		else
#			error "Cannot define thread_local"
#		endif
#	endif
#endif//STANDALONE

// TEMP: disable __tlocal__ to check system resources consuption
#undef __tlocal__
#define __tlocal__



//********************************************************************
/*** Portability Issues ***/

#ifdef _WIN32
#	define WIN32_LEAN_AND_MEAN
#	define NOMINMAX
#	include <winsock2.h>
#	include <ws2tcpip.h>
#	include <conio.h>
#	include <windows.h>
	typedef unsigned long in_addr_t;
#	define EWOULDBLOCK WSAEWOULDBLOCK
#	undef ERROR // due to macro redefinition
#else

#	include <netinet/in.h>
#	include <sys/socket.h>
#	include <sys/wait.h>

#	ifndef O_BINARY
#		define O_BINARY 0
#	endif

#endif
#ifdef __ANDROID__
#	include <netinet/in.h>
#	include <sys/socket.h>
#	include <sys/wait.h>
	typedef unsigned long in_addr_t;
#endif
#ifdef __linux__
#	include <netinet/in.h>
#	include <sys/socket.h>
#	include <sys/wait.h>
#	ifndef O_BINARY
#		define O_BINARY 0
#	endif
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

#	include <netinet/in.h>
#	include <sys/socket.h>
#	include <sys/wait.h>
#	include <sys/wait.h>
#	ifndef O_BINARY
#		define O_BINARY 0
#	endif
#endif

// Threading (pthread)
#ifndef STANDALONE

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

#endif//STANDALONE
#define STATIC static __inline__

// -=( fifo )=------------------------------------------------
#ifndef STANDALONE
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

#endif//STANDALONE


// --------------------------------------------------------
// -=( dl )=-----------------------------------------------
#ifndef JAVASCRIPT
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
void *dlsym  (void *handle, const char *name)
{
	FARPROC function;

	function = GetProcAddress((HANDLE)handle, name);
	return function;
}

#else
#include <dlfcn.h>
#endif

#endif//STANDALONE
// -=( OL )=----------------------------------------------------------------------
// --
//
// виртуальная машина
typedef struct OL
{
#ifndef STANDALONE
	pthread_t tid;
	struct fifo i; // обе очереди придется держать здесь, так как данные должны быть доступны даже после того, как vm остановится.
	struct fifo o;
#endif//STANDALONE
} OL;

// основной тип даных, зависит от разрядности машины
// based on C99 standard, <stdint.h>:
// unsigned int that is capable of storing a pointer
typedef uintptr_t word;


//;; DESCRIPTOR FORMAT
//                            .------------> 24-bit payload if immediate
//                            |      .-----> type tag if immediate
//                            |      |.----> immediateness
//   .------------------------| .----||.---> mark bit (can only be 1 during gc, removable?)
//  [pppppppp pppppppp pppppppp tttttti0]
//   '-------------------------------|
//                                   '-----> 4- or 8-byte aligned pointer if not immediate
//      младшие 2 нулевые бита для указателя (mark бит снимается при работе) позволяют работать только с выравненными
//       внутренними указателями - таким образом, ВСЕ объекты в куче выравнены по границе слова
//
// object headers are further
//  [ssssssss ssssssss ????rppp tttttt10] // bit "immediate" у заголовкой всегда! выставлен в 1
//   '---------------| '--||'-| '----|
//                   |    ||  |      '-----> object type
//                   |    ||  '------------> number of padding (unused) bytes at end of object if raw (0-(wordsize-1))
//                   |    |'---------------> rawness bit (raw objects have no decriptors in them)
//                   |    '----------------> your tags here! e.g. tag for closing file descriptors in gc
//                   '---------------------> object size in words
//
//; note - there are 6 type bits, but one is currently wasted in old header position
//; to the right of them, so all types must be <32 until they can be slid to right
//; position.

// todo: вот те 4 бита можно использовать для кастомных типов - в спецполя складывать ptr на функцию, что вызывает mark для подпоинтеров,
//	и ptr на функцию, что делает финализацию.

//#pragma pack(push, sizeof(word))
typedef struct object
{
	word header;
	word ref[];
} __attribute__ ((aligned(sizeof(word)), packed))
	object;
//#pragma pack(pop)

/*typedef struct pair
{
	word header;
	word *car;
	word *cdr;
} pair;*/


// Для экономии памяти

#define IPOS                        8  /* offset of immediate payload */
#define SPOS                        16 /* offset of size bits in header immediate values */
#define TPOS                        2  /* offset of type bits in header */

#define V(ob)                       *((word *) (ob)) // *ob, ob[0]
#define W                           sizeof(word)

//#define NWORDS                      1024*1024*8    /* static malloc'd heap size if used as a library */
#define FBITS                       24             /* bits in fixnum, on the way to 24 and beyond */
#define FMAX                        ((1<<FBITS)-1) /* maximum fixnum (and most negative fixnum) */
#define MAXOBJ                      0xffff         /* max words in tuple including header */
#define RAWBIT                      (1<<11)
#define RAW(t)                      (t | (RAWBIT >> TPOS))
#define make_immediate(value, type)    (((value) << IPOS) | ((type) << TPOS)                         | 2)
#define make_header(size, type)        (( (size) << SPOS) | ((type) << TPOS)                         | 2)
#define make_raw_header(size, type, p) (( (size) << SPOS) | ((type) << TPOS) | (RAWBIT) | ((p) << 8) | 2)
// p is padding

#define F(val)                      (((val) << IPOS) | 2)
#define TRUEFALSE(cval)             ((cval) ? ITRUE : IFALSE)
#define ZEROFALSE(cval)             ((cval) ?  F(0) : IFALSE)
#define fixval(desc)                ((desc) >> IPOS) // unsigned shift!!!
#define fixnump(desc)               (((desc) & 0xFF) == 2)
#define fliptag(ptr)                ((word)ptr ^ 2) /* make a pointer look like some (usually bad) immediate object */

#define NR                          96 // was 190 /* see n-registers in register.scm */
//#define header(x)                   *(word *x)
#define imm_type(x)                 ((((unsigned int)x) >> TPOS) & 0x3F)
#define imm_val(x)                   (((unsigned int)x) >> IPOS)
#define hdrsize(x)                  ((((word)x) >> SPOS) & MAXOBJ)
#define padsize(x)                  ((((word)x) >> 8) & 7)
#define hdrtype(x)                  ((((word)x) & 0xFF) >> 2) // 0xFF from (p) << 8) in make_raw_header

#define immediatep(x)               (((word)x) & 2)
#define allocp(x)                   (!immediatep(x))
#define rawp(hdr)                   ((hdr) & RAWBIT)
#define pairp(ob)                   (allocp(ob) && V(ob)==PAIRHDR)

#define is_pointer(x)               (!immediatep(x))
#define is_flagged(x)               (((word)x) & 1) // flag - mark for GC

// встроенные типы (смотреть defmac.scm по "ALLOCATED")
#define TFIX                         (0)      // type-fix+
#define TFIXN                        (0 + 32) // type-fix-
#define TPAIR                        1
#define TTUPLE                       2
#define TSTRING                      3

#define TPORT                       (12)
#define TRAWPORT                    RAW(TPORT)
#define TCONST                      13
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

#define INULL                       make_immediate(0, TCONST)
#define IFALSE                      make_immediate(1, TCONST)
#define ITRUE                       make_immediate(2, TCONST)
#define IEMPTY                      make_immediate(3, TCONST) /* empty ff */
#define IEOF                        make_immediate(4, TCONST)
#define IHALT                       INULL /* FIXME: adde a distinct IHALT */

static const word I[]               = { F(0), INULL, ITRUE, IFALSE };  /* for ldi and jv */

#define PAIRHDR                     make_header(3, TPAIR)
#define NUMHDR                      make_header(3, TINT) // <- on the way to 40, see type-int+ in defmac.scm

#define FFRIGHT                     1
#define FFRED                       2

#define flagged_or_raw(hdr)         (hdr & (RAWBIT|1))
#define likely(x)                   __builtin_expect((x), 1)
#define unlikely(x)                 __builtin_expect((x), 0)

#define MEMPAD                      (NR+2)*8 /* space at end of heap for starting GC */
#define MINGEN                      1024*32  /* minimum generation size before doing full GC  */
#define INITCELLS                   1000

/*** Globals and Prototypes ***/
// память виртуальной машины локальна по отношению к ее потоку. пусть пока побудет так - не буду
//  ее тащить в структуру VM

static __tlocal__ word max_heap_size; /* max heap size in MB */

static int breaked;      /* set in signal handler, passed over to owl in thread switch */

static int seccompp;     /* are we in seccomp? */
static unsigned long seccomp_time; /* virtual time within seccomp sandbox in ms */

int slice;

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





/*** Garbage Collector, based on "Efficient Garbage Compaction Algorithm" by Johannes Martin (1982) ***/
// несколько ссылок "на почитать" по теме GC:
//   shamil.free.fr/comp/ocaml/html/book011.html

// allocation pointer (top of allocated heap)
static __tlocal__ word *fp;

// выделить сырой блок памяти
#define new(size) ({\
	word* addr = fp;\
	fp += size;\
	/*return*/ addr;\
})

// аллоцировать новый объект
#define new_object(size, type) ({\
	word* p = new (size);\
	*p = make_header(size, type);\
	/*return*/(object*)p;\
})

// создать новый порт
#define new_port_old(a) ({\
	word value = (word)a;\
	word *addr = new (2);\
	addr[0] = make_header(2, TRAWPORT);\
	addr[1] = value;\
	/*return*/ addr;\
})

// или в структурно ориентированном стиле:
#define new_port(a) ({\
	word value = (word) a;\
	object *me = new_object (2, TRAWPORT);\
	me->ref[0] = value;\
	/*return*/ me;\
})


// car, cdr надо предвычислить перед тем, как выделим память,
//	так как в параметрах могут быть аллоцируемые объекты.
#define new_pair(a1, a2) ({\
	word data1 = (word) a1;\
	word data2 = (word) a2;\
	/* точка следования */ \
	object *me = new_object(3, TPAIR);\
	me->ref[0] = data1;\
	me->ref[1] = data2;\
	/*return*/ me;\
})
// а по факту эта функция сводится к простому
static __inline__ word* new_pair_old (word* car, word* cdr)
{
	word *object = fp;

	fp[0] = PAIRHDR;
	fp[1] = (word) car;
	fp[2] = (word) cdr;

	fp += 3;
	return object;
}

static __inline__ word* new_string (size_t length, char* string)
{
	word* object = fp;

	int size = (length / W) + ((length % W) ? 2 : 1);
	int pads = (size-1) * W - length;

	*fp = make_raw_header(size, TSTRING, pads);
	char* p = ((char *) fp) + W;
	while (length--) *p++ = *string++;

	fp += size;
	return object;
}

static __inline__ word* new_npair (word* car, word* cdr)
{
	word *object = fp;

	fp[0] = NUMHDR;
	fp[1] = (word) car;
	fp[2] = (word) cdr;

	fp += 3;
	return object;
}

static __inline__ word* new_tuple (size_t length)
{
	word *object = fp;

	fp[0] = make_header(length + 1, TTUPLE);

	fp += (length + 1);
	return object;
}
static __inline__ word* new_tuplei (size_t length, ...)
{
	word *object = fp;

	va_list argp;
	va_start(argp, length);

	fp[0] = make_header(length + 1, TTUPLE);
	int i = 0;
	while (i < length)
		fp[++i] = va_arg(argp, word);

	va_end(argp);

	fp += (length + 1);
	return object;
}


struct heap
{
	//  begin <= genstart <= end
	word *begin;     // was: memstart
	word *end;       // was: memend

	word *genstart;  // was: genstart
//	word *top; // fp
}
static __tlocal__ heap; // память машины, управляемая сборщиком мусора


#define cont(n)                     V((word)n & ~1)  // ~ - bitwise NOT (корректное разименование указателя, без учета бита mark)

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

static int marked;
// просматривает список справа налево
static void mark(word *pos, word *end)
{
	marked = 0;
//	assert(pos is NOT flagged)
	while (pos != end) {
		word val = pos[0]; // pos header
		if (is_pointer(val) && val >= ((word) heap.genstart)) { // genstart - начало молодой генерации
			if (is_flagged(val)) {
				pos = chase((word*) val);
				pos--;
			}
			else {
				word hdr = *(word *) val;
//				//if (immediatep(hdr))
//					*(word *) val |= 1; // flag this (таки надо, иначе часть объектов не распознается как pinned!)
				marked++;

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
static word *compact()
{
//	printf("%s\n", "compact()");

	word* heapbegin = heap.begin;
	word* heapgenstart = heap.genstart;


	word *old = heap.genstart;
	word *end = heap.end - 1;

	word *newobject = old;
	while (old < end) {
		if (is_flagged(*old)) {
			// 1. test for presence of "unmovable objects" in this object
			// 2. find the header:
//			word* real = chase(old);
//			word hdr_value = *real;

			/*if (!rawp(hdr_value)) {
				int found = 0;
				int i = hdrsize(*real);
				word* p = old;
				while (--i) {
					word q = *++p;
					// 1. попробуем НЕ перемещать блоки, которые с флажком (значит, содержат ссылки вперед - так как сслки назад все резолвятся во время
					//  разворачивания заголовков - ссылок!)
					if (flagged(q)) {
						//printf("(found: type = %d)\n", hdrtype(q));
						found = 1;
						break;
					}
				}
				// вот тут надо по умному использовать предыдущее свободное место: записать пустой объект с размером равным свободному месту
				// tbd.
				//pleasefixme
				if (found) {
					pinned++;
					while (old - newobj > 0x7000) { // 0xFFFF
						*newobj = make_raw_header(0x7000, TVOID, 0); // TSTRING
						newobj += 0x7000;
					}
					if (newobj != old)
						*newobj = make_raw_header((old - newobj), TVOID, 0); // TSTRING
					newobj = old;
					//printf("%c", '-');
				}
			}*/

			word val = *newobject = *old;
			while (is_flagged(val)) {
				val &= ~1; //clear mark

				word* ptr = (word*)val;
				*newobject = *ptr;
				*ptr = (word)newobject;


//				if (immediatep(*newobj) && flagged(*newobj))
//					*newobj &= (~1); // was: = flag(*newobj);
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
//	printf(">\n");
	return newobject;
}


#ifdef _LP64
typedef int64_t   wdiff;
#else
typedef int32_t   wdiff;
#endif

static void fix_pointers(word *pos, wdiff delta, word *end)
{
	while (1) {
		word hdr = *pos;
		int n = hdrsize(hdr);
		if (hdr == 0) return; // end marker reached. only dragons beyond this point.
		if (rawp(hdr))
			pos += n; // no pointers in raw objects
		else {
			pos++;
			n--;
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
static wdiff adjust_heap(int cells)
{
	if (seccompp) /* realloc is not allowed within seccomp */
		return 0;

	/* add newobj realloc + heap fixer here later */
	word nwords = heap.end - heap.begin + MEMPAD; /* MEMPAD is after memend */
	word new_words = nwords + ((cells > 0xffffff) ? 0xffffff : cells); /* limit heap growth speed  */
	if (((cells > 0) && (new_words*W < nwords*W)) || ((cells < 0) && (new_words*W > nwords*W)))
		return 0; /* don't try to adjust heap if the size_t would overflow in realloc */

	word *old = heap.begin;
	heap.begin = realloc(heap.begin, new_words*W);
	if (heap.begin == old) { /* whee, no heap slide \o/ */
		heap.end = heap.begin + new_words - MEMPAD; // leave MEMPAD words alone
		return 0;
	} else if (heap.begin) { /* d'oh! we need to O(n) all the pointers... */
		wdiff delta = (word)heap.begin - (word)old;
		heap.end = heap.begin + new_words - MEMPAD; // leave MEMPAD words alone
		fix_pointers(heap.begin, delta, heap.end);
		return delta;
	} else {
		breaked |= 8; /* will be passed over to mcp at thread switch*/
		return 0;
	}
}


/*static void check_memory_consistence(const char* text)
{
	return;
	word* p = heap.begin;
	word* e = fp;

	printf("%s\n", text);

	while (p < e) {
		word header = *p;
		word length = hdrsize(header);
		word type = hdrtype(header);

		if (type == TSTRING)
			printf("%c", 's');
		else if (type == TPAIR)
			printf("%c", 'p');
		else if (type == TVOID)
			printf("%c", '#');
		else if (type == TTUPLE)
			printf("%c", 't');
		else
			printf("%c", 'o');

		p += length;
	}
	printf("\n");
}*/


/* input desired allocation size and (the only) pointer to root object
   return a pointer to the same object after heap compaction, possible heap size change and relocation */

// todo: ввести третий generation
static word *gc(int size, word *regs) {
	clock_t uptime;
	word *root;
	int nfree;

	word *realend = heap.end;

	heap.end = fp;

	*fp = make_header(2, TTUPLE); // (в *fp спокойно можно оставить мусор)
	root = fp + 1; // skip header
//	root = &fp[1]; // same

	// непосредственно сам GC
	uptime = -(1000 * clock()) / CLOCKS_PER_SEC;
	root[0] = regs;
	mark(root, fp); // assert (root > fp)
	fp = compact();
	regs = root[0];
	uptime += (1000 * clock()) / CLOCKS_PER_SEC;

//	if (pinned)
		fprintf(stderr, "GC done in %4d ms (use: %8d bytes): marked %6d, moved %6d, pinned %2d, moved %8d bytes total\n",
				uptime,
				sizeof(word) * (fp - heap.begin), marked, -1, -1, -1);

//	regs = (word *) *root;
	heap.end = realend;
	nfree = (word)heap.end - (word)regs;
	if (heap.genstart == heap.begin) {
		word heapsize = (word) heap.end - (word) heap.begin;
		word nused = heapsize - nfree;
		if ((heapsize/(1024*1024)) > max_heap_size)
			breaked |= 8; /* will be passed over to mcp at thread switch*/

		nfree -= size*W + MEMPAD;   /* how much really could be snipped off */
		if (nfree < (heapsize / 10) || nfree < 0) {
			/* increase heap size if less than 10% is free by ~10% of heap size (growth usually implies more growth) */
			regs[hdrsize(*regs)] = 0; /* use an invalid descriptor to denote end live heap data  */
			regs = (word *) ((word)regs + adjust_heap(size*W + nused/10 + 4096));
			nfree = heap.end - regs;
			if (nfree <= size)
				breaked |= 8; /* will be passed over to mcp at thread switch. may cause owl<->gc loop if handled poorly on lisp side! */
		}
		else if (nfree > (heapsize/5)) {
			/* decrease heap size if more than 20% is free by 10% of the free space */
			int dec = -(nfree/10);
			int newobj = nfree - dec;
			if (newobj > size*W*2 + MEMPAD) {
				regs[hdrsize(*regs)] = 0; /* as above */
				regs = (word *) ((word)regs + adjust_heap(dec+MEMPAD*W));
				heapsize = (word) heap.end - (word) heap.begin;
				nfree = (word) heap.end - (word) regs;
			}
		}
		heap.genstart = regs; /* always start newobj generation */
	} else if (nfree < MINGEN || nfree < size*W*2) {
		heap.genstart = heap.begin; /* start full generation */
		return gc(size, regs);
	} else {
		heap.genstart = regs; /* start newobj generation */
	}
	return regs;
}


/*** OS Interaction and Helpers ***/
//static
void set_blocking(int sock, int blockp) {
#ifdef _WIN32
   unsigned long flags = 1;
   if (sock > 3) { // stdin is read differently, out&err block
      ioctlsocket(sock, FIONBIO, &flags);
   }
#else
   fcntl(sock, F_SETFL, (blockp ?: O_NONBLOCK));
#endif
}

#ifndef _WIN32
static
void signal_handler(int signal) {
   switch(signal) {
      case SIGINT:
         breaked |= 2; break;
      case SIGPIPE: break; /* can cause loop when reporting errors */
      default:
         // printf("vm: signal %d\n", signal);
         breaked |= 4;
   }
}
#endif

/* small functions defined locally after hitting some portability issues */
static __inline__ void bytecopy(char *from, char *to, int n) { while(n--) *to++ = *from++; }
static __inline__ void wordcopy(word *from, word *to, int n) { while(n--) *to++ = *from++; }
static __inline__
unsigned int lenn(char *pos, unsigned int max) { /* added here, strnlen was missing in win32 compile */
   unsigned int p = 0;
   while (p < max && *pos++) p++;
   return p;
}


/* list length, no overflow or valid termination checks */
#ifndef _WIN32
static
int llen(word *ptr) {
   int len = 0;
   while (allocp(ptr) && *ptr == PAIRHDR) {
      len++;
      ptr = (word *) ptr[2];
   }
   return len;
}
#endif

void set_signal_handler() {
#ifndef _WIN32
   struct sigaction sa;
   sa.sa_handler = signal_handler;
   sigemptyset(&sa.sa_mask);
   sa.sa_flags = SA_RESTART;
   sigaction(SIGINT, &sa, NULL);
   sigaction(SIGPIPE, &sa, NULL);
#endif
}

/* make a byte vector object to hold len bytes (compute size, advance fp, set padding count) */
static word *mkbvec(int len, int type) {
   int nwords = (len/W) + ((len % W) ? 2 : 1);
   int pads = (nwords-1)*W - len;
   word *ob = fp;
   fp += nwords;
   *ob = make_raw_header(nwords, type, pads);
   return ob;
}

/* map a null or C-string to False, Null or owl-string, false being null or too large string */
word strp2owl(char *sp) {
   int len;
   word *res;
   if (!sp) return IFALSE;
   len = lenn(sp, FMAX+1);
   if (len == FMAX+1) return INULL; /* can't touch this */
   res = mkbvec(len, TBVEC); /* make a bvec instead of a string since we don't know the encoding */
   bytecopy(sp, ((char *)res)+W, len);
   return (word)res;
}


/*** Primops called from VM and generated C-code ***/

static word prim_connect(word *host, word port) {
   int sock;
   unsigned char *ip = ((unsigned char *) host) + W;
   unsigned long ipfull;
   struct sockaddr_in addr;
   port = fixval(port);
   if (!allocp(host))  /* bad host type */
      return IFALSE;
   if ((sock = socket(PF_INET, SOCK_STREAM, 0)) == -1)
      return IFALSE;
   addr.sin_family = AF_INET;
   addr.sin_port = htons(port);
   addr.sin_addr.s_addr = (in_addr_t) host[1];
   ipfull = (ip[0]<<24) | (ip[1]<<16) | (ip[2]<<8) | ip[3];
   addr.sin_addr.s_addr = htonl(ipfull);
   if (connect(sock, (struct sockaddr *) &addr, sizeof(struct sockaddr_in)) < 0) {
      close(sock);
      return IFALSE;
   }
   set_blocking(sock,0);
   return F(sock);
}

static word prim_less(word a, word b) {
   if (immediatep(a)) {
      return immediatep(b) ? TRUEFALSE(a < b) : ITRUE;  /* imm < alloc */
   } else {
      return immediatep(b) ? IFALSE : TRUEFALSE(a < b); /* alloc > imm */
   }
}

static word prim_get(word *ff, word key, word def) { /* ff assumed to be valid */
   while((word) ff != IEMPTY) { /* ff = [header key value [maybe left] [maybe right]] */
      word this = ff[1], hdr;
      if (this == key)
         return ff[2];
      hdr = *ff;
      switch(hdrsize(hdr)) {
         case 3: return def;
         case 4:
            if (key < this) {
               ff = (word *) ((hdr & (1 << TPOS)) ? IEMPTY : ff[3]);
            } else {
               ff = (word *) ((hdr & (1 << TPOS)) ? ff[3] : IEMPTY);
            }
            break;
         default:
            ff = (word *) ((key < this) ? ff[3] : ff[4]);
      }
   }
   return def;
}

static word prim_cast(word *object, int type) {
	if (immediatep(object))
		return make_immediate(imm_val((word)object), type);
	else
	{ /* make a clone of more desired type */

		// todo: добавить каст с конверсией. например, из большого целого числа в handle или float
		// это лучше сделать тут, наверное, а не отдельной командой

      word hdr = *object++;
      int size = hdrsize(hdr);
      word *newobj, *res; /* <- could also write directly using *fp++ */
      newobj = new (size);
      res = newobj;
      /* (hdr & 0b...11111111111111111111100000000111) | tttttttt000 */
      //*newobj++ = (hdr&(~2040))|(type<<TPOS);
      *newobj++ = (hdr&(~252))|(type<<TPOS); /* <- hardcoded ...111100000011 */
      wordcopy(object,newobj,size-1);
      return (word)res;
   }
}

static int prim_refb(word pword, int pos) {
   word *ob = (word *) pword;
   word hdr, hsize;
   if (immediatep(ob))
      return -1; // todo: return IFALSE
   hdr = *ob;
   hsize = ((hdrsize(hdr)-1)*W) - padsize(hdr); /* bytes - pads */
   if (pos >= hsize)
      return IFALSE;
   return F(((unsigned char *) ob)[pos+W]);
}

static word prim_ref(word pword, word pos)  {
   word *ob = (word *) pword;
   word hdr, size;
   pos = fixval(pos);
   if(immediatep(ob)) { return IFALSE; }
   hdr = *ob;
   if (rawp(hdr)) { /* raw data is #[hdrbyte{W} b0 .. bn 0{0,W-1}] */
      size = ((hdrsize(hdr)-1)*W) - padsize(hdr);
      if (pos >= size) { return IFALSE; }
      return F(((unsigned char *) ob)[pos+W]);
   }
   size = hdrsize(hdr);
   if (!pos || size <= pos) /* tuples are indexed from 1 (probably later 0-255)*/
      return IFALSE;
   return ob[pos];
}

static word prim_set(word wptr, word pos, word val) {
   word *ob = (word *) wptr;
   word hdr;
   word *newobj;
   int p = 0;
   pos = fixval(pos);
   if (immediatep(ob)) { return IFALSE; }
   hdr = *ob;
   if (rawp(hdr) || hdrsize(hdr) < pos) { return IFALSE; }
   word size = hdrsize(hdr);
   newobj = new (size);
   while(p <= size) {
      newobj[p] = (pos == p && p) ? val : ob[p];
      p++;
   }
   return (word) newobj;
}

static word syscall(word op, word a, word b, word c)
{
	switch (op)
	{
		// SOCKET
		case 41: { // socket (options: STREAM or DGRAM)
			int port = fixval(b);
			int sock = socket(PF_INET, port, 0);
			if (sock == -1)
				break;

			return F(sock);
		}

		// ACCEPT
		// http://linux.die.net/man/2/accept
		case 43: {
			int sockfd = fixval(a);

			struct sockaddr_in addr;
			socklen_t len = sizeof(addr);
			int sock = accept(sockfd, (struct sockaddr *)&addr, &len);
			// On error, -1 is returned
			if (sock < 0)
				break;

			return F(sock);
		}

		// SEND
		// http://linux.die.net/man/2/send
		case 44: { // fd, buf, len
			int fd = fixval(a);
			word *data = (word *)b;
			int len = fixval(c);

			int size = (hdrsize(*data)-1) * W;
			if (len > size)
				break;

			int sent = sendto(fd, ((char *)data)+W, len, 0, NULL, 0);
			if (sent == -1) // On error, -1 is returned
				break;

			return F(sent);
		}

		// SHUTDOWN
		// http://linux.die.net/man/2/shutdown
		case 48: {
			int sd = fixval(a);

			// On error, -1 is returned
			if (shutdown(sd, 0) != 0)
				break;

			return ITRUE;
		}

		// BIND (socket, port, #false) // todo: c for options
		// http://linux.die.net/man/2/bind
		case 49: {
			// todo: assert on argument types
			int sock = fixval(a);
			int port = fixval(b);

			struct sockaddr_in interface;
			interface.sin_family = AF_INET;
			interface.sin_port = htons(port);
			interface.sin_addr.s_addr = INADDR_ANY;

			// On success, zero is returned.
			if (bind(sock, (struct sockaddr *) &interface, sizeof(interface)) == 0)
				return ITRUE;
			break;
		}


		// LISTEN
		// http://linux.die.net/man/2/listen
		// listen() marks the socket referred to by sockfd as a passive socket, that is,
		// as a socket that will be used to accept incoming connection requests using accept(2).
		case 50: {
			int sockfd = fixval(a);

			// On success, zero is returned.
			if (listen(sockfd, 1024) == 0) {
	//					set_blocking(sockfd, 0);
				return ITRUE;
			}

			break;
		}
		default:
			break;
	}

	return IFALSE;
}

/* system- and io primops */
// todo: move to the parental switch
static word prim_sys(int op, word a, word b, word c) {
   switch(op) {
      case 1001: { /* 1 = fopen <str> <mode> <to> */
         char *path = (char *) a;
         int mode = fixval(b);
         int val;
         struct stat sb;
         if (!(allocp(path) && imm_type(*path) == TSTRING))
            return IFALSE;
         mode |= O_BINARY | ((mode > 0) ? O_CREAT | O_TRUNC : 0);
         val = open(((char *) path) + W, mode,(S_IRUSR|S_IWUSR));
         if (val < 0 || fstat(val, &sb) == -1 || (sb.st_mode & S_IFDIR)) {
            close(val);
            return IFALSE;
         }
         set_blocking(val,0);
         return F(val); }
      case 1002:
         return close(fixval(a)) ? IFALSE : ITRUE;
      case 1003: { /* 3 = sopen port -> False | fd  */
         int port = fixval(a);
         int s;
         int opt = 1; /* TRUE */
         struct sockaddr_in myaddr;
         myaddr.sin_family = AF_INET;
         myaddr.sin_port = htons(port);
         myaddr.sin_addr.s_addr = INADDR_ANY;
         s = socket(AF_INET, SOCK_STREAM, 0);
#ifndef _WIN32
         if (s < 0) return IFALSE;
#else
	 if (s == INVALID_SOCKET) return IFALSE;
#endif
         if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&opt, sizeof(opt)) \
             || bind(s, (struct sockaddr *) &myaddr, sizeof(myaddr)) != 0 \
             || listen(s, 5) != 0) {
            close(s);
            return IFALSE;
         }
         set_blocking(s,0);
         return F(s); }
      case 1004: { /* 4 = accept port -> rval=False|(ip . fd) */
         int sock = fixval(a);
         struct sockaddr_in addr;
         socklen_t len = sizeof(addr);
         int fd;
         word *pair;
         char *ipa;
         fd = accept(sock, (struct sockaddr *)&addr, &len);
         if (fd < 0) return IFALSE;
         set_blocking(fd,0);
         ipa = (char *) &addr.sin_addr;
         *fp = make_raw_header(2, TBVEC, 4%W);
         bytecopy(ipa, ((char *) fp) + W, 4);
         fp[2] = PAIRHDR;
         fp[3] = (word) fp;
         fp[4] = F(fd);
         pair = fp+2;
         fp += 5;
         return (word)pair; }



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
         res = mkbvec(len, 3); /* make a fake raw string (OS may not use valid UTF-8) */
         bytecopy((char *)&dire->d_name, (char *) (res + 1), len); /* *no* terminating null, this is an owl bvec */
         return (word)res; }
      case 1013: /* sys-closedir dirp _ _ -> ITRUE */
         closedir((DIR *)fliptag(a));
         return ITRUE;
      case 1014: { /* set-ticks n _ _ -> old */
         word old = F(slice);
         slice = fixval(a);
         return old; }
      case 1015: { /* 0 fsocksend fd buff len r → n if wrote n, 0 if busy, False if error (argument or write) */
         int fd = fixval(a);
         word *buff = (word *) b;
         int wrote, size, len = fixval(c);
         if (immediatep(buff)) return IFALSE;
         size = (hdrsize(*buff)-1)*W;
         if (len > size) return IFALSE;
         wrote = send(fd, ((char *)buff)+W, len, 0); /* <- no MSG_DONTWAIT in win32 */
         if (wrote > 0) return F(wrote);
         if (errno == EAGAIN || errno == EWOULDBLOCK) return F(0);
         return IFALSE; }
      case 1016: { /* getenv <owl-raw-bvec-or-ascii-leaf-string> */
         char *name = (char *)a;
         if (!allocp(name)) return IFALSE;
         return strp2owl(getenv(name + W)); }
      case 1017: { /* exec[v] path argl ret */
#ifndef _WIN32
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
         set_blocking(0,1); /* try to return stdio to blocking mode */
         set_blocking(1,1); /* warning, other file descriptors will stay in nonblocking mode */
         set_blocking(2,1);
         execv(path, args); /* may return -1 and set errno */
         set_blocking(0,0); /* exec failed, back to nonblocking io for owl */
         set_blocking(1,0);
         set_blocking(2,0);
#endif
         return IFALSE; }
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

static word prim_lraw(word wptr, int type, word revp) {
   word *lst = (word *) wptr;
   int nwords, len = 0, pads;
   unsigned char *pos;
   word *raw, *ob;
   if (revp != IFALSE) {
	   exit(1);
   } /* <- to be removed */
   ob = lst;
   while (allocp(ob) && *ob == PAIRHDR) {
      len++;
      ob = (word *) ob[2];
   }
   if ((word) ob != INULL) return IFALSE;
   if (len > FMAX) return IFALSE;
   nwords = (len/W) + ((len % W) ? 2 : 1);
   raw = new (nwords);
   pads = (nwords-1)*W - len; /* padding byte count, usually stored to top 3 bits */
   *raw = make_raw_header(nwords, type, pads);
   ob = lst;
   pos = ((unsigned char *) raw) + W;
   while ((word) ob != INULL) {
      *pos++ = fixval(ob[1])&255;
      ob = (word *) ob[2];
   }
   while(pads--) { *pos++ = 0; } /* clear the padding bytes */
   return (word)raw;
}


static word prim_mkff(word t, word l, word k, word v, word r) {
   word *ob = fp;
   ob[1] = k;
   ob[2] = v;
   if (l == IEMPTY) {
      if (r == IEMPTY) {
         *ob = make_header(3, t);
         fp += 3;
      } else {
         *ob = make_header(4, t|FFRIGHT);
         ob[3] = r;
         fp += 4;
      }
   } else if (r == IEMPTY) {
      *ob = make_header(4, t);
      ob[3] = l;
      fp += 4;
   } else {
      *ob = make_header(5, t);
      ob[3] = l;
      ob[4] = r;
      fp += 5;
   }
   return (word) ob;
}

/*
ptrs[0] = make_raw_header(nobjs + 1, 0, 0);
int sss = hdrsize(ptrs[0]);
free((void *) file_heap);
*/

//#define OGOTO(f, n)                 ob = (word *)R[f]; acc = n; goto apply
//#define RET(n)                      ob = (word *)R[3]; R[3] = R[n]; acc = 1; goto apply

#define OCLOSE(proctype)            { \
	word size = *ip++, tmp; word *ob = new (size); tmp = R[*ip++]; tmp = ((word *) tmp)[*ip++]; \
	*ob = make_header(size, proctype); ob[1] = tmp; tmp = 2; \
	while (tmp != size) { ob[tmp++] = R[*ip++]; } R[*ip++] = (word) ob; }
#define CLOSE1(proctype)            { \
	word size = *ip++, tmp; word *ob = new (size); tmp = R[  1  ]; tmp = ((word *) tmp)[*ip++]; \
	*ob = make_header(size, proctype); ob[1] = tmp; tmp = 2; \
	while (tmp != size) { ob[tmp++] = R[*ip++]; } R[*ip++] = (word) ob; }
#define NEXT(n)                     ip += n; goto main_dispatch
//#define SKIP(n)                     ip += n; break;
#define TICKS                       10000 /* # of function calls in a thread quantum  */
#define ERROR(opcode, a, b)         { R[4] = F(opcode); R[5] = (word) a; R[6] = (word) b; goto invoke_mcp; }
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
#define G(ptr, n)                   ((word *)(ptr))[n]

// структура с параметрами для запуска виртуальной машины
struct args
{
	OL *vm;	// виртуальная машина (из нее нам нужны буфера ввода/вывода)

	// структура памяти VM. распределяется еще до запуска самой машины
	word max_heap_size; /* max heap size in MB */

	// memstart <= genstart <= memend
	struct heap heap;
	word *fp; // allocation pointer (top of allocated heap)

	void *userdata;
	volatile char signal;
};
// args + 0 = (list "arg0" "arg 1") or ("arg0 arg1" . NIL) ?
// args + 3 = objects list
// Несколько замечаний по этой функции:
//  http://msdn.microsoft.com/en-us/library/windows/desktop/ms686736(v=vs.85).aspx
//  The return value should never be set to STILL_ACTIVE (259), as noted in GetExitCodeThread.
__tlocal__ int forcegc = 0;

static
void* runtime(void *args) // heap top
{
	seccompp = 0;
	slice = TICKS; // default thread slice (n calls per slice)

	//
	max_heap_size = ((struct args*)args)->max_heap_size; /* max heap size in MB */

	// инициализируем локальную память
	heap.begin    = ((struct args*)args)->heap.begin;
	heap.end      = ((struct args*)args)->heap.end;
	heap.genstart = ((struct args*)args)->heap.genstart; // разделитель Old Generation и New Generation (?)

	// allocation pointer (top of allocated heap)
	fp            = ((struct args*)args)->fp;
	// подсистема взаимодействия с виртуальной машиной посредством ввода/вывода
#	ifndef STANDALONE
	fifo *fi =&((struct args*)args)->vm->o;
	fifo *fo =&((struct args*)args)->vm->i;
#	endif//STANDALONE

	// все, машина инициализирована, отсигналимся
	((struct args*)args)->signal = 1;

	void *userdata = ((struct args*)args)->userdata;
	// точка входа в программу - это последняя лямбда загруженного образа (λ (args))
	// todo: может стоит искать и загружать какой-нибудь main()?
	word* ptrs = (word*)userdata + 3;
	int nobjs = hdrsize(ptrs[0]) - 1;
	word* ob = (word*) ptrs[nobjs-1]; // выполним последний объект в списке /он должен быть (λ (args))/


	// регистры
	word R[NR];

	int i = 0;
	while (i < NR)
		R[i++] = INULL;
	R[0] = IFALSE;
	R[3] = IHALT;
	R[4] = (word) userdata;

	// Instruction Pointer
	unsigned char *ip;

		int bank = 0; /* ticks deposited at interop */
		int ticker = slice; /* any initial value ok */
	// usegc = 1; /* enble gc (later have if always evabled) */

	unsigned short acc = 2; // boot always calls with 2 args, no support for >255arg functions

apply: // apply something at ob to values in regs, or maybe switch context
	while (1) {
		if ((word)ob == IEMPTY && acc > 1) { /* ff application: (False key def) -> def */
			ob = (word *) R[3]; /* call cont */
			R[3] = (acc > 2) ? R[5] : IFALSE; /* default arg or false if none */
			acc = 1;
			continue;
		}
		if ((word)ob == IHALT) {
			/* a tread or mcp is calling the final continuation  */
			ob = (word *) R[0];
			if (!allocp(ob)) {
				fprintf(stderr, "Unexpected virtual machine exit\n");
				// todo: maybe required same behaviour as "exit" call
				return (void*)fixval(R[3]);
			}

			R[0] = IFALSE;
			breaked = 0;
			R[4] = R[3];
			R[3] = F(2);
			R[5] = IFALSE;
			R[6] = IFALSE;
			ticker = 0xffffff;
			bank = 0;
			acc = 4;
			continue;
		} /* <- add a way to call the newobj vm prim table also here? */

		if (allocp(ob)) { // если это аллоцированный объект
			word hdr = *ob & 0x0FFF; // cut size out, take just header info
			if (hdr == make_header(0, TPROC)) { // proc
				R[1] = (word) ob; ob = (word *) ob[1]; // ob = car(ob)
			}
			else
			if (hdr == make_header(0, TCLOS)) { // clos
				R[1] = (word) ob; ob = (word *) ob[1]; // ob = car(ob)
				R[2] = (word) ob; ob = (word *) ob[1]; // ob = car(ob)
			}
			else
			if (((hdr>>TPOS) & 60) == TFF) { /* low bits have special meaning */
				word *cont = (word *) R[3];
				switch (acc)
				{
				case 2:
					R[3] = prim_get(ob, R[4],    0);
					if (!R[3])
						ERROR(260, ob, R[4]);
					break;
				case 3:
					R[3] = prim_get(ob, R[4], R[5]);
					break;
				default:
					ERROR(259, ob, INULL);
				}
				ob = cont;
				acc = 1;
				continue;
			}
			else
				if (((hdr >> TPOS) & 63) != TBYTECODE) /* not even code, extend bits later */
					ERROR(259, ob, INULL);

			// todo: сюда надо добавить реакцию на внешние колбеки
			if (!ticker--) {
				// время потока вышло, переключим на следующий
				if (R[0] == IFALSE) // no mcp, ignore
					ticker = TICKS;
				else {
				  /* save vm state and enter mcp cont at R0 */
				  word *state, pos = 1;
				  ticker = 0xffffff;
				  bank = 0;
				  acc = acc + 4;
				  R[acc] = (word) ob;
				  state = new (acc);
				  *state = make_header(acc, TTHREAD);
				  state[acc-1] = R[acc];
				  while (pos < acc-1) {
					 state[pos] = R[pos];
					 pos++;
				  }
				  ob = (word *) R[0];
				  R[0] = IFALSE; /* remove mcp cont */
				  /* R3 marks the interop to perform */
				  R[3] = breaked ? ((breaked & 8) ? F(14) : F(10)) : F(1); /* fixme - handle also differnet signals via one handler  */
				  R[4] = (word) state;
				  R[5] = F(breaked);
				  R[6] = IFALSE;
				  acc = 4;
				  breaked = 0;
				}
				continue;
			}

			ip = (unsigned char *) &ob[1];
			break; // goto invoke
		}
		// else
		ERROR(257, ob, INULL); // not callable
	}

invoke: // nargs and regs ready, maybe gc and execute ob

	// если места в буфере не хватает, то мы вызываем GC, а чтобы автоматически подкорректировались
	//  регистры, мы их складываем в память во временный объект-список.
	if (forcegc || (fp >= heap.end - 16*1024)) { // (((word)fp) + 1024*64 >= ((word) memend))
		if (forcegc)
			printf("(forcegc)\n");
		forcegc = 0;
		int p = 0, N = NR;
		// создадим в топе временный объект со значениями всех регистров
		word *regs = new_tuple (N + 1); // hdr r_0 .. r_(NR-1) ob // was: 50 as type
		while (++p <= N) regs[p] = R[p-1];
		regs[p] = (word) ob;
		// выполним сборку мусора
		regs = gc(16*1024 * sizeof(word), regs); // GC, как правило занимает 0-15 ms
		// и восстановим все регистры, уже скорректированные сборщиком
		ob = (word *) regs[p];
		while (--p >= 1) R[p-1] = regs[p];

		fp = regs; // вручную сразу удалим временный объект, это оптимизация
		ip = (unsigned char *) (ob + 1);
	}

	// управляющие команды:
#	define APPLY 20
#	define INTEROP 27
#	define RUN   50
#	define RET   24

	// список команд: работающий транслятор в С смотреть в cgen.scm в (translators)
#	define REFI   1       // refi a, p, t:   Rt = Ra[p], p unsigned (indirect-ref from-reg offset to-reg)
#	define MOVE   9       //
#	define MOV2   5       //
#	define GOTO   2       // jmp a, nargs
#	define JP    16       // JZ, JN, JT, JF
#	define JLQ    8       // jlq ?

#	define LDI   13       // похоже, именно 13я команда не используется, а только 77 (LDN), 141 (LDT), 205 (LDF)
#	define LD    14

#	define JF2   25
//	define GOTO_CODE 18
//	define GOTO_PROC 19
//	define GOTO_CLOS 21


	// примитивы языка:
#	define CONS  51
#	define CAR   52
#	define CDR   53
#	define NCONS 29
#	define NCAR  30
#	define NCDR  31

	//
#	define SET   45

	// АЛУ
#	define EQ    54

#	define CLOCK 61
#	define SYSCALL 63

	// free numbers: 34 (was _connect)

	// ip - счетчик команд (опкод - младшие 6 бит команды, старшие 2 бита - модификатор(если есть) опкода)
	// Rn - регистр машины (R[n])
	// An - регистр, на который ссылается операнд N (записанный в параметре n команды, начиная с 0)
	// todo: добавить в комменты к команде теоретическое количество тактов на операцию
	word *T; // временный регистр, валидный только атомарно в обработчике операции
	while (1) { // todo: добавить условие выхода из цикла
		int op; // operation to execute
		switch ((op = *ip++) & 0x3F) {
		case 0:
			op = (ip[0] << 8) | ip[1]; // big endian
			//super_dispatch: run macro instructions
			// todo: add here JIT
			switch (op) {
			/* AUTOGENERATED INSTRUCTIONS */
			default:
				ERROR(258, F(op), ITRUE);
			}
			goto apply;


		case INTEROP: // interop cont op arg1 arg2
			ob = (word *) R[0];
			R[0] = IFALSE;
			R[3] = A1; R[4] = R[*ip]; R[5] = A2; R[6] = A3;
			acc = 4;
			if (ticker > 10)
				bank = ticker; // deposit remaining ticks for return to thread
			goto apply;

		case GOTO:
			ob = (word *)A0; acc = ip[1];
			goto apply;

		case APPLY: {
			int reg, arity;
			word *lst;
			if (op == APPLY) { /* normal apply: cont=r3, fn=r4, a0=r5, */
				reg = 4; /* include cont */
				arity = 1;
				ob = (word *) R[reg];
				acc -= 3; /* ignore cont, function and stop before last one (the list) */
				while(acc--) { /* move explicitly given arguments down by one to correct positions */
					R[reg] = R[reg+1]; /* copy args down*/
					reg++;
					arity++;
				}
				lst = (word *) R[reg+1];
			}
			else { // _sans_cps apply: func=r3, a0=r4,
				reg = 3; /* include cont */
				arity = 0;
				ob = (word *) R[reg];
				acc -= 2; /* ignore function and stop before last one (the list) */
				while(acc--) { /* move explicitly given arguments down by one to correct positions */
					R[reg] = R[reg+1]; /* copy args down*/
					reg++;
					arity++;
				}
				lst = (word *) R[reg+1];
			}

			while (allocp(lst) && *lst == PAIRHDR) { /* unwind argument list */
				/* FIXME: unwind only up to last register and add limited rewinding to arity check */
				if (reg > 128) { /* dummy handling for now */
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

		case RUN: { // run thunk quantum
			word hdr;
			ob = (word *) A0;
			R[0] = R[3];
			ticker = bank ? bank : fixval(A1);
			bank = 0;
			CHECK(allocp(ob), ob, 50);
			hdr = *ob;
			if (imm_type(hdr) != TTHREAD) {
				// call a thunk with terminal continuation
				R[3] = IHALT; // exit via R0 when the time comes
				acc = 1;
				goto apply;
			}
			// else TTHREAD
			int pos = hdrsize(hdr) - 1;
			word code = ob[pos];
			acc = pos - 3;
			while (--pos)
				R[pos] = ob[pos];
			ip = ((unsigned char *) code) + W;
			break;
		}

		case RET: // return value
			ob = (word *) R[3];
			R[3] = R[ip[0]];
			acc = 1;
			goto apply;

		/************************************************************************************/
		// операции с данными
		case LDI:    // 13,  -> ldi(ldn, ldt, ldf){2bit what} [to]
			A0 = I[op>>6];
			ip += 1; break;
		case LD:
			A1 = F(ip[0]);
			ip += 2; break;

		//
//			   op2: OGOTO(ip[0], ip[1]); /* fixme, these macros are not used in cgen output anymore*/
//			   ob = (word *)R[f]; acc = n; goto apply

		case JP:    // JZ, JN, JT, JF a hi lo
			// was: FIXME, convert this to jump-const <n> comparing to make_immediate(<n>,TCONST),
			//  но я считаю, что надо просто добавить еще одну команду, а эти так и оставить
			if (A0 == I[op>>6])
				ip += (ip[2] << 8) + ip[1]; // little-endian
			ip += 3; break;

		case JLQ: /* jlq a b o, extended jump  */
			if (A0 == A1)
				ip += (ip[3] << 8) + ip[2]; // little-endian
			ip += 4; break;

		// используется в (func ...) в primop.scm
		case JF2: { /* jmp-nargs(>=?) a hi lo */
			int needed = ip[0]; // arity?
			if (acc == needed) {
				if (op & 0x40) /* add empty extra arg list */
					R[acc + 3] = INULL;
			}
			else
			if ((op & 0x40) && acc > needed) {
			         word tail = INULL;  /* todo: no call overflow handling yet */
			         while (acc > needed) {
			        	 tail = (word)new_pair (R[acc + 2], tail);
//			            fp[0] = PAIRHDR;   // todo: make as function for implicitly use fp
//			            fp[1] = R[acc + 2];
//			            fp[2] = tail;
//			            tail = (word) fp;
//			            fp += 3;
			            acc--;
			         }
			         R[acc + 3] = tail;
			}
			else
				ip += (ip[1] << 8) | ip[2];
			ip += 3; break;
		}


		op18: /* goto-code p */
		      ob = (word *) R[*ip]; /* needed in opof gc */
		      acc = ip[1];
		      ip = ((unsigned char *) R[*ip]) + W;
		      goto invoke;
		   op19: { /* goto-proc p */
		      word *this = (word *) R[*ip];
		      R[1] = (word) this;
		      acc = ip[1];
		      ob = (word *) this[1];
		      ip = ((unsigned char *) ob) + W;
		      goto invoke; }
		   op21: { /* goto-clos p */
		      word *this = (word *) R[*ip];
		      R[1] = (word) this;
		      acc = ip[1];
		      this = (word *) this[1];
		      R[2] = (word) this;
		      ob = (word *) this[1];
		      ip = ((unsigned char *) ob) + W;
		      goto invoke; }


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
//
		op47:  /* ref t o r */ /* fixme: deprecate this later */
		      A2 = prim_ref(A0, A1);
		      NEXT(3);
		op48: /* refb t o r */ /* todo: merge with ref, though 0-based  */
		      A2 = prim_refb(A0, fixval(A1));
		      NEXT(3);

		// операции со списками
		case CONS:   // cons a b r:   Rr = (cons Ra Rb)
			A2 = (word)new_pair(A0, A1); // видимо, вызывается очень часто, так как замена на макрос дает +10% к скорости
			ip += 3; break;

		case CAR:    // car a r:
			T = (word *) A0;
			CHECK(pairp(T), T, CAR);
			A1 = T[1];
			ip += 2; break;

		case CDR:    // car a r:
			T = (word *) A0;
			CHECK(pairp(T), T, CDR); // bug? was 52 instead of CDR(53)
			A1 = T[2];
			ip += 2; break;

		// то же самое, но для числовых пар
		case NCONS:  /* ncons a b r */
			A2 = (word)new_npair ((word*)A0, (word*)A1);
			ip += 3; break;

		case NCAR:   /* ncar a rd */
			T = (word*) A0;
			CHECK(allocp(T), T, NCAR);
			A1 = T[1];
			ip += 2; break;

		case NCDR:   /* ncdr a r */
			T = (word*) A0;
			CHECK(allocp(T), T, NCDR);
			A1 = T[2];
			ip += 2; break;


		case SET: // set t o v r
			A3 = prim_set(A0, A1, A2);
			ip += 4; break;


		// АЛУ (арифметическо-логическое устройство)
	   op38: { /* fx+ a b r o, types prechecked, signs ignored, assume fixnumbits+1 fits to machine word */
		  word res = fixval(A0) + fixval(A1);
		  word low = res & FMAX;
		  A3 = (res & (1 << FBITS)) ? ITRUE : IFALSE;
		  A2 = F(low);
		  NEXT(4); }
	   op39: { /* fx* a b l h */
		  uint64_t res = ((uint64_t) ((uint64_t) fixval(R[*ip])) * ((uint64_t) fixval(A1)));
		  A2 = F(((word)(res&FMAX)));
		  A3 = F(((word)(res>>FBITS)&FMAX));
		  NEXT(4); }
	   op40: { /* fx- a b r u, args prechecked, signs ignored */
		  word r = (fixval(A0)|(1<<FBITS)) - fixval(A1);
		  A3 = (r & (1<<FBITS)) ? IFALSE : ITRUE;
		  A2 = F(r&FMAX);
		  NEXT(4); }

		case EQ: // eq a b r
			A2 = TRUEFALSE(A0 == A1);
			ip += 3; break;

	   op55: { /* band a b r, prechecked */
	      word a = R[*ip];
	      word b = A1;
	      A2 = a & b;
	      NEXT(3); }
	   op56: { /* bor a b r, prechecked */
	      word a = R[*ip];
	      word b = A1;
	      A2 = a | b;
	      NEXT(3); }
	   op57: { /* bxor a b r, prechecked */
	      word a = R[*ip];
	      word b = A1;
	      A2 = a ^ (b & (FMAX << IPOS)); /* inherit a's type info */
	      NEXT(3); }
	   op58: { /* fx>> a b hi lo */
	      uint64_t r = ((uint64_t) fixval(A0)) << (FBITS - fixval(A1));
	      A2 = F(r>>FBITS);
	      A3 = F(r&FMAX);
	      NEXT(4); }
	   op59: { /* fx<< a b hi lo */
	      uint64_t res = (uint64_t) fixval(R[*ip]) << fixval(A1);
	      A2 = F(res>>FBITS);
	      A3 = F(res&FMAX);
	      NEXT(4); }
	   op26: { /* fx/ ah al b qh ql r, b != 0, int32 / int16 -> int32, as fixnums */
	      uint64_t a = (((uint64_t) fixval(A0))<<FBITS) | fixval(A1);
	      word b = fixval(A2);
	      uint64_t q;
	      q = a / b;
	      A3 = F(q>>FBITS);
	      A4 = F(q&FMAX);
	      A5 = F(a - q*b);
	      NEXT(6); }



		case 3: goto op3; case 4: goto op4;
		case 6: goto op6; case 7: goto op7;
		case 15: goto op15;
		case 18: goto op18; case 19: goto op19; case 21: goto op21;
		case 22: goto op22; case 23: goto op23; case 26: goto op26;
		case 28: goto op28; case 32: goto op32;
		case 35: goto op35; case 36: goto op36; case 37: goto op37; case 38: goto op38; case 39: goto op39;
		case 40: goto op40; case 41: goto op41; case 42: goto op42; case 43: goto op43; case 44: goto op44;
		case 46: goto op46; case 47: goto op47; case 48: goto op48; case 49: goto op49;
		case 55: goto op55; case 56: goto op56; case 57: goto op57;
		case 58: goto op58; case 59: goto op59; case 60: goto op60;

//		// ошибки!
		case 17: /* arity error */
			ERROR(17, ob, F(acc));
			// неиспользуемые коды (историческое наследие, при желании можно реюзать)
		case 33:
			ERROR(33, IFALSE, IFALSE);


		// мутатор
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
			assert (allocp(pair) && pair[0] == PAIRHDR);
			word value = (word)A1;
			pair[1] = value;

			A2 = A0;
			ip += 3; break;
		}
		case 12: { // (set-cdr! pair value)
			word *pair = (word *)A0;
			assert (allocp(pair) && pair[0] == PAIRHDR);
			word value = (word)A1;
			pair[2] = value;

			A2 = A0;
			ip += 3; break;
		}

//		   op22: { /* cast o t r */
//		      word *ob = (word *) R[*ip];
//		      word type = fixval(A1) & 63;
//		      A2 = prim_cast(ob, type);
//		      NEXT(3); }
//


		// ff функции
		   op23: { /* mkt t s f1 .. fs r */
			  word t = *ip++;
			  word s = *ip++ + 1; /* the argument is n-1 to allow making a 256-tuple with 255, and avoid 0-tuples */
			  word *ob = new (s+1), p = 0; // s fields + header
			  *ob = make_header(s+1, t);
			  while (p < s) {
				 ob[p+1] = R[ip[p]];
				 p++;
			  }
			  R[ip[p]] = (word) ob;
			  NEXT(s+1); }
		   op41: { /* red? node r (has highest type bit?) */
			  word *node = (word *) R[*ip];
			  A1 = TRUEFALSE(allocp(node) && ((*node)&(FFRED<<TPOS)));
			  NEXT(2); }
		   op42: /* mkblack l k v r t */
			  A4 = prim_mkff(TFF,A0,A1,A2,A3);
			  NEXT(5);
		   op43: /* mkred l k v r t */
			  A4 = prim_mkff(TFF|FFRED,A0,A1,A2,A3);
			  NEXT(5);
		   op46: { /* fftoggle - toggle node color */
				 word *node = (word *) R[*ip];
				 word *newobj, h;
				 CHECK(allocp(node), node, 46);
				 newobj = fp; // todo: make as function with using fp
				 h = *node++;
				 A1 = (word) newobj;
				 *newobj++ = (h^(FFRED<<TPOS));
				 switch (hdrsize(h)) {
					case 5:  *newobj++ = *node++;
					case 4:  *newobj++ = *node++;
					default: *newobj++ = *node++;
							 *newobj++ = *node++; }
				 fp = newobj;
				 NEXT(2); }
		   op49: { /* withff node l k v r */
			  word hdr, *ob = (word *) R[*ip];
			  hdr = *ob++;
			  A2 = *ob++; /* key */
			  A3 = *ob++; /* value */
			  switch(hdrsize(hdr)) {
				 case 3: A1 = A4 = IEMPTY; break;
				 case 4:
					if (hdr & (1 << TPOS)) { /* has right? */
					   A1 = IEMPTY; A4 = *ob;
					} else {
					   A1 = *ob; A4 = IEMPTY;
					}
					break;
				 default:
					A1 = *ob++;
					A4 = *ob;
			  }
			  NEXT(5); }


		case CLOCK: { // clock <secs> <ticks>
			word *ob = new (6); // space for 32-bit bignum - [NUM hi [NUM lo null]]
			ob[0] = ob[3] = NUMHDR;
			A0 = (word) (ob + 3);
			ob[2] = INULL;
			ob[5] = (word) ob;

			if (seccompp) {
				unsigned long secs = seccomp_time / 1000;
				A1 = F(seccomp_time - (secs * 1000));
				ob[1] = F(secs >> FBITS);
				ob[4] = F(secs & FMAX);
				seccomp_time += ((seccomp_time + 10) > seccomp_time) ? 10 : 0; /* virtual 10ms passes */
			}
			else {
				struct timeval tp;
				gettimeofday(&tp, NULL);
				A1 = F(tp.tv_usec / 1000);
				ob[1] = F(tp.tv_sec >> FBITS);
				ob[4] = F(tp.tv_sec & FMAX);
			}
			ip += 2; break;
		}

			// этот case должен остаться тут - как последний из кейсов
			//  todo: переименовать в компиляторе sys-prim на syscall (?)
			// http://docs.cs.up.ac.za/programming/asm/derick_tut/syscalls.html
		case SYSCALL: { // sys-call (was sys-prim) op arg1 arg2 arg3  r1
			// linux syscall list: http://blog.rchapman.org/post/36801038863/linux-system-call-table-for-x86-64
			//                     http://www.x86-64.org/documentation/abi.pdf
			word op = fixval(A0);
			word a = A1, b = A2, c = A3;
			word result = IFALSE; // default returned value is #false

//			printf("SYSCALL(%d, %d, %d, %d)\n", op, a, b, c);

			switch (op) {
				// todo: сюда надо перенести все prim_sys операции, что зависят от глобальных переменных
				//  остальное можно спокойно оформлять отдельными функциями

				// READ
				case 1005: { /* fread fd max -> obj | eof | F (read error) | T (would block) */
					word fd = fixval(a);  // file descriptor
					word max = fixval(b); // buffer capacity

					word *res;
					int n, nwords = (max/W) + 2;
					res = new (nwords);

#					ifndef STANDALONE
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
#					endif//STANDALONE
					{
#ifdef _WIN32
			            if (!_isatty(fd) || _kbhit()) { /* we don't get hit by kb in pipe */
			               n = read(fd, ((char *) res) + W, max);
			            } else {
			               n = -1;
			               errno = EAGAIN;
			            }
#else
						n = read(fd, ((char *) res) + W, max); // from <unistd.h>
#endif
					}
					if (n > 0) { // got some bytes
						word read_nwords = (n/W) + ((n%W) ? 2 : 1);
						int pads = (read_nwords-1)*W - n;
						fp = res + read_nwords;
						*res = make_raw_header(read_nwords, TBVEC, pads);
						result = (word)res;
						break;
					}
					fp = res; // иначе удалим выделенную под результат переменную

					if (n == 0)
						result = IEOF;
					else // EAGAIN: Resource temporarily unavailable (may be the same value as EWOULDBLOCK) (POSIX.1)
						result = TRUEFALSE(errno == EAGAIN || errno == EWOULDBLOCK);
					break;
				}
				
				// WRITE
				case 1000: { /* 0 fsend fd buff len r → n if wrote n, 0 if busy, False if error (argument or write) */
					int fd = fixval(a);
					word *buff = (word *) b;

					result = IFALSE;

					int wrote, size, len = fixval(c);
					if (immediatep(buff))
						break;
					size = (hdrsize(*buff)-1) * W;
					if (len > size)
						break;

#					ifndef STANDALONE
					if (fd == 1) // stdout wrote to the fo
						wrote = fifo_puts(fo, ((char *)buff)+W, len);
					else
#					endif//STANDALONE
						wrote = write(fd, ((char *)buff)+W, len);

					if (wrote > 0)
						result = F(wrote);
					else
						result = ZEROFALSE(errno == EAGAIN || errno == EWOULDBLOCK);
					break;
				}


			// CONNECT // todo: change this!
			case 1042: { // connect(host, port) // todo: check this
				word* host = (word*)a;
				if (!allocp(host))  // bad host type
					break;
				unsigned char *ip = ((unsigned char *) host) + W;

				unsigned long ipfull;
				struct sockaddr_in addr;
				int port = fixval(b);

				int sock;
				if ((sock = socket(PF_INET, SOCK_STREAM, 0)) == -1)
					break;
				addr.sin_family = AF_INET;
				addr.sin_port = htons(port);
				addr.sin_addr.s_addr = (in_addr_t) host[1];
				ipfull = (ip[0]<<24) | (ip[1]<<16) | (ip[2]<<8) | ip[3];
				addr.sin_addr.s_addr = htonl(ipfull);
				if (connect(sock, (struct sockaddr *) &addr, sizeof(struct sockaddr_in)) < 0) {
					close(sock);
					break;
				}
				set_blocking(sock, 0);
				result = F(sock);
				break;
			}


				// EXIT
				case 1006:
					free(heap.begin); // освободим занятую память
#					ifndef STANDALONE
					// подождем, пока освободится место в консоли
					while (fifo_full(fo)) pthread_yield();
					fifo_put(fo, EOF); // и положим туда EOF

					pthread_exit((void*)fixval(a));
#					else
					exit(fixval(a));
#					endif//STANDALONE
					break;

				case 1007: /* set memory limit (in mb) */ // todo: переделать на другой номер
					result = max_heap_size;
					max_heap_size = fixval(a);
					break;
				case 1008: /* get machine word size (in bytes) */ // todo: переделать на другой номер
					  result = F(W);
					  break;
				case 1009: /* get memory limit (in mb) */ // todo: переделать на другой номер
					  result = F(max_heap_size);
					  break;

				case 1022:
					result = (ticker & FMAX);
					ticker = fixval(a);
					break;


				// -=( pinvoke )=-------------------------------------------------
#				ifndef JAVASCRIPT
				//   а тут у нас реализация pinvoke механизма. пример в opengl.scm
				case 1030: { // dlopen
					word *filename = (word*)a;
					int mode = fixval(b);

					if (!(allocp(filename) && hdrtype(*filename) == TSTRING))
			        	 break;

					void* module = dlopen((char*) (filename + 1), mode);
					if (module) // тут сразу создаем длинный port, так как адреса скорее всего более 24 бит
						result = (word)new_port((word)module);
					//						if (function <= 0xFFFFFF)
					//							result = make_immediate(function, TPORT);
//					if (module) {
//						result = (word)fp; // todo: разобраться тут правильно с размерами типов
//						fp[0] = make_raw_header(2, THANDLE, 0); //was: sizeof(void*) % sizeof(word)); // sizeof(void*) % sizeof(word) as padding
//						fp[1] = (word)module;
//						fp += 2;
//					}
					break;
				}
				case 1031: { // dlsym
					word* A = (word*)a;

					assert (hdrtype(A[0]) == TPORT);
					void* module = (void*) A[1];
					word* name = (word*)b;

					// http://www.symantec.com/connect/articles/dynamic-linking-linux-and-windows-part-one
					if (!(immediatep(name) || hdrtype(*name) == TSTRING))
						break;

					word function = (word)dlsym(module, immediatep(name)
							? (char*) imm_val((word)name)
							: (char*) (name + 1));
					if (function) // тут сразу создаем длинный port, так как адреса скорее всего более 24 бит
						result = (word)new_port(function);
					break;
				}
				// временный тестовый вызов
				case 1033: { // temp
					forcegc = 1;
/*					printf("opengl version: %s\n", glGetString(GL_VERSION));
					int glVersion[2] = {-1, -1}; // Set some default values for the version
					glGetIntegerv(GL_MAJOR_VERSION, &glVersion[0]); // Get back the OpenGL MAJOR version we are using
					glGetIntegerv(GL_MINOR_VERSION, &glVersion[1]); // Get back the OpenGL MAJOR version we are using

					GLint status;*/
//					PFNGLGETSHADERIVPROC  glGetShaderiv  = (PFNGLGETSHADERIVPROC)wglGetProcAddress("glGetShaderiv");
//					glGetShaderiv(3, GL_COMPILE_STATUS, &status);

//					PFNGLGETPROGRAMIVPROC glGetProgramiv = (PFNGLGETSHADERIVPROC)wglGetProcAddress("glGetProgramiv");
//					glGetProgramiv(1, GL_LINK_STATUS, &status);

					result = INULL;
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

				// вызвать библиотечную функцию
				case 1032: { // pinvoke
					// http://byteworm.com/2010/10/12/container/ (lambdas in c)
					unsigned int call(int convention, void* function, int args[], int count) {
						// todo: ограничиться количеством функций поменьше
						//	а можно сделать все в одной switch:
						// i += 5 * (returntype >> 6); // 5 - количество поддерживаемых функций
						// todo: а можно лямбдой оформить и засунуть эту лябмду в функцию еще в get-proc-address
						// todo: проанализировать частоту количества аргументов и переделать все в
						//   бинарный if

/*						__stdcall // gcc style for lambdas in pure C
						int (*stdcall[])(char*) = {
								({ int $(char *str){ printf("Test: %s\n", str); } $; })
						};*/

						#define CALL(conv) \
							switch (count) {\
							case  0: return ((conv unsigned int (*) ())\
											function) ();\
							case  1: return ((conv unsigned int (*) (int))\
											function) (args[0]);\
							case  2: return ((conv unsigned int (*) (int, int)) function)\
											(args[0], args[1]);\
							case  3: return ((conv unsigned int (*) (int, int, int)) function)\
											(args[0], args[1], args[2]);\
							case  4: return ((conv unsigned int (*) (int, int, int, int)) function)\
											(args[0], args[1], args[2], args[3]);\
							case  5: return ((conv unsigned int (*) (int, int, int, int, int)) function)\
											(args[0], args[1], args[2], args[3], args[4]);\
							case  6: return ((conv unsigned int (*) (int, int, int, int, int, int)) function)\
											(args[0], args[1], args[2], args[3], args[4], args[5]);\
							case  7: return ((conv unsigned int (*) (int, int, int, int, int, int, int)) function)\
											(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);\
							case  8: return ((conv unsigned int (*) (int, int, int, int, int, int, int, int)) function)\
											(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);\
							case  9: return ((conv unsigned int (*) (int, int, int, int, int, int, int, int, int)) function)\
											(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8]);\
							case 10: return ((conv unsigned int (*) (int, int, int, int, int, int, int, int, int, int)) function)\
											(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9]);\
							case 11: return ((conv unsigned int (*) (int, int, int, int, int, int, int, int, int, int, int)) function)\
											(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10]);\
							case 12: return ((conv unsigned int (*) (int, int, int, int, int, int, int, int, int, int, int, int))\
											function) (args[0], args[1], args[2], args[3],   \
													   args[4], args[5], args[6], args[7],   \
													   args[8], args[9], args[10], args[11]);\
							case 18: return ((conv unsigned int (*) (int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int))\
											function) (args[0], args[1], args[2], args[3],   \
													   args[4], args[5], args[6], args[7],   \
													   args[8], args[9], args[10], args[11], \
													   args[12], args[13], args[14], args[15], args[16], args[17]);\
							default: fprintf(stderr, "Too match parameters for pinvoke function: %d", count);\
								break;\
							}
#ifdef __linux__
						// чуть-чуть ускоримся для линукса
						CALL(__cdecl);
#else
//						*(char*)0 = 123;
						switch (convention >> 6) {
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
						return 0;
					}
					int from_fix(word* arg) {
						int value = fixval((unsigned int)arg);
						// этот кусок временный - потом переделать в трюк
						// алгоритмические трюки:
						// x = (x xor t) - t, где t - y >>(s) 31 (все 1, или все 0)
						if ((unsigned int)arg & 0x80)
							return -value;
						return value;
					}
					int from_int(word* arg) {
						// так как в стек мы все равно большое сложить не сможем, то возьмем только то, что влазит (первые два члена)
						assert (immediatep(arg[1]));
						assert (allocp(arg[2]));

						return (arg[1] >> 8) | ((((word*)arg[2])[1] >> 8) << 24);
					}
					float from_int_to_float(word* arg) {
						// читаем длинное число в float формат
						assert (immediatep(arg[1]));
						float f = (unsigned)arg[1] >> 8;
						float mul = 0x1000000; // 1 << 24
						while (allocp(arg[2])) {
							arg = (word*)arg[2];
							f += (unsigned)(arg[1] >> 8) * mul;
							mul *= 0x1000000;
						}
						assert (arg[2] == INULL);

						return f;
					}
					float from_rational(word* arg) {
						word* pa = (word*)arg[1];
						word* pb = (word*)arg[2];

						float a, b;
						if (immediatep(pa))
							a = from_fix(pa);
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
							b = from_fix(pb);
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
					word* A = (word*)a;
					word* B = (word*)b;
					word* C = (word*)c;

					assert (/*hdrtype(a) == TPORT || */hdrtype(A[0]) == TPORT);
					assert ((word)B == INULL || hdrtype(B[0]) == TPAIR);
					assert ((word)C != INULL && hdrtype(C[0]) == TPAIR);
					// C[1] = return-type
					// C[2] = argument-types

					// todo: добавить разные конвенции вызова: __ccall, __stdcall, __fastcall

					int args[18]; // пока только 12 аргумента максимум (18 - специально для gluLookAt)
					void *function = (void*) (A[1]);
					assert (function != 0);
					int returntype = imm_val (C[1]);

					unsigned int got;   // результат вызова функции
					int i = 0;   // количество аргументов
					word* p = (word*)B; // сами аргументы
					word* t = (word*)C[2];
					while ((word)p != INULL) { // пока есть аргументы
						assert (hdrtype(*p) == TPAIR); // assert list
						assert (hdrtype(*t) == TPAIR); // assert list

						int type = imm_val (t[1]);
						word* arg = (word*) p[1]; // car

/*						// todo: add argument overriding as PAIR as argument value
						if (hdrtype(p[1]) == TPAIR) {
							type = imm_val (((word*)p[1])[1]);
							arg = ((word*)p[1])[2];
						}*/

						// destination type
						switch (type) {
						case TFIX:
						case TINT:
							if (immediatep(arg))
								args[i] = from_fix(arg);
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
							case TBVEC:
								args[i] = arg[1];
								break;
							default:
								args[i] = INULL; // todo: error
							}
							break;

						case TFLOAT:
							if (immediatep(arg))
								*(float*)&args[i] = (float)from_fix(arg);
							else
							switch (hdrtype(arg[0])) {
							case TINT: // source type
								*(float*)&args[i] = +(float)from_int(arg);
								break;
							case TINTN:
								*(float*)&args[i] = -(float)from_int(arg);
								break;
							case TRATIONAL:
								*(float*)&args[i] = (float)from_rational(arg);
								break;
							default:
								*(float*)&args[i] = (float)0.0; // todo: error, return infinity, maybe, or NaN
							}
							break;
						case TDOUBLE:
							if (immediatep(arg))
								*(double*)&args[i++] = (double)from_fix(arg);
							else
							switch (hdrtype(arg[0])) {
							case TINT: // source type
								*(double*)&args[i++] = +(double)from_int(arg);
								break;
							case TINTN:
								*(double*)&args[i++] = -(double)from_int(arg);
								break;
							case TRATIONAL:
								*(double*)&args[i++] = (double)from_rational(arg);
								break;
							default:
								*(double*)&args[i++] = (double)0.0; // todo: error, same as float
							}
							break;


						case TBVEC:
						case TSTRING:
							if ((word)arg == INULL)
								args[i] = (word) (void*)0;
							else
							switch (hdrtype(arg[0])) {
							case TBVEC:
							case TSTRING:
//							case THANDLE:
//							case TCONST:
								// in arg[0] size got size of string
								args[i] = (int)(&arg[1]);
								break;
							default:
								args[i] = INULL; // todo: error
							}
							break;
						case TPORT:
							if ((word)arg == INULL)
								args[i] = (word) (void*)0;
							else
							switch (hdrtype(arg[0])) {
							case TPORT:
								args[i] = (int)(arg[1]);
								break;
							default:
								args[i] = INULL; // todo: error
							}
							break;
						case TTUPLE:
							if ((word)arg == INULL)
								args[i] = (word) (void*)0;
							else
							switch (hdrtype(arg[0])) {
/*							case TCONST:
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
	//								int j;
	//								for (j = 1; j < size; j++)
								}
								break;
							default:
								args[i] = INULL; // todo: error
							}
							break;
						}

						p = (word*)p[2]; // cdr
						t = (word*)t[2]; // cdr
						i++;
					}
					assert ((word)t == INULL); // количество аргументов совпало!

					got = call(returntype, function, args, i);

					// todo: добавить type-void который возращает просто INULL
					switch (returntype & 0x3F) {
						case TINT:
							if (got > 0xFFFFFF) {
								// прошу внимания!
								//  в числовой паре надо сначала положить старшую часть, и только потом младшую!
								word* hi = fp; fp += 3; // high 8 bits
								hi[0] = NUMHDR;
								hi[1] = make_immediate(got >> 24, 0); // type-fx+
								hi[2] = INULL;
								word* lo = fp; fp += 3; // low 24 bits
								lo[0] = NUMHDR;
								lo[1] = make_immediate(got & 0xFFFFFF, 0); // type-fx+
								lo[2] = (word)hi;

								result = (word)lo;
								break;
							}
							// иначе вернем type-fx+
						case 0: // type-fix+ - если я уверен, что число заведомо меньше 0x00FFFFFF!
							result = F(got);
							break;
						case TPORT:
							// todo: uncomment this
//							if (got > 0xFFFFFF)
								result = new_port(got);
//							else
//								result = make_immediate(got, TPORT);
							break;
						// todo: TRATIONAL

						case TSTRING:
							if (got == 0)
								result = INULL;
							else {
								int len = lenn((char*)got, FMAX+1);
								result = (word)mkbvec(len, TSTRING);
								//if (len == FMAX+1) return INULL; /* can't touch this */
								bytecopy((char*)got, ((char*)result)+W, len);
							}
							break;
						case TVOID:
						default:
							result = INULL;
					}

					break; // case 32
				}
#				endif//STANDALONE
				default:
					if (op >= 1000)
						result = prim_sys(op, a, b, c);
					else
						result = syscall(op, a, b, c);
					break;
				}

				A4 = result;
				ip += 5; break;
			}
		}
		main_dispatch: continue; // временная замена вызову "break" в свиче, пока не закончу рефакторинг

   op3: OCLOSE(TCLOS); NEXT(0);
   op4: OCLOSE(TPROC); NEXT(0);
   op6: CLOSE1(TCLOS); NEXT(0);
   op7: CLOSE1(TPROC); NEXT(0);

   op15: { /* type o r <- actually sixtet */
      word x = R[*ip++];
      if (allocp(x)) x = V(x);
      R[*ip++] = F((x>>TPOS)&63);
      NEXT(0); }
   op22: { /* cast o t r */
      word *ob = (word *) R[*ip];
      word type = fixval(A1) & 63;
      A2 = prim_cast(ob, type);
      NEXT(3); }

   op28: { /* sizeb obj to */
      word* ob = (word*)R[*ip];
      if (immediatep(ob)) {
         A1 = IFALSE;
      } else {
         word hdr = *ob;
         A1 = (rawp(hdr)) ? F((hdrsize(hdr)-1)*W - padsize(hdr)) : IFALSE;
      }
      NEXT(2); }
   op32: { /* bind tuple <n> <r0> .. <rn> */ // todo: move to sys-prim?
      word *tuple = (word *) R[*ip++];
      word hdr, pos = 1, n = *ip++;
      CHECK(allocp(tuple), tuple, 32);
      hdr = *tuple;
      CHECK(!(rawp(hdr) || hdrsize(hdr)-1 != n), tuple, 32);
      while(n--) { R[*ip++] = tuple[pos++]; }
      NEXT(0); }
   op35: { /* listuple type size lst to */
      word type = fixval(R[*ip]);
      word size = fixval(A1);
      word *lst = (word *) A2;
      word *ob = new (size+1);
      A3 = (word) ob;
      *ob++ = make_header(size+1, type);
      while(size--) {
         CHECK((allocp(lst) && *lst == PAIRHDR), lst, 35);
         *ob++ = lst[1];
         lst = (word *) lst[2];
      }
      NEXT(4); }
   op36: { /* size o r */
      word *ob = (word *) R[ip[0]];
      R[ip[1]] = (immediatep(ob)) ? IFALSE : F(hdrsize(*ob)-1);
      NEXT(2); }
   op37: { /* ms r */
#ifndef _WIN32
      if (!seccompp)
         usleep(fixval(A0)*1000);
#else
      Sleep(fixval(A0));
#endif
      A1 = TRUEFALSE(errno == EINTR);
      NEXT(2); }

   op44: /* less a b r */
      A2 = prim_less(A0, A1);
      NEXT(3);

   op60: /* lraw lst type dir r (fixme, alloc amount testing compiler pass not in place yet!) */
      A3 = prim_lraw(A0, fixval(A1), A2);
      NEXT(4);

	} // switch(1)

//   // todo: add here JIT
//super_dispatch: /* run macro instructions */
//   switch(op) {
///* AUTOGENERATED INSTRUCTIONS */
//      default:
//         ERROR(258, F(op), ITRUE);
//   }
//   goto apply;

invoke_mcp: /* R4-R6 set, set R3=cont and R4=interop and call mcp */
   ob = (word *) R[0];
   R[0] = IFALSE;
   R[3] = F(3);
   if (allocp(ob)) {
      acc = 4;
      goto apply;
   }
   return (void*)1; /* no mcp to handle error (fail in it?), so nonzero exit  */
}


// ======================================================================
//       загрузчик скомпилированного образа и его десериализатор
//

// fasl decoding
/* count number of objects and measure heap size */
static __tlocal__
unsigned char *hp;       /* heap pointer when loading heap */

static
word get_nat() {
   word result = 0;
   word newobj, i;
   do {
      i = *hp++;
      newobj = result << 7;
      if (result != (newobj >> 7))
    	  exit(9); // overflow kills
      result = newobj + (i & 127);
   } while (i & 128);
   return result;
}

static
word *get_field(word *ptrs, int pos) {
   if (0 == *hp) {
      unsigned char type;
      word val;
      hp++;
      type = *hp++;
      val = make_immediate(get_nat(), type);
      *fp++ = val;
   } else {
      word diff = get_nat();
      *fp++ = ptrs[pos-diff];
   }
   return fp;
}

static
word *deserialize(word *ptrs, int me)
{
   int type, size;
   if (ptrs != NULL) ptrs[me] = (word) fp;
   switch (*hp++) { /* todo: adding type information here would reduce fasl and executable size */
      case 1: {
         type = *hp++;
         size = get_nat();
         *fp++ = make_header(size+1, type); /* +1 to include header in size */
         while (size--) { fp = get_field(ptrs, me); }
         break; }
      case 2: {
         int bytes, pads;
         unsigned char *wp;
         type = *hp++ & 31; /* low 5 bits, the others are pads */
         bytes = get_nat();
         size = ((bytes % W) == 0) ? (bytes/W)+1 : (bytes/W) + 2;
         pads = (size-1)*W - bytes;
         *fp++ = make_raw_header(size, type, pads);
         wp = (unsigned char *) fp;
         while (bytes--) { *wp++ = *hp++; };
         while (pads--) { *wp++ = 0; };
         fp = (word *) wp;
         break; }
      default: puts("bad object in heap");
      exit(42);
   }
   return fp;
}

// функция подсчета количества объектов в загружаемом образе
static
word decode_word(unsigned char** _hp) {
	unsigned char* hp = *_hp;
	word result = 0;
	word newobj, i;
	do {
		i = *hp++;
		newobj = result << 7;
		assert (result == (newobj >> 7));
//			if (result != (newobj >> 7)) exit(9); // overflow kills
		result = newobj + (i & 127);
	}
	while (i & 128);
	*_hp = hp;
	return result;
}


static
int count_fasl_objects(word *words, unsigned char *lang) {
	unsigned char* hp;

	int n = 0;
	hp = lang;

	int allocated = 0;
	while (*hp != 0) {
		switch (*hp++) {
		case 1: {
			hp++; ++allocated;
			int size = decode_word(&hp);
			while (size--) {
				if (*hp == 0) {
					hp++; ++allocated;
					hp++;
				}
				decode_word(&hp); // simply skip word
			}
			break;
		}
		case 2: {
			hp++;
			int size = decode_word(&hp);
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

// ----------------------------------------------------------------
// -=( virtual machine functions )=--------------------------------
//
// this is NOT thread safe function
#ifndef STANDALONE
OL*
vm_new(unsigned char* language, void (*release)(void*))
#else
#ifndef NOLANGUAGE
extern unsigned char* language;
#endif
int main(int argc, char** argv)
#endif//STANDALONE
{
	// создадим виртуальную машину
	OL *handle = malloc(sizeof(OL));
	memset(handle, 0x0, sizeof(OL));

	// подготовим очереди в/в
	//fifo_clear(&handle->i);
	//fifo_clear(&handle->o); (не надо, так как хватает memset вверху)

	// выделим память машине:
	max_heap_size = (W == 4) ? 4096 : 65535; // can be set at runtime
	int required_memory_size = (INITCELLS + FMAX + MEMPAD) * sizeof(word);
	heap.begin = (word*) malloc(required_memory_size); // at least one argument string always fits
	if (!heap.begin) {
		fprintf(stderr, "Failed to allocate %d bytes for vm memory\n", required_memory_size);
		goto failed;
	}
	heap.end = heap.begin + FMAX + INITCELLS - MEMPAD;
	heap.genstart = heap.begin;

	// подготовим в памяти машины параметры командной строки:

	// create '("some string" . NIL) as parameter for the start lambda
	// а вообще, от этого блока надо избавится.
	//  но пока оставлю как пример того, как можно предварительно
	//  загрузить в память аргументы перед вызовом образа
	// по совместительству, это еще и корневой объект
	fp = heap.begin;
	word oargs;
	{
		oargs = (word*)INULL;
#ifndef TEST_GC2

#ifndef STANDALONE
		char* filename = "#";
		char *pos = filename;

		int len = 0;
		while (*pos++) len++;

		oargs = new_pair (new_string (len, filename), oargs);
#else
		for (int i = argc - 1; i > 0; i--)
			oargs = new_pair(new_string(strlen(argv[i]), argv[i]), oargs);
#endif
#else
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

		gc(0, e);
#endif
	}

#ifdef NOLANGUAGE
	unsigned char* language;

	// загрузим образ из файла
	if (argc < 2) {
		printf("usage: olvm binary_image\n");
		exit(1);
	}
	else
	{
		struct stat st;
		int pos = 0;
		if (stat(argv[1], &st)) exit(1);

		char* ptr = (char*)malloc(st.st_size);
		if (ptr == NULL) exit(2);
		FILE *fd = fopen(argv[1], "rb");
		if (!fd) exit(3);
		while (pos < st.st_size) {
			int n = fread(ptr+pos, 1, st.st_size-pos, fd);
			if (n < 0) exit(4);
			pos += n;
		}
		fclose(fd);

		language = (unsigned char*) ptr;
	}
#endif

	// а теперь поработаем со сериализованным образом:
	word nwords = 0;
	word nobjs = count_fasl_objects(&nwords, language); // подсчет количества слов и объектов в образе

	//	oargs = gc(nwords + (128*1024), oargs); // get enough space to load the heap without triggering gc

	// Десериализация загруженного образа в объекты
	word* ptrs = fp;
	fp += nobjs + 1;

	hp = language; // десериализатор использует hp как итератор по образу

	int pos;
	for (pos = 0; pos < nobjs; pos++) {
		if (fp >= heap.end) {
			fprintf(stderr, "gc needed during heap import\n");
			goto failed;
		}
		deserialize(ptrs, pos);
	}
	ptrs[0] = make_raw_header(nobjs + 1, 0, 0);

	// все, программа в памяти, можно освобождать исходник
#ifndef STANDALONE
	if (release)
		release(language);
#else
#ifdef NOLANGUAGE
	free(language);
#endif
#endif


	struct args args; // аргументы для запуска
	args.vm = handle; // виртуальной машины OL

	// а это инициализационные аргументы для памяти виртуальной машины
	args.heap.begin    = heap.begin;
	args.heap.end      = heap.end;
	args.heap.genstart = heap.genstart;
	args.max_heap_size = max_heap_size; // max heap size in MB
	args.fp = fp;
	args.userdata      = (word*) oargs;

#ifndef STANDALONE
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
	runtime(&args);
#endif//STANDALONE

failed:
	free(heap.begin);
	free(handle);

#ifndef STANDALONE
	return NULL;
#else
	return 0;
#endif
}

#ifndef STANDALONE

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
#endif//STANDALONE
