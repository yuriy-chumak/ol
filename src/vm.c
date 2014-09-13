// максимальное число для элементарной математики: 16777215
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
#include "vm.h"

// todo: проверить, что все работает в 64-битном коде
// todo: переименовать tuple в array. array же неизменяемый, все равно.
//  а изменяемые у нас ветора

// http://joeq.sourceforge.net/about/other_os_java.html
// call/cc - http://fprog.ru/lib/ferguson-dwight-call-cc-patterns/

// компилятор поддерживает только несколько специальных форм:
//	lambda, quote, rlambda, receive, _branch, _define, _case-lambda, values (смотреть env.scm)
//	все остальное - макросами (?)

#include <stddef.h>
//nclude <malloc.h>
#include <signal.h>
#include <unistd.h>
#include <assert.h>
#include <dirent.h>
#include <string.h>

#include <errno.h>
#include <time.h>
#include <inttypes.h>
#include <fcntl.h>
//nclude <dlfcn.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>

// thread local storage modifier for virtual machine private variables -
// виртуальная машина у нас работает в отдельном потоке, соответственно ее
// локальные переменные можно держать в TLS, а не в какой-то VM структуре
#ifndef thread_local
# if __STDC_VERSION__ >= 201112 && !defined __STDC_NO_THREADS__
#  define thread_local _Thread_local
# elif defined _WIN32 && ( \
       defined _MSC_VER || \
       defined __ICL || \
       defined __DMC__ || \
       defined __BORLANDC__ )
#  define thread_local __declspec(thread)
/* note that ICC (linux) and Clang are covered by __GNUC__ */
# elif defined __GNUC__ || \
       defined __SUNPRO_C || \
       defined __xlC__
#  define thread_local __thread
# else
#  error "Cannot define thread_local"
# endif
#endif

/*** Portability Issues ***/

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <ws2tcpip.h>
#include <conio.h>
#include <windows.h>
//#include <GL/gl.h> // temp
//#include "glext.h"
typedef unsigned long in_addr_t;
#define EWOULDBLOCK WSAEWOULDBLOCK
#undef ERROR // due to macro redefinition
#else
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/wait.h>
#ifndef O_BINARY
#define O_BINARY 0
#endif
#endif

#ifdef __gnu_linux__
#ifndef NO_SECCOMP
#include <sys/prctl.h>
#include <sys/syscall.h>
/* normal exit() segfaults in seccomp */
#define EXIT(n) syscall(__NR_exit, n); exit(n)
#else
#define EXIT(n) exit(n)
#endif
#else
#define EXIT(n) exit(n)
#endif

#define STATIC static __inline__

typedef uintptr_t word;

#ifdef _LP64
typedef int64_t   wdiff;
#else
typedef int32_t   wdiff;
#endif

// -=( fifo )=------------------------------------------------
// кольцевой текстовый буфер для общения с виртуальной машиной
#define FIFOLENGTH (1 << 12) // 4096 for now
struct fifo
{
	unsigned int putp, getp;
	char buffer[FIFOLENGTH];
}  static thread_local
   fi = {0, 0}, fo = {0, 0}; // input/output
   typedef struct fifo fifo;

static __inline__
char fifo_put(struct fifo* f, char c)
{
	f->buffer[f->putp++ % FIFOLENGTH] = c;
	return c;
}
static __inline__
char fifo_get(struct fifo* f) // use only after fido_empty
{
	char
	c = f->buffer[f->getp++ % FIFOLENGTH];
	return c;
}

static __inline__
char fifo_empty(struct fifo* f)
{
	return ((f->getp - f->putp) % FIFOLENGTH == 0);
}
static __inline__
char fifo_full(struct fifo* f)
{
	return ((f->getp - f->putp) % FIFOLENGTH == 1);
}
static __inline__
void fifo_clear(struct fifo* f)
{
	f->getp = f->putp = 0;
}

// utility fifo functions
static
int fifo_puts(struct fifo* f, char *message, int n)
{
	char *ptr = message;
	while (n--) {
		while (fifo_full(f))
			Sleep(1);
		fifo_put(f, *ptr++);
	}
	return ptr - message;
}
static
int fifo_gets(struct fifo* f, char *message, int n)
{
	char *ptr = message;
	while (--n) {
		while (fifo_empty(f))
			Sleep(1);
		*ptr++ = fifo_get(f);

		if (ptr[-1] == '\n')
			break;
	}
	*--ptr = '\0';
	return ptr - message;
}


// -=( dl )=-----------------------------------------------
// интерфейс к динамическому связыванию системных библиотек
#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
// seen at https://github.com/dlfcn-win32/dlfcn-win32/blob/master/dlfcn.c

//static thread_local char *dlerrno = 0;

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
int   dlclose(void *handle)
{
	return FreeLibrary((HMODULE)handle);
}

void *dlsym  (void *handle, const char *name)
{
	FARPROC function;

	function = GetProcAddress((HANDLE)handle, name);
	return function;
}

#else
#include <dlfcn.h>
#endif



//typedef struct fifo fifo;
// виртуальная машина
typedef struct OL
{
	HANDLE thread;
	fifo *fi, *fo;
} OL;



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
//                                    .----> immediate
//  [ssssssss ssssssss ????rppp tttttt10]
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

// Для экономии памяти

#define IPOS                        8 /* offset of immediate payload */
#define SPOS                        16 /* offset of size bits in header immediate values */
#define TPOS                        2  /* offset of type bits in header */

#define V(ob)                       *((word *) (ob)) // *ob, ob[0]
#define W                           sizeof(word)

//#define NWORDS                      1024*1024*8    /* static malloc'd heap size if used as a library */
#define FBITS                       24             /* bits in fixnum, on the way to 24 and beyond */
#define FMAX                        ((1<<FBITS)-1) /* maximum fixnum (and most negative fixnum) */
#define MAXOBJ                      0xffff         /* max words in tuple including header */
#define RAWBIT                      2048
#define make_immediate(value, type)    (((value) << IPOS) | ((type) << TPOS)                         | 2)
#define make_header(size, type)        (( (size) << SPOS) | ((type) << TPOS)                         | 2)
#define make_raw_header(size, type, p) (( (size) << SPOS) | ((type) << TPOS) | (RAWBIT) | ((p) << 8) | 2)
// p is padding

#define F(val)                      (((val) << IPOS) | 2)
#define TRUEFALSE(cval)             ((cval) ? ITRUE : IFALSE)
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

// встроенные типы (смотреть defmac.scm по "ALLOCATED")
#define TPAIR                        1
#define TCONST                      13
#define TTUPLE                       2
#define TTHREAD                     31 // type-thread-state
#define TFF                         24
#define TBVEC                       19
#define TBYTECODE                   16
#define TPROC                       17
#define TCLOS                       18
#define TSTRING                      3
#define TSTRINGWIDE                 22
#define THANDLE                     45
#define TVOID                       48 // type-void
#define TINT                        40 // type-int+
#define TINTN                       41 // type-int-
#define TRATIONAL                   42
#define TCOMPLEX                    43
#define TFIX                         0 // type-fix+
#define TFIXN                       32 // type-fix-

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

#define _thread_local

/*** Globals and Prototypes ***/
// память виртуальной машины локальна по отношению к ее потоку. пусть пока побудет так - не буду
//  ее тащить в структуру VM

static thread_local word max_heap_mb; /* max heap size in MB */

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
#ifndef WIN32
int execv(const char *path, char *const argv[]);
#endif





/*** Garbage Collector, based on "Efficient Garbage Compaction Algorithm" by Johannes Martin (1982) ***/
// несколько ссылок "на почитать" по теме GC:
//   shamil.free.fr/comp/ocaml/html/book011.html
struct heap
{
	//  begin <= genstart <= end
	word *begin;     // was: memstart
	word *end;       // was: memend

	word *genstart;  // was: genstart
//	word *top; // fp
} thread_local heap; // память машины, управляемая сборщиком мусора

#define cont(n)                     V((word)n & (~1)) // ~ - bitwise NOT (корректное разименование указателя, без учета бита mark)
#define flagged(n)                  (n & 1)           // is flagged ?
#define flag(n)                     (((word)n) ^ 1)   // same value with changed flag


// cont(n) = V((word)n & (~1)) // *n (с игнорированием флажка mark)

// *pos <- **pos, **pos <- pos with !flag(*pos)
// pos -> *pos -> **pos превращается в pos -> **pos,
// то-есть тут мы выбрасываем промежуточный указатель?
static __inline__
void rev(word* pos) {
//	*pos can be flagged and can be not flagged
	word ppos = *pos;
	*pos = cont(ppos); // next
	cont(ppos) = ((word)pos | 1) ^ (ppos & 1);
}

// возвращает последнего "flagged" в цепочке:
static __inline__
word *chase(word* pos) {
//	assert(pos IS flagged)
	word ppos = cont(pos);
	while (allocp(ppos) && flagged(ppos)) {
		pos = (word *) ppos;
		ppos = cont(pos);
	}
//	assert(pos IS flagged)
	return pos;
}

static void mark(word *pos, word *end)
{
//	assert(pos is NOT flagged)
	while (pos != end) {
		word val = *pos;
		if (allocp(val) && val >= ((word) heap.genstart)) { // блядь, что такое genstart???????
			if (flagged(val))
				pos = ((word *) flag(chase((word *) val))) - 1; // flag in this context equal to CLEAR flag
			else {
				word hdr = *(word *) val;
				rev(pos);
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

static word *compact()
{
	word *old = heap.genstart;
	word *end = heap.end - 1;

	word *newobj = old;
	while (old < end) {
		if (flagged(*old)) {
			word h;
			*newobj = *old;
			while (flagged(*newobj)) {
				rev(newobj);
				if (immediatep(*newobj) && flagged(*newobj))
					*newobj &= (~1); // was: = flag(*newobj);
			}
			h = hdrsize(*newobj);
			if (old == newobj) {
				old += h;
				newobj += h;
			}
			else {
				while (--h) *++newobj = *++old;
				old++;
				newobj++;
			}
		}
		else
			old += hdrsize(*old);
	}
	return newobj;
}

static void fix_pointers(word *pos, wdiff delta, word *end)
{
	while(1) {
		word hdr = *pos;
		int n = hdrsize(hdr);
		if (hdr == 0) return; /* end marker reached. only dragons beyond this point.*/
		if (rawp(hdr))
			pos += n; /* no pointers in raw objects */
		else {
			pos++;
			n--;
			while(n--) {
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
/*   if (!usegc) { // only run when the vm is running (temp)
      return 0;
   }*/
	if (((cells > 0) && (new_words*W < nwords*W)) || ((cells < 0) && (new_words*W > nwords*W)))
		return 0; /* don't try to adjust heap if the size_t would overflow in realloc */

	word *old = heap.begin;
	heap.begin = realloc(heap.begin, new_words*W);
	if (heap.begin == old) { /* whee, no heap slide \o/ */
		heap.end = heap.begin + new_words - MEMPAD; /* leave MEMPAD words alone */
		return 0;
	} else if (heap.begin) { /* d'oh! we need to O(n) all the pointers... */
		wdiff delta = (word)heap.begin - (word)old;
		heap.end = heap.begin + new_words - MEMPAD; /* leave MEMPAD words alone */
		fix_pointers(heap.begin, delta, heap.end);
		return delta;
	} else {
		breaked |= 8; /* will be passed over to mcp at thread switch*/
		return 0;
	}
}


// allocation pointer (top of allocated heap)
static thread_local word *fp;
static __inline__ word* new (size_t size)
{
	word* object = fp;
	fp += size;
	return object;
}

/* input desired allocation size and (the only) pointer to root object
   return a pointer to the same object after heap compaction, possible heap size change and relocation */
static word *gc(int size, word *regs) {
	word *root;
	word *realend = heap.end;
	int nfree;
	fp = regs + hdrsize(*regs); // следущий после regs (?)
	root = fp + 1;

	*root = (word) regs;

	heap.end = fp;
	mark(root, fp);
	fp = compact();
	regs = (word *) *root;
	heap.end = realend;
	nfree = (word)heap.end - (word)regs;
	if (heap.genstart == heap.begin) {
		word heapsize = (word) heap.end - (word) heap.begin;
		word nused = heapsize - nfree;
		if ((heapsize/(1024*1024)) > max_heap_mb)
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
#ifdef WIN32
   unsigned long flags = 1;
   if (sock > 3) { // stdin is read differently, out&err block
      ioctlsocket(sock, FIONBIO, &flags);
   }
#else
   fcntl(sock, F_SETFL, (blockp ?: O_NONBLOCK));
#endif
}

#ifndef WIN32
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
#ifndef WIN32
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
#ifndef WIN32
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
      return -1;
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
   if(immediatep(ob)) { return IFALSE; }
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

/* system- and io primops */
static word prim_sys(int op, word a, word b, word c) {
   switch(op) {
      case 0: { /* 0 fsend fd buff len r → n if wrote n, 0 if busy, False if error (argument or write) */
         int fd = fixval(a);
         word *buff = (word *) b;

         int wrote, size, len = fixval(c);
         if (immediatep(buff)) return IFALSE;
         size = (hdrsize(*buff)-1)*W;
         if (len > size) return IFALSE;
         // STDOUT ?
         if (fd == 1) {
        	 wrote = fifo_puts(&fo, ((char *)buff)+W, len);
         }
         else
        	 wrote = write(fd, ((char *)buff)+W, len);
         if (wrote > 0) return F(wrote);
         if (errno == EAGAIN || errno == EWOULDBLOCK) return F(0);
         return IFALSE; }
      case 1: { /* 1 = fopen <str> <mode> <to> */
         char *path = (char *) a;
         int mode = fixval(b);
         int val;
         struct stat sb;
         if (!(allocp(path) && imm_type(*path) == TSTRING))
            return IFALSE;
         mode |= O_BINARY | ((mode > 0) ? O_CREAT | O_TRUNC : 0);
         val = open(((char *) path) + W, mode,(S_IRUSR|S_IWUSR));
         if (val < 0 || fstat(val, &sb) == -1 || sb.st_mode & S_IFDIR) {
            close(val);
            return IFALSE;
         }
         set_blocking(val,0);
         return F(val); }
      case 2:
         return close(fixval(a)) ? IFALSE : ITRUE;
      case 3: { /* 3 = sopen port -> False | fd  */
         int port = fixval(a);
         int s;
         int opt = 1; /* TRUE */
         struct sockaddr_in myaddr;
         myaddr.sin_family = AF_INET;
         myaddr.sin_port = htons(port);
         myaddr.sin_addr.s_addr = INADDR_ANY;
         s = socket(AF_INET, SOCK_STREAM, 0);
#ifndef WIN32
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
      case 4: { /* 4 = accept port -> rval=False|(ip . fd) */
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

      /* FREAD */
      case 5: { /* fread fd max -> obj | eof | F (read error) | T (would block) */
         word fd = fixval(a);  // file descriptor
         word max = fixval(b); // buffer capacity

         word *res;
         int n, nwords = (max/W) + 2;
         res = new (nwords);
#ifndef WIN32
         n = read(fd, ((char *) res) + W, max);
#else
			if (fd == 0) { // windows stdin in special apparently
				if (fifo_empty(&fi)) {
					n = -1;
					errno = EAGAIN;
				}
				else {
					char *d = ((char*) res) + W;
					while (!fifo_empty(&fi))
						*d++ = fifo_get(&fi);
					n = d - (((char*) res) + W);
				}

/*				// read some data or return EAGAIN for continue internal threads
				if (coni) {
					char *s = coni;
					char *d = ((char*) res) + W;
					while (*d++ = *s++) ;

					// n == 0 is EOF
					n = (s-1) - coni;
					coni = 0;
				}
				else {
					n = -1;
					errno = EAGAIN;
				}*/

				/*if(!_isatty(0) || _kbhit()) { // we don't get hit by kb in pipe
					n = read(fd, ((char *) res) + W, max);
				} else {
					n = -1;
					errno = EAGAIN;
				}*/
			} else
				n = read(fd, ((char *) res) + W, max);
#endif
         if (n > 0) { // got some bytes
            word read_nwords = (n/W) + ((n%W) ? 2 : 1);
            int pads = (read_nwords-1)*W - n;
            fp = res + read_nwords;
            *res = make_raw_header(read_nwords, TBVEC, pads);
            return (word)res;
         }
         fp = res;
         if (n == 0)
            return IEOF;

         // EAGAIN: Resource temporarily unavailable (may be the same value as EWOULDBLOCK) (POSIX.1)
         return TRUEFALSE(errno == EAGAIN || errno == EWOULDBLOCK); }


      case 10: /* enter linux seccomp mode */
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
      case 11: { /* sys-opendir path _ _ -> False | dirobjptr */
         char *path = W + (char *) a; /* skip header */
         DIR *dirp = opendir(path);
         if(!dirp) return IFALSE;
         return fliptag(dirp); }
      case 12: { /* sys-readdir dirp _ _ -> bvec | eof | False */
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
      case 13: /* sys-closedir dirp _ _ -> ITRUE */
         closedir((DIR *)fliptag(a));
         return ITRUE;
      case 14: { /* set-ticks n _ _ -> old */
         word old = F(slice);
         slice = fixval(a);
         return old; }
      case 15: { /* 0 fsocksend fd buff len r → n if wrote n, 0 if busy, False if error (argument or write) */
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
      case 16: { /* getenv <owl-raw-bvec-or-ascii-leaf-string> */
         char *name = (char *)a;
         if (!allocp(name)) return IFALSE;
         return strp2owl(getenv(name + W)); }
      case 17: { /* exec[v] path argl ret */
#ifndef WIN32
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
      case 20: { /* chdir path res */
         char *path = ((char *)a) + W;
         if (chdir(path) < 0)
            return IFALSE;
         return ITRUE; }
#ifndef WIN32
      case 19: { /* wait <pid> <respair> _ */
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
      case 18: { /* fork ret → #false=failed, fixnum=ok we're in parent process, #true=ok we're in child process */
         pid_t pid = fork();
         if (pid == -1) /* fork failed */
            return IFALSE;
         if (pid == 0) /* we're in child, return true */
            return ITRUE;
         if ((int)pid > FMAX)
            fprintf(stderr, "vm: child pid larger than max fixnum: %d\n", pid);
         return F(pid&FMAX); }
      case 21: /* kill pid signal → fixnum */
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
   if (revp != IFALSE) { exit(1); } /* <- to be removed */
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

#define OGOTO(f, n)                 ob = (word *)R[f]; acc = n; goto apply
#define RET(n)                      ob = (word *)R[3]; R[3] = R[n]; acc = 1; goto apply

#define OCLOSE(proctype)            \
	{ word size = *ip++, tmp; word *ob = new (size); tmp = R[*ip++]; tmp = ((word *) tmp)[*ip++]; *ob = make_header(size, proctype); ob[1] = tmp; tmp = 2; while(tmp != size) { ob[tmp++] = R[*ip++]; } R[*ip++] = (word) ob; }
#define CLOSE1(proctype)            \
	{ word size = *ip++, tmp; word *ob = new (size); tmp = R[  1  ]; tmp = ((word *) tmp)[*ip++]; *ob = make_header(size, proctype); ob[1] = tmp; tmp = 2; while(tmp != size) { ob[tmp++] = R[*ip++]; } R[*ip++] = (word) ob; }
#define NEXT(n)                     ip += n; goto main_dispatch
#define SKIP(n)                     ip += n; break;
#define TICKS                       10000 /* # of function calls in a thread quantum  */
#define ERROR(opcode, a, b)         { R[4] = F(opcode); R[5] = (word) a; R[6] = (word) b; goto invoke_mcp; }
#define CHECK(exp,val,code)         if (unlikely(!(exp))) ERROR(code, val, ITRUE);

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


struct args
{
	OL *vm;	// виртуальная машина (из нее нам нужны буфера ввода/вывода)

	// структура памяти VM. распределяется еще до запуска самой машины
	word max_heap_mb; /* max heap size in MB */

	// memstart <= genstart <= memend
	struct heap heap;
	word *fp; // allocation pointer (top of allocated heap)

	void *userdata;
	char ready;
};
// args + 0 = (list "arg0" "arg 1") or ("arg0 arg1" . NIL) ?
// args + 3 = objects list
DWORD WINAPI
runtime(void *args) // heap top
{
	seccompp = 0;
	slice = TICKS; // default thread slice (n calls per slice)

	//
	max_heap_mb = ((struct args*)args)->max_heap_mb; /* max heap size in MB */

	// инициализируем локальную память
	heap.begin    = ((struct args*)args)->heap.begin;
	heap.end      = ((struct args*)args)->heap.end;
	heap.genstart = ((struct args*)args)->heap.genstart; // разделитель Old Generation и New Generation (?)

	// allocation pointer (top of allocated heap)
	fp       = ((struct args*)args)->fp;
	// подсистема взаимодействия с виртуальной машиной посредством ввода/вывода
	((struct args*)args)->vm->fi = &fi;
	((struct args*)args)->vm->fo = &fo;
	  fifo_clear(&fi), fifo_clear(&fo); // todo: проверить, нужен ли этот вызов

	// все, машина инициализирована, отсигналимся
	((struct args*)args)->ready = 1;

	void *userdata = ((struct args*)args)->userdata;
	// точка входа в программу - это последняя лямбда загруженного образа (λ (args))
	// todo: может стоит искать и загружать какой-нибудь main()?
	word* ptrs = (word*)userdata + 3;
	int nobjs = hdrsize(ptrs[0]) - 1;
	word* ob = (word*) ptrs[nobjs-1]; // выполним последний объект в списке /он должен быть (λ (args))/

	word R[NR];

	// clear blank regs
	int i = 0;
	while (i < NR)
		R[i++] = INULL;
	R[0] = IFALSE;
	R[3] = IHALT;
	R[4] = (word) userdata;

	unsigned char *ip;

		int bank = 0; /* ticks deposited at syscall */
		int ticker = slice; /* any initial value ok */
	// usegc = 1; /* enble gc (later have if always evabled) */

	unsigned short acc = 2; // boot always calls with 2 args, no support for >255arg functions

apply: // apply something at ob to values in regs, or maybe switch context
	if (allocp(ob)) { // если это аллоцированный объект
		word hdr = *ob & 0x0FFF; // cut size out, take just header info
		if (hdr == make_header(0, TPROC)) { // proc
			R[1] = (word) ob; ob = (word *) ob[1];
		}
		else
		if (hdr == make_header(0, TCLOS)) { // clos
			R[1] = (word) ob; ob = (word *) ob[1];
			R[2] = (word) ob; ob = (word *) ob[1];
		}
		else
		if (((hdr>>TPOS)&60) == TFF) { /* low bits have special meaning */
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
			goto apply;
		}
		else
		if (((hdr >> TPOS) & 63) != TBYTECODE) /* not even code, extend bits later */
			ERROR(259, ob, INULL);

		// время потока вышло, переключим на следующий
		if (unlikely(!ticker--)) goto switch_thread;

		ip = (unsigned char *) &ob[1];
		goto invoke;
   }
	else if ((word)ob == IEMPTY && acc > 1) { /* ff application: (False key def) -> def */
      ob = (word *) R[3]; /* call cont */
      R[3] = (acc > 2) ? R[5] : IFALSE; /* default arg or false if none */
      acc = 1;
      goto apply;
   }
	else if ((word)ob == IHALT) {
      /* a tread or mcp is calling the final continuation  */
      ob = (word *) R[0];
      if (allocp(ob)) {
         R[0] = IFALSE;
         breaked = 0;
         R[4] = R[3];
         R[3] = F(2);
         R[5] = IFALSE;
         R[6] = IFALSE;
         ticker = 0xffffff;
         bank = 0;
         acc = 4;
         goto apply;
      }
      return fixval(R[3]);
   } /* <- add a way to call the newobj vm prim table also here? */
   ERROR(257, ob, INULL); /* not callable */

switch_thread: /* enter mcp if present */
	if (R[0] == IFALSE) { /* no mcp, ignore */
		ticker = TICKS;
		goto apply;
	} else {
      /* save vm state and enter mcp cont at R0 */
      word *state, pos = 1;
      ticker = 0xffffff;
      bank = 0;
      acc = acc + 4;
      R[acc] = (word) ob;
      state = new (acc);
      *state = make_header(acc, TTHREAD);
      state[acc-1] = R[acc];
      while(pos < acc-1) {
         state[pos] = R[pos];
         pos++;
      }
      ob = (word *) R[0];
      R[0] = IFALSE; /* remove mcp cont */
      /* R3 marks the syscall to perform */
      R[3] = breaked ? ((breaked & 8) ? F(14) : F(10)) : F(1); /* fixme - handle also differnet signals via one handler  */
      R[4] = (word) state;
      R[5] = F(breaked);
      R[6] = IFALSE;
      acc = 4;
      breaked = 0;
      goto apply;
   }

invoke: /* nargs and regs ready, maybe gc and execute ob */
	if (fp >= heap.end - 16*1024) { // (((word)fp) + 1024*64 >= ((word) memend))
		int p = 0;
		*fp = make_header(NR+2, 50); /* hdr r_0 .. r_(NR-1) ob */
		while (p < NR) { fp[p+1] = R[p]; p++; }
		fp[p+1] = (word) ob;
		fp = gc(16*1024 * sizeof(word), fp);
		ob = (word *) fp[p+1];
		while(--p >= 0) { R[p] = fp[p+1]; }
		ip = (unsigned char *)(ob + 1);
	}

	// список команд: работающий транслятор в С смотреть в cgen.scm в (translators)
#	define REFI   1       // refi a, p, t:   Rt = Ra[p], p unsigned (indirect-ref from-reg offset to-reg)
#	define MOVE   9       //
#	define MOV2   5       //
#	define GOTO   2       // jmp a, nargs
#	define LDI   13       // похоже, именно 13я команда не используется, а только 77 (LDN), 141 (LDT), 205 (LDF)
#	define LD    14
#	define JP    16       // JZ, JN, JT, JF
#	define JLQ    8       // jlq ?
#	define JF2   25

	// примитивы языка
#	define CONS  51
#	define CAR   52
#	define CDR   53
#	define NCONS 29
#	define NCAR  30
#	define NCDR  31
	// АЛУ
#	define EQ    54

#	define SYSPRIM 63

	// ip - счетчик команд (опкод - младшие 6 бит команды, старшие 2 бита - модификатор(если есть) опкода)
	// Rn - регистр машины (R[n])
	// An - регистр, на который ссылается операнд N (записанный в параметре n команды, начиная с 0)
	// todo: добавить в комменты к команде теоретическое количество тактов на операцию
	word *T; // временный регистр, валидный только атомарно в обработчике операции
	while (1) { // todo: добавить условие выхода из цикла
		int op; // operation to execute
		switch ((op = *ip++) & 0x3F) {
		case 0:
			op = (ip[0] << 8) | ip[1]; // big endian??
			//super_dispatch: run macro instructions
			// todo: add here JIT
			switch (op) {
			/* AUTOGENERATED INSTRUCTIONS */
			default:
				ERROR(258, F(op), ITRUE);
			}
			goto apply;

		// операции с данными
		case LDI:    // 13,  -> ldi{2bit what} [to], ldn, ldt, ldf
			A0 = I[op>>6];
			ip += 1; break;
		case LD:     // некоторая нелогичность - в LDI первым идет приемник, а в LD - он вторым (?), или все ок?
			A1 = F(ip[0]);
			ip += 2; break;

		//
		case GOTO:
			ob = (word *)A0; acc = ip[1];
			goto apply;
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
			            fp[0] = PAIRHDR;   // todo: make as function for implicitly use fp
			            fp[1] = R[acc + 2];
			            fp[2] = tail;
			            tail = (word) fp;
			            fp += 3;
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
			fp[0] = PAIRHDR; // todo: make as function with using fp
			fp[1] = A0, fp[2] = A1;
			A2 = (word) fp;
			fp += 3;
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

		// то же самое, но для числовых пар (todo: проверить, можно ли заменить ncons на cons и т.д.
		case NCONS:  /* ncons a b r */
			fp[0] = NUMHDR; // todo: make as function with using fp
			fp[1] = A0, fp[2] = A1;
			A2 = (word) fp;
			fp += 3;
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
	   op26: { /* fxqr ah al b qh ql r, b != 0, int32 / int16 -> int32, as fixnums */
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
		case 18: goto op18; case 19: goto op19; case 20: goto op20; case 21: goto op21;
		case 22: goto op22; case 23: goto op23; case 24: goto op24; case 26: goto op26; case 27: goto op27;
		case 28: goto op28; case 32: goto op32;
		case 34: goto op34; case 35: goto op35; case 36: goto op36; case 37: goto op37; case 38: goto op38; case 39: goto op39;
		case 40: goto op40; case 41: goto op41; case 42: goto op42; case 43: goto op43; case 44: goto op44; case 45: goto op45;
		case 46: goto op46; case 47: goto op47; case 48: goto op48; case 49: goto op49; case 50: goto op50;
		case 55: goto op55; case 56: goto op56; case 57: goto op57;
		case 58: goto op58; case 59: goto op59; case 60: goto op60; case 61: goto op61; case 62: goto op62;

//		// ошибки
		case 17: /* arity error */
			ERROR(17, ob, F(acc));

		// неиспользуемые коды (историческое наследие, при желании можно реюзать)
		case 10: /* unused */
			ERROR(10, IFALSE, IFALSE);
		case 11: /* unused */
			ERROR(11, IFALSE, IFALSE);
		case 12: /* unused */
			ERROR(12, IFALSE, IFALSE);
		case 33:
			ERROR(33, IFALSE, IFALSE);


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

			// этот case должен остаться тут - как последний из кейсов
			//  todo: переименовать в компиляторе sys-prim на syscall
			// http://docs.cs.up.ac.za/programming/asm/derick_tut/syscalls.html
			case SYSPRIM: { /* sys-prim op arg1 arg2 arg3 r1 */
				// todo: make prim_sys inlined here!
				word op = fixval(A0);
				word a = A1, b = A2, c = A3;
				word result;

				switch (op) {
				// todo: сюда надо перенести все prim_sys операции, что зависят от глобальных переменных
				//  остальное можно спокойно оформлять отдельными функциями
				// todo: добавить функции LoadLibrary, GetProcAddress и вызов этих функций с параметрами (+)

				case 6: // todo: переделать на другой номер
					free(heap.begin); // освободим занятую память
					#if WIN32
						ExitThread(fixval(a));
					#else
						EXIT(fixval(a)); /* stop the press */
					#endif

				case 7: /* set memory limit (in mb) */ // todo: переделать на другой номер
					 max_heap_mb = fixval(a);
					 result = a;
					 break;
				  case 8: /* get machine word size (in bytes) */ // todo: переделать на другой номер
					  result = F(W);
					  break;
				  case 9: /* get memory limit (in mb) */ // todo: переделать на другой номер
					  result = F(max_heap_mb);
					  break;

				// -=( pinvoke )=-------------------------------------------------
				//   а тут у нас реализация pinvoke механизма. пример в opengl.scm
				case 30: { // dlopen
					word *filename = (word*)a;
					int mode = fixval(b);

					if (!(allocp(filename) && hdrtype(*filename) == TSTRING))
			        	 return IFALSE;

					void* module = dlopen((char*) (filename + 1), mode);
					//void* module = LoadLibrary((char*) (filename + 1));

					result = (word)fp; // todo: разобраться тут правильно с размерами типов
					fp[0] = make_raw_header(2, THANDLE, 0); //was: sizeof(void*) % sizeof(word)); // sizeof(void*) % sizeof(word) as padding
					fp[1] = (word)module;
					fp += 2;
					break;
				}
				case 31: { // dlsym
					word* A = (word*)a;

					assert (hdrtype(A[0]) == THANDLE);
					void* module = (void*) A[1];
					word* name = (word*)b;

					// http://www.symantec.com/connect/articles/dynamic-linking-linux-and-windows-part-one
					if (!(immediatep(name) || hdrtype(*name) == TSTRING))
						return IFALSE;

					void* function = dlsym(module, immediatep(name)
							? (LPSTR) imm_val((word)name)
							: (LPSTR) (char*) (name + 1));

					// todo: в качестве оптимизации можно возвращать уже подготовленную структуру с параметрами и конвеншеном вызова
					result = (word)fp;
					fp[0] = make_raw_header(2, THANDLE, 0);
					fp[1] = (word)function;
					fp += 2;
					break;
				}
				// временный тестовый вызов
				case 33: {
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
					break;
				}

				// вызвать библиотечную функцию
				case 32: {
					// http://byteworm.com/2010/10/12/container/ (lambdas in c)
					int call(int convention, void* function, int args[], int count) {
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

						switch (convention >> 6) {
						case 0: // __stdcall
							switch (count) {
							case  0: return ((__stdcall int (*) ())
											function) ();
							case  1: return ((__stdcall int (*) (int)) function)
											(args[0]);
							case  2: return ((__stdcall int (*) (int, int)) function)
											(args[0], args[1]);
							case  3: return ((__stdcall int (*) (int, int, int)) function)
											(args[0], args[1], args[2]);
							case  4: return ((__stdcall int (*) (int, int, int, int)) function)
											(args[0], args[1], args[2], args[3]);
							case  5: return ((__stdcall int (*) (int, int, int, int, int)) function)
											(args[0], args[1], args[2], args[3], args[4]);
							case  6: return ((__stdcall int (*) (int, int, int, int, int, int)) function)
											(args[0], args[1], args[2], args[3], args[4], args[5]);
							case  7: return ((__stdcall int (*) (int, int, int, int, int, int, int)) function)
											(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
							case  8: return ((__stdcall int (*) (int, int, int, int, int, int, int, int)) function)
											(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
							case  9: return ((__stdcall int (*) (int, int, int, int, int, int, int, int, int)) function)
											(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8]);
							case 10: return ((__stdcall int (*) (int, int, int, int, int, int, int, int, int, int)) function)
											(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9]);
							case 11: return ((__stdcall int (*) (int, int, int, int, int, int, int, int, int, int, int)) function)
											(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10]);
							case 12: return ((__stdcall int (*) (int, int, int, int, int, int, int, int, int, int, int, int))
											function) (args[0], args[1], args[2], args[3],
											           args[4], args[5], args[6], args[7],
											           args[8], args[9], args[10], args[11]);
							default: fprintf(stderr, "Too match parameters for pinvoke function");
								break;
							}
							break;
						case 1: // __cdecl
							switch (count) {
							case  0: return ((__cdecl   int (*) ())
											function) ();
							case  1: return ((__cdecl   int (*) (int)) function)
											(args[0]);
							case  2: return ((__cdecl   int (*) (int, int)) function)
											(args[0], args[1]);
							case  3: return ((__cdecl   int (*) (int, int, int)) function)
											(args[0], args[1], args[2]);
							case  4: return ((__cdecl   int (*) (int, int, int, int)) function)
											(args[0], args[1], args[2], args[3]);
							case  5: return ((__cdecl   int (*) (int, int, int, int, int)) function)
											(args[0], args[1], args[2], args[3], args[4]);
							case  6: return ((__cdecl   int (*) (int, int, int, int, int, int)) function)
											(args[0], args[1], args[2], args[3], args[4], args[5]);
							case  7: return ((__cdecl   int (*) (int, int, int, int, int, int, int)) function)
											(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
							case  8: return ((__cdecl   int (*) (int, int, int, int, int, int, int, int)) function)
											(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
							case  9: return ((__cdecl   int (*) (int, int, int, int, int, int, int, int, int)) function)
											(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8]);
							case 10: return ((__cdecl   int (*) (int, int, int, int, int, int, int, int, int, int)) function)
											(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9]);
							case 11: return ((__cdecl   int (*) (int, int, int, int, int, int, int, int, int, int, int)) function)
											(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10]);
							case 12: return ((__cdecl   int (*) (int, int, int, int, int, int, int, int, int, int, int, int))
											function) (args[0], args[1], args[2], args[3],
											           args[4], args[5], args[6], args[7],
											           args[8], args[9], args[10], args[11]);
							default: fprintf(stderr, "Too match parameters for pinvoke function");
							}
						}
						return 0;
					}

					// a - function address
					// b - return type
					// c - arguments (may be pair with req type in car and arg in cdr - not yet done)
					word* A = (word*)a;

					assert (hdrtype(A[0]) == THANDLE);
					assert (hdrsize(A[0]) == 2); // в списке один системный ворд // это временно, а вообще надо будет использовать fix+ и собирать его по битикам
					assert (immediatep(b));

					// todo: добавить разные конвенции вызова: __ccall, __stdcall, __fastcall

					int args[12]; // пока только 12 аргумента максимум
					void *function = (void*) (A[1]);
					int returntype = imm_val(b);

					int got;    // результат вызова функции
					int i = 0;	// количество аргументов
					word* p = (word*)c; // аргументы
					while ((int)p != INULL) { // пока есть аргументы
						assert (hdrtype(*p) == TPAIR);
						word* arg = (word*)p[1]; // car
						if (immediatep(arg)) {
							// type-fix+, type-fix-
							args[i] = fixval((unsigned int)arg);
							// этот кусок временный - потом переделать в трюк
							if ((int)arg & 0x80)
								args[i] *= -1;
							// алгоритмические трюки:
							// x = (x xor t) - t, где t - y >>(s) 31 (все 1, или все 0)
						}
						else { // allocp
							int from_fix(word* arg) {
								int value = fixval((unsigned int)arg);
								if ((int)arg & 0x80)
									value = -1 * value;
								return value;
							}
							int from_int(word* arg) {
								// это большие числа. а так как в стек мы все равно большое сложить не сможем, то возьмем только то, что влазит
								assert (immediatep(arg[1]));
								assert (allocp(arg[2]));

								return (arg[1] >> 8) | ((((word*)arg[2])[1] >> 8) << 24);
							}
							int from_rational(word* arg) {
								word* pa = (word*)arg[1];
								word* pb = (word*)arg[2];

								// временно огрманичимся небольшими nominator/denominator, а дальше посмотрим
								word a, b;
								if (immediatep(pa))
									a = from_fix(pa);
								else {
									switch (hdrtype(pa[0])) {
									case TINT:
										a = +(float)from_int(pa);
										break;
									case TINTN:
										a = -(float)from_int(pa);
										break;
									}
								}
								if (immediatep(pb))
									b = from_fix(pb);
								else {
									switch (hdrtype(pb[0])) {
									case TINT:
										b = +(float)from_int(pb);
										break;
									case TINTN:
										b = -(float)from_int(pb);
										break;
									}
								}

								float result = (float)a / (float)b;
								return *(int*)&result;
							}


							int type = hdrtype(arg[0]);
							// если тип представляет собой пару значений, то
							//	в car лежит требуемый тип, а в cdr само значение
							if (type == TPAIR) {
								assert (immediatep(arg[1]));
								int type1 = imm_val(arg[1]);

								if (type1 == 46) { // FLOAT // todo: change to new switch level :)
									arg = (word*)arg[2];
									if (immediatep(arg))
										// type-fix+, type-fix-;
										*(float*)&args[i] =  (float)from_fix(arg);
									else
									switch (hdrtype(arg[0])) {
									case TINT:
										*(float*)&args[i] = +(float)from_int(arg);
										break;
									case TINTN:
										*(float*)&args[i] = -(float)from_int(arg);
										break;
									case TRATIONAL:
										*(float*)&args[i] =  (float)from_rational(arg);
										break;
									}
								}
								// if (type1 == 45)
								//   make integer numbers
							}
							else
							switch (type) {
							case THANDLE:
								args[i] = (int)(arg[1]);
								break;
							case TBVEC:
							case TSTRING:
//							case TCONST:
								// in arg[0] size got size of string
								args[i] = (int)(&arg[1]);
								break;

							case TINT: { // type-int+
								args[i] = +from_int(arg);
								break;
							}
							case TINTN: { // type-int-
								args[i] = -from_int(arg);
								break;
							}
							case TRATIONAL:
								args[i] = from_rational(arg);
								// тут надо разделить два числа (возможно большие) и пушнуть float
								// если числа большие (если в car и cdr лежат ссылки), то надо это
								// сделать в столбик. ну или добавить в компилятор числа с плавающей запятой
								break;


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
								break;
							}

							default:
								// notification about unknown argument type!
								args[i] = INULL;
								break;
							}
						}

						p = (word*)p[2]; // cdr
						i++;
					}
					got = call(returntype & 0x3F, function, args, i);

					// todo: добавить type-void который возращает просто INULL
					switch (returntype & 0x3F) {
						case 0: // type-fix+
							result = F(got);
							break;
						case TINT:
							if (got > 0xFFFFFF) {
								word* lo = fp; fp += 3; // low 24 bits
								lo[0] = NUMHDR;
								lo[1] = make_immediate(got & 0xFFFFFF, 0); // type-fx+
								lo[2] = (word)fp;
								word* hi = fp; fp += 3; // high 8 bits
								hi[0] = NUMHDR;
								hi[1] = make_immediate(got >> 24, 0); // type-fx+
								hi[2] = INULL;

								result = (word)lo;
								break;
							}
							// иначе вернем type-fx+
							result = F(got);
							break;
						case THANDLE:
							result = (word)fp;
							fp[0] = make_raw_header(2, THANDLE, 0);
							fp[1] = got;
							fp += 2;
							break;

						case TVOID:
						default:
							result = INULL;
					}

					break; // case 32
				}
				default:
					result = prim_sys(op, a, b, c);
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

   op15: { /* type-byte o r <- actually sixtet */
      word x = R[*ip++];
      if (allocp(x)) x = V(x);
      R[*ip++] = F((x>>TPOS)&63);
      NEXT(0); }
   op20: {
      int reg, arity;
      word *lst;
      if (op == 20) { /* normal apply: cont=r3, fn=r4, a0=r5, */
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
      } else { /* _sans_cps apply: func=r3, a0=r4, */
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
      while(allocp(lst) && *lst == PAIRHDR) { /* unwind argument list */
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
      goto apply; }
   op22: { /* cast o t r */
      word *ob = (word *) R[*ip];
      word type = fixval(A1) & 63;
      A2 = prim_cast(ob, type);
      NEXT(3); }

   op24: /* ret val */
      ob = (word *) R[3];
      R[3] = R[*ip];
      acc = 1;
      goto apply;

   op27: /* syscall cont op arg1 arg2 */
      ob = (word *) R[0];
      R[0] = IFALSE;
      R[3] = A1; R[4] = R[*ip]; R[5] = A2; R[6] = A3;
      acc = 4;
      if (ticker > 10) bank = ticker; /* deposit remaining ticks for return to thread */
      goto apply;
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
   op34: { /* connect <host-ip> <port> <res> -> fd | False, via an ipv4 tcp stream */ // todo: move to sys-prim?
      A2 = prim_connect((word *) A0, A1); /* fixme: remove and put to prim-sys*/
      NEXT(3); }
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
#ifndef WIN32
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
   op45: { /* set t o v r */
      A3 = prim_set(A0, A1, A2);
      NEXT(4); }
   op50: { /* run thunk quantum */ /* fixme: maybe move to sys */
      word hdr;
      ob = (word *) A0;
      R[0] = R[3];
      ticker = bank ? bank : fixval(A1);
      bank = 0;
      CHECK(allocp(ob),ob,50);
      hdr = *ob;
      if (imm_type(hdr) == TTHREAD) {
         int pos = hdrsize(hdr) - 1;
         word code = ob[pos];
         acc = pos - 3;
         while(--pos) { R[pos] = ob[pos]; }
         ip = ((unsigned char *) code) + W;
      } else {
         /* call a thunk with terminal continuation */
         R[3] = IHALT; /* exit via R0 when the time comes */
         acc = 1;
         goto apply;
      }
      NEXT(0); }

   op60: /* lraw lst type dir r (fixme, alloc amount testing compiler pass not in place yet!) */
      A3 = prim_lraw(A0, fixval(A1), A2);
      NEXT(4);
   op61: /* clock <secs> <ticks> */ { /* fixme: sys */
      struct timeval tp;
      word *ob = new (6); /* space for 32-bit bignum - [NUM hi [NUM lo null]] */
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
      } else {
         gettimeofday(&tp, NULL);
         A1 = F(tp.tv_usec / 1000);
         ob[1] = F(tp.tv_sec >> FBITS);
         ob[4] = F(tp.tv_sec & FMAX);
      }
      NEXT(2); }
   op62: /* set-ticker <val> <to> -> old ticker value */ /* fixme: sys */
      A1 = F(ticker & FMAX);
      ticker = fixval(A0);
      NEXT(2);


	} // switch(1)

//super_dispatch: /* run macro instructions */
//   // todo: add here JIT
//   switch(op) {
///* AUTOGENERATED INSTRUCTIONS */
//      default:
//         ERROR(258, F(op), ITRUE);
//   }
//   goto apply;

invoke_mcp: /* R4-R6 set, set R3=cont and R4=syscall and call mcp */
   ob = (word *) R[0];
   R[0] = IFALSE;
   R[3] = F(3);
   if (allocp(ob)) {
      acc = 4;
      goto apply;
   }
   return 1; /* no mcp to handle error (fail in it?), so nonzero exit  */
}


// ======================================================================
//       загрузчик скомпилированного образа и его десериализатор
//

// fasl decoding
/* count number of objects and measure heap size */
static unsigned char *hp;       /* heap pointer when loading heap */

word get_nat() {
   word result = 0;
   word newobj, i;
   do {
      i = *hp++;
      newobj = result << 7;
      if (result != (newobj >> 7)) exit(9); // overflow kills
      result = newobj + (i & 127);
   } while (i & 128);
   return result;
}

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
   if(ptrs != NULL) ptrs[me] = (word) fp;
   switch(*hp++) { /* todo: adding type information here would reduce fasl and executable size */
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
      default: puts("bad object in heap"); exit(42);
   }
   return fp;
}

// функция подсчета количества объектов в загружаемом образе
static
int count_fasl_objects(word *words, unsigned char *lang) {
	unsigned char* hp;
	word decode_word() {
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
		return result;
	}

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
				decode_word(); // simply skip word
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
			puts("bad object in heap"); exit(42);
		}

		n++;
	}

	*words = allocated;
	return n;
}

// ----------------------------------------------------------------
// -=( virtual machine functions )=--------------------------------
//
//  this is NOT thread safe function!
OL* vm_start(unsigned char* language)
{
	OL *handle = malloc(sizeof(OL));
	memset(handle, 0x0, sizeof(OL));

	// выделим память машине
	max_heap_mb = (W == 4) ? 4096 : 65535; // can be set at runtime
	heap.begin = heap.genstart = (word*) malloc((INITCELLS + FMAX + MEMPAD) * sizeof(word)); // at least one argument string always fits
	if (!heap.begin) {
		fprintf(stderr, "Failed to allocate initial memory\n");
		exit(4);
	}
	heap.end = heap.begin + FMAX + INITCELLS - MEMPAD;

//
//	// create '() as root object (этот закомментареный кусок кода неправильный!)
//	{
//		word *oargs = fp;
//		oargs[0] = PAIRHDR; // '()
//		oargs[1] =   INULL;
//		oargs[2] =   INULL;
//		fp += 3;
//	}
//
	// подготовим в памяти машины параметры командной строки:

	// create '("some string", NIL) as parameter for the start lambda
	// todo: добавить возможность компиляции самого компилятора, для этого надо
	//  втянуть компилятор из owl/ol.scm и запустить его с параметрами командной строки
	//  например, "-s none -o fasl/compiled.fasl", а не так как сейчас
	word *oargs = fp = heap.begin;
	{
		char* filename = "#";
//		char* filename = "--run owl/ol.scm -s none -o fasl/bootp.fasl";
		char *pos = filename;

		int len = 0;
		while (*pos++) len++;

		int size = (len / W) + ((len % W) ? 2 : 1);
		int pads = (size-1) * W - len;

		*fp = make_raw_header(size, TSTRING, pads);
		pos = ((char *) fp) + W;
		while (*filename) *pos++ = *filename++;

		fp += size;
		fp[0] = PAIRHDR;
		fp[1] = (word) oargs;
		fp[2] = (word) INULL;
		oargs = fp;
	}

	// а теперь поработаем со скомпилированным образом:
	word nwords = 0;
	word nobjs = count_fasl_objects(&nwords, language); // подсчет количества слов и объектов в образе

	oargs = gc(nwords + (128*1024), oargs); // get enough space to load the heap without triggering gc
	fp = oargs + 3; // move fp too

	// deserialize heap to the objects
	word* ptrs = fp;
	fp += nobjs + 1;

	hp = language; // десериализатор использует hp как итератор по образу

	int pos;
	for (pos = 0; pos < nobjs; pos++) {
		if (fp >= heap.end) {
			puts("gc needed during heap import\n");
			exit(1);
		}
		deserialize(ptrs, pos);
	}
	ptrs[0] = make_raw_header(nobjs + 1, 0, 0);

	struct args args; // аргументы для запуска
	args.vm = handle; // виртуальной машины OL

	// а это инициализационные аргументы для памяти виртуальной машины
	args.heap.begin = heap.begin;
	args.heap.end   = heap.end;
	args.heap.genstart = heap.genstart;
	args.max_heap_mb = max_heap_mb; // max heap size in MB
	args.fp = fp;

	args.ready = 0;
	args.userdata = oargs;

//	vm(oargs);
	handle->thread =
	CreateThread(NULL, 0, &runtime, &args, 0, NULL);
//	ResumeThread(machine->thread);
//	WaitForSingleObject(machine->thread, INFINITE); // wait for init
	while (!args.ready)
		Sleep(1);

	return handle;
}

/*void eval(char* message, char* response, int length)
{
	fifo_puts(&fi, message, strlen(message) + 1);
	fifo_gets(&fo, response, length);
}
void eval2(char* message)
{
	fifo_puts(&fi, message, strlen(message) + 1);
}*/

int vm_puts(OL* vm, char *message, int n)
{
	return fifo_puts(vm->fi, message, n);
}
int vm_gets(OL* vm, char *message, int n)
{
	return fifo_gets(vm->fo, message, n);
}

int vm_stop(OL* vm)
{
	vm_puts(vm, "(halt 0)\n", 9);
	// do not wait to the end (?)
	WaitForSingleObject(vm->thread, INFINITE);

	return 0;
}
