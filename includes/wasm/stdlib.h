#pragma once

typedef __SIZE_TYPE__ size_t;
typedef __PTRDIFF_TYPE__ ssize_t;

#include <malloc.h>

// .......
extern int mkstemp(...);
extern int unlink(...);
typedef __INT32_TYPE__ off_t; // <sys/types.h>
extern int send(...);

extern int exit(...);
extern int signal(...);
typedef int jmp_buf;
#define INFINITY 0
#define NAN 1
extern int modf(...);
extern int longjmp(...);
#define htole16(x) x
typedef __UINT16_TYPE__ uint16_t;
typedef __INT16_TYPE__ int16_t;

// time.h
struct timeval {
	int	tv_sec;		/* seconds */
	int	tv_usec;	/* microseconds */
};

extern int gettimeofday(...);
typedef __INT8_TYPE__ int8_t;
typedef __INT32_TYPE__ int32_t;
typedef __INT64_TYPE__ int64_t;
typedef __UINT8_TYPE__ uint8_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;

// signal.h
#define SIGINT		 2
#define SIGPIPE		13
#define SIG_IGN	1 // signal-defs.h
#define SIGCHLD		17

extern int pipe(...);
#define F_GETFL		3	/* Get file status flags.  */
extern int dup(...);
extern int dup2(...);
struct termios {int x; };
extern int tcgetattr(...);
struct timespec {
	long	tv_sec;		/* seconds */
	long			tv_nsec;	/* nanoseconds */
};
extern int nanosleep(...);
extern int dlopen(...);
extern int dlclose(...);
extern int dlsym(...);
extern int dlerror(...);
typedef int time_t;
extern int time(...);
extern struct tm* localtime(...);
extern int strftime(...);
struct rlimit
  {
    int rlim_cur;
    int rlim_max;
  };
extern int getrlimit(...);
#define RLIMIT_CPU 0
#define RLIMIT_FSIZE 1
#define RLIMIT_DATA 2
#define RLIMIT_AS 9

struct rusage
{
	struct timeval ru_utime;
	struct timeval ru_stime;
};
#define RUSAGE_SELF 0
extern int getrusage(...);

struct sysinfo {
	int uptime;		/* Seconds since boot */
	int loads[3];	/* 1, 5, and 15 minute load averages */
	int totalram;	/* Total usable main memory size */
	int freeram;	/* Available memory size */
	int sharedram;	/* Amount of shared memory */
	int bufferram;	/* Memory used by buffers */
	int totalswap;	/* Total swap space size */
	int freeswap;	/* swap space still available */
	int procs;		   	/* Number of current processes */
	int pad;		   	/* Explicit padding for m68k */
	int totalhigh;	/* Total high memory size */
	int freehigh;	/* Available high memory size */
	int mem_unit;			/* Memory unit size in bytes */
};
extern int sysinfo(...);
extern int unsetenv(...);
extern int setenv(...);
extern int getenv(...);
extern int strchr(...);
extern int system(...);
#define	EILSEQ		84	/* Illegal byte sequence */
#define alloca __builtin_alloca
extern int setjmp(...);
extern void abort();

extern void console_log(char* str);