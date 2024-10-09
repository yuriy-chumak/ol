#pragma once
#ifdef _WIN32

#if _WIN32_WINNT >= 0x0600 // Vista
#	include <sysinfoapi.h>
#endif

#include <winbase.h>

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

	// Seconds since boot
#if _WIN32_WINNT >= 0x0600 // Vista
	info->uptime = GetTickCount64() / 1000;
#else
	info->uptime = GetTickCount() / 1000;
#endif

	// Total usable main memory size
#if _WIN32_WINNT >= 0x0601 // 7
	ULONGLONG mem;
	if (GetPhysicallyInstalledSystemMemory(&mem))
		info->totalram = (mem > ULONG_MAX / 1024)
			? ULONG_MAX
			: (unsigned long) (mem * 1024);
#endif

	// 
	return 0;
}

#endif
