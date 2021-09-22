#pragma once
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
