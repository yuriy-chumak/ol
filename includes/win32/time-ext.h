#pragma once
#ifdef _WIN32

static LARGE_INTEGER freq = {0};

int clock_gettime(clockid_t clock_id, struct timespec *ts)
{
	switch (clock_id) {
		case CLOCK_REALTIME: {
			FILETIME time;
			ULARGE_INTEGER ul;
			GetSystemTimeAsFileTime(&time);
			ul.LowPart = time.dwLowDateTime;
			ul.HighPart = time.dwHighDateTime;

			unsigned __int64 relative_ticks = (ul.QuadPart / 10) - 116444736000000000ULL; // UNIX_TIME_START

    		ts->tv_sec = relative_ticks / 1000000ULL;
			ts->tv_nsec = relative_ticks % 1000000ULL;
			return 0;
		}
		case CLOCK_MONOTONIC: {
    		if (freq.QuadPart == 0)
        		QueryPerformanceFrequency(&freq);

		    LARGE_INTEGER count;
		    QueryPerformanceCounter(&count);
    		ts->tv_sec = count.QuadPart / freq.QuadPart;
    		ts->tv_nsec = (long)((count.QuadPart % freq.QuadPart) * 1000000000.0 / freq.QuadPart);
		    return 0;
		}
	}
	return -1;
}

#endif