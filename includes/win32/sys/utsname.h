/*
   This is a simplest version of uname for MsWindows.

   The larger version of uname is accessible in the "extensions/win32/uname.c"
   which must be compiled as "ol-uname.dll" (use `make ol-uname.dll`).
   Olvm tries to load "ol-uname.dll" and change uname to a new one at startup.
*/
#ifdef _WIN32

// todo: load large "ol-uname.dll" or use default simplified one function (below)
struct utsname
{
	char sysname[65];  //
	char nodename[65];
	char release[65];
	char version[65];
	char machine[65];
};

static
int uname(struct utsname* out) {
	DWORD nns = sizeof(out->nodename);
	GetComputerNameA(out->nodename, &nns);

	SYSTEM_INFO si;
	GetSystemInfo(&si);

	OSVERSIONINFOEXA oi;
	oi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEXA);
	if (!GetVersionExA((OSVERSIONINFOA*)&oi)) {
		DWORD dwVersion = GetVersion();
		oi.dwMajorVersion = (DWORD)(LOBYTE(LOWORD(dwVersion)));
		oi.dwMinorVersion = (DWORD)(HIBYTE(LOWORD(dwVersion)));

		if (dwVersion < 0x80000000)
			oi.dwBuildNumber = (DWORD)(HIWORD(dwVersion));
		else
			oi.dwBuildNumber = 0;
	}

	strncpy(out->sysname, "Windows", sizeof(out->sysname));
	strncpy(out->version, "Windows", sizeof(out->version));
	strncpy(out->machine,
		si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64 ? "x86_64" :
		si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_ARM   ? "arm" :
		si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_ARM64 ? "aarch64" :
		si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_IA64  ? "ia64" :
		si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_INTEL ? "i686" :
		"Unknown", sizeof(out->machine));

	snprintf(out->release, sizeof(out->release),
		"%d.%d.%d", oi.dwMajorVersion, oi.dwMinorVersion, oi.dwBuildNumber);
	return 0;
};

#endif
