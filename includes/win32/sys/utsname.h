#pragma once
#ifdef _WIN32

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
	DWORD nodenamesize = sizeof(out->nodename);
	GetComputerNameA(out->nodename, &nodenamesize);

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
