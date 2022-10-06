#pragma once
#ifdef _WIN32
// seen at https://github.com/dlfcn-win32/dlfcn-win32/blob/master/dlfcn.c

static DWORD dlerrno = 0;
static
void *dlopen(const char *filename, int mode/*unused*/)
{
	HMODULE hModule;
	// Do not let Windows display the critical-error-handler message box */
	// UINT uMode = SetErrorMode( SEM_FAILCRITICALERRORS );

	UINT errorMode = SetErrorMode(SEM_FAILCRITICALERRORS);
	if (filename == 0) {
		hModule = GetModuleHandle(NULL);
	}
	else {
		/* POSIX says the search path is implementation-defined.
		 * LOAD_WITH_ALTERED_SEARCH_PATH is used to make it behave more closely
		 * to UNIX's search paths (start with system folders instead of current
		 * folder).
		 */
		SetErrorMode(errorMode | SEM_FAILCRITICALERRORS);
		hModule = LoadLibraryEx((LPSTR)filename, NULL, LOAD_WITH_ALTERED_SEARCH_PATH);
	}

	dlerrno = GetLastError();
	SetErrorMode(errorMode);
	return hModule;
}

static
int dlclose(void *handle)
{
	return FreeLibrary((HMODULE)handle);
}

static
void *dlsym(void *handle, const char *name)
{
	FARPROC function;

	function = GetProcAddress((HMODULE)handle, name);
	return function;
}

static
char* dlerror() {
//	size_t size = FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, dlerrno, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT))
	return "description unavailable";
}

#endif
