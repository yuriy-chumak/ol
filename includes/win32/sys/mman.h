#pragma once
#ifdef _WIN32

#include <io.h>
#include <fcntl.h>
#include <assert.h>

#define PROT_READ 1
#define PROT_WRITE 2
#define PROT_EXEC 4

#define MAP_ANONYMOUS 0x20
#define MAP_PRIVATE 0x02

static
int memfd_create (char* name, unsigned int flags)
{
	(void) name;
	assert (flags == 0);

	TCHAR path[MAX_PATH];
	GetTempPath(MAX_PATH, path);
	TCHAR file[MAX_PATH];
	GetTempFileName(path, "memfd_olvm", 0, file);

	HANDLE handle = CreateFile(file, GENERIC_READ | GENERIC_WRITE, 0,NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
	return _open_osfhandle((intptr_t) handle, 0);
}

#endif
