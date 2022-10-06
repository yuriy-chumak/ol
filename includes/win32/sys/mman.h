#pragma once
#ifdef _WIN32

#include <io.h>
#include <fcntl.h>

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

	HANDLE handle = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, 1, NULL);
	return _open_osfhandle((intptr_t) handle, _O_TEXT|_O_RDWR);
}

/*static
void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset)
{
	assert (addr == 0);
	assert (prot == PROT_READ | PROT_WRITE | PROT_EXEC);
	assert (fd == -1); assert (offset == 0);

	HANDLE mh = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_EXECUTE_READWRITE,
				0, length, NULL);
	if (!mh)
		return (char*)-1;
	void* ptr = MapViewOfFile(mh, FILE_MAP_ALL_ACCESS, 0, 0, length);
	CloseHandle(mh);
	if (!ptr)
		return (char*)-1;

	return ptr;
}*/

#endif
