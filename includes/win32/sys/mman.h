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

int memfd_create (const char* name, unsigned int flags);

#endif
