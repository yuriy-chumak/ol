// TAR archives in path support

#define _GNU_SOURCE
#include <sys/mman.h> // memfd_create
#include <sys/sendfile.h> // sendfile

#include <ol/vm.h>

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

int tar_open(const char *filename, int flags, int mode, void* userdata) {
	(void) userdata;

	int fd = open(filename, flags, mode);
	if (fd != -1)
		return fd;

	// no file? let's try to search in tars
	// folder/ -> folder.tar
	char path[PATH_MAX+5]; // 5 for ".tar"
	char* ptr = (char*)filename;

	while (*ptr) {
		if (*ptr == '/') {
			strncpy(path, filename, ptr-filename);
			strcpy(&path[ptr-filename], ".tar"); // todo strncpy(..., ".tar", sizeof(path)-...)
			
			int tar = open(path, O_RDONLY);
			if (tar != -1) {
				ssize_t len = 0;
				char buffer[0x200];
				for(;;) {
					len = read(tar, buffer, sizeof(buffer));
					if (len != sizeof(buffer)) // eof or invalid file
						break;
					// assert(buffer[0x100..] == "\0ustar\0")

					int size = 0;
					for (int i = 124; i < 124+11; i++)
						size = size * 8 + buffer[i] - '0';

					if (strcmp(buffer, ptr+1) == 0) {
						fd = memfd_create(filename, 0);
						sendfile(fd, tar, NULL, size);
						close(tar);
						lseek(fd, 0, SEEK_SET);
						return fd;
					}
					else {
						lseek(tar, (size + 0x1FF) & -0x200, SEEK_CUR);
					}
				}
				close(tar);
			}
		}
		ptr++;
	}
	return -1;
}
