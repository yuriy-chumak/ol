#pragma once
#ifdef _WIN32

size_t
sendfile(int out_fd, int in_fd, off_t *offset, size_t count);

#endif
