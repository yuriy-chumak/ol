#pragma once

#define O_NONBLOCK	00004000
#define O_CREAT		00000100	/* not fcntl */
#define O_TRUNC		00001000	/* not fcntl */
#define O_RDWR		00000002
#define O_RDONLY	00000000

#define S_IRUSR 00400
#define S_IWUSR 00200
#define S_IFDIR  0040000
#define S_IFMT  00170000
#define S_ISDIR(m)	(((m) & S_IFMT) == S_IFDIR)

extern ssize_t read(int fd, void *buf, size_t count);
extern ssize_t write(int fd, const void *buf, size_t count);

extern int open(const char *path, int flags, int mode);
extern int close(int fd);

extern int lseek(...);
# define SEEK_SET	0	/* Seek from beginning of file.  */
# define SEEK_CUR	1	/* Seek from current position.  */
# define SEEK_END	2	/* Seek from end of file.  */
extern int lstat(...);
extern int fsync(...);
