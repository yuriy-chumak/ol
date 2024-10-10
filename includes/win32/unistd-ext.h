#pragma once
#ifdef _WIN32

#define lstat stat

int pipe(int pipes[2]);

int pipe(int pipes[2])
{
	SECURITY_ATTRIBUTES saAttr;
	saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
	saAttr.bInheritHandle = TRUE;
	saAttr.lpSecurityDescriptor = NULL; 

	HANDLE i, o;
	if (CreatePipe(&i, &o, &saAttr, 1024)) {
		pipes[0] = _open_osfhandle((intptr_t)i, _O_APPEND | _O_RDONLY);
		pipes[1] = _open_osfhandle((intptr_t)o, _O_APPEND | _O_WRONLY);
		return 0;
	}
	return -1;
}

// -----------------------------------------------
// non-blocking win32 terminal input
// 1 producer, 1 consumer

char char_buffer[4096];
ssize_t buffer_chars=0;

HANDLE ghReadEvent = NULL;
HANDLE ConsoleReadThread = NULL;

static
DWORD WINAPI ConsoleReadProc(CONST LPVOID lpParam)
{
	while (1) {
		// must wait to avoid storing old cursor position
		WaitForSingleObject(ghReadEvent, INFINITE);
		if (buffer_chars) // don't read next portion if haven't processed already read
			continue;

		buffer_chars =
			_read(STDIN_FILENO, &char_buffer, sizeof(char_buffer));
	}
}

static
void win32_io_setup()
{
	ghReadEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
	ConsoleReadThread = CreateThread(NULL, 0, &ConsoleReadProc, NULL, 0, NULL);
}

// note: _kbhit() is not working under raw wine but working under wine cmd
static
ssize_t readEx(int fd, void *buf, size_t size)
{
	int got = 0;
#ifdef _WIN64
	int chunk_size = size;
#else
	int chunk_size = 24 * 1024;
#endif

	// asynchronous terminal input:
	if (fd == STDIN_FILENO && _isatty(fd) && ConsoleReadThread) {
		// todo: handle "size"
		int chars = 0;
		if ((chars = buffer_chars) > 0) {
			memcpy(buf, &char_buffer, chars);

			buffer_chars = 0; // got it!
			return chars;
		}
		else {
			SetEvent(ghReadEvent);
			errno = EAGAIN;
			return -1;
		}
	}
	else
		// regular reading
		got = _read(fd, (char *) buf, min(chunk_size, size));

	if (got == -1) {
		switch (errno) {
#if HAVE_SOCKETS
		case EBADF: // have we tried to read from socket?
			got = recv(fd, (char *) buf, size, 0);
			if (got < 0 && WSAGetLastError() == WSAEWOULDBLOCK)
				errno = EAGAIN;
			break;
#endif

		// https://lists.gnu.org/archive/html/bug-gnulib/2011-04/msg00170.html
		// The other failure of the non-blocking I/O on pipes test on mingw is because
		// when read() is called on a non-blocking pipe fd with an empty buffer, it
		// fails with EINVAL. Whereas POSIX says that it should fail with EAGAIN.
		case EINVAL: {
			HANDLE handle = (HANDLE)_get_osfhandle(fd);
			// pipe?
			if (GetFileType(handle) == FILE_TYPE_PIPE) {
				DWORD state;
				// pipe in non-blocking mode?
				if (GetNamedPipeHandleState (handle, &state, NULL, NULL, NULL, NULL, 0)
						  && (state & PIPE_NOWAIT) != 0) {
					errno = EAGAIN;
				}

			}
			break; }
		}
	}
	return got;
}
#define read readEx

// write workaround:
static
ssize_t writeEx(int fd, void *buf, size_t size)
{
	int wrote;

	// regular writing (files and pipes)
	wrote = write(fd, buf, size);

#if HAVE_SOCKETS
	// sockets workaround
	if (wrote == -1 && errno == EBADF) {
		wrote = send(fd, buf, size, 0);
	}
#endif

	return wrote;
}
#define write writeEx

int fsync(int fd)
{
	HANDLE h = (HANDLE) _get_osfhandle(fd);

	if (h == INVALID_HANDLE_VALUE) {
		errno = EBADF;
		return -1;
	}
	if (!FlushFileBuffers(h)) {
		switch (GetLastError()) {
			case ERROR_ACCESS_DENIED:
				return 0;
			case ERROR_INVALID_HANDLE: // fsync a tty?
				errno = EINVAL;
				break;
			default:
				errno = EIO;
		}
		return -1;
	}
	return 0;
}

// --------------------------------------------------------
// -=( fork )=---------------------------------------------
// sample implementation can be found at
// https://github.com/jonclayden/multicore/blob/master/src/forknt.c
// originally from: "Windows NT/2000 native API reference" ISBN 1-57870-199-6.

// TBD.

#endif
