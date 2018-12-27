#pragma once
# ifdef _WIN32

size_t
sendfile(int out_fd, int in_fd, off_t *offset, size_t count)
{
	char buf[2*4096];
	size_t toRead, numRead, numSent, totSent;

	totSent = 0;
	while (count > 0) {
		toRead = (sizeof(buf) < count) ? sizeof(buf) : count;

		// read
		numRead = read(in_fd, buf, toRead);
		if (numRead == -1) {
			return -1;
		}
		if (numRead == 0) {
			break;                      /* EOF */
		}

		// send
		resend:
		numSent = send(out_fd, buf, numRead, 0);
		if (numSent == SOCKET_ERROR) {
			int err = WSAGetLastError();
			if (err != WSAEWOULDBLOCK)
				return -1;

			Sleep(1);
			goto resend;
		}
		if (numSent == 0) {               /* Should never happen */
			return 0;
		}

		count -= numSent;
		totSent += numSent;
	}
	if (shutdown(out_fd, SD_SEND) == SOCKET_ERROR) {
		return -1;
	}
	return totSent;
}
# endif
