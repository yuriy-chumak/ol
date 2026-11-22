#pragma once

#define F_GETFD		1	/* get close_on_exec */
#define F_SETFL		4	/* set file->f_flags */

extern int fcntl(int fd, int cmd, ...);
