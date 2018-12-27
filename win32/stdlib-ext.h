#pragma once
#ifdef _WIN32

int mkstemp(char *unused);

// note: we ignore filename template
int mkstemp(char *unused)
{
	CHAR path[MAX_PATH-14];
	DWORD len = GetTempPath(sizeof(path)/sizeof(path[0]), (LPSTR)path);
	if (len == 0 || len > sizeof(path)/sizeof(path[0]))
		path[0] = 0;

	CHAR filename[MAX_PATH];

	UINT x = GetTempFileName(path, "olvm", 0, (LPSTR)filename);
	if (x == 0)
		return -1;

	return open(filename, O_WRONLY, (S_IRUSR | S_IWUSR));
}

#endif
