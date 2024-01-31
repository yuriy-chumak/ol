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

char *getenv(const char *name)
{
	static char value[32767];
	if (GetEnvironmentVariable(name, (char*)&value, sizeof(value)))
		return (char*)&value;
	return 0;
}

int setenv(const char *name, const char *value, int overwrite)
{
	if (!overwrite && GetEnvironmentVariable(name, NULL, 0) > 0)
		return 0;

	if (!SetEnvironmentVariable(name, value))
		return -1;

	return 0;
}

int unsetenv(const char *name)
{
	return SetEnvironmentVariable(name, NULL);
}

#endif
