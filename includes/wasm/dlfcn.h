#pragma once

extern void *dlopen(const char *path, int flags);
extern void *dlsym(void *handle, const char *symbol);

extern int dlclose(...);
extern int dlerror(...);
