#pragma once

static
void* wasm_memmove(void *dest, void* src, size_t n)
{
	// D("wasm_memmove(%p, %p, %d)", dest, src, n);
	unsigned char *d = (unsigned char *)dest;
	unsigned char *s = (unsigned char *)src;
	if (d < s)
		while (n--)
			*d++ = *s++;
	else {
		d += n;
		s += n;
		while (n--)
			*--d = *--s;
	}
	return dest;
}
#define memmove wasm_memmove

// __attribute__((optimize("no-tree-loop-distribute-patterns")))
static
void* wasm_memcpy(void *dest, const void *src, size_t n)
{
	unsigned char *d = (unsigned char *)dest;
	unsigned char *s = (unsigned char *)src;
	while (n--)
		*d++ = *s++;
	return dest;
}
#define memcpy wasm_memcpy
/*void* memcpy(void *dest, void* src, size_t n)
{
	//D("memcpy(%p, %p, %d)", dest, src, n);
	unsigned char *d = (unsigned char *)dest;
	unsigned char *s = (unsigned char *)src;
	while (n--)
		*d++ = *s++;
	return dest;
}*/

static
size_t strlen(char *str)
{
	unsigned int l = 0;
	while (*str++) l++;

	return l;
}

static
int strcmp(unsigned char *str1, unsigned char *str2)
{
	unsigned char a, b;
	do {
		a = *str1++, b = *str2++;
		if (a < b) return -1;
		if (a > b) return +1;
	}
	while (a && b);
	return 0;
}

static
int strncmp(unsigned char *str1, unsigned char *str2, size_t n)
{
	int i = 0;
	unsigned char a, b;
	do {
		a = *str1++, b = *str2++;
		if (a < b) return -1;
		if (a > b) return +1;
	}
	while (a && b && i++ < n);
	return 0;
}

static
void* memset (void *dest, unsigned char ch, size_t count)
{
	// D("memset(%p, %d, %d)", dest, ch, count);
	unsigned char *p = (unsigned char *)dest;
	while (count--)
		*p++ = ch;

	return dest;
}
