#pragma once

#include <gles-2.0.h>

typedef struct float4_t {
	union {
		float_t x;
		float_t r;
		float_t s;
	};
	union {
		float_t y;
		float_t g;
		float_t t;
	};
	union {
		float_t z;
		float_t b;
		float_t p;
	};
	union {
		float_t w;
		float_t a;
		float_t q;
	};
} float4_t;

typedef struct attrib_t {
	float4_t vertex;
	float4_t texcoord;
	float4_t color;

} attrib_t;

typedef struct list_t { // todo: begin, end, ptr? without size, etc.?
	size_t size;  // общий размер буфера
	size_t cursor;
	attrib_t* attribs;
	// ...
} list_t;

list_t* List(size_t number);
attrib_t* List_Attrib(list_t* list, size_t number);

// OpenGL:
GLuint glGenLists(GLsizei range);
