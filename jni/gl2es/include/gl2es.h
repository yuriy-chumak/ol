#pragma once

#include <log.h>
#include <gles-2.0.h>

// typedef double GLdouble;
// typedef double GLclampd;
#include <GL/gl.h>
#include <GL/glext.h>

// default inexact type
typedef GLfloat float_t;

// OpenGL Display Lists
#include <lists.h>

// OpenGL states:
typedef struct {
	int current_list; //?

	GLenum begin_mode; // GL_QUADS, GL_POINTS, ... 0 if not active
	float4_t color; // glColor

	// Display Lists (todo: stucture lists_t ?)
	list_t*lists;
	size_t lists_count;
	size_t lists_selected; // todo: rename to lists_current ?
	GLenum lists_mode; // mode between glNewList() and glEndList()
} state_t;

extern state_t GL2;
