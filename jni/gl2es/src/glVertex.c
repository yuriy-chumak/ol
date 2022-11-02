#include <gl2es.h>
#include <lists.h>

#include <stdlib.h>

static
void glVertex(float_t x, float_t y, float_t z, float_t w)
{
	// ILOG("glVertex(%f,%f,%f,%f)", x,y,z,w);
	int selected = GL2.lists_selected;
	list_t* list = List(selected);
	attrib_t* attrib;

	attrib = List_Attrib(list, list->cursor);
	attrib->vertex.x = x;
	attrib->vertex.y = y;
	attrib->vertex.z = z;
	attrib->vertex.w = w;

	list->cursor++;

	// grow attribs vector if needed
	size_t size = list->size;
	size_t number = list->cursor;
	if (number >= size) {
		size_t new_size = number + number/3;
		list->attribs = realloc(list->attribs, new_size * sizeof(attrib_t));
		list->size = new_size;
	}

	// copy color data
	attrib = List_Attrib(list, list->cursor);
	attrib->color.r = GL2.color.r;
	attrib->color.g = GL2.color.g;
	attrib->color.b = GL2.color.b;
	attrib->color.a = GL2.color.a;

	(void) 0;
}

// -=( OpenGL )=--------------------
#define X 0
#define Y 0
#define Z 0
#define W 1

__attribute__((visibility("default")))
void glVertex4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w)
{
	(void) glVertex((float_t)x, (float_t)y, (float_t)z, (float_t)w);
}

__attribute__((visibility("default")))
void glVertex3f(GLfloat x, GLfloat y, GLfloat z)
{
	(void) glVertex((float_t)x, (float_t)y, (float_t)z, (float_t)W);
}

__attribute__((visibility("default")))
void glVertex2f(GLfloat x, GLfloat y)
{
	(void) glVertex((float_t)x, (float_t)y, (float_t)Z, (float_t)W);
}

__attribute__((visibility("default")))
void glVertex1f(GLfloat x)
{
	(void) glVertex((float_t)x, (float_t)Y, (float_t)Z, (float_t)W);
}
