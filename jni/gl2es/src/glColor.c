#include <gl2es.h>
#include <lists.h>

static
void glColor(float_t r, float_t g, float_t b, float_t a)
{
	int selected = GL2.lists_selected;
	list_t* list = List(selected);
	attrib_t* attrib = List_Attrib(list, list->cursor);

	attrib->color.r = GL2.color.r = r;
	attrib->color.g = GL2.color.g = g;
	attrib->color.b = GL2.color.b = b;
	attrib->color.a = GL2.color.a = a;

	(void) 0;
}

// -=( OpenGL )=--------------------
#define R 1
#define G 1
#define B 1
#define A 1

__attribute__((visibility("default")))
void glColor4f(GLfloat r, GLfloat g, GLfloat b, GLfloat a)
{
	(void) glColor((float_t)r, (float_t)g, (float_t)b, (float_t)a);
}

__attribute__((visibility("default")))
void glColor3f(GLfloat r, GLfloat g, GLfloat b)
{
	(void) glColor((float_t)r, (float_t)g, (float_t)b, (float_t)A);
}

__attribute__((visibility("default")))
void glColor2f(GLfloat r, GLfloat g)
{
	(void) glColor((float_t)r, (float_t)g, (float_t)B, (float_t)A);
}

__attribute__((visibility("default")))
void glColor1f(GLfloat r)
{
	(void) glColor((float_t)r, (float_t)G, (float_t)B, (float_t)A);
}
