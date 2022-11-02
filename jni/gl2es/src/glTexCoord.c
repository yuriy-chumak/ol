#include <gl2es.h>
#include <lists.h>

static
void glTexCoord(float_t s, float_t t, float_t p, float_t q)
{
	// ILOG("glTexCoord2f(%f,%f,%f,%f)", s,t,r,q);
	int selected = GL2.lists_selected;
	list_t* list = List(selected);
	attrib_t* attrib = List_Attrib(list, list->cursor);

	// attrib->texcoord.s = s;
	// attrib->texcoord.t = t;
	// attrib->texcoord.p = p;
	// attrib->texcoord.q = q;

	(void) 0;
}

// -=( OpenGL )=--------------------
#define S 0
#define T 0
#define P 0
#define Q 1

__attribute__((visibility("default")))
void glTexCoord4f(GLfloat s, GLfloat t, GLfloat p, GLfloat q)
{
	(void) glTexCoord((float_t)s, (float_t)t, (float_t)p, (float_t)q);
}

__attribute__((visibility("default")))
void glTexCoord3f(GLfloat s, GLfloat t, GLfloat p)
{
	(void) glTexCoord((float_t)s, (float_t)t, (float_t)p, (float_t)Q);
}

__attribute__((visibility("default")))
void glTexCoord2f(GLfloat s, GLfloat t)
{
	(void) glTexCoord((float_t)s, (float_t)t, (float_t)P, (float_t)Q);
}

__attribute__((visibility("default")))
void glTexCoord1f(GLfloat s)
{
	(void) glTexCoord((float_t)s, (float_t)T, (float_t)P, (float_t)Q);
}
