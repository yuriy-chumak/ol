#include "log.h"

#include <GLES3/gl3.h>
#include <GLES2/gl2ext.h>
#include <GLES3/gl3ext.h>

#include <gl2es.h>
#include <string.h>
#include <stdlib.h>

state_t GL2;

void Init()
{
	memset(&GL2, 0, sizeof(GL2));

	// create default display list for (glBegin/glEnd)
	GL2.lists = calloc(0, sizeof(list_t)); // yes zero
	glGenLists(1);

	// ...
}

void Done()
{

}
