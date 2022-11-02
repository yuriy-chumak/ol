#include <gl2es.h>
#include <lists.h>

#include <stdlib.h>
#include <assert.h>

__attribute__((visibility("default")))
void glEndList(void)
{
	assert (GL2.lists_selected != 0);

	// free the space after end of real list size
	list_t* list = List(GL2.lists_selected);
	size_t number = list->cursor;
	list->attribs = realloc(list->attribs, list->cursor * sizeof(attrib_t));
	list->size = number;

	if (GL2.lists_mode == GL_COMPILE_AND_EXECUTE)
		glCallList(GL2.lists_selected);

	GL2.lists_mode = 0;
	GL2.lists_selected = 0;
}
