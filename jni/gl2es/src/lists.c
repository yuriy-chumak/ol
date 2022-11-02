#include <gl2es.h>

#include <assert.h>
#include <stdlib.h>

extern state_t GL2;

list_t* List(size_t id)
{
	assert (id < GL2.lists_count);

	return &GL2.lists[id];
}

// size_t Selected()
// {
// 	return GL2.lists_selected;
// }

attrib_t* List_Attrib(list_t* list, size_t number)
{
	assert (number < list->size);

	return &list->attribs[number];
}
