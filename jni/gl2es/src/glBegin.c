#include <gl2es.h>

__attribute__((visibility("default")))
void glBegin(GLenum mode)
{
	GL2.begin_mode = mode;

	// текущий список перезапускаем на каждый glBegin
	if (GL2.lists_selected == 0) {
		list_t* list = List(0);
		list->cursor = 0;
	}

	(void) 0;
}