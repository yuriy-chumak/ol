#include <gl2es.h>

__attribute__((visibility("default")))
void glEnd(void)
{
	// add possible final vertices, and update lists_mode to the final one (triangles, points, etc.)

	int selected = GL2.lists_selected;
	if (selected == 0)
		glCallList(0);

	(void) 0;
}
