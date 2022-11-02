#include <gl2es.h>

#include <assert.h>

__attribute__((visibility("default")))
void glCallList(GLuint id)
{
	list_t* list = List(id);

	GLint program = 0;
	real_glGetIntegerv(GL_CURRENT_PROGRAM, &program);

	GLint gl_Vertex = glGetAttribLocation(program, "gl2es_Vertex");
	if (gl_Vertex != -1) {
		real_glVertexAttribPointer(gl_Vertex, 4, GL_FLOAT, GL_FALSE,
				sizeof(attrib_t),
				&list->attribs[0].vertex);
		real_glEnableVertexAttribArray(gl_Vertex);
	}

	GLint gl_Color = glGetAttribLocation(program, "gl2es_Color");
	if (gl_Color != -1) {
		real_glVertexAttribPointer(gl_Color, 4, GL_FLOAT, GL_FALSE,
				sizeof(attrib_t),
				&list->attribs[0].color);
		real_glEnableVertexAttribArray(gl_Color);
	}

	// real_glEnableVertexAttribArray(gl2es_Vertex);
	// real_glDrawArrays(GL_TRIANGLES, 0, 3);
	// ... others, others, others...

	assert (GL2.begin_mode == GL_POINTS ||
			GL2.begin_mode == GL_LINE_STRIP || GL2.begin_mode == GL_LINE_LOOP || GL2.begin_mode == GL_LINES ||
			GL2.begin_mode == GL_TRIANGLE_STRIP || GL2.begin_mode == GL_TRIANGLE_FAN || GL2.begin_mode == GL_TRIANGLES);
	glDrawArrays(GL_TRIANGLES, 0, list->cursor);

	(void) 0;
}