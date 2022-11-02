#include <gl2es.h>

__attribute__((visibility("default")))
void gl2es_glShaderSource(
    GLuint shader, 
    GLsizei count, 
    const GLchar * const * string, 
    const GLint * length)
{
   ILOG("glShaderSource()");
   (void) real_glShaderSource(shader, count, string, length);
}

// TODO: add glHint() for "precision mediump float;", for example