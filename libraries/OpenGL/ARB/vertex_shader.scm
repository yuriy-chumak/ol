; ===========================================================================
; ARB_vertex_shader                                  (included in OpenGL 2.0)
;
;  https://registry.khronos.org/OpenGL/extensions/ARB/ARB_vertex_shader.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB vertex_shader)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
;
(import
   (OpenGL 1.0)
   (OpenGL ARB shader_objects))

; ---------------------------------------------------------------------------
(export ARB_vertex_shader

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glVertexAttrib1fARB
   glVertexAttrib1sARB
   glVertexAttrib1dARB
   glVertexAttrib2fARB
   glVertexAttrib2sARB
   glVertexAttrib2dARB
   glVertexAttrib3fARB
   glVertexAttrib3sARB
   glVertexAttrib3dARB
   glVertexAttrib4fARB
   glVertexAttrib4sARB
   glVertexAttrib4dARB
   glVertexAttrib4NubARB
   glVertexAttrib1fvARB
   glVertexAttrib1svARB
   glVertexAttrib1dvARB
   glVertexAttrib2fvARB
   glVertexAttrib2svARB
   glVertexAttrib2dvARB
   glVertexAttrib3fvARB
   glVertexAttrib3svARB
   glVertexAttrib3dvARB
   glVertexAttrib4fvARB
   glVertexAttrib4svARB
   glVertexAttrib4dvARB
   glVertexAttrib4ivARB
   glVertexAttrib4bvARB
   glVertexAttrib4ubvARB
   glVertexAttrib4usvARB
   glVertexAttrib4uivARB
   glVertexAttrib4NbvARB
   glVertexAttrib4NsvARB
   glVertexAttrib4NivARB
   glVertexAttrib4NubvARB
   glVertexAttrib4NusvARB
   glVertexAttrib4NuivARB
   glVertexAttribPointerARB

   glEnableVertexAttribArrayARB
   glDisableVertexAttribArrayARB

   glBindAttribLocationARB
   glGetActiveAttribARB
   glGetAttribLocationARB

   glGetVertexAttribdvARB
   glGetVertexAttribfvARB
   glGetVertexAttribivARB
   glGetVertexAttribPointervARB

; ---------------------------------------------------------------------------
; New Tokens

   GL_VERTEX_SHADER_ARB

   GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB
   GL_MAX_VARYING_FLOATS_ARB
   GL_MAX_VERTEX_ATTRIBS_ARB
   GL_MAX_TEXTURE_IMAGE_UNITS_ARB
   GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB
   GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB
   GL_MAX_TEXTURE_COORDS_ARB

   GL_VERTEX_PROGRAM_POINT_SIZE_ARB
   GL_VERTEX_PROGRAM_TWO_SIDE_ARB

   GL_OBJECT_ACTIVE_ATTRIBUTES_ARB
   GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB

   GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB
   GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB
   GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB
   GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB
   GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB
   GL_CURRENT_VERTEX_ATTRIB_ARB
   GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB

   GL_FLOAT
   GL_FLOAT_VEC2_ARB
   GL_FLOAT_VEC3_ARB
   GL_FLOAT_VEC4_ARB
   GL_FLOAT_MAT2_ARB
   GL_FLOAT_MAT3_ARB
   GL_FLOAT_MAT4_ARB

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_vertex_shader (gl:QueryExtension "GL_ARB_vertex_shader"))

   (setq GL gl:GetProcAddress)
   (define glVertexAttrib1fARB (GL GLvoid "glVertexAttrib1fARB" GLuint GLfloat))
   (define glVertexAttrib1sARB (GL GLvoid "glVertexAttrib1sARB" GLuint GLshort))
   (define glVertexAttrib1dARB (GL GLvoid "glVertexAttrib1dARB" GLuint GLdouble))
   (define glVertexAttrib2fARB (GL GLvoid "glVertexAttrib2fARB" GLuint GLfloat GLfloat))
   (define glVertexAttrib2sARB (GL GLvoid "glVertexAttrib2sARB" GLuint GLshort GLshort))
   (define glVertexAttrib2dARB (GL GLvoid "glVertexAttrib2dARB" GLuint GLdouble GLdouble))
   (define glVertexAttrib3fARB (GL GLvoid "glVertexAttrib3fARB" GLuint GLfloat GLfloat GLfloat))
   (define glVertexAttrib3sARB (GL GLvoid "glVertexAttrib3sARB" GLuint GLshort GLshort GLshort))
   (define glVertexAttrib3dARB (GL GLvoid "glVertexAttrib3dARB" GLuint GLdouble GLdouble GLdouble))
   (define glVertexAttrib4fARB (GL GLvoid "glVertexAttrib4fARB" GLuint GLfloat GLfloat GLfloat GLfloat))
   (define glVertexAttrib4sARB (GL GLvoid "glVertexAttrib4sARB" GLuint GLshort GLshort GLshort GLshort))
   (define glVertexAttrib4dARB (GL GLvoid "glVertexAttrib4dARB" GLuint GLdouble GLdouble GLdouble GLdouble))
   (define glVertexAttrib4NubARB (GL GLvoid "glVertexAttrib4NubARB" GLuint GLubyte GLubyte GLubyte GLubyte))
   (define glVertexAttrib1fvARB (GL GLvoid "glVertexAttrib1fvARB" GLuint GLfloat*))
   (define glVertexAttrib1svARB (GL GLvoid "glVertexAttrib1svARB" GLuint GLshort*))
   (define glVertexAttrib1dvARB (GL GLvoid "glVertexAttrib1dvARB" GLuint GLdouble*))
   (define glVertexAttrib2fvARB (GL GLvoid "glVertexAttrib2fvARB" GLuint GLfloat*))
   (define glVertexAttrib2svARB (GL GLvoid "glVertexAttrib2svARB" GLuint GLshort*))
   (define glVertexAttrib2dvARB (GL GLvoid "glVertexAttrib2dvARB" GLuint GLdouble*))
   (define glVertexAttrib3fvARB (GL GLvoid "glVertexAttrib3fvARB" GLuint GLfloat*))
   (define glVertexAttrib3svARB (GL GLvoid "glVertexAttrib3svARB" GLuint GLshort*))
   (define glVertexAttrib3dvARB (GL GLvoid "glVertexAttrib3dvARB" GLuint GLdouble*))
   (define glVertexAttrib4fvARB (GL GLvoid "glVertexAttrib4fvARB" GLuint GLfloat*))
   (define glVertexAttrib4svARB (GL GLvoid "glVertexAttrib4svARB" GLuint GLshort*))
   (define glVertexAttrib4dvARB (GL GLvoid "glVertexAttrib4dvARB" GLuint GLdouble*))
   (define glVertexAttrib4ivARB (GL GLvoid "glVertexAttrib4ivARB" GLuint GLint*))
   (define glVertexAttrib4bvARB (GL GLvoid "glVertexAttrib4bvARB" GLuint GLbyte*))
   (define glVertexAttrib4ubvARB (GL GLvoid "glVertexAttrib4ubvARB" GLuint GLubyte*))
   (define glVertexAttrib4usvARB (GL GLvoid "glVertexAttrib4usvARB" GLuint GLushort*))
   (define glVertexAttrib4uivARB (GL GLvoid "glVertexAttrib4uivARB" GLuint GLuint*))
   (define glVertexAttrib4NbvARB (GL GLvoid "glVertexAttrib4NbvARB" GLuint GLbyte*))
   (define glVertexAttrib4NsvARB (GL GLvoid "glVertexAttrib4NsvARB" GLuint GLshort*))
   (define glVertexAttrib4NivARB (GL GLvoid "glVertexAttrib4NivARB" GLuint GLint*))
   (define glVertexAttrib4NubvARB (GL GLvoid "glVertexAttrib4NubvARB" GLuint GLubyte*))
   (define glVertexAttrib4NusvARB (GL GLvoid "glVertexAttrib4NusvARB" GLuint GLushort*))
   (define glVertexAttrib4NuivARB (GL GLvoid "glVertexAttrib4NuivARB" GLuint GLuint*))

   (define glVertexAttribPointerARB (GL GLvoid "glVertexAttribPointerARB" GLuint GLint GLenum GLboolean GLsizei GLvoid*))

   (define glEnableVertexAttribArrayARB (GL GLvoid "glEnableVertexAttribArrayARB" GLuint))
   (define glDisableVertexAttribArrayARB (GL GLvoid "glDisableVertexAttribArrayARB" GLuint))

   (define glBindAttribLocationARB (GL GLvoid "glBindAttribLocationARB" GLhandleARB GLuint GLcharARB*))
   (define glGetActiveAttribARB (GL GLvoid "glGetActiveAttribARB" GLhandleARB GLuint GLsizei GLsizei* GLint* GLenum* GLcharARB*))
   (define glGetAttribLocationARB (GL GLint "glGetAttribLocationARB" GLhandleARB GLcharARB*))

   (define glGetVertexAttribdvARB (GL GLvoid "glGetVertexAttribdvARB" GLuint GLenum GLdouble*))
   (define glGetVertexAttribfvARB (GL GLvoid "glGetVertexAttribfvARB" GLuint GLenum GLfloat*))
   (define glGetVertexAttribivARB (GL GLvoid "glGetVertexAttribivARB" GLuint GLenum GLint*))
   (define glGetVertexAttribPointervARB (GL GLvoid "glGetVertexAttribPointervARB" GLuint GLenum GLvoid*))


   (define GL_VERTEX_SHADER_ARB                               #x8B31)

   (define GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB               #x8B4A)
   (define GL_MAX_VARYING_FLOATS_ARB                          #x8B4B)
   (define GL_MAX_VERTEX_ATTRIBS_ARB                          #x8869)
   (define GL_MAX_TEXTURE_IMAGE_UNITS_ARB                     #x8872)
   (define GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB              #x8B4C)
   (define GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB            #x8B4D)
   (define GL_MAX_TEXTURE_COORDS_ARB                          #x8871)

   (define GL_VERTEX_PROGRAM_POINT_SIZE_ARB                   #x8642)
   (define GL_VERTEX_PROGRAM_TWO_SIDE_ARB                     #x8643)

   (define GL_OBJECT_ACTIVE_ATTRIBUTES_ARB                    #x8B89)
   (define GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB          #x8B8A)

   (define GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB                 #x8622)
   (define GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB                    #x8623)
   (define GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB                  #x8624)
   (define GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB                    #x8625)
   (define GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB              #x886A)
   (define GL_CURRENT_VERTEX_ATTRIB_ARB                       #x8626)
   (define GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB                 #x8645)

   (define GL_FLOAT                                           #x1406)
   (define GL_FLOAT_VEC2_ARB                                  #x8B50)
   (define GL_FLOAT_VEC3_ARB                                  #x8B51)
   (define GL_FLOAT_VEC4_ARB                                  #x8B52)
   (define GL_FLOAT_MAT2_ARB                                  #x8B5A)
   (define GL_FLOAT_MAT3_ARB                                  #x8B5B)
   (define GL_FLOAT_MAT4_ARB                                  #x8B5C)

))
