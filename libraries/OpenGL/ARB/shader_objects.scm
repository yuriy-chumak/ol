; ==========================================================================
; ARB_shader_objects
;
;     https://registry.khronos.org/OpenGL/extensions/ARB/ARB_shader_objects.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB shader_objects)

(import (scheme core)
   (OpenGL platform))

; --------------------------------------------------------------------------
; Dependencies
(import
   (OpenGL 1.0))

; --------------------------------------------------------------------------
(export ARB_shader_objects

   GLcharARB* GLcharARB**
   GLhandleARB GLhandleARB*

; --------------------------------------------------------------------------
; New Procedures and Functions

   glDeleteObjectARB
   glGetHandleARB
   glDetachObjectARB
   glCreateShaderObjectARB

   glShaderSourceARB
   glCompileShaderARB

   glCreateProgramObjectARB
   glAttachObjectARB
   glLinkProgramARB
   glUseProgramObjectARB
   glValidateProgramARB

   glUniform1fARB
   glUniform2fARB
   glUniform3fARB
   glUniform4fARB
   glUniform1iARB
   glUniform2iARB
   glUniform3iARB
   glUniform4iARB
   glUniform1fvARB
   glUniform2fvARB
   glUniform3fvARB
   glUniform4fvARB
   glUniform1ivARB
   glUniform2ivARB
   glUniform3ivARB
   glUniform4ivARB
   glUniformMatrix2fvARB
   glUniformMatrix3fvARB
   glUniformMatrix4fvARB

   glGetObjectParameterfvARB
   glGetObjectParameterivARB

   glGetInfoLogARB
   glGetAttachedObjectsARB
   glGetUniformLocationARB
   glGetActiveUniformARB
   glGetUniformfvARB
   glGetUniformivARB
   glGetShaderSourceARB

; --------------------------------------------------------------------------
; New Tokens

   GL_PROGRAM_OBJECT_ARB

   GL_OBJECT_TYPE_ARB
   GL_OBJECT_SUBTYPE_ARB
   GL_OBJECT_DELETE_STATUS_ARB
   GL_OBJECT_COMPILE_STATUS_ARB
   GL_OBJECT_LINK_STATUS_ARB
   GL_OBJECT_VALIDATE_STATUS_ARB
   GL_OBJECT_INFO_LOG_LENGTH_ARB
   GL_OBJECT_ATTACHED_OBJECTS_ARB
   GL_OBJECT_ACTIVE_UNIFORMS_ARB
   GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB
   GL_OBJECT_SHADER_SOURCE_LENGTH_ARB

   GL_SHADER_OBJECT_ARB

   GL_FLOAT
   GL_FLOAT_VEC2_ARB
   GL_FLOAT_VEC3_ARB
   GL_FLOAT_VEC4_ARB
   GL_INT
   GL_INT_VEC2_ARB
   GL_INT_VEC3_ARB
   GL_INT_VEC4_ARB
   GL_BOOL_ARB
   GL_BOOL_VEC2_ARB
   GL_BOOL_VEC3_ARB
   GL_BOOL_VEC4_ARB
   GL_FLOAT_MAT2_ARB
   GL_FLOAT_MAT3_ARB
   GL_FLOAT_MAT4_ARB
   GL_SAMPLER_1D_ARB
   GL_SAMPLER_2D_ARB
   GL_SAMPLER_3D_ARB
   GL_SAMPLER_CUBE_ARB
   GL_SAMPLER_1D_SHADOW_ARB
   GL_SAMPLER_2D_SHADOW_ARB
   GL_SAMPLER_2D_RECT_ARB
   GL_SAMPLER_2D_RECT_SHADOW_ARB
)

; --------------------------------------------------------------------------
(begin
   (define ARB_shader_objects (gl:QueryExtension "GL_ARB_shader_objects"))

   (define GLcharARB* type-string)
   (define GLcharARB** (fft* GLcharARB*))
   (define GLhandleARB type-vptr)
   (define GLhandleARB* (fft* GLhandleARB)) ;?
   (define GLhandleARB& (fft& GLhandleARB)) ;?
   (define GLfloat& (fft& GLfloat))
   (define GLsizei& (fft& GLsizei))
   (define GLsizei* (fft* GLsizei))
   (define GLenum* (fft* GLenum))

   (setq GL gl:GetProcAddress)
   (define glDeleteObjectARB (GL GLvoid "glDeleteObjectARB" GLhandleARB))
   (define glGetHandleARB (GL GLhandleARB "glGetHandleARB" GLenum))
   (define glDetachObjectARB (GL GLvoid "glDetachObjectARB" GLhandleARB GLhandleARB))
   (define glCreateShaderObjectARB (GL GLhandleARB "glCreateShaderObjectARB" GLenum))
   (define glShaderSourceARB (GL GLvoid "glShaderSourceARB" GLhandleARB GLsizei GLcharARB** GLint*))
   (define glCompileShaderARB (GL GLvoid "glCompileShaderARB" GLhandleARB))
   (define glCreateProgramObjectARB (GL GLhandleARB "glCreateProgramObjectARB"))
   (define glAttachObjectARB (GL GLvoid "glAttachObjectARB" GLhandleARB GLhandleARB))
   (define glLinkProgramARB (GL GLvoid "glLinkProgramARB" GLhandleARB))
   (define glUseProgramObjectARB (GL GLvoid "glUseProgramObjectARB" GLhandleARB))
   (define glValidateProgramARB (GL GLvoid "glValidateProgramARB" GLhandleARB))
   (define glUniform1fARB (GL GLvoid "glUniform1fARB" GLint GLfloat))
   (define glUniform2fARB (GL GLvoid "glUniform2fARB" GLint GLfloat GLfloat))
   (define glUniform3fARB (GL GLvoid "glUniform3fARB" GLint GLfloat GLfloat GLfloat))
   (define glUniform4fARB (GL GLvoid "glUniform4fARB" GLint GLfloat GLfloat GLfloat GLfloat))
   (define glUniform1iARB (GL GLvoid "glUniform1iARB" GLint GLint))
   (define glUniform2iARB (GL GLvoid "glUniform2iARB" GLint GLint GLint))
   (define glUniform3iARB (GL GLvoid "glUniform3iARB" GLint GLint GLint GLint))
   (define glUniform4iARB (GL GLvoid "glUniform4iARB" GLint GLint GLint GLint GLint))
   (define glUniform1fvARB (GL GLvoid "glUniform1fvARB" GLint GLsizei GLfloat*))
   (define glUniform2fvARB (GL GLvoid "glUniform2fvARB" GLint GLsizei GLfloat*))
   (define glUniform3fvARB (GL GLvoid "glUniform3fvARB" GLint GLsizei GLfloat*))
   (define glUniform4fvARB (GL GLvoid "glUniform4fvARB" GLint GLsizei GLfloat*))
   (define glUniform1ivARB (GL GLvoid "glUniform1ivARB" GLint GLsizei GLint*))
   (define glUniform2ivARB (GL GLvoid "glUniform2ivARB" GLint GLsizei GLint*))
   (define glUniform3ivARB (GL GLvoid "glUniform3ivARB" GLint GLsizei GLint*))
   (define glUniform4ivARB (GL GLvoid "glUniform4ivARB" GLint GLsizei GLint*))
   (define glUniformMatrix2fvARB (GL GLvoid "glUniformMatrix2fvARB" GLint GLsizei GLboolean GLfloat*))
   (define glUniformMatrix3fvARB (GL GLvoid "glUniformMatrix3fvARB" GLint GLsizei GLboolean GLfloat*))
   (define glUniformMatrix4fvARB (GL GLvoid "glUniformMatrix4fvARB" GLint GLsizei GLboolean GLfloat*))

   (define glGetObjectParameterfvARB (GL GLvoid "glGetObjectParameterfvARB" GLhandleARB GLenum GLfloat&))
   (define glGetObjectParameterivARB (GL GLvoid "glGetObjectParameterivARB" GLhandleARB GLenum GLint&))

   (define glGetInfoLogARB (GL GLvoid "glGetInfoLogARB" GLhandleARB GLsizei GLsizei& type-bytevector))
   (define glGetAttachedObjectsARB (GL GLvoid "glGetAttachedObjectsARB" GLhandleARB GLsizei GLsizei& GLhandleARB&)) ;?

   (define glGetUniformLocationARB (GL GLint "glGetUniformLocationARB" GLhandleARB GLcharARB*))

   (define glGetActiveUniformARB (GL GLvoid "glGetActiveUniformARB" GLhandleARB GLuint GLsizei GLsizei* GLint* GLenum* GLcharARB*))
   (define glGetUniformfvARB (GL GLvoid "glGetUniformfvARB" GLhandleARB GLint GLfloat*))
   (define glGetUniformivARB (GL GLvoid "glGetUniformivARB" GLhandleARB GLint GLint*))
   (define glGetShaderSourceARB (GL GLvoid "glGetShaderSourceARB" GLhandleARB GLsizei GLsizei* GLcharARB*))

   (define GL_PROGRAM_OBJECT_ARB                              #x8B40)

   (define GL_OBJECT_TYPE_ARB                                 #x8B4E)
   (define GL_OBJECT_SUBTYPE_ARB                              #x8B4F)
   (define GL_OBJECT_DELETE_STATUS_ARB                        #x8B80)
   (define GL_OBJECT_COMPILE_STATUS_ARB                       #x8B81)
   (define GL_OBJECT_LINK_STATUS_ARB                          #x8B82)
   (define GL_OBJECT_VALIDATE_STATUS_ARB                      #x8B83)
   (define GL_OBJECT_INFO_LOG_LENGTH_ARB                      #x8B84)
   (define GL_OBJECT_ATTACHED_OBJECTS_ARB                     #x8B85)
   (define GL_OBJECT_ACTIVE_UNIFORMS_ARB                      #x8B86)
   (define GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB            #x8B87)
   (define GL_OBJECT_SHADER_SOURCE_LENGTH_ARB                 #x8B88)

   (define GL_SHADER_OBJECT_ARB                               #x8B48)

   (define GL_FLOAT                                           #x1406)
   (define GL_FLOAT_VEC2_ARB                                  #x8B50)
   (define GL_FLOAT_VEC3_ARB                                  #x8B51)
   (define GL_FLOAT_VEC4_ARB                                  #x8B52)
   (define GL_INT                                             #x1404)
   (define GL_INT_VEC2_ARB                                    #x8B53)
   (define GL_INT_VEC3_ARB                                    #x8B54)
   (define GL_INT_VEC4_ARB                                    #x8B55)
   (define GL_BOOL_ARB                                        #x8B56)
   (define GL_BOOL_VEC2_ARB                                   #x8B57)
   (define GL_BOOL_VEC3_ARB                                   #x8B58)
   (define GL_BOOL_VEC4_ARB                                   #x8B59)
   (define GL_FLOAT_MAT2_ARB                                  #x8B5A)
   (define GL_FLOAT_MAT3_ARB                                  #x8B5B)
   (define GL_FLOAT_MAT4_ARB                                  #x8B5C)
   (define GL_SAMPLER_1D_ARB                                  #x8B5D)
   (define GL_SAMPLER_2D_ARB                                  #x8B5E)
   (define GL_SAMPLER_3D_ARB                                  #x8B5F)
   (define GL_SAMPLER_CUBE_ARB                                #x8B60)
   (define GL_SAMPLER_1D_SHADOW_ARB                           #x8B61)
   (define GL_SAMPLER_2D_SHADOW_ARB                           #x8B62)
   (define GL_SAMPLER_2D_RECT_ARB                             #x8B63)
   (define GL_SAMPLER_2D_RECT_SHADOW_ARB                      #x8B64)

))
