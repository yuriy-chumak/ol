; OpenGL 2.0 (7 Sep 2004)
(define-library (OpenGL version-2-0)
(export
      (exports (OpenGL version-1-5))
   ; + ARB_shader_objects, heavily modified
   ; + ARB_vertex_shader, heavily modified
   ; + ARB_fragment_shader, heavily modified
   ; + ARB_shading_language_100, heavily modified
   ; + ARB_draw_buffers
   ; + ARB_texture_non_power_of_two
   ; + ARB_point_sprite
   ; + ATI_separate_stencil
   ; + EXT_stencil_two_side

   GL_VERSION_2_0

glCreateShader
GL_VERTEX_SHADER
GL_FRAGMENT_SHADER
glShaderSource
glCompileShader
glCreateProgram
glAttachShader
glDetachShader
glLinkProgram
glUseProgram
glGetShaderiv
glGetProgramiv
GL_COMPILE_STATUS
GL_LINK_STATUS
GL_VALIDATE_STATUS
GL_INFO_LOG_LENGTH
glGetShaderInfoLog
glGetProgramInfoLog
glGetUniformLocation
glUniform1i
glUniform1f
glUniform2f
glUniform3f
glUniform4f
glUniform1fv
glUniformMatrix4fv
glEnableVertexAttribArray
glVertexAttribPointer
GL_FLOAT
glDrawArrays

GL_CURRENT_PROGRAM

   gl:CreateProgram)

   (import (scheme core)
      (owl string) (owl io)
      (scheme bytevector)
      (OpenGL version-1-5))
(begin
   (define GL_VERSION_2_0 1)

(define GLchar* type-string)
(define GLchar** (fft* GLchar*))
;(define GLchar** type-vector)?

(define GLint* (fft* GLint))
(define GLsizei* (fft* GLsizei))
(define void* fft-void*)

  (define glCreateShader    (gl:GetProcAddress GLuint "glCreateShader" GLenum))
    (define GL_VERTEX_SHADER   #x8B31)
    (define GL_FRAGMENT_SHADER #x8B30)
  (define glShaderSource    (gl:GetProcAddress GLvoid "glShaderSource" GLuint GLsizei GLchar** GLint*))
  (define glCompileShader   (gl:GetProcAddress GLvoid "glCompileShader" GLuint))
  (define glCreateProgram   (gl:GetProcAddress GLuint "glCreateProgram"))
  (define glAttachShader    (gl:GetProcAddress GLvoid "glAttachShader" GLuint GLuint))
  (define glDetachShader    (gl:GetProcAddress GLvoid "glDetachShader" GLuint GLuint))
  (define glLinkProgram     (gl:GetProcAddress GLvoid "glLinkProgram" GLuint))
  (define glUseProgram      (gl:GetProcAddress GLvoid "glUseProgram" GLuint))
  (define glGetShaderiv     (gl:GetProcAddress GLvoid "glGetShaderiv" GLuint GLenum GLint&))
    (define GL_COMPILE_STATUS  #x8B81)
    (define GL_LINK_STATUS     #x8B82)
    (define GL_VALIDATE_STATUS #x8B83)
    (define GL_INFO_LOG_LENGTH #x8B84)
   (define glGetProgramiv     (gl:GetProcAddress GLvoid "glGetProgramiv" GLuint GLenum GLint&))
  (define glGetShaderInfoLog (gl:GetProcAddress GLvoid "glGetShaderInfoLog" GLuint GLsizei GLsizei* type-bytevector))
  (define glGetProgramInfoLog (gl:GetProcAddress GLvoid "glGetProgramInfoLog" GLuint GLsizei GLsizei* type-bytevector))
  (define glGetUniformLocation (gl:GetProcAddress GLint "glGetUniformLocation" GLuint GLchar*))
    (define glUniform1i     (gl:GetProcAddress GLvoid "glUniform1i" GLint GLint))
    (define glUniform1f     (gl:GetProcAddress GLvoid "glUniform1f" GLint GLfloat))
    (define glUniform2f     (gl:GetProcAddress GLvoid "glUniform2f" GLint GLfloat GLfloat))
    (define glUniform3f     (gl:GetProcAddress GLvoid "glUniform3f" GLint GLfloat GLfloat GLfloat))
    (define glUniform4f     (gl:GetProcAddress GLvoid "glUniform4f" GLint GLfloat GLfloat GLfloat GLfloat))
    (define glUniform1fv    (gl:GetProcAddress GLvoid "glUniform1fv" GLint GLsizei GLfloat*)) ; TEMP from GLfloat*
    (define glUniformMatrix4fv (gl:GetProcAddress GLvoid "glUniformMatrix4fv" GLint GLsizei GLboolean GLfloat*)) ; TEMPORARY RENAMED FROM GLfloat*
  (define glEnableVertexAttribArray (gl:GetProcAddress GLvoid "glEnableVertexAttribArray" GLuint))
  (define glVertexAttribPointer (gl:GetProcAddress GLvoid "glVertexAttribPointer" GLuint GLint GLenum GLboolean GLsizei void*))
    (define GL_FLOAT #x1406)
  (define glDrawArrays         (gl:GetProcAddress GLvoid "glDrawArrays" GLenum GLint GLsizei))

   (define GL_CURRENT_PROGRAM #x8B8D)


   (define (gl:CreateProgram . args)
      (print-to stderr "gl:CreateProgram is deprecated.")
      (print-to stderr "  Use (import (lib gl2)) and (gl:create-program . args) instead."))
))
