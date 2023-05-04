; OpenGL 2.0 (7 Sep 2004), GLSL 1.1
(define-library (OpenGL version-2-0)
(export
      (exports (OpenGL 1.5))
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
glDeleteProgram
glAttachShader
glDetachShader
glDeleteShader
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
glUniform2fv
glUniform3f
glUniform3fv
glUniform4f
glUniform4fv
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
      (OpenGL 1.5))
(begin
   (define GL_VERSION_2_0 1)

   (setq GL gl:GetProcAddress)

(define GLchar* type-string)
(define GLchar** (fft* GLchar*))
;(define GLchar** type-vector)?

(define GLint* (fft* GLint))
(define GLsizei* (fft* GLsizei))
(define void* fft-void*)
(define GLsizei& (fft& GLsizei))

  (define glCreateShader    (GL GLuint "glCreateShader" GLenum))
    (define GL_VERTEX_SHADER   #x8B31)
    (define GL_FRAGMENT_SHADER #x8B30)
  (define glShaderSource    (GL GLvoid "glShaderSource" GLuint GLsizei GLchar** GLint*))
  (define glCompileShader   (GL GLvoid "glCompileShader" GLuint))
  (define glCreateProgram   (GL GLuint "glCreateProgram"))
  (define glDeleteProgram   (GL GLvoid "glDeleteProgram" GLuint))
  (define glAttachShader    (GL GLvoid "glAttachShader" GLuint GLuint))
  (define glDetachShader    (GL GLvoid "glDetachShader" GLuint GLuint))
  (define glDeleteShader    (GL GLvoid "glDeleteShader" GLuint))
  (define glLinkProgram     (GL GLvoid "glLinkProgram" GLuint))
  (define glUseProgram      (GL GLvoid "glUseProgram" GLuint))
  (define glGetShaderiv     (GL GLvoid "glGetShaderiv" GLuint GLenum GLint&))
    (define GL_COMPILE_STATUS  #x8B81)
    (define GL_LINK_STATUS     #x8B82)
    (define GL_VALIDATE_STATUS #x8B83)
    (define GL_INFO_LOG_LENGTH #x8B84)
   (define glGetProgramiv     (GL GLvoid "glGetProgramiv" GLuint GLenum GLint&))
  (define glGetShaderInfoLog (GL GLvoid "glGetShaderInfoLog" GLuint GLsizei GLsizei& type-bytevector))
  (define glGetProgramInfoLog (GL GLvoid "glGetProgramInfoLog" GLuint GLsizei GLsizei& type-bytevector))
  (define glGetUniformLocation (GL GLint "glGetUniformLocation" GLuint GLchar*))
    (define glUniform1i     (GL GLvoid "glUniform1i" GLint GLint))
    (define glUniform1f     (GL GLvoid "glUniform1f" GLint GLfloat))
    (define glUniform2f     (GL GLvoid "glUniform2f" GLint GLfloat GLfloat))
    (define glUniform2fv    (GL GLvoid "glUniform2fv" GLint GLsizei GLfloat*))
    (define glUniform3f     (GL GLvoid "glUniform3f" GLint GLfloat GLfloat GLfloat))
    (define glUniform3fv    (GL GLvoid "glUniform3fv" GLint GLsizei GLfloat*))
    (define glUniform4f     (GL GLvoid "glUniform4f" GLint GLfloat GLfloat GLfloat GLfloat))
    (define glUniform4fv    (GL GLvoid "glUniform4fv" GLint GLsizei GLfloat*))
    (define glUniform1fv    (GL GLvoid "glUniform1fv" GLint GLsizei GLfloat*))
    (define glUniformMatrix4fv (GL GLvoid "glUniformMatrix4fv" GLint GLsizei GLboolean GLfloat*))
  (define glEnableVertexAttribArray (GL GLvoid "glEnableVertexAttribArray" GLuint))
  (define glVertexAttribPointer (GL GLvoid "glVertexAttribPointer" GLuint GLint GLenum GLboolean GLsizei void*))
    (define GL_FLOAT #x1406)
  (define glDrawArrays         (GL GLvoid "glDrawArrays" GLenum GLint GLsizei))

   (define GL_CURRENT_PROGRAM #x8B8D)


   (define (gl:CreateProgram . args)
      (print-to stderr "gl:CreateProgram is deprecated.")
      (print-to stderr "  Use (import (lib gl-2)) and (gl:create-program . args) instead."))
))
