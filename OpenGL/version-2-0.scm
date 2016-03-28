; OpenGL 2.0 (2004)

(define-library (OpenGL version-2-0)
(export
       (exports (OpenGL version-1-5))
   GL_VERSION_2_0

   gl:GetVersion

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
GL_COMPILE_STATUS
GL_LINK_STATUS
GL_VALIDATE_STATUS
GL_INFO_LOG_LENGTH
glGetShaderInfoLog
glGetUniformLocation
glUniform1i
glUniform1f
glUniform2f
glEnableVertexAttribArray
glVertexAttribPointer
GL_FLOAT
glDrawArrays
)

   (import (otus lisp)
      (OpenGL version-1-5))
(begin
   (define GL_VERSION_2_0 1)

   (define % (dlopen GL_LIBRARY))

;  using GLchar      = System.Byte;    // char

(define GLchar** (vm:or type-string #x40))
;(define GLchar** type-tuple)

(define GLint* type-void*)
(define GLsizei* type-void*)
(define GLchar* type-string)
(define void* type-void*)

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
  (define glGetShaderiv     (gl:GetProcAddress GLvoid "glGetShaderiv" GLuint GLenum GLint*))
    (define GL_COMPILE_STATUS  #x8B81)
    (define GL_LINK_STATUS     #x8B82)
    (define GL_VALIDATE_STATUS #x8B83)
    (define GL_INFO_LOG_LENGTH #x8B84)
  (define glGetShaderInfoLog (gl:GetProcAddress GLvoid "glGetShaderInfoLog" GLuint GLsizei GLsizei* GLchar*))
  (define glGetUniformLocation (gl:GetProcAddress GLint "glGetUniformLocation" GLuint GLchar*))
    (define glUniform1i     (gl:GetProcAddress GLvoid "glUniform1i" GLint GLint))
    (define glUniform1f     (gl:GetProcAddress GLvoid "glUniform1f" GLint GLfloat))
    (define glUniform2f     (gl:GetProcAddress GLvoid "glUniform2f" GLint GLfloat GLfloat))
  (define glEnableVertexAttribArray (gl:GetProcAddress GLvoid "glEnableVertexAttribArray" GLuint))
  (define glVertexAttribPointer (gl:GetProcAddress GLvoid "glVertexAttribPointer" GLuint GLint GLenum GLboolean GLsizei void*))
    (define GL_FLOAT #x1406)
  (define glDrawArrays         (gl:GetProcAddress GLvoid "glDrawArrays" GLenum GLint GLsizei))


(define (gl:GetVersion)
   (cons 2 0))

))