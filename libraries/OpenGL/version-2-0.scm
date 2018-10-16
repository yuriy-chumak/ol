; OpenGL 2.0 (2004)

(define-library (OpenGL version-2-0)
(export

   GL_VERSION_2_0

   gl:GetVersion
   gl:CreateProgram

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
glUniform3f
glUniform1fv
glUniformMatrix4fv
glEnableVertexAttribArray
glVertexAttribPointer
GL_FLOAT
glDrawArrays

   (exports (OpenGL version-1-5)))

   (import (otus lisp)
      (OpenGL version-1-5))
(begin
   (define GL_VERSION_2_0 1)

(define GLchar* type-string)
(define GLchar** (fft* GLchar*))
;(define GLchar** type-tuple)

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
  (define glGetShaderInfoLog (gl:GetProcAddress GLvoid "glGetShaderInfoLog" GLuint GLsizei GLsizei* GLchar*))
  (define glGetUniformLocation (gl:GetProcAddress GLint "glGetUniformLocation" GLuint GLchar*))
    (define glUniform1i     (gl:GetProcAddress GLvoid "glUniform1i" GLint GLint))
    (define glUniform1f     (gl:GetProcAddress GLvoid "glUniform1f" GLint GLfloat))
    (define glUniform2f     (gl:GetProcAddress GLvoid "glUniform2f" GLint GLfloat GLfloat))
    (define glUniform3f     (gl:GetProcAddress GLvoid "glUniform3f" GLint GLfloat GLfloat GLfloat))
    (define glUniform1fv    (gl:GetProcAddress GLvoid "glUniform1fv" GLint GLsizei GLvoid*)) ; TEMP from GLfloat*
    (define glUniformMatrix4fv (gl:GetProcAddress GLvoid "glUniformMatrix4fv" GLint GLsizei GLboolean GLvoid*)) ; TEMPORARY RENAMED FROM GLfloat*
  (define glEnableVertexAttribArray (gl:GetProcAddress GLvoid "glEnableVertexAttribArray" GLuint))
  (define glVertexAttribPointer (gl:GetProcAddress GLvoid "glVertexAttribPointer" GLuint GLint GLenum GLboolean GLsizei void*))
    (define GL_FLOAT #x1406)
  (define glDrawArrays         (gl:GetProcAddress GLvoid "glDrawArrays" GLenum GLint GLsizei))


(define (gl:GetVersion)
   (cons 2 0))

(define (gl:CreateProgram vstext fstext)
(let ((po (glCreateProgram))
      (vs (glCreateShader GL_VERTEX_SHADER))
      (fs (glCreateShader GL_FRAGMENT_SHADER)))
   (if (= po 0)
      (runtime-error "Can't create shader program." #f))

   ; пример, как можно передать в функцию массив указателей на строки:
   ; vertex shader:
   ; http://steps3d.narod.ru/tutorials/lighting-tutorial.html
   (glShaderSource vs 1 (list (c-string vstext)) #false)
   (glCompileShader vs)
   (let ((isCompiled (box 0)))
      (glGetShaderiv vs GL_COMPILE_STATUS isCompiled)

      (if (eq? (unbox isCompiled) 0)
         (let*((maxLength (box 0))
               (_ (glGetShaderiv vs GL_INFO_LOG_LENGTH maxLength))
               (maxLengthValue (unbox maxLength))
               (errorLog (make-string maxLengthValue 0))
               (_ (glGetShaderInfoLog vs maxLengthValue maxLength errorLog)))
            (runtime-error errorLog vs))))
   (glAttachShader po vs)

   ; fragment shader:
   (glShaderSource fs 1 (list (c-string fstext)) #false)
   (glCompileShader fs)
   (let ((isCompiled '(0)))
      (glGetShaderiv fs GL_COMPILE_STATUS isCompiled)

      (if (eq? (unbox isCompiled) 0)
         (let*((maxLength (box 0))
               (_ (glGetShaderiv vs GL_INFO_LOG_LENGTH maxLength))
               (maxLengthValue (unbox maxLength))
               (errorLog (make-string maxLengthValue 0))
               (_ (glGetShaderInfoLog vs maxLengthValue maxLength errorLog)))
            (runtime-error errorLog fs))))

   (glAttachShader po fs)

   (glLinkProgram po)
   (glDetachShader po fs)
   (glDetachShader po vs)

   po)) ; return program

))
