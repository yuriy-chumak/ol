(define SHADER_NUM 1)


; http://www.scheme.com/tspl4/ - The Scheme Programming Language (Fourth Edition)
; http://community.schemewiki.org/?scheme-faq-standards#implementations
;!
(define *USE_GLBEGIN* 1)

;  (define isCompiled (list->byte-vector '(0 0 0 0)))
;  (sys-prim 1033 isCompiled #false #false)
(import (owl pinvoke))
(import (lib windows))
(import (OpenGL version-2-1))

; вспомогательный макрос для собрать в кучку все bor
(define OR (lambda list (fold bor 0 list)))
(define (make-byte-vector n elem)
   (list->byte-vector (repeat elem n)))

(define width 1280)
(define height 720)
(define window (CreateWindowEx
    (OR WS_EX_APPWINDOW WS_EX_WINDOWEDGE) "#32770" "OL OpenGL Sample 0" ; #32770 is for system classname for DIALOG
    (OR WS_OVERLAPPEDWINDOW WS_CLIPSIBLINGS WS_CLIPCHILDREN)
    0 0 width height ; x y width height
    null ; no parent window
    null ; no menu
    null ; instance
    null)) ; todo: override as '(INTEGER . 0)
; переключение в полноєкранній режим - http://blogs.msdn.com/b/oldnewthing/archive/2010/04/12/9994016.aspx
; PIXELFORMATDESCRIPTOR
(define pfd (list->byte-vector '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                                   00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))                        
(define hDC (GetDC window))
(define PixelFormat (ChoosePixelFormat hDC pfd))
(print "PixelFormat = " PixelFormat)
(print "SetPixelFormat = "
(SetPixelFormat hDC PixelFormat pfd))
(define hRC (wglCreateContext hDC))

(print "wglMakeCurrent = "
(wglMakeCurrent hDC hRC))

(print "OpenGL version: " (glGetString GL_VERSION))
(print "OpenGL vendor: " (glGetString GL_VENDOR))
(print "OpenGL renderer: " (glGetString GL_RENDERER))

; todo: тип для (dlsym) - всегда число, добавить в проверку

; my temporary stubs for opengl (у меня пока ж нет структур и т.д.)

; real code
(define void    type-void)
(define GLvoid  type-void)  ; void GLvoid
(define GLenum  type-int+)  ; typedef unsigned int GLenum - fix+ значит, что это целое число
(define GLfloat type-float) ; typedef float GLfloat (same as type-rational)
(define GLint   type-int+)  ; typedef int GLint
(define GLsizei type-int+)  ; typedef int GLsizei
(define GLbitfield type-int+);typedef unsigned int GLbitfield;
;typedef double GLdouble;
(define GLuint  type-int+)  ;typedef unsigned int GLuint;
(define GLboolean type-fix+);typedef unsigned char GLboolean;
(define GLubyte type-int+)  ;typedef unsigned char GLubyte;
(define GLubyte* type-string)
(define GLclampf type-float)

; проверка, что все запустилось.
(define (msgbox)
(if (=
  (MessageBox 0 "Please, press OK for test pass!" (c-string "load-library test")
    (bor MB_OKCANCEL MB_ICONASTERISK))
  IDOK)
    (print "OK")
    (print "CANCEL")))
; todo: вроде бы все строки и так заканчиваются на '\0' - проверить
;(define echo "echo server")

; в момент импорта сделать все нужные привязки
; export (MessageBox)  и т.д.

;(wglMakeCurrent (tuple "a" "b" "c"))

(define GLchar** type-tuple)
(define GLint* type-vector-raw)
(define GLsizei* type-vector-raw)
(define GLchar* type-string)
(define void* type-vector-raw)


;  ; opengl 1.2 https://www.opengl.org/registry/api/GL/glext.h
  (define glCreateShader    (glGetProcAddress GLuint "glCreateShader" GLenum))
    (define GL_VERTEX_SHADER   #x8B31)
    (define GL_FRAGMENT_SHADER #x8B30)
  (define glShaderSource    (glGetProcAddress void "glShaderSource" GLuint GLsizei GLchar** GLint*))
  (define glCompileShader   (glGetProcAddress void "glCompileShader" GLuint))
  (define glCreateProgram   (glGetProcAddress GLuint "glCreateProgram"))
  (define glAttachShader    (glGetProcAddress void "glAttachShader" GLuint GLuint))
  (define glDetachShader    (glGetProcAddress void "glDetachShader" GLuint GLuint))
  (define glLinkProgram     (glGetProcAddress void "glLinkProgram" GLuint))
  (define glUseProgram      (glGetProcAddress void "glUseProgram" GLuint))
  (define glGetShaderiv     (glGetProcAddress void "glGetShaderiv" GLuint GLenum GLint*))
    (define GL_COMPILE_STATUS  #x8B81)
    (define GL_LINK_STATUS     #x8B82)
    (define GL_VALIDATE_STATUS #x8B83)
    (define GL_INFO_LOG_LENGTH #x8B84)
  (define glGetShaderInfoLog (glGetProcAddress void "glGetShaderInfoLog" GLuint GLsizei GLsizei* GLchar*))
  (define glGetUniformLocation (glGetProcAddress GLint "glGetUniformLocation" GLuint GLchar*))
    (define glUniform1i     (glGetProcAddress void "glUniform1i" GLint GLint))
    (define glUniform1f     (glGetProcAddress void "glUniform1f" GLint GLfloat))
    (define glUniform2f     (glGetProcAddress void "glUniform2f" GLint GLfloat GLfloat))
  (define glEnableVertexAttribArray (glGetProcAddress void "glEnableVertexAttribArray" GLuint))
  (define glVertexAttribPointer (glGetProcAddress void "glVertexAttribPointer" GLuint GLint GLenum GLboolean GLsizei void*))
    (define GL_FLOAT #x1406)
  (define glDrawArrays         (glGetProcAddress void "glDrawArrays" GLenum GLint GLsizei))

(define (file->string path)
   (bytes->string
      (vec-iter
         (let ((vec (file->vector path)))
            (if vec vec
               (error "Unable to load: " path))))))

; пример как хранить скомпилированные значения
; а можно еще попробовать их перед этим закриптовать
;(fasl-save (file->string "raw/geometry.fs") "raw/geometry.compiled.fs")
;(fasl-load "raw/geometry.compiled.fs" "void main(void) { gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0); }")

(define po (glCreateProgram))
(print "po: " po)

(define vs (glCreateShader GL_VERTEX_SHADER))
(print "vs: " vs)


; пример, как можно передать в функцию массив указателей на строки:
(glShaderSource vs 2 (tuple (c-string "#version 120 // OpenGL 2.1\n")
                            (c-string "
	void main() {
		gl_Position = gl_Vertex; // - vec4(1.0, 1.0, 0.0, 0.0); // gl_ModelViewMatrix * gl_Vertex
	}")) null)
(glCompileShader vs)
  (define isCompiled "word")
  (glGetShaderiv vs GL_COMPILE_STATUS isCompiled)
  
  (if (= (ref isCompiled 0) 0)
    (begin
      (define maxLength "word")
      (glGetShaderiv vs GL_INFO_LOG_LENGTH maxLength)
      (define maxLengthValue (+ (ref maxLength 0) (* (ref maxLength 1) 256)))
      (define errorLog (make-string maxLengthValue 0))
      (glGetShaderInfoLog vs maxLengthValue maxLength errorLog)
      (print errorLog)
      (print "@")
      (halt 0)))
(glAttachShader po vs)

;; полезные шейдеры:
;;  http://glslsandbox.com/e#19171.3 - цифровое табло
(define fs (glCreateShader GL_FRAGMENT_SHADER))
(glShaderSource fs 1 (tuple (c-string (file->string
  (case SHADER_NUM
    (2 "raw/geometry.fs")
    (3 "raw/water.fs")
    (4 "raw/18850")
    (5 "raw/minecraft.fs")
    (6 "raw/moon.fs")
    (0 "raw/black.fs")
    (1 "raw/itsfullofstars.fs"))))) null)
(glCompileShader fs)
  (define isCompiled "word")
  (glGetShaderiv fs GL_COMPILE_STATUS isCompiled)
  
  (if (= (ref isCompiled 0) 0)
    (begin
      (define maxLength "word")
      (glGetShaderiv fs GL_INFO_LOG_LENGTH maxLength)
      (define maxLengthValue (+ (ref maxLength 0) (* (ref maxLength 1) 256)))
      (define errorLog (make-string maxLengthValue 0))
      (glGetShaderInfoLog fs maxLengthValue maxLength errorLog)
      (print errorLog)
      (print "@")
      (halt 0)))
(glAttachShader po fs)

(glLinkProgram po)
(glDetachShader po fs)
(glDetachShader po vs)

  (define time (glGetUniformLocation po (c-string "time")))
  (define resolution (glGetUniformLocation po (c-string "resolution")))

;(print "glGetUniformLocation: " (glGetUniformLocation po "color"))
;(sys-prim 1032 (cdr function) (car function) args))))


(ShowWindow window SW_SHOW)
(SetForegroundWindow window)
(SetFocus window)

; ResizeGLScene
(glViewport 0 0 width height)

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)

(glMatrixMode GL_MODELVIEW)
(glLoadIdentity)

(glShadeModel GL_SMOOTH)
(glClearColor 0 0 1 1)


;(define vertexPositions (list->byte-vector '(
;;        (glVertex2i 2 0)
;  00 00 #x00 #x40    0 0 #x00 #x00    0 0 0 0    00 00 #x80 #x3F
;;        (glVertex2i 1 2)
;  00 00 #x80 #x3F    0 0 #x00 #x40    0 0 0 0    00 00 #x80 #x3F
;;        (glVertex2i 0 0)
;  00 00 #x00 #x00    0 0 #x00 #x00    0 0 0 0    00 00 #x80 #x3F
;)))

;(define MSG (make-byte-vector 28 0)) ; sizeof(MSG)=28
(define MSG "1234567890123456789012345678") ; sizeof(MSG)=28
;(call/cc (lambda (return)
(let cycle ()   ;MSG
  (if (= 1 (PeekMessage MSG null 0 0 PM_REMOVE))
    (begin
      (TranslateMessage MSG)
      (DispatchMessage MSG))
      
    (begin ; DrawGLScene
       (glClear GL_COLOR_BUFFER_BIT)
       
       (glUseProgram po)

      (let* ((ss ms (clock)))
        (glUniform1f time (+ (/ ms 1000) (mod ss 3600)))) ; раз в час будем сбрасывать период
      (if (> resolution 0)
        (glUniform2f resolution width height))
      
      (glBegin GL_TRIANGLE_STRIP)
;        (glVertex3i 0 0 0)
;        (glVertex3i 2 0 0)
;        (glVertex3i 0 2 0)
;        (glVertex3i 2 2 0)
        (glVertex2f -1 -1)
        (glVertex2f +1 -1)
        (glVertex2f -1 +1)
        (glVertex2f +1 +1)
      (glEnd)

;      (glEnableVertexAttribArray 0)
;      (glVertexAttribPointer 0 4 GL_FLOAT 0 0 vertexPositions)
;      (glDrawArrays GL_TRIANGLES 0 3)
       
       (glUseProgram 0)
       (SwapBuffers hDC)))
  (if (= (GetAsyncKeyState 27) 0) (cycle)))

; KillGLWindow
; (wglMakeCurrent 0 0)
(wglDeleteContext hRC)
(ReleaseDC window hDC)
(DestroyWindow window)
