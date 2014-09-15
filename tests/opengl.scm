; http://www.scheme.com/tspl4/ - The Scheme Programming Language (Fourth Edition)
;!
(define *USE_GLBEGIN* 1)

;  (define isCompiled (list->byte-vector '(0 0 0 0)))
;  (sys-prim 33 isCompiled #false #false)
(import (owl pinvoke))
(import (OpenGL version-2-1))
(import (owl windows))

; вспомогательный макрос для собрать в кучку все bor
(define OR (lambda list (fold bor 0 list)))

; todo: тип для (dlsym) - всегда число, добавить в проверку

;(sys-prim 33 (/ 1245678912456789 1245678912456788) (cast 1 type-rational) #false)

; my temporary stubs for opengl (у меня пока ж нет структур и т.д.)

; вспомогательные константы (временно, пока не научусь работать с флоатами)
(define FLOAT=1 #x3F800000)
(define FLOAT=0 #x00000000)
(define FLOAT-1 #xBF800000)
(define FLOAT0 #x00000000)
(define FLOAT1 #x3F800000)
(define FLOAT2 #x40000000)
         
; real code

(define opengl32 (dlopen "opengl32" 0))
  
  (define glClear           (dlsym type-fix+ opengl32 "glClear"))
    (define GL_COLOR_BUFFER_BIT #x00004000)
    (define GL_DEPTH_BUFFER_BIT #x00000100)
  (define glLoadIdentity    (dlsym type-fix+ opengl32 "glLoadIdentity"))
  (define glMatrixMode      (dlsym type-fix+ opengl32 "glMatrixMode"))
    (define GL_PROJECTION #x1701)
    (define GL_MODELVIEW  #x1700)
  (define glTranslatef      (dlsym type-fix+ opengl32 "glTranslatef"))

  (define glShadeModel      (dlsym type-fix+ opengl32 "glShadeModel"))
    (define GL_SMOOTH #x1D01)
  (define glClearColor      (dlsym type-fix+ opengl32 "glClearColor"))
  (define glHint            (dlsym type-fix+ opengl32 "glHint"))
    (define GL_PERSPECTIVE_CORRECTION_HINT #x0C50)
    (define GL_NICEST #x1102)


  ; https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml
  (define glColor3i         (dlsym type-fix+ opengl32 "glColor3i"))
  (define glColor3ub        (dlsym type-fix+ opengl32 "glColor3ub"))
  (define glVertex2i        (dlsym type-fix+ opengl32 "glVertex2i"))
  (define glVertex3i        (dlsym type-fix+ opengl32 "glVertex3i"))
  (define glVertex2f        (dlsym type-fix+ opengl32 "glVertex2f"))
  (define glBegin           (dlsym type-fix+ opengl32 "glBegin"))
    (define GL_TRIANGLES      #x0004)
    (define GL_TRIANGLE_STRIP #x0005) ; http://www.uraldev.ru/articles/35/page/4
  (define glEnd             (dlsym type-fix+ opengl32 "glEnd"))

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

(define width 1280)
(define height 720)
(define window (CreateWindowEx
    (OR WS_EX_APPWINDOW WS_EX_WINDOWEDGE) "#32770" "OL OpenGL Sample 0" ; #32770 is for system classname for DIALOG
    (OR WS_OVERLAPPEDWINDOW WS_CLIPSIBLINGS WS_CLIPCHILDREN)
    0 0 width height ; x y width height
    0 ; no parent window
    0 ; no menu
    0 ; instance
    0)) ; don't pass anything to WM_CREATE
; переключение в полноєкранній режим - http://blogs.msdn.com/b/oldnewthing/archive/2010/04/12/9994016.aspx
; PIXELFORMATDESCRIPTOR
(define pfd (list->byte-vector '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                                   00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))                        
(define hDC (GetDC window))
(define PixelFormat (ChoosePixelFormat hDC pfd))
(SetPixelFormat hDC PixelFormat pfd)
(define hRC (wglCreateContext hDC))

(wglMakeCurrent hDC hRC)

;(wglMakeCurrent (tuple "a" "b" "c"))

;(sys-prim 33 (cast type-fix+ 3/7) #false #false)

;  ; opengl 1.2 https://www.opengl.org/registry/api/GL/glext.h
  (define glCreateShader    (glGetProcAddress type-fix+ "glCreateShader"))
    (define GL_VERTEX_SHADER   #x8B31)
    (define GL_FRAGMENT_SHADER #x8B30)
  (define glShaderSource    (glGetProcAddress type-fix+ "glShaderSource"))
  (define glCompileShader   (glGetProcAddress type-fix+ "glCompileShader"))
  (define glCreateProgram   (glGetProcAddress type-fix+ "glCreateProgram"))
  (define glAttachShader    (glGetProcAddress type-fix+ "glAttachShader"))
  (define glDetachShader    (glGetProcAddress type-fix+ "glDetachShader"))
  (define glLinkProgram     (glGetProcAddress type-fix+ "glLinkProgram"))
  (define glUseProgram      (glGetProcAddress type-fix+ "glUseProgram"))
  (define glGetShaderiv     (glGetProcAddress type-void "glGetShaderiv"))
    (define GL_COMPILE_STATUS  #x8B81)
    (define GL_LINK_STATUS     #x8B82)
    (define GL_VALIDATE_STATUS #x8B83)
    (define GL_INFO_LOG_LENGTH #x8B84)
  (define glGetShaderInfoLog (glGetProcAddress type-fix+ "glGetShaderInfoLog"))
  (define glGetUniformLocation (glGetProcAddress type-fix+ "glGetUniformLocation"))
    (define glUniform1i     (glGetProcAddress type-fix+ "glUniform1i"))
  (define glEnableVertexAttribArray (glGetProcAddress type-fix+ "glEnableVertexAttribArray"))
  (define glVertexAttribPointer (glGetProcAddress type-fix+ "glVertexAttribPointer"))
    (define GL_FLOAT #x1406)
  (define glDrawArrays         (glGetProcAddress type-fix+ "glDrawArrays"))

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
;(print "po: " po)

(define vs (glCreateShader GL_VERTEX_SHADER))
;(print "vs: " vs)
; пример, как можно передать в функцию массив указателей на строки:
(glShaderSource vs 2 (tuple (c-string "#version 120 // OpenGL 2.1\n")
                            (c-string "
	void main() {
		gl_Position = gl_Vertex; // - vec4(1.0, 1.0, 0.0, 0.0); // gl_ModelViewMatrix * gl_Vertex
	}")) 0)
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
  (case 0
    (2 "raw/geometry.fs")
    (3 "raw/water.fs")
    (4 "raw/18850")
    (5 "raw/minecraft.fs")
    (0 "raw/black.fs")
    (1 "raw/itsfullofstars.fs"))))) 0)
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

  (define time (glGetUniformLocation po "time2"))

;(print "glGetUniformLocation: " (glGetUniformLocation po "color"))
;(sys-prim 32 (cdr function) (car function) args))))

  ; todo: проверить возвращаемый результат


(ShowWindow window SW_SHOW)
(SetForegroundWindow window)
(SetFocus window)

; ResizeGLScene
(glViewport 0 0 width height)

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)

(glMatrixMode GL_MODELVIEW)
(glLoadIdentity)
;(glTranslatef FLOAT-1 FLOAT-1 0)

(glShadeModel GL_SMOOTH)
(glClearColor 0 0 0 (FLOAT 1))
;(glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)

;(glClearColor 0 0 FLOAT=1 0)

;(WinMain 0 0 0 0)
(define vertexPositions (list->byte-vector '(
;        (glVertex2i 2 0)
  00 00 #x00 #x40    0 0 #x00 #x00    0 0 0 0    00 00 #x80 #x3F
;        (glVertex2i 1 2)
  00 00 #x80 #x3F    0 0 #x00 #x40    0 0 0 0    00 00 #x80 #x3F
;        (glVertex2i 0 0)
  00 00 #x00 #x00    0 0 #x00 #x00    0 0 0 0    00 00 #x80 #x3F
)))

(define MSG (make-vector 28 0)) ; sizeof(MSG)=28
;(call/cc (lambda (return)
(define (cycle)   ;MSG
  (if (= 1 (PeekMessage MSG 0 0 0 PM_REMOVE))
    (begin  
      (TranslateMessage MSG)
      (DispatchMessage MSG))
      
    (begin ; DrawGLScene
      (glClear GL_COLOR_BUFFER_BIT)

      (glUseProgram po)
      (let* ((ss ms (clock)))
        (glUniform1i time (+ ms (* 1000 (mod ss 3600))))) ; раз в час будем сбрасывать период
      
      (glBegin GL_TRIANGLE_STRIP)
;        (glVertex3i 0 0 0)
;        (glVertex3i 2 0 0)
;        (glVertex3i 0 2 0)
;        (glVertex3i 2 2 0)
        (glVertex2f (FLOAT -1) (FLOAT -1))
        (glVertex2f (FLOAT +1) (FLOAT -1))
        (glVertex2f (FLOAT -1) (FLOAT +1))
        (glVertex2f (FLOAT +1) (FLOAT +1))
      (glEnd)

;      (glEnableVertexAttribArray 0)
;      (glVertexAttribPointer 0 4 GL_FLOAT 0 0 vertexPositions)
;      (glDrawArrays GL_TRIANGLES 0 3)

      (glUseProgram 0)
      (SwapBuffers hDC)))
  (if (= (GetAsyncKeyState 27) 0) (cycle)))
(cycle)

; KillGLWindow
(wglMakeCurrent 0 0)
(wglDeleteContext hRC)
(ReleaseDC window hDC)
(DestroyWindow window)

(print "@")
