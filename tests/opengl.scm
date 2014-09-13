;!
(define *USE_GLBEGIN* 1)

;  (define isCompiled (list->byte-vector '(0 0 0 0)))
;  (sys-prim 33 isCompiled #false #false)

; функция dlopen ищет динамическую библиотеку *name* (если она не загружена - загружает)
;  и возвращает ее уникальный handle
(define (dlopen name flag) (sys-prim 30 (c-string name) flag #false))
; функция get-proc-address (todo: rename to dlsym) связывает название функции с самой функцией и позволяет ее вызывать 
(define (get-proc-address  type dll name) ; todo: переименовать в get-proc-address ?
   (let ((function (cons type (sys-prim 31 dll (c-string name) #false)))) ; todo: избавиться от (c-string)
      (lambda args
         (sys-prim 32 (cdr function) (car function) args))))
; get-proc-address-c - аналог get-proc-address, то с правилом вызова __cdecl         
(define (get-proc-address-c type dll name)
; todo: отправлять тип функции третим параметром (sys-prim 31) и в виртуальной машине
;   возвращать структуру с (byte-vector адрес-функции адрес-вызыватора-с-соответвующей-конвенцией) ? 
   (let ((function (cons (bor type 64) (sys-prim 31 dll (c-string name) #false)))) ; todo: переделать 64 во что-то поприятнее
      (lambda args ;  function       type          ;arguments
         (sys-prim 32 (cdr function) (car function) args))))

; а тут система типов функций, я так думаю, что проверку аргументов надо забабахать сюда?
(define (INTEGER arg) (cons 45 arg))
(define (FLOAT arg)   (cons 46 arg))
;(define (DOUBLE arg)  '(47 arg))

; для результата, что превышает x00FFFFFF надо использовать type-handle
(define type-handle 45)
(define type-float  46)
;(define type-double 47) ; пока нету, но возможно будет
(define type-void   48)

; вспомогательный макрос для собрать в кучку все bor
(define OR (lambda list (fold bor 0 list)))

; todo: определить константы возвращаемого типа и использовать их в описании возврата функций
; что-то вроде (define rt-int 1)

; todo: тип для (get-proc-address) - всегда число, добавить в проверку

;(sys-prim 33 (/ 1245678912456789 1245678912456788) (cast 1 type-rational) #false)

; my temporary stubs for opengl (у меня пока ж нет структур и т.д.)
; пример, как можно получить свои собственные функции (если они экспортируются, конечно)
(define kernel32_dll (dlopen "kernel32" 0))
  (define GetModuleHandle (get-proc-address type-handle kernel32_dll "GetModuleHandleA"))

;(define _exe (GetModuleHandle 0))
;(define CreateGLWindow (get-proc-address-c type-fix+ _exe "CreateGLWindow"))

; вспомогательные константы (временно, пока не научусь работать с флоатами)
(define FLOAT=1 #x3F800000)
(define FLOAT=0 #x00000000)
(define FLOAT-1 #xBF800000)
(define FLOAT0 #x00000000)
(define FLOAT1 #x3F800000)
(define FLOAT2 #x40000000)
         
; real code
(define user32 (dlopen "user32" 0))
  (define IDOK 1)
  (define IDCANCEL 2)

  (define MessageBox (get-proc-address type-fix+ user32 "MessageBoxA"))
    (define MB_OK 0)
    (define MB_OKCANCEL 1)
    (define MB_ICONASTERISK 64)
  (define PeekMessage      (get-proc-address type-fix+ user32 "PeekMessageA"))
    (define PM_REMOVE 1)
  (define TranslateMessage (get-proc-address type-fix+ user32 "TranslateMessage"))
  (define DispatchMessage  (get-proc-address type-fix+ user32 "DispatchMessageA"))
  (define PostQuitMessage  (get-proc-address type-fix+ user32 "PostQuitMessage"))
  ;; давление юры 06/09/2014 в 13:43 - 125/ 91
  ;;                           14.07 - 130/101 (после чашки кофе, голова пре-болеть перестала)
  (define GetKeyState      (get-proc-address type-fix+ user32 "GetKeyState"))
  (define GetAsyncKeyState (get-proc-address type-fix+ user32 "GetAsyncKeyState"))
  (define GetKeyboardState (get-proc-address type-fix+ user32 "GetKeyboardState"))
  
  ;; функции работы с win32 окнами
  (define CreateWindowEx   (get-proc-address type-handle user32 "CreateWindowExA")) ; ANSI version
    (define WS_EX_APPWINDOW      #x00040000)
    (define WS_EX_WINDOWEDGE     #x00000100)
    (define WS_OVERLAPPEDWINDOW  (OR #x00000000 #x00C00000 #x00080000 #x00040000 #x00020000 #x00010000))
    (define WS_CLIPSIBLINGS      #x04000000)
    (define WS_CLIPCHILDREN      #x02000000)
  (define DestroyWindow    (get-proc-address type-fix+   user32 "DestroyWindow"))
    
  (define GetDC               (get-proc-address type-handle user32 "GetDC"))
  (define ReleaseDC           (get-proc-address type-fix+   user32 "ReleaseDC"))
  (define ShowWindow          (get-proc-address type-fix+   user32 "ShowWindow"))
    (define SW_SHOW 5)
  (define SetForegroundWindow (get-proc-address type-fix+   user32 "SetForegroundWindow"))
  (define SetFocus            (get-proc-address type-fix+   user32 "SetFocus"))
  
  
  
(define gdi32 (dlopen "gdi32" 0))
  (define ChoosePixelFormat (get-proc-address type-fix+ gdi32 "ChoosePixelFormat"))
  (define SetPixelFormat    (get-proc-address type-fix+ gdi32 "SetPixelFormat"))
  (define SwapBuffers       (get-proc-address type-fix+ gdi32 "SwapBuffers"))


(define opengl32 (dlopen "opengl32" 0))
  (define wglCreateContext  (get-proc-address type-handle opengl32 "wglCreateContext"))
  (define wglMakeCurrent    (get-proc-address type-fix+   opengl32 "wglMakeCurrent"))
  (define wglDeleteContext  (get-proc-address type-fix+   opengl32 "wglDeleteContext"))
  (define wglGetProcAddress (get-proc-address type-handle opengl32 "wglGetProcAddress"))
    (define (wgl-proc-address type name)
      (let ((function (cons type (wglGetProcAddress (c-string name)))))
        (lambda args
          (sys-prim 32 (cdr function) (car function) args))))

  
  (define glClear           (get-proc-address type-fix+ opengl32 "glClear"))
    (define GL_COLOR_BUFFER_BIT #x00004000)
    (define GL_DEPTH_BUFFER_BIT #x00000100)
  (define glLoadIdentity    (get-proc-address type-fix+ opengl32 "glLoadIdentity"))
  (define glViewport        (get-proc-address type-fix+ opengl32 "glViewport"))
  (define glMatrixMode      (get-proc-address type-fix+ opengl32 "glMatrixMode"))
    (define GL_PROJECTION #x1701)
    (define GL_MODELVIEW  #x1700)
  (define glTranslatef      (get-proc-address type-fix+ opengl32 "glTranslatef"))

  (define glShadeModel      (get-proc-address type-fix+ opengl32 "glShadeModel"))
    (define GL_SMOOTH #x1D01)
  (define glClearColor      (get-proc-address type-fix+ opengl32 "glClearColor"))
  (define glHint            (get-proc-address type-fix+ opengl32 "glHint"))
    (define GL_PERSPECTIVE_CORRECTION_HINT #x0C50)
    (define GL_NICEST #x1102)


  ; https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml
  (define glColor3i         (get-proc-address type-fix+ opengl32 "glColor3i"))
  (define glColor3ub        (get-proc-address type-fix+ opengl32 "glColor3ub"))
  (define glVertex2i        (get-proc-address type-fix+ opengl32 "glVertex2i"))
  (define glVertex3i        (get-proc-address type-fix+ opengl32 "glVertex3i"))
  (define glVertex2f        (get-proc-address type-fix+ opengl32 "glVertex2f"))
  (define glBegin           (get-proc-address type-fix+ opengl32 "glBegin"))
    (define GL_TRIANGLES      #x0004)
    (define GL_TRIANGLE_STRIP #x0005) ; http://www.uraldev.ru/articles/35/page/4
  (define glEnd             (get-proc-address type-fix+ opengl32 "glEnd"))

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
  (define glCreateShader    (wgl-proc-address type-fix+ "glCreateShader"))
    (define GL_VERTEX_SHADER   #x8B31)
    (define GL_FRAGMENT_SHADER #x8B30)
  (define glShaderSource    (wgl-proc-address type-fix+ "glShaderSource"))
  (define glCompileShader   (wgl-proc-address type-fix+ "glCompileShader"))
  (define glCreateProgram   (wgl-proc-address type-fix+ "glCreateProgram"))
  (define glAttachShader    (wgl-proc-address type-fix+ "glAttachShader"))
  (define glDetachShader    (wgl-proc-address type-fix+ "glDetachShader"))
  (define glLinkProgram     (wgl-proc-address type-fix+ "glLinkProgram"))
  (define glUseProgram      (wgl-proc-address type-fix+ "glUseProgram"))
  (define glGetShaderiv     (wgl-proc-address type-void "glGetShaderiv"))
    (define GL_COMPILE_STATUS  #x8B81)
    (define GL_LINK_STATUS     #x8B82)
    (define GL_VALIDATE_STATUS #x8B83)
    (define GL_INFO_LOG_LENGTH #x8B84)
  (define glGetShaderInfoLog (wgl-proc-address type-fix+ "glGetShaderInfoLog"))
  (define glGetUniformLocation (wgl-proc-address type-fix+ "glGetUniformLocation"))
    (define glUniform1i     (wgl-proc-address type-fix+ "glUniform1i"))
  (define glEnableVertexAttribArray (wgl-proc-address type-fix+ "glEnableVertexAttribArray"))
  (define glVertexAttribPointer (wgl-proc-address type-fix+ "glVertexAttribPointer"))
    (define GL_FLOAT #x1406)
  (define glDrawArrays         (wgl-proc-address type-fix+ "glDrawArrays"))

(define (file->string path)
   (bytes->string
      (vec-iter
         (let ((vec (file->vector path)))
            (if vec vec
               (error "Unable to load: " path))))))

; пример как хранить скомпилированные значения
; а можно еще попробовать их перед этим закриптовать )
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
  (case 3
    (2 "raw/geometry.fs")
    (3 "raw/water.fs")
    (4 "raw/18850")
    (5 "raw/minecraft.fs")
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
