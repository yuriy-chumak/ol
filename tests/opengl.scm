;!
(define (load-library name flag) (sys-prim 30 (c-string name) flag #false))
(define (get-proc-address  type dll name) ; todo: переименовать в get-proc-address ?
   (let ((function (cons type (sys-prim 31 dll (c-string name) #false)))) ; todo: избавиться от (c-string)
      (lambda args
         (sys-prim 32 (cdr function) (car function) args))))
(define (get-proc-address-c type dll name) ; todo: переименовать в get-proc-address ?
; todo: отправлять тип функции третим параметром (sys-prim 31) и в виртуальной машине
; возвращать структуру с (byte-vector адрес-функции адрес-вызыватора-с-соответвующей-конвенцией) 
   (let ((function (cons (bor type 64) (sys-prim 31 dll (c-string name) #false)))) ; todo: переделать 64 во что-то поприятнее
      (lambda args
         (sys-prim 32 (cdr function) (car function) args))))
(define type-handle 45)

; вспомогательный макрос для собрать в кучку все bor
(define OR (lambda list (fold bor 0 list)))

; todo: определить константы возвращаемого типа и использовать их в описании возврата функций
; что-то вроде (define rt-int 1)

; todo: тип для (get-proc-address) - всегда число, добавить в проверку

; для результата, что превышает x00FFFFFF надо использовать type-int+ (?)


; my temporary stubs for opengl (у меня пока ж нет структур и т.д.)
(define kernel32_dll (load-library "kernel32" 0))
  (define GetModuleHandle (get-proc-address type-handle kernel32_dll "GetModuleHandleA"))

(define _exe (GetModuleHandle 0))
;(define wmain          (get-proc-address type-fix+ _exe "wmain@12")) ; test
;(wmain 1 2 3)                                                        ; test
;(define cmain          (get-proc-address-c type-fix+ _exe "cmain"))  ; test
;(cmain 1 2 3)                                                        ; test


(define CreateGLWindow (get-proc-address-c type-fix+ _exe "CreateGLWindow"))

         
; real code
(define user32 (load-library "user32" 0))
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
  
  
  
(define gdi32 (load-library "gdi32" 0))
  (define ChoosePixelFormat (get-proc-address type-fix+ gdi32 "ChoosePixelFormat"))
  (define SetPixelFormat    (get-proc-address type-fix+ gdi32 "SetPixelFormat"))
  (define SwapBuffers       (get-proc-address type-fix+ gdi32 "SwapBuffers"))


(define opengl32 (load-library "opengl32" 0))
  (define wglCreateContext  (get-proc-address type-handle opengl32 "wglCreateContext"))
  (define wglMakeCurrent    (get-proc-address type-fix+   opengl32 "wglMakeCurrent"))
  (define wglDeleteContext  (get-proc-address type-fix+   opengl32 "wglDeleteContext"))
  
  (define glClear           (get-proc-address type-fix+ opengl32 "glClear"))
    (define GL_COLOR_BUFFER_BIT #x00004000)
    (define GL_DEPTH_BUFFER_BIT #x00000100)
  (define glLoadIdentity    (get-proc-address type-fix+ opengl32 "glLoadIdentity"))


; проверка, что все запустилось.

;(if (=
;  (MessageBox 0 "Please, press OK for test pass!" (c-string "load-library test")
;    (bor MB_OKCANCEL MB_ICONASTERISK))
;  IDOK)
;    (print "OK")
;    (print "CANCEL"))
; todo: вроде бы все строки и так заканчиваются на '\0' - проверить
;(define echo "echo server")

; в момент импорта сделать все нужные привязки
; export (MessageBox)  и т.д.

;(define window 0)
(define window (CreateWindowEx
    (OR WS_EX_APPWINDOW WS_EX_WINDOWEDGE) "LISTBOX" "OL OpenGL Framework"
    (OR WS_OVERLAPPEDWINDOW WS_CLIPSIBLINGS WS_CLIPCHILDREN)
    0 0 640 480 ; x y width height
    0 ; no parent window
    0 ; no menu
    0 ; instance
    0)) ; don't pass anything to WM_CREATE
    
; PIXELFORMATDESCRIPTOR
(define pfd (list->vector '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                              00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))                        
(define hDC (GetDC window))
(define PixelFormat (ChoosePixelFormat hDC pfd))
(SetPixelFormat hDC PixelFormat pfd)
(define hRC (wglCreateContext hDC))

(wglMakeCurrent hDC hRC)
(ShowWindow window SW_SHOW)
(SetForegroundWindow window)
(SetFocus window)

(CreateGLWindow window 16 0 hDC hRC)

;(WinMain 0 0 0 0)

(define MSG (make-vector 28 0)) ; sizeof(MSG)=28
;(call/cc (lambda (return)
(define (cycle)   ;MSG
  (if (= 1 (PeekMessage MSG 0 0 0 PM_REMOVE))
    (begin  
      (TranslateMessage MSG)
      (DispatchMessage MSG))
      
    (begin ; DrawGLScene
      (glClear GL_COLOR_BUFFER_BIT)
      (glLoadIdentity)
      (SwapBuffers hDC)))
  (if (= (GetAsyncKeyState 27) 0) (cycle)))
(cycle)

; KillGLWindow
(wglMakeCurrent 0 0)
(wglDeleteContext hRC)
(ReleaseDC window hDC)
(DestroyWindow window)

(print "@")

