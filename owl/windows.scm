(define-library (owl windows)
  (export 
    GetModuleHandle  ; 
    
    user32 IDOK IDCANCEL
    MessageBox       ;
      MB_OK MB_OKCANCEL MB_ICONASTERISK
    
    PeekMessage      ;
      PM_REMOVE
    TranslateMessage ;
    DispatchMessage  ;
    PostQuitMessage  ;
    
    GetKeyState      ;
    GetAsyncKeyState ;
    GetKeyboardState ;
    
    CreateWindowEx   ;
    WS_EX_APPWINDOW
    WS_EX_WINDOWEDGE
    WS_OVERLAPPEDWINDOW
    WS_CLIPSIBLINGS
    WS_CLIPCHILDREN
    DestroyWindow   ;
    GetDC ReleaseDC
    ShowWindow SW_SHOW
    SetForegroundWindow SetFocus
    
    ; gdi32
    ChoosePixelFormat
    SetPixelFormat
    SwapBuffers
    
    ; wgl
    wglCreateContext wglMakeCurrent wglDeleteContext wglGetProcAddress
  )

  (import
      (owl defmac) (owl io)
      (owl list) (owl string)
      (owl math)
      (owl pinvoke))
  (begin

; пример, как можно получить свои собственные функции (если они экспортируются, конечно)
(define kernel32_dll (dlopen "kernel32" 0))
  (define GetModuleHandle (dlsym type-handle kernel32_dll "GetModuleHandleA"))

; вспомогательный макрос для собрать в кучку все bor
(define OR (lambda list (fold bor 0 list)))


;(define _exe (GetModuleHandle 0))
;(define CreateGLWindow (dlsym-c type-fix+ _exe "CreateGLWindow"))

(define user32 (dlopen "user32" 0))
  (define IDOK 1)
  (define IDCANCEL 2)

  (define MessageBox (dlsym type-fix+ user32 "MessageBoxA"))
    (define MB_OK 0)
    (define MB_OKCANCEL 1)
    (define MB_ICONASTERISK 64)
  (define PeekMessage      (dlsym type-fix+ user32 "PeekMessageA"))
    (define PM_REMOVE 1)
  (define TranslateMessage (dlsym type-fix+ user32 "TranslateMessage"))
  (define DispatchMessage  (dlsym type-fix+ user32 "DispatchMessageA"))
  (define PostQuitMessage  (dlsym type-fix+ user32 "PostQuitMessage"))
  ;; давление юры 06/09/2014 в 13:43 - 125/ 91
  ;;                           14.07 - 130/101 (после чашки кофе, голова пре-болеть перестала)
  (define GetKeyState      (dlsym type-fix+ user32 "GetKeyState"))
  (define GetAsyncKeyState (dlsym type-fix+ user32 "GetAsyncKeyState"))
  (define GetKeyboardState (dlsym type-fix+ user32 "GetKeyboardState"))
  
  ;; функции работы с win32 окнами
  (define CreateWindowEx   (dlsym type-handle user32 "CreateWindowExA")) ; ANSI version
    (define WS_EX_APPWINDOW      #x00040000)
    (define WS_EX_WINDOWEDGE     #x00000100)
    (define WS_OVERLAPPEDWINDOW  (OR #x00000000 #x00C00000 #x00080000 #x00040000 #x00020000 #x00010000))
    (define WS_CLIPSIBLINGS      #x04000000)
    (define WS_CLIPCHILDREN      #x02000000)
  (define DestroyWindow    (dlsym type-fix+   user32 "DestroyWindow"))
    
  (define GetDC               (dlsym type-handle user32 "GetDC"))
  (define ReleaseDC           (dlsym type-fix+   user32 "ReleaseDC"))
  (define ShowWindow          (dlsym type-fix+   user32 "ShowWindow"))
    (define SW_SHOW 5)
  (define SetForegroundWindow (dlsym type-fix+   user32 "SetForegroundWindow"))
  (define SetFocus            (dlsym type-fix+   user32 "SetFocus"))
  
  
  
(define gdi32 (dlopen "gdi32" 0))
  (define ChoosePixelFormat (dlsym type-fix+ gdi32 "ChoosePixelFormat"))
  (define SetPixelFormat    (dlsym type-fix+ gdi32 "SetPixelFormat"))
  (define SwapBuffers       (dlsym type-fix+ gdi32 "SwapBuffers"))

; -=( wgl )=------------------------------------------------------------
(define opengl32 (dlopen "opengl32" 0))
  (define wglCreateContext  (dlsym type-handle opengl32 "wglCreateContext" ))
  (define wglMakeCurrent    (dlsym type-fix+   opengl32 "wglMakeCurrent" ))
  (define wglDeleteContext  (dlsym type-fix+   opengl32 "wglDeleteContext" ))
  (define wglGetProcAddress (dlsym type-handle opengl32 "wglGetProcAddress" ))
    (define (wgl-proc-address type name)
      (let ((function (cons type (wglGetProcAddress (c-string name)))))
        (lambda args
          (sys-prim 32 (cdr function) (car function) args))))

))