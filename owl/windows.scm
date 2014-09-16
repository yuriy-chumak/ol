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

(define INTEGER type-int+)

; Windows Data Types http://msdn.microsoft.com/en-us/library/windows/desktop/aa383751(v=vs.85).aspx
; todo: please, sort this types:
(define DWORD     INTEGER)
(define LPCTSTR   type-string)
(define int       INTEGER)
(define HWND      INTEGER)
(define HMENU     INTEGER)
(define HINSTANCE INTEGER)
(define LPVOID    type-tuple)
(define UINT      INTEGER)
(define BOOL      type-fix+)

(define LPMSG     type-vector-raw)
(define LRESULT   INTEGER)
(define VOID      type-void)
(define SHORT     INTEGER)
(define PBYTE     type-vector-raw)

(define HDC       INTEGER) ;type-handle)
(define HGLRC     INTEGER)
(define PROC      type-handle)
(define LPCSTR    type-string)



; пример, как можно получить свои собственные функции (если они экспортируются, конечно)
(define kernel32_dll (dlopen "kernel32" 0))
  (define GetModuleHandle (dlsym kernel32_dll type-handle "GetModuleHandleA" LPCTSTR))

; вспомогательный макрос для собрать в кучку все bor
(define OR (lambda args (fold bor 0 args)))


;(define _exe (GetModuleHandle 0))
;(define CreateGLWindow (dlsym-c type-fix+ _exe "CreateGLWindow"))

(define user32 (dlopen "user32" 0))
  (define IDOK 1)
  (define IDCANCEL 2)

  (define MessageBox (dlsym user32 int "MessageBoxA" HWND LPCTSTR LPCTSTR UINT))
    (define MB_OK 0)
    (define MB_OKCANCEL 1)
    (define MB_ICONASTERISK 64)
  (define PeekMessage      (dlsym user32 (__stdcall BOOL) "PeekMessageA" LPMSG HWND UINT UINT UINT))
    (define PM_REMOVE 1)
  (define TranslateMessage (dlsym user32 (__stdcall BOOL) "TranslateMessage" LPMSG))
  (define DispatchMessage  (dlsym user32 (__stdcall LRESULT) "DispatchMessageA" LPMSG))
  (define PostQuitMessage  (dlsym user32 (__stdcall VOID) "PostQuitMessage" int))
  ;; давление юры 06/09/2014 в 13:43 - 125/ 91
  ;;                           14.07 - 130/101 (после чашки кофе, голова пре-болеть перестала)
  (define GetKeyState      (dlsym user32 (__stdcall SHORT) "GetKeyState" int))
  (define GetAsyncKeyState (dlsym user32 (__stdcall SHORT) "GetAsyncKeyState" int))
  (define GetKeyboardState (dlsym user32 (__stdcall BOOL) "GetKeyboardState" PBYTE))
  
  ;; функции работы с win32 окнами
  (define CreateWindowEx   (dlsym user32 (__stdcall HWND) "CreateWindowExA" DWORD LPCTSTR LPCTSTR DWORD int int int int HWND HMENU HINSTANCE LPVOID)) ; ANSI version
    (define WS_EX_APPWINDOW      #x00040000)
    (define WS_EX_WINDOWEDGE     #x00000100)
    (define WS_OVERLAPPEDWINDOW  (OR #x00000000 #x00C00000 #x00080000 #x00040000 #x00020000 #x00010000))
    (define WS_CLIPSIBLINGS      #x04000000)
    (define WS_CLIPCHILDREN      #x02000000)
  (define DestroyWindow    (dlsym user32 (__stdcall BOOL) "DestroyWindow" HWND))
    
  (define GetDC               (dlsym user32 (__stdcall HDC) "GetDC" HWND))
  (define ReleaseDC           (dlsym user32 (__stdcall int) "ReleaseDC" HWND HDC))
  (define ShowWindow          (dlsym user32 (__stdcall BOOL) "ShowWindow" HWND int))
    (define SW_SHOW 5)
  (define SetForegroundWindow (dlsym user32 BOOL "SetForegroundWindow" HWND))
  (define SetFocus            (dlsym user32
    HWND ; If the function succeeds, the return value is the handle to the window
         ; that previously had the keyboard focus. If the hWnd parameter is invalid
         ; or the window is not attached to the calling thread's message queue, the
         ; return value is NULL. To get extended error information, call GetLastError.
    "SetFocus"
    HWND ;hWnd
         ; A handle to the window that will receive the keyboard input. If this parameter is NULL, keystrokes are ignored.
    ))
  
(define PIXELFORMATDESCRIPTOR* type-vector-raw)
  
(define gdi32 (dlopen "gdi32" 0))
  (define ChoosePixelFormat (dlsym gdi32 int "ChoosePixelFormat" HDC PIXELFORMATDESCRIPTOR*))
  (define SetPixelFormat    (dlsym gdi32 BOOL "SetPixelFormat" HDC int PIXELFORMATDESCRIPTOR*))
  (define SwapBuffers       (dlsym gdi32 BOOL "SwapBuffers" HDC))

; -=( wgl )=------------------------------------------------------------
(define opengl32 (dlopen "opengl32" 0))
  (define wglCreateContext  (dlsym opengl32 HGLRC "wglCreateContext" HDC))
  (define wglMakeCurrent    (dlsym opengl32 BOOL  "wglMakeCurrent" HDC HGLRC))
  (define wglDeleteContext  (dlsym opengl32 BOOL  "wglDeleteContext" HGLRC))
  (define wglGetProcAddress (dlsym opengl32 PROC  "wglGetProcAddress" LPCSTR))
;  (define (wgl-proc-address type name)
;    (let ((function (cons type (wglGetProcAddress (c-string name)))))
;      (lambda args
;        (sys-prim 32 (cdr function) (car function) args))))

))