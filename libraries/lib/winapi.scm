(define-library (lib winapi)
(export
   kernel32
   GetModuleHandle  ;


   user32 IDOK IDCANCEL
   MessageBox       ;
     MB_OK MB_OKCANCEL MB_ICONASTERISK

    PeekMessage      ;
      PM_REMOVE
    TranslateMessage ;
    DispatchMessage  ;
    PostQuitMessage  ;
      WM_SIZE WM_WINDOWPOSCHANGED
      WM_CREATE WM_LBUTTONDOWN
      WM_SIZING
      WM_KEYDOWN WM_KEYUP
      WM_PAINT

    GetKeyState      ;
    GetAsyncKeyState ;
    GetKeyboardState ;

    GetSystemMetrics ;
      SM_CXSCREEN
      SM_CYSCREEN

    CreateWindowEx   ;
      WS_EX_APPWINDOW
      WS_EX_WINDOWEDGE
      WS_OVERLAPPEDWINDOW
      WS_CLIPSIBLINGS
      WS_CLIPCHILDREN
      WS_POPUP
    DestroyWindow   ;
    GetDC ReleaseDC ;
    ShowWindow
      SW_SHOW
    SetForegroundWindow SetFocus
    GetWindowRect
    GetClientRect
    GetCursorPos
    ScreenToClient

   ; gdi32
    ChoosePixelFormat
    SetPixelFormat
    SwapBuffers
   
   ; wgl
    wglCreateContext wglMakeCurrent wglDeleteContext wglGetProcAddress
  )

  (import
      (r5rs core) (owl io)
      (owl list) (owl string)
      (owl math) (otus ffi))
(begin

; NOT: __stdcall not required - olvm automatically releases user by caller stack arguments

; Windows Data Types
; https://msdn.microsoft.com/en-us/library/windows/desktop/aa383751(v=vs.85).aspx

(define LONG_PTR
   (cond
      ((eq? (vm:wordsize) 4) ; 32-bit windows
         fft-int32)
      (else                  ; 64-bit windows
         fft-int64)))

(define int       fft-int)

(define DWORD     fft-unsigned-long)   ; 32-bit for all windows'es
(define LPCTSTR   type-string)
(define PVOID     type-vptr)
(define LPVOID    type-vptr)

(define UINT      fft-unsigned-int)
(define BOOL      fft-int)

(define HANDLE    PVOID)

(define HWND      HANDLE)
(define HMENU     HANDLE)
(define HINSTANCE HANDLE)

(define LPMSG     type-vptr)
(define LRESULT   LONG_PTR)
(define VOID      fft-void)
(define SHORT     fft-short)

(define BYTE      fft-unsigned-char)
(define PBYTE     (fft* BYTE))

(define HDC       HANDLE)
(define HGLRC     HANDLE)
(define PROC      type-vptr)
(define LPCSTR    type-string)
(define LPRECT    type-vptr)
(define LPPOINT   type-vptr)


; -= kernel32 =-----
(define kernel32 (load-dynamic-library "kernel32"))
(define GetModuleHandle (kernel32 type-vptr "GetModuleHandleA" LPCTSTR))

; вспомогательный макрос для собрать в кучку все bor
(define OR (lambda args (fold bor 0 args)))

; -= user32 =-------
(define user32 (load-dynamic-library "user32"))
  (define IDOK 1)
  (define IDCANCEL 2)

  (define MessageBox (user32 int "MessageBoxW" HWND type-string-wide type-string-wide UINT))
    (define MB_OK 0)
    (define MB_OKCANCEL 1)
    (define MB_ICONASTERISK 64)
  (define PeekMessage      (user32 BOOL "PeekMessageA" LPMSG HWND UINT UINT UINT))
    (define PM_REMOVE 1)
  (define TranslateMessage (user32 BOOL "TranslateMessage" LPMSG))
  (define DispatchMessage  (user32 LRESULT "DispatchMessageA" LPMSG))
  (define PostQuitMessage  (user32 VOID "PostQuitMessage" int))
    (define WM_CREATE #x0001)
    (define WM_SIZE #x0005)
    (define WM_WINDOWPOSCHANGED #x0047)
    (define WM_LBUTTONDOWN #x0201)
    (define WM_SIZING #x0214)
    (define WM_KEYDOWN 256)
    (define WM_KEYUP 257)
    (define WM_PAINT 15)
 ;; давление юры 06/09/2014 в 13:43 - 125/ 91
 ;;                           14.07 - 130/101 (после чашки кофе, голова пре-болеть перестала)
  (define GetKeyState      (user32 SHORT "GetKeyState" int))
  (define GetAsyncKeyState (user32 SHORT "GetAsyncKeyState" int))
  (define GetKeyboardState (user32 BOOL "GetKeyboardState" PBYTE))

  (define GetSystemMetrics (user32 int "GetSystemMetrics" int))
    (define SM_CXSCREEN 0)
    (define SM_CYSCREEN 1)

  ;; функции работы с win32 окнами
  (define CreateWindowEx   (user32 HWND "CreateWindowExA" DWORD LPCTSTR LPCTSTR DWORD int int int int HWND HMENU HINSTANCE LPVOID)) ; ANSI version
    (define WS_EX_APPWINDOW      #x00040000)
    (define WS_EX_WINDOWEDGE     #x00000100)
    (define WS_OVERLAPPEDWINDOW  (OR #x00000000 #x00C00000 #x00080000 #x00040000 #x00020000 #x00010000))
    (define WS_CLIPSIBLINGS      #x04000000)
    (define WS_CLIPCHILDREN      #x02000000)
    (define WS_POPUP             #x80000000)
  (define DestroyWindow    (user32 BOOL "DestroyWindow" HWND))

  (define GetDC               (user32 HDC "GetDC" HWND))
  (define ReleaseDC           (user32 int "ReleaseDC" HWND HDC))
  (define ShowWindow          (user32 BOOL "ShowWindow" HWND int))
    (define SW_SHOW 5)
  (define SetForegroundWindow (user32 BOOL "SetForegroundWindow" HWND))
  (define SetFocus            (user32
    HWND ; If the function succeeds, the return value is the handle to the window
         ; that previously had the keyboard focus. If the hWnd parameter is invalid
         ; or the window is not attached to the calling thread's message queue, the
         ; return value is NULL. To get extended error information, call GetLastError.
    "SetFocus"
    HWND ;hWnd
         ; A handle to the window that will receive the keyboard input. If this parameter is NULL, keystrokes are ignored.
    ))
  (define GetWindowRect       (user32 BOOL "GetWindowRect" HWND LPRECT))
  (define GetClientRect       (user32 BOOL "GetClientRect" HWND LPRECT))
  (define GetCursorPos        (user32 BOOL "GetCursorPos" LPPOINT))
  (define ScreenToClient      (user32 BOOL "ScreenToClient" HWND LPPOINT))

(define PIXELFORMATDESCRIPTOR* type-vptr)

(define gdi32 (load-dynamic-library "gdi32"))
  (define ChoosePixelFormat (gdi32 int  "ChoosePixelFormat" HDC PIXELFORMATDESCRIPTOR*))
  (define SetPixelFormat    (gdi32 BOOL "SetPixelFormat" HDC int PIXELFORMATDESCRIPTOR*))
  (define SwapBuffers       (gdi32 BOOL "SwapBuffers" HDC))

; -=( wgl )=------------------------------------------------------------
(define opengl32 (load-dynamic-library "opengl32"))
  (define wglCreateContext  (opengl32 HGLRC "wglCreateContext" HDC))
  (define wglMakeCurrent    (opengl32 BOOL  "wglMakeCurrent" HDC HGLRC))
  (define wglDeleteContext  (opengl32 BOOL  "wglDeleteContext" HGLRC))
  (define wglGetProcAddress (opengl32 PROC  "wglGetProcAddress" LPCSTR))
;  (define (wgl-proc-address type name)
;    (let ((function (cons type (wglGetProcAddress (c-string name)))))
;      (lambda args
;        (syscall 32 (cdr function) (car function) args))))

))