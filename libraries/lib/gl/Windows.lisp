(setq user32 (load-dynamic-library "user32.dll"))
(setq gdi32  (load-dynamic-library "gdi32.dll"))
(setq opengl32 (load-dynamic-library "opengl32.dll"))

(setq CreateWindowEx (user32 fft-void* "CreateWindowExW"
                        fft-int type-string-wide type-string-wide
                        fft-int fft-int fft-int fft-int fft-int
                        fft-void* fft-void* fft-void* fft-void*))
(setq GetDC            (user32 fft-void* "GetDC" fft-void*))
(setq ShowWindow       (user32 fft-int "ShowWindow" fft-void* fft-int))
(setq ChoosePixelFormat(gdi32  fft-int "ChoosePixelFormat" fft-void* fft-void*))
(setq SetPixelFormat   (gdi32  fft-int "SetPixelFormat" fft-void* fft-int fft-void*))
(setq SwapBuffers      (gdi32  fft-int "SwapBuffers" fft-void*))

(setq GetWindowLong    (user32 fft-long "GetWindowLongW" fft-void* fft-int))
(setq SetWindowLong    (user32 fft-void* "SetWindowLongW" fft-void* fft-int type-callable))
(setq GetWindowLongPtr (user32 fft-void* "GetWindowLongPtrW" fft-void* fft-int))
(setq SetWindowLongPtr (user32 fft-void* "SetWindowLongPtrW" fft-void* fft-int type-callable))
(setq DefWindowProc    (user32 fft-long "DefWindowProcW" fft-void* fft-unsigned-int fft-void* fft-void*))

(setq SetWindowsHookEx (user32 fft-void* "SetWindowsHookExA" fft-int type-callable fft-void* fft-int))
(setq CallNextHookEx   (user32 fft-int "CallNextHookEx" fft-void* fft-int fft-int fft-int))
(setq GetForegroundWindow (user32 fft-void* "GetForegroundWindow"))

(setq GetWindowRect    (user32 fft-int "GetWindowRect" fft-void* (fft& fft-int)))
(setq GetClientRect    (user32 fft-int "GetClientRect" fft-void* (fft& fft-int)))

(setq GetCursorPos     (user32 fft-int "GetCursorPos" fft-int&))
(setq ScreenToClient   (user32 fft-int "ScreenToClient" type-vptr fft-int&))

(setq PeekMessage      (user32 fft-bool "PeekMessageA"     fft-void* fft-void* fft-int fft-int fft-int))
(setq PostMessage      (user32 fft-bool "PostMessageA"     fft-void* fft-int fft-int fft-int))
(setq TranslateMessage (user32 fft-bool "TranslateMessage" fft-void*))
(setq DispatchMessage  (user32 fft-int "DispatchMessageA" fft-void*))

(setq wglCreateContext (opengl32 type-vptr "wglCreateContext" fft-void*))
(setq wglMakeCurrent   (opengl32 fft-int "wglMakeCurrent" fft-void* fft-void*))

; functions
(define (native:create-context title)
   (let*((window (CreateWindowEx
            #x00040100 "#32770" title ; WS_EX_APPWINDOW|WS_EX_WINDOWEDGE, #32770 is system classname for DIALOG
            #x06cf0000 ; WS_OVERLAPPEDWINDOW | WS_CLIPSIBLINGS | WS_CLIPCHILDREN
            0 0 WIDTH HEIGHT ; x y width height
            #false ; no parent window
            #false ; no menu
            #false ; instance
            #false))
         (pfd (make-bytevector '(
                #x28 00 ; nSize
                   1 00 ; nVersion
                #x25 00 00 00 ; dwFlags - PFD_DRAW_TO_WINDOW|PFD_SUPPORT_OPENGL|PFD_DOUBLEBUFFER
                  00 ; iPixelType - PFD_TYPE_RGBA
                  24 ; cColorBits
                  00 00 00 00 00 00 ; cRedBits, cRedShift, cGreenBits, cGreenShift, cBlueBits, cBlueShift (Not used)
                  00 00             ; cAlphaBits, cAlphaShift 
                  00 00 00 00 00    ; cAccumBits, cAccumRedBits, cAccumGreenBits, cAccumBlueBits, cAccumAlphaBits
                  32 ; cDepthBits
                  00 ; cStencilBits
                  00 ; cAuxBuffers
                  00 ; iLayerType - PFD_MAIN_PLANE
                  00 ; bReserved
                  00 00 00 00 ; dwLayerMask
                  00 00 00 00 ; dwVisibleMask
                  00 00 00 00 ; dwDamageMask
               )))
         (hDC (GetDC window))
         (PixelFormat (ChoosePixelFormat hDC pfd)))
      (SetPixelFormat hDC PixelFormat pfd)

      ; we must capture WM_SIZE messages to be processed by opengl coroutine
      (when GetWindowLongPtr ; supported only by 64-bit windows
         (define OldProc (GetWindowLongPtr window -4))

         (define WndProc (vm:pin (cons
               (cons fft-long (list
                     fft-void* fft-unsigned-int fft-void* fft-void*))
               (lambda (hWnd Msg wParam lParam)
                  (case Msg
                     (5 ;WM_SIZE
                        (define rect '(0 0 0 0))
                        (GetClientRect hWnd rect)
                        (mail 'opengl ['resize (lref rect 2) (lref rect 3)]))
                     (else #false)) ; nothing
                  ; pass through to the original handler
                  (ffi OldProc
                     (cons fft-void*
                     (list fft-void* fft-unsigned-int fft-void* fft-void*))
                     (list hWnd Msg wParam lParam))))))
         (SetWindowLongPtr window -4 (make-callback WndProc)))

      (when SetWindowsHookEx
         (define LowLevelMouseProc (vm:pin (cons
            (cons fft-int (list
                  fft-int fft-int fft-int))
            (lambda (nCode wParam lParam)
               (call/cc (lambda (ret)
                  (when (and (eq? nCode 0) (equal? (GetForegroundWindow) window))
                     ; mouse position inside window
                     (define pos [0 0])
                     (GetCursorPos pos)
                     (ScreenToClient window pos)

                     ; window client size
                     (define rect [0 0 0 0])
                     (GetClientRect window rect)

                     (when (and (<= 0 (ref pos 1) (ref rect 3))
                                (<= 0 (ref pos 2) (ref rect 4)))
                        (case wParam
                           (#x0201 ; WM_LBUTTONDOWN
                              (PostMessage window #x0201 0 (+ (<< (ref pos 1) 16) (ref pos 2)))
                              (ret -1))
                           (#x0204 ; WM_RBUTTONDOWN
                              (PostMessage window #x0204 0 (+ (<< (ref pos 1) 16) (ref pos 2)))
                              (ret -1))
                           (else #false)))) ; skip
                  (CallNextHookEx #f nCode wParam lParam)))))))
         (SetWindowsHookEx 14 (make-callback LowLevelMouseProc) #f 0)) ; WH_MOUSE_LL

      (ShowWindow window 5)
      (let ((hRC (wglCreateContext hDC)))
         (wglMakeCurrent hDC hRC)
         (print-to stderr "OpenGL version: " (glGetString GL_VERSION))
         (print-to stderr "OpenGL vendor: " (glGetString GL_VENDOR))
         (print-to stderr "OpenGL renderer: " (glGetString GL_RENDERER))

         [hDC hRC window])))

(define (native:enable-context context)
   (vector-apply context (lambda (dc glrc window)
      (wglMakeCurrent dc glrc))))

(define (native:disable-context context)
   (wglMakeCurrent #f #f))

(define (native:swap-buffers context)
   (vector-apply context (lambda (dc glrc window)
      (SwapBuffers dc))))

; -=( process events )=--------------------------------
(setq sizeof-MSG       (if (eq? (size nullptr) 4) 28 48));sizeof(MSG)

(define (native:process-events context handler)
   (define MSG (make-bytevector sizeof-MSG))

   (let loop ()
      (when (PeekMessage MSG #F 0 0 1) ; PM_REMOVE
         (let*((w (size nullptr))
               (message (bytevector->int32 MSG w)))
            (define wParam (bytevector->int32 MSG (+ w w)))
            ;; (print "message: " message)
            (cond
               ((and (eq? message 273) ; WM_COMMAND
                     (eq? wParam 2))   ; IDCANCEL
                  (handler ['quit]))
               ((and (eq? message 256)); WM_KEYDOWN
                  (handler ['keyboard wParam]))
               ((and (eq? message #x0201))
                  (define y (bytevector->int16 MSG (+ w w w)))
                  (define x (bytevector->int16 MSG (+ w w w 2)))
                  (handler ['mouse 1 x y]))
               ((and (eq? message #x0204))
                  (define y (bytevector->int16 MSG (+ w w w)))
                  (define x (bytevector->int16 MSG (+ w w w 2)))
                  (handler ['mouse 2 x y]))
               (else
                  (TranslateMessage MSG)
                  (DispatchMessage MSG)
                  (loop)))))))

; ---
(setq SetWindowText (user32 fft-int "SetWindowTextW" fft-void* type-string-wide))

(define (gl:SetWindowTitle context title)
   (vector-apply context (lambda (dc glrc window)
      (SetWindowText window title))))

; ---
(setq MoveWindow (user32 fft-int "MoveWindow" fft-void* fft-int fft-int fft-int fft-int fft-int))
(setq AdjustWindowRectEx (user32 fft-bool "AdjustWindowRectEx" fft-int& fft-int fft-int fft-int))
   (setq GWL_STYLE -16) (setq GWL_EXSTYLE -20)
(setq GetMenu (user32 fft-void* "GetMenu" fft-void*))

(define (gl:SetWindowSize context width height)
   (vector-apply context (lambda (dc glrc window)
      (let ((rect '(0 0 0 0))
            (area (list 0 0 width height)))
         (GetWindowRect window rect)
         (AdjustWindowRectEx area (GetWindowLong window GWL_STYLE) (if (GetMenu window) 1 0) (GetWindowLong window GWL_EXSTYLE))
         (MoveWindow window (list-ref rect 0) (list-ref rect 1)
            (- (list-ref area 2) (list-ref area 0))
            (- (list-ref area 3) (list-ref area 1))
            0)))))

; ---
(setq ShowCursor (user32 fft-int "ShowCursor" fft-int))
(define (gl:HideCursor context)
   (ShowCursor 0))

; ---
(define (gl:GetMousePos context)
   (vector-apply context (lambda (dc glrc window)
      (let ((pos '(0 0)))
         (GetCursorPos pos)
         (ScreenToClient window pos)
         (cons (car pos) (cadr pos))))))

;;          (setq GDI (load-dynamic-library "gdi32.dll"))
;;          (define gl:CreateContext (WGL type-vptr "wglCreateContext" fft-void*))
;;          (define gl:MakeCurrent (WGL fft-int "wglMakeCurrent" fft-void* fft-void*))

;;          (define gl:SwapBuffers
;;             (let ((SwapBuffers (GDI fft-int "SwapBuffers" fft-void*)))
;;                (lambda (context)
;;                   (if context
;;                      (SwapBuffers (ref context 1))))))
