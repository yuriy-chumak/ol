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

(setq SetWindowLong    (user32 fft-void* "SetWindowLongW" fft-void* fft-int type-callable))
(setq GetWindowLongPtr (user32 fft-void* "GetWindowLongPtrW" fft-void* fft-int))
(setq SetWindowLongPtr (user32 fft-void* "SetWindowLongPtrW" fft-void* fft-int type-callable))
(setq DefWindowProc    (user32 fft-long "DefWindowProcW" fft-void* fft-unsigned-int fft-void* fft-void*))

(setq GetWindowRect    (user32 fft-int "GetWindowRect" fft-void* (fft& fft-int)))

(setq wglCreateContext (opengl32 type-vptr "wglCreateContext" fft-void*))
(setq wglMakeCurrent   (opengl32 fft-int "wglMakeCurrent" fft-void* fft-void*))

; functions
(define (native:create-context title)
   (print "native:create-context")
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
                        (GetWindowRect hWnd rect)
                        (mail 'opengl ['resize (lref rect 2) (lref rect 3)]))
                     (else
                        #false)) ; nothing
                  ; pass trough to the original handler
                  (ffi OldProc
                     (cons fft-void*
                     (list fft-void* fft-unsigned-int fft-void* fft-void*))
                     (list hWnd Msg wParam lParam))))))
         (SetWindowLongPtr window -4 (make-callback WndProc)))

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
(setq PeekMessage      (user32 fft-int "PeekMessageA"     fft-void* fft-void* fft-int fft-int fft-int))
(setq TranslateMessage (user32 fft-int "TranslateMessage" fft-void*))
(setq GetMessage       (user32 fft-int "GetMessageA"      fft-void* fft-void* fft-int fft-int))
(setq DispatchMessage  (user32 fft-int "DispatchMessageA" fft-void*))

(define (native:process-events context handler)
   (define MSG (make-bytevector 48)) ; 28 for x32

   (let loop ()
      (when (= (PeekMessage MSG #false 0 0 1) 1)
         (let*((w (size nullptr))
               (message (bytevector->int32 MSG w)))      ; 4 for x32
            ;(print message ": " message)
            (cond
               ((and (eq? message 273) ; WM_COMMAND
                     (eq? (+ (<< (ref MSG (+ 0 (* w 2))) 0)
                              (<< (ref MSG (+ 1 (* w 2))) 8)) 2)) ; wParam, IDCANCEL
                  (handler ['quit])) ; EXIT
               ;; ((and (eq? message 256) ; WM_KEYDOWN (?)
               ;;       (eq? (+ (<< (ref MSG (+ 0 (* w 2))) 0)
               ;;               (<< (ref MSG (+ 1 (* w 2))) 8)) #x51)) ; Q key
               ;;    (handler ['keyboard Q]))
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

(define (gl:SetWindowSize context width height)
   (vector-apply context (lambda (dc glrc window)
      (let ((rect '(0 0 0 0)))
         (GetWindowRect window rect)
         (MoveWindow window (list-ref rect 0) (list-ref rect 1) width height 0)))))

; ---
(setq ShowCursor (user32 fft-int "ShowCursor" fft-int))
(define (gl:HideCursor context)
   (ShowCursor 0))

; ---
(setq GetCursorPos (user32 fft-int "GetCursorPos" fft-int&))
(setq ScreenToClient (user32 fft-int "ScreenToClient" type-vptr fft-int&))

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
