(define-library (lib opengl)
   (import
      (otus lisp) (otus ffi)
      (OpenGL version-1-2))

   (export
      (exports (OpenGL version-1-2))
      gl:run

      gl:Create ; create window + context
      gl:Enable gl:Disable

      gl:SwapBuffers
      gl:ProcessEvents)

(begin
(define OS (ref (uname) 1))

(define win32? (string-ci=? OS "Windows"))
(define linux? (string-ci=? OS "Linux"))

; check the platform
(or win32? linux?
   (runtime-error "Unsupported platform" OS))

(define WIDTH 640)
(define HEIGHT 480)

; ===================================================
(define gl:Enable (cond
   (win32?  (lambda (context)
               (let ((dc   (ref context 1))
                     (glrc (ref context 2)))
                  (gl:MakeCurrent dc glrc))))
   (linux?  (lambda (context)
               (let ((display (ref context 1))
                    ;(screen  (ref context 2))
                     (window  (ref context 3))
                     (cx      (ref context 4)))
                  (gl:MakeCurrent display window cx))))))

(define gl:Disable (cond
   (win32?  (lambda (context)
                  (gl:MakeCurrent #f #f)))
   (linux?  (lambda (context)
               (let ((display (ref context 1)))
                  (gl:MakeCurrent display #f #f))))))

(define gl:Create (cond
   ; -=( win32 )=---------------------------------------------------------------------
   (win32?
      (let ((user32 (load-dynamic-library "user32.dll"))
            (gdi32  (load-dynamic-library "gdi32")))
      (let ((CreateWindowEx   (user32 fft-void* "CreateWindowExA" fft-int type-string type-string fft-int fft-int fft-int fft-int fft-int fft-void* fft-void* fft-void* fft-void*))
            (GetDC            (user32 fft-void* "GetDC" fft-void*))
            (ShowWindow       (user32 fft-int "ShowWindow" fft-void* fft-int))
            (ChoosePixelFormat(gdi32  fft-int "ChoosePixelFormat" fft-void* fft-void*))
            (SetPixelFormat   (gdi32  fft-int "SetPixelFormat" fft-void* fft-int fft-void*)))
      (lambda (title)
         (let*((window (CreateWindowEx
                  #x00040100 (c-string "#32770") (c-string title) ; WS_EX_APPWINDOW|WS_EX_WINDOWEDGE, #32770 is system classname for DIALOG
                  #x06cf0000 ; WS_OVERLAPPEDWINDOW | WS_CLIPSIBLINGS | WS_CLIPCHILDREN
                  0 0 WIDTH HEIGHT ; x y width height
                  #false ; no parent window
                  #false ; no menu
                  #false ; instance
                  #false))
               (pfd (vm:new-raw-object type-vector-raw '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                                          00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))
               (hDC (GetDC window))
               (PixelFormat (ChoosePixelFormat hDC pfd)))
            (SetPixelFormat hDC PixelFormat pfd)
         (let ((hRC (gl:CreateContext hDC)))
            (gl:MakeCurrent hDC hRC)
            (print "OpenGL version: " (glGetString GL_VERSION))
            (print "OpenGL vendor: " (glGetString GL_VENDOR))
            (print "OpenGL renderer: " (glGetString GL_RENDERER))
           ;(gl:MakeCurrent #f #f)

            (ShowWindow window 5)
            (tuple hDC hRC window)))))))
   ; -=( linux )=---------------------------------------------------------------------
   (linux?
      (let ((libX11 (load-dynamic-library "libX11.so"))
            (libGLX (load-dynamic-library "libGL.so.1")))
      (let ((XOpenDisplay  (libX11 type-vptr "XOpenDisplay" type-string))
            (XDefaultScreen(libX11 fft-int "XDefaultScreen" type-vptr))
            (XRootWindow   (libX11 type-vptr "XRootWindow" type-vptr fft-int))
            (XBlackPixel   (libX11 type-vptr "XBlackPixel" type-vptr fft-int))
            (XWhitePixel   (libX11 type-vptr "XWhitePixel" type-vptr fft-int))
            (XCreateSimpleWindow (libX11 type-vptr "XCreateSimpleWindow"
                              type-vptr type-vptr ; display, parent
                              fft-int fft-int fft-unsigned-int fft-unsigned-int ; x y width height
                              fft-unsigned-int  ; border width
                              type-vptr ; border
                              type-vptr ; background
                           ))
            (XSelectInput (libX11 fft-int "XSelectInput" type-vptr type-vptr fft-long))
            (XMapWindow   (libX11 fft-int "XMapWindow" type-vptr type-vptr))
            (XStoreName   (libX11 fft-int "XStoreName" type-vptr type-vptr type-string))
            (glXChooseVisual  (libGLX type-vptr "glXChooseVisual" type-vptr fft-int fft-int*))
            (glXCreateContext (libGLX type-vptr "glXCreateContext" type-vptr type-vptr type-vptr fft-int)))
      (lambda (title)
         (let*((display (XOpenDisplay #false))
               (screen  (XDefaultScreen display))
               (window  (XCreateSimpleWindow display (XRootWindow display screen)
                           0 0 WIDTH HEIGHT 1
                           (XBlackPixel display screen)
                           (XWhitePixel display screen)))
               (vi (glXChooseVisual display screen
                     '( 4 ; GLX_RGBA
                        8  1 ; GLX_RED_SIZE
                        9  1 ; GLX_GREEN_SIZE
                       10  1 ; GLX_BLUE_SIZE
                       12  24 ; GLX_DEPTH_SIZE
                        5 ; GLX_DOUBLEBUFFER
                        0)))); None
            (XSelectInput display window 32769) ; ExposureMask
            (XStoreName display window title)
            (XMapWindow display window)
            (let ((cx (gl:CreateContext display vi #false 1)))
               (gl:MakeCurrent display window cx)
               (print "OpenGL version: " (glGetString GL_VERSION))
               (print "OpenGL vendor: " (glGetString GL_VENDOR))
               (print "OpenGL renderer: " (glGetString GL_RENDERER))
              ;(gl:MakeCurrent display window #f)

               (tuple display screen window cx)))))))
   (else
      (runtime-error "Unknown platform" OS))))


(define gl:ProcessEvents (cond ; todo: add "onClose" handler
   (win32?
      (let ((user32 (load-dynamic-library "user32.dll")))
      (let ((PeekMessage      (user32 fft-int "PeekMessageA"     fft-void* fft-void* fft-int fft-int fft-int))
            (TranslateMessage (user32 fft-int "TranslateMessage" fft-void*))
            (GetMessage       (user32 fft-int "GetMessageA"      fft-void* fft-void* fft-int fft-int))
            (DispatchMessage  (user32 fft-int "DispatchMessageA" fft-void*)))
      (lambda (context)
         (let ((MSG (vm:new-raw-object type-vector-raw 48))) ; 28 for win32
         (let loop ()
            (if (= 1 (PeekMessage MSG #f 0 0 1))
               (let*((w (vm:wordsize))
                     (message (+ (<< (ref MSG (+ 0 (* w 1)))  0)      ; 4 for win32
                                 (<< (ref MSG (+ 1 (* w 1)))  8)
                                 (<< (ref MSG (+ 2 (* w 1))) 16)
                                 (<< (ref MSG (+ 3 (* w 1))) 24))))
                  ;(print message ": " MSG)
                  (cond
                     ((and (eq? message 273) ; WM_COMMAND
                           (eq? (+ (<< (ref MSG (+ 0 (* w 2))) 0)
                                   (<< (ref MSG (+ 1 (* w 2))) 8)) 2)) ; wParam, IDCANCEL
                        24) ; EXIT
                     ((and (eq? message 256) ; WM_KEYDOWN
                           (eq? (+ (<< (ref MSG (+ 0 (* w 2))) 0)
                                   (<< (ref MSG (+ 1 (* w 2))) 8)) #x51)) ; Q key
                        24) ;
                     (else 
                        (TranslateMessage MSG)
                        (DispatchMessage MSG)
                        (loop)))))))))))
   (linux?
      (let ((libX11 (load-dynamic-library "libX11.so")))
      (let ((XPending  (libX11 fft-int "XPending" type-vptr))
            (XNextEvent(libX11 fft-int "XNextEvent" type-vptr type-vptr)))
      (lambda (context)
         (let ((display (ref context 1)))
         (let loop ((XEvent (vm:new-raw-object type-vector-raw 192)))
            (if (> (XPending display) 0)
               (begin
                  (XNextEvent display XEvent)
                  (if (eq? (int32->ol XEvent 0) 2)
                     (int32->ol XEvent 84)
                     (loop XEvent))))))))))
   (else
      (runtime-error "Unknown platform" OS))))

; ====================================================================================================
(define (gl:run context init renderer)
(let ((context (if (string? context) (gl:Create context) context)))

   (gl:Enable context)
   (let ((userdata (init)))
   (gl:Disable context)

   (call/cc (lambda (return)
   (let this ((userdata userdata))
      (let ((message (gl:ProcessEvents context)))
         (if (eq? message 24)
            (return message)))

      (gl:Enable context)
      (let ((userdata (if renderer (apply renderer userdata) userdata)))
      (gl:SwapBuffers context)
      (gl:Disable context)

      (this userdata))))))))

;(define gl:run (lambda args
;   (let run ((title #f) (init #f) (draw #f) (args args) (selector #f))
;      (if (null? args)
;         (gl:run title init draw)
;      (cond
;      ((eq? (car args) 'init)
;         (run title (cadr args) draw (cddr args) selector))
;      ((eq? (car args) 'draw)
;         (run title init (cadr args) (cddr args) selector))
;      (else
;         (if selector
;            (selector title init draw args)
;            (run title init draw args (lambda (title init draw args)
;               (run (car args) init draw (cdr args) (lambda (title init draw args)
;                  (run title (car args) draw (cdr args) (lambda (title init draw args)
;                     (run title init (car args) (cdr args) #f))))))))))))))

(define gl:run (lambda args
   (let run ((title #f) (init #f) (draw #f) (args args))
      (if (null? args)
         (gl:run title init draw)
      (cond
      ((eq? (car args) 'init)
         (run title (cadr args) draw (cddr args)))
      ((eq? (car args) 'draw)
         (run title init (cadr args) (cddr args)))
      (else (cond
         ((eq? title #f)
            (run (car args) init draw (cdr args)))
         ((eq? init #f)
            (run title (car args) draw (cdr args)))
         ((eq? draw #f)
            (run title init (car args) (cdr args))))))))))

))


;(define gl:run2 (lambda args
;   (print args)))

;         (gl:ProcessEvents context)
;      (let ((XEvent (vm:new-raw-object fft-void* (repeat 0 192))))
;         (let process-events ((unused 0))
;            (if (> (XPending display) 0)
;               (process-events (XNextEvent display XEvent))))
