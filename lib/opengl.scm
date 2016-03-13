(define-library (lib opengl)
   (import
      (r5rs core) (owl io) (owl primop) (owl tuple) (owl string)
      (owl pinvoke) (owl list) (owl math) (owl vector) (owl list-extra) (owl ff)
      (owl interop)

      (OpenGL version-1-0))

   (export
      (exports (OpenGL version-1-0))
      gl:run

      gl:Create ; create window + context
      gl:Enable gl:Disable

      gl:SwapBuffers
      gl:ProcessEvents
   )

(begin
(define uname (syscall 63 #f #f #f))

(define win32? (string-ci=? (ref uname 1) "Windows"))
(define linux? (string-ci=? (ref uname 1) "Linux"))

; ===================================================
(define gl:Enable (cond
   (win32?  (lambda (context)
               (let ((dc   (ref context 1))
                     (glrc (ref context 2)))
                  (gl:MakeCurrent dc glrc))))
   (linux?  (lambda (context)
               (let ((display (ref context 1))
                     ;screen  (ref context 2))
                     (window  (ref context 3))
                     (cx      (ref context 4)))
                  (gl:MakeCurrent display window cx))))
   (else   (runtime-error "Unknown platform" uname))))


(define gl:Disable (cond
   (win32?  (lambda (context)
                  (gl:MakeCurrent null null)))
   (linux?  (lambda (context)
               (let ((display (ref context 1)))
                  (gl:MakeCurrent display null null))))
   (else   (runtime-error "Unknown platform" uname))))


(define gl:Create (cond
   ; -=( win32 )=---------------------------------------------------------------------
   (win32?
      (let ((user32 (dlopen "user32.dll"))
            (gdi32  (dlopen "gdi32")))
      (let ((CreateWindowEx   (dlsym user32 type-port "CreateWindowExA" type-int+ type-string type-string type-int+ type-int+ type-int+ type-int+ type-int+ type-port type-port type-port type-vector-raw))
            (GetDC            (dlsym user32 type-port "GetDC" type-port))
            (ShowWindow       (dlsym user32 type-fix+ "ShowWindow" type-port type-int+))
            (ChoosePixelFormat(dlsym gdi32  type-int+ "ChoosePixelFormat" type-port type-vector-raw))
            (SetPixelFormat   (dlsym gdi32  type-fix+ "SetPixelFormat" type-port type-int+ type-vector-raw)))
      (lambda (title)
         (let*((window (CreateWindowEx
                  #x00040100 "#32770" (c-string title) ; WS_EX_APPWINDOW|WS_EX_WINDOWEDGE, #32770 is system classname for DIALOG
                  #x06cf0000 ; WS_OVERLAPPEDWINDOW | WS_CLIPSIBLINGS | WS_CLIPCHILDREN
                  0 0 640 480 ; x y width height
                  null ; no parent window
                  null ; no menu
                  null ; instance
                  null))
               (pfd (raw type-vector-raw '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                                       00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))
               (hDC (GetDC window))
               (PixelFormat (ChoosePixelFormat hDC pfd)))
            (SetPixelFormat hDC PixelFormat pfd)
         (let ((hRC (gl:CreateContext hDC)))
            (gl:MakeCurrent hDC hRC)
            (print "OpenGL version: " (glGetString GL_VERSION))
            (print "OpenGL vendor: " (glGetString GL_VENDOR))
            (print "OpenGL renderer: " (glGetString GL_RENDERER))
            ;gl:MakeCurrent '() '())

            (ShowWindow window 5)
            (tuple hDC hRC window)))))))
   ; -=( linux )=---------------------------------------------------------------------
   (linux?
      (let ((libx11 (dlopen "libX11.so"))
            (libGL  (dlopen "libGL.so")))
      (let ((XOpenDisplay  (dlsym libx11 type-port "XOpenDisplay" type-string))
            (XDefaultScreen(dlsym libx11 type-int+ "XDefaultScreen" type-port))
            (XRootWindow   (dlsym libx11 type-port "XRootWindow" type-port type-int+))
            (XBlackPixel   (dlsym libx11 type-port "XBlackPixel" type-port type-int+))
            (XWhitePixel   (dlsym libx11 type-port "XWhitePixel" type-port type-int+))
            (XCreateSimpleWindow (dlsym libx11 type-port "XCreateSimpleWindow"
                              type-port type-port ; display, parent Window
                              type-int+ type-int+ type-int+ type-int+ ; x y width height
                              type-int+ ; border width
                              type-int+ ; border
                              type-int+ ; background
                           ))
            (XSelectInput  (dlsym libx11 type-int+ "XSelectInput" type-port type-port type-int+))
            (XMapWindow    (dlsym libx11 type-int+ "XMapWindow" type-port type-port))
            (XStoreName    (dlsym libx11 type-int+ "XStoreName" type-port type-port type-string))
            (glXChooseVisual     (dlsym libGL type-port "glXChooseVisual" type-port type-int+ type-vector-raw))
            (glXCreateContext    (dlsym libGL type-port "glXCreateContext" type-port type-port type-int+ type-int+)))
      (lambda (title)
         (let*((display (XOpenDisplay null))
               (screen (XDefaultScreen display))
               (window (XCreateSimpleWindow display (XRootWindow display screen)
                  0 0 640 480 1
                  (XBlackPixel display screen) (XWhitePixel display screen)))
               (vi (glXChooseVisual display screen
                     (raw type-vector-raw '(
                        4 0 0 0 ; GLX_RGBA
                        5 0 0 0 ; GLX_DOUBLEBUFFER
                        8 0 0 0  8 0 0 0 ; GLX_RED_SIZE
                        9 0 0 0  8 0 0 0 ; GLX_GREEN_SIZE
                       10 0 0 0  8 0 0 0 ; GLX_BLUE_SIZE
                       12 0 0 0  24 0 0 0   ; GLX_DEPTH_SIZE
                        0 0 0 0))))); None
            (XSelectInput display window  (<< 1 15)) ; ExposureMask
            (XStoreName display window title)
            (XMapWindow display window)
            (let ((cx (gl:CreateContext display vi 0 1)))
               (gl:MakeCurrent display window cx)
               (print "OpenGL version: " (glGetString GL_VERSION))
               (print "OpenGL vendor: " (glGetString GL_VENDOR))
               (print "OpenGL renderer: " (glGetString GL_RENDERER))
               ;gl:MakeCurrent display windows '())

               (tuple display screen window cx)))))))

   (else
      (runtime-error "Unknown platform" uname))))


(define gl:ProcessEvents (cond ; todo: add "onClose" handler
   (win32?
      (let ((user32 (dlopen "user32.dll")))
      (let ((PeekMessage      (dlsym user32 type-fix+ "PeekMessageA"     type-vector-raw type-port type-int+ type-int+ type-int+))
            (TranslateMessage (dlsym user32 type-fix+ "TranslateMessage" type-vector-raw))
            (GetMessage       (dlsym user32 type-fix+ "GetMessageA"      type-vector-raw type-port type-int+ type-int+))
            (DispatchMessage  (dlsym user32 type-int+ "DispatchMessageA" type-vector-raw)))
      (lambda (context)
         (let ((MSG (raw type-vector-raw (repeat 0 28))))
         (let loop ()
            (if (= 1 (PeekMessage MSG '() 0 0 1))
               (let ((message (+ (<< (ref MSG 4)  0)
                                 (<< (ref MSG 5)  8)
                                 (<< (ref MSG 6) 16)
                                 (<< (ref MSG 7) 24))))
                  ;(print message ": " MSG)
                  (if (and
                        (eq? message 273) ; WM_COMMAND
                        (eq? (+ (ref MSG 8) (<< (ref MSG 9) 8)) 2)) ; IDCANCEL
                     2 ; EXIT
                     (begin
                        (TranslateMessage MSG)
                        (DispatchMessage MSG)
                        (loop)))))))))))
   (linux?
      (let ((libx11 (dlopen "libX11.so")))
      (let ((XPending  (dlsym libx11 type-int+ "XPending"   type-port))
            (XNextEvent(dlsym libx11 type-int+ "XNextEvent" type-port type-vector-raw)))
      (lambda (context)
         (let ((XEvent (raw type-vector-raw (repeat 0 192)))
               (display (ref context 1)))
         (let loop ()
            (if (> (XPending display) 0)
            (begin
               (XNextEvent display XEvent)
               (loop)))))))))
   (else
      (runtime-error "Unknown platform" uname))))

; ====================================================================================================
(define (gl:run context init renderer)
(let ((context (if (string? context) (gl:Create context) context))
      (swap-buffers (cond
         (win32? (lambda (context)
            (gl:SwapBuffers (ref context 1))))
         (linux? (lambda (context)
            (gl:SwapBuffers (ref context 1) (ref context 3))))
         (else (runtime-error "Unknown platform" uname)))))

   (gl:Enable context)
   (let ((userdata (init)))
   (gl:Disable context)

   (call/cc (lambda (return)
   (let this ((userdata userdata))
      (let ((message (gl:ProcessEvents context)))
         (if (eq? message 2)
            (return message)))

      (gl:Enable context)
      (let ((userdata (if renderer (apply renderer userdata) userdata)))
      (swap-buffers context)
      (gl:Disable context)

      (this userdata))))))))
))

;(define gl:run2 (lambda args
;   (print args)))

;         (gl:ProcessEvents context)
;      (let ((XEvent (raw type-vector-raw (repeat 0 192))))
;         (let process-events ((unused 0))
;            (if (> (XPending display) 0)
;               (process-events (XNextEvent display XEvent))))
