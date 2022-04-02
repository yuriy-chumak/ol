(define-library (lib gl)
   (version 1.0)
   (license MIT/LGPL3)
   (description "otus-lisp gl library")
(import
   (otus lisp) (otus ffi)
   (scheme dynamic-bindings)
   (otus case-apply)
   (lib gl config)
   (lib keyboard))

(cond-expand
   (Android
      (import (OpenGL platform))) ; via gl4es
   ((or Linux Windows Emscripten)
      (import (OpenGL platform)))
   (else
      (begin (runtime-error "Unsupported platform:" *uname*))))


(export
   gl:set-window-title gl:set-window-size
   gl:set-renderer
   gl:set-mouse-handler
   gl:set-keyboard-handler
   gl:set-resize-handler ;todo: rename to reshape-handler
   gl:finish ; if renderer exists - wait for window close, else just glFinish

   gl:window-dimensions
   gl:get-window-width gl:get-window-height

   gl:hide-cursor

   native:enable-context native:disable-context
   hook:exit)

; notes:
;  WGL context creation https://www.GL.org/wiki/Creating_an_OpenGL_Context_(WGL)
;  GLX context creation https://www.GL.org/wiki/Tutorial:_OpenGL_3.0_Context_Creation_(GLX)

(begin
   (setq x32? (eq? (size nullptr) 4))

   (define WIDTH  (get config 'width  854))
   (define HEIGHT (get config 'height 480))

; assume that window size can not be large than 16777215 for x32 build
;                                  and 72057594037927935 for x64 build.
   (define STATE [0 0 WIDTH HEIGHT]) ; x y width height, current window state

   (define (gl:get-window-width)
      (ref STATE 3))
   (define (gl:get-window-height)
      (ref STATE 4))

   (define gl:window-dimensions STATE)
)

; ===================================================
(cond-expand
   ; -=( Android )=------------------------------------------
   (Android
      (begin

         (setq EGL (load-dynamic-library "libEGL.so"))
			(setq gl4es (load-dynamic-library "libgl4es.so"))

         ; no context creation, we use already created context
         (define (native:create-context title)
            #false)

         ; android printing to the stdin means "DEBUG" logcat message
         (print-to stdin "OpenGL version: " (glGetString GL_VERSION))
         (print-to stdin "OpenGL vendor: " (glGetString GL_VENDOR))
         (print-to stdin "OpenGL renderer: " (glGetString GL_RENDERER))
         (print-to stdin "OpenGL extensions: " (glGetString GL_EXTENSIONS))

         (define width '(1184)) ; todo: query display
         (define height '(672))
         ;; (eglQuerySurface display surface #x3057 width) ;EGL_WIDTH
         ;; (eglQuerySurface display surface #x3056 height) ;EGL_HEIGHT

         (set-ref! gl:window-dimensions 3 (car width))
         (set-ref! gl:window-dimensions 4 (car height))

;            (glViewport 0 0 (car width) (car height)))

         (define (native:enable-context context)
            #false)
         (define (native:disable-context context)
            #false)
         (define (native:process-events context handler)
            ; todo: process 'resize event
            #false)

         (define (gl:SetWindowTitle context title)
            #false)
         (define (gl:SetWindowSize context width height)
            #false)
         (define (gl:HideCursor context)
            #false)
         (define (gl:GetMousePos context)
            #false)
   ))

   ; -=( Linux )=------------------------------------------
   (Linux
      (import (prefix (lib x11 config) x11))
      (begin

         (setq libX11 (or (load-dynamic-library "libX11.so")
                          (load-dynamic-library "libX11.so.6")))
         (setq libGLX GL_LIBRARY)

         (setq XOpenDisplay  (libX11 type-vptr "XOpenDisplay" type-string))
         (setq XDefaultScreen(libX11 fft-int "XDefaultScreen" type-vptr))
         (setq XRootWindow   (libX11 fft-int "XRootWindow" type-vptr fft-int))
         (setq XBlackPixel   (libX11 type-vptr "XBlackPixel" type-vptr fft-int))
         (setq XWhitePixel   (libX11 type-vptr "XWhitePixel" type-vptr fft-int))
         (setq XCreateColormap (libX11 type-vptr "XCreateColormap" type-vptr fft-int type-vptr fft-int))
         (setq XCreateSimpleWindow (libX11 type-vptr "XCreateSimpleWindow"
                              type-vptr fft-int ; display, parent
                              fft-int fft-int fft-unsigned-int fft-unsigned-int ; x y width height
                              fft-unsigned-int  ; border width
                              type-vptr ; border
                              type-vptr ; background
                           ))
         (setq XCreateWindow (libX11 type-vptr "XCreateWindow"
                              type-vptr fft-int ; display, parent
                              fft-int fft-int fft-unsigned-int fft-unsigned-int ; x y width height
                              fft-unsigned-int  fft-int ; border_width, depth
                              fft-unsigned-int type-vptr ; class, visual
                              fft-unsigned-long ; valuemask
                              type-vptr)) ; XSetWindowAttributes* attributes
         (setq XSelectInput  (libX11 fft-int "XSelectInput" type-vptr type-vptr fft-long))
         (setq XMapWindow    (libX11 fft-int "XMapWindow" type-vptr type-vptr))
         (setq XStoreName    (libX11 fft-int "XStoreName" type-vptr type-vptr type-string))

         (setq glXQueryExtension (libGLX fft-int "glXQueryExtension" type-vptr fft-int* fft-int*))
         (setq glXChooseVisual   (libGLX type-vptr "glXChooseVisual" type-vptr fft-int fft-int*))
         (setq glXCreateContext  (libGLX type-vptr "glXCreateContext" type-vptr type-vptr type-vptr fft-int))

         (define (native:create-context title)
            (let*((display (XOpenDisplay #false))
                  (screen  (XDefaultScreen display))
                  ; (unless (glxQueryExtension display #f #f) (halt "X server has no OpenGL GLX extension")
                  ;; ; Single Buffer Solution:
                  ;; (vi (glXChooseVisual display screen
                  ;;       '( 4 ; GLX_RGBA
                  ;;          8  1 ; GLX_RED_SIZE
                  ;;          9  1 ; GLX_GREEN_SIZE
                  ;;         10  1 ; GLX_BLUE_SIZE
                  ;;         12  24 ; GLX_DEPTH_SIZE
                  ;;          0))); None
                  ;; (XVisualInfo (vptr->bytevector vi 64)) ; sizeof(XVisualInfo) = 64
                  ;; ; *unless (eq? 4 (class (bytevector->int32 XVisualInfo 24))) (halt "TrueColor visual required for this program") ; offsetof(XVisualInfo, class)
                  ;; ;(cx (gl:CreateContext display vi #false 1))

                  ;; (visual (vector->vptr XVisualInfo 0)) ;
                  ;; (root (XRootWindow display screen))
                  ;; (colormap (XCreateColormap display root visual 0)) ; 0 == AllocNone

                  ;; ; ...
                  ;; (XSetWindowAttributes (make-bytevector 112 0)) ; sizeof(XSetWindowAttributes)
                  ;; (_ (vector-set-vptr! XSetWindowAttributes 96 colormap))
                  ;; (_ (vector-set-int!  XSetWindowAttributes 24 0)) ; border_pixel
                  ;; (_ (vector-set-int!  XSetWindowAttributes 72 163844)); event_mask (ExposureMask | ButtonPressMask | StructureNotifyMask)

                  ;; (window (XCreateWindow display root
                  ;;             0 0 WIDTH HEIGHT 0
                  ;;             24 1; vi->depth InputOutput
                  ;;             visual
                  ;;             10248 ; CWBorderPixel | CWColormap | CWEventMask
                  ;;             XSetWindowAttributes)))
                  ; Double Buffer Solution
                  (window  (XCreateSimpleWindow display (XRootWindow display screen)
                              0 0 WIDTH HEIGHT 1
                              (XBlackPixel display screen)
                              (XWhitePixel display screen)))
                  (vi (glXChooseVisual display screen
                        (list
                           4 ; GLX_RGBA
                           8  (get config 'red #xFFFFFFFF) ; GLX_RED_SIZE = GLX_DONT_CARE
                           9  (get config 'green #xFFFFFFFF) ; GLX_GREEN_SIZE = GLX_DONT_CARE
                          10  (get config 'blue #xFFFFFFFF) ; GLX_BLUE_SIZE = GLX_DONT_CARE
                          12  (get config 'depth 24) ; GLX_DEPTH_SIZE
                           5 ; GLX_DOUBLEBUFFER
                           0)))); None

               ; common code
               (XSelectInput display window #b100000000000000101) ; StructureNotifyMask | KeyPressMask | ButtonPressMask
               (XStoreName display window title)
               (XMapWindow display window)
               (let ((cx (gl:CreateContext display vi #false 1)))
                  (gl:MakeCurrent display window cx)
                  (print-to stderr "OpenGL version: " (glGetString GL_VERSION))
                  (print-to stderr "OpenGL vendor: " (glGetString GL_VENDOR))
                  (print-to stderr "OpenGL renderer: " (glGetString GL_RENDERER))
                  ;(gl:MakeCurrent display #f #f)
                  (mail 'opengl ['set-context [display screen window cx]]) ; notify opengl server
                  ;(interact 'opengl ['get-context]) ; синхронизация (не нужна, вроде)

                  [display screen window cx])))

         (define (native:enable-context context)
            (let ((display (ref context 1))
                  ;(screen  (ref context 2))
                  (window  (ref context 3))
                  (cx      (ref context 4)))
               (gl:MakeCurrent display window cx)))

         (define (native:disable-context context)
            (let ((display (ref context 1)))
               (gl:MakeCurrent display #f #f)))

         (setq XPending      (libX11 fft-int "XPending" type-vptr))
         (setq XNextEvent    (libX11 fft-int "XNextEvent" type-vptr type-vptr))

         (define (native:process-events context handler)
            (let ((display (ref context 1)))
            (let loop ((XEvent (make-bytevector (x11config '|sizeof XEvent|)))) ; 96 for x32
               (if (> (XPending display) 0)
                  (begin
                     (XNextEvent display XEvent)
                     ; https://tronche.com/gui/x/xlib/events/types.html
                     (case (bytevector->int32 XEvent 0)
                        (2 ; KeyPress
                           (handler ['keyboard (bytevector->int32 XEvent (x11config '|XKeyEvent.keycode|))]))
                        (3 #f) ; KeyRelease
                        (4 ; ButtonPress
                           (let ((x (bytevector->int32 XEvent (x11config '|XButtonEvent.x|)))
                                 (y (bytevector->int32 XEvent (x11config '|XButtonEvent.y|)))
                                 (button (bytevector->int32 XEvent (x11config '|XButtonEvent.button|))))
                              (handler ['mouse button x y])))
                        (5 #f) ; ButtonRelease
                        (17 #f); DestroyNotify
                        (19 #f) ; MapNotify
                        (21 #f) ; ReparentNotify
                        (22 ; ConfigureNotify
                           ;(print "ConfigureNotify: " XEvent)
                           (let (;(x (bytevector->int32 XEvent (if x32? ? ?)))
                                 ;(y (bytevector->int32 XEvent (if x32? ? ?)))
                                 (w (bytevector->int32 XEvent (x11config '|XConfigureEvent.width|)))
                                 (h (bytevector->int32 XEvent (x11config '|XConfigureEvent.height|))))
                              (handler ['resize w h]))) ; todo: add x y, change to 'configure
                        (else ;
                           (print "Unknown window event: " (bytevector->int32 XEvent 0))))
                     (loop XEvent))))))

         ; ---
         (setq XStoreName (libX11 fft-int "XStoreName" type-vptr type-vptr type-string))
         (setq XInternAtom (libX11 type-vptr "XInternAtom" type-vptr type-string fft-int))
         (setq XChangeProperty (libX11 fft-void "XChangeProperty" type-vptr type-vptr type-vptr type-vptr fft-int fft-int (fft* fft-unsigned-char) fft-int))

         (define (gl:SetWindowTitle context title)
            (let ((display (ref context 1))
                  ;(screen  (ref context 2))
                  (window  (ref context 3))
                  (cx      (ref context 4)))
               (case (type title)
                  (type-string
                     (XStoreName display window title))
                  (type-string-wide (let ((title (string->bytes title)))
                     (XChangeProperty display window
                        (XInternAtom display "_NET_WM_NAME" 0)
                        (XInternAtom display "UTF8_STRING" 0)
                        8 0 ;PropModeReplace
                        title (length title)))))))

         ; ---
         (setq XResizeWindow (libX11 fft-int "XResizeWindow" type-vptr type-vptr fft-int fft-int))

         (define (gl:SetWindowSize context width height)
            (let ((display (ref context 1))
                  (window  (ref context 3)))
               (XResizeWindow display window width height)))

         ; ---
         (setq XDefineCursor (libX11 fft-void "XDefineCursor" type-vptr type-vptr type-vptr))
         (setq XDefaultColormap (libX11 fft-int "XDefaultColormap" type-vptr fft-int))
         (setq XAllocNamedColor (libX11 fft-void "XAllocNamedColor" type-vptr fft-int type-string type-vptr type-vptr))
         (setq XCreateBitmapFromData (libX11 type-vptr "XCreateBitmapFromData" type-vptr type-vptr type-vptr fft-unsigned-int fft-unsigned-int))
         (setq XCreatePixmapCursor (libX11 type-vptr "XCreatePixmapCursor" type-vptr type-vptr type-vptr type-vptr type-vptr fft-unsigned-int fft-unsigned-int))
         (setq XFreeCursor (libX11 type-vptr "XFreeCursor" type-vptr type-vptr))
         (setq XFreePixmap (libX11 type-vptr "XFreePixmap" type-vptr type-vptr))

         ; ---
         (define (gl:HideCursor context)
            (let*((bm_no_data '(0 0 0 0 0 0 0 0))
                  (cmap (XDefaultColormap (ref context 1) (ref context 2)))
                  ; sizeof XColor is 12 for x64 and 9 for x86
                  (black (make-bytevector 12))
                  (dummy (make-bytevector 12))
                  ;(? (XAllocNamedColor (ref context 1) cmap "black" black dummy))
                  (bm_no (XCreateBitmapFromData (ref context 1) (ref context 3) black 8 8))
                  (cursor (XCreatePixmapCursor (ref context 1) bm_no bm_no black black 0 0)))
               (XDefineCursor (ref context 1) (ref context 3) cursor)
               (XFreeCursor (ref context 1) cursor)
               (XFreePixmap (ref context 1) bm_no)))

         ; ---
         (setq XGetInputFocus (libX11 fft-void "XGetInputFocus" type-vptr (fft* type-vptr) (fft& fft-int)))
         (setq XQueryPointer (libX11 fft-int "XQueryPointer" type-vptr type-vptr (fft* type-vptr) (fft* type-vptr) (fft& fft-int) (fft& fft-int) (fft& fft-int) (fft& fft-int) (fft& fft-unsigned-int)))

         (define (gl:GetMousePos context)
            (let ((window (make-vptr))
                  (revert_to_return (box 0)))
               (XGetInputFocus (ref context 1) window revert_to_return)
               (if (equal? window (ref context 3))
                  (let ((root (make-vptr))
                        (child (make-vptr))
                        (root_x (box 0)) (root_y (box 0))
                        (x (box 0)) (y (box 0)) (mask (box 0)))
                     (if (eq? 1 (XQueryPointer (ref context 1) (ref context 3) root child root_x root_y x y mask))
                        (cons (unbox x) (unbox y)))))))

      ))

   ; -=( WebGL )=-----------------------------------------
   (Emscripten
      (import
         (EGL version-1-1))
      (begin
         (define (native:create-context title)
            ;(print "Checking WebGL support...")
            (let ((major (make-32bit-array 1))
                  (minor (make-32bit-array 1))
                  (numConfigs (make-32bit-array 1))
                  (attribList '(
                     #x3024 5 ; red
                     #x3023 6 ; green
                     #x3022 5 ; blue
                     #x3021 8 ; alpha
                     #x3025 8 ; depth
                     ;#x3026 ; stencil
                     #x3032 0 ; sample buffers
                     #x3038)) ; EGL_NONE
                  (config (make-vptr-array 1))
                  (contextAttribs '(
                     #x3098 2 #x3038 #x3038))) ; EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE, EGL_NONE
            (print) ; empty print, some king of 'flag' in output

            (define display (eglGetDisplay #false))
            (mail 'opengl ['set-display display])

            (eglInitialize display major minor)
            (print "eglInitialize: " (car major) "." (car minor))

            (eglGetConfigs display config 0 numConfigs)
            (print "eglGetConfigs: " (car numConfigs))

            (eglChooseConfig display attribList config (car numConfigs) numConfigs)
            (define surface (eglCreateWindowSurface display (car config) 2 #false)) ; temp "2" instead of XCreateWindow
            (mail 'opengl ['set-surface surface])

            (define context (eglCreateContext display (car config) EGL_NO_CONTEXT contextAttribs))
            (mail 'opengl ['set-context context])

            ; gl2es part
            (define gl4es (dlopen))
            (define LIBGL_BEGINEND #xA10D)  (glHint LIBGL_BEGINEND 0)
            (define initialize_gl4es (dlsym gl4es "initialize_gl4es"))
            
            (ffi initialize_gl4es (cons fft-int #null) #null)

            (print-to stderr "OpenGL version: " (glGetString GL_VERSION))
            (print-to stderr "OpenGL vendor: " (glGetString GL_VENDOR))
            (print-to stderr "OpenGL renderer: " (glGetString GL_RENDERER))
            ;; (print-to stderr "OpenGL extensions: " (glGetString GL_EXTENSIONS))

            ;; (define width '(1184))
            ;; (define height '(672))
            ;; ;; (eglQuerySurface display surface #x3057 width) ;EGL_WIDTH
            ;; ;; (eglQuerySurface display surface #x3056 height) ;EGL_HEIGHT

            ;; (set-ref! gl:window-dimensions 3 (car width))
            ;; (set-ref! gl:window-dimensions 4 (car height))

            ;; (glViewport 0 0 (car width) (car height)))
         ))

         (define (native:enable-context context)
            #false)
         (define (native:disable-context context)
            #false)
         (define (native:process-events context handler)
            #false)

         (define (gl:SetWindowTitle context title)
            #false)
         (define (gl:SetWindowSize context width height)
            #false)
         (define (gl:HideCursor context)
            #false)
         (define (gl:GetMousePos context)
            #false)

      ))

   ; -=( Windows )=---------------------------------------
   (Windows
      (begin

         (setq user32 (load-dynamic-library "user32.dll"))
         (setq gdi32  (load-dynamic-library "gdi32"))

         (setq CreateWindowEx   (user32 fft-void* "CreateWindowExW"
                                 fft-int type-string-wide type-string-wide
                                 fft-int fft-int fft-int fft-int fft-int
                                 fft-void* fft-void* fft-void* fft-void*))
         (setq GetDC            (user32 fft-void* "GetDC" fft-void*))
         (setq ShowWindow       (user32 fft-int "ShowWindow" fft-void* fft-int))
         (setq ChoosePixelFormat(gdi32  fft-int "ChoosePixelFormat" fft-void* fft-void*))
         (setq SetPixelFormat   (gdi32  fft-int "SetPixelFormat" fft-void* fft-int fft-void*))

         (setq SetWindowLong    (user32 fft-void* "SetWindowLongW" fft-void* fft-int type-callable))
         (setq GetWindowLongPtr (user32 fft-void* "GetWindowLongPtrW" fft-void* fft-int))
         (setq SetWindowLongPtr (user32 fft-void* "SetWindowLongPtrW" fft-void* fft-int type-callable))
         (setq DefWindowProc    (user32 fft-long "DefWindowProcW" fft-void* fft-unsigned-int fft-void* fft-void*))

         (setq GetWindowRect    (user32 fft-int "GetWindowRect"    fft-void* (fft& fft-int)))

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

               ; we need to capture WM_SIZE messages to be processed by opengl coroutine
               (define OldProc (GetWindowLongPtr window -4))
               (define WndProc (vm:pin (cons
                     (cons fft-long (list fft-void* fft-unsigned-int fft-void* fft-void*))
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
                           (cons fft-void* (list fft-void* fft-unsigned-int fft-void* fft-void*))
                           (list hWnd Msg wParam lParam))))))
;                        (DefWindowProc hWnd Msg wParam lParam)))))

               (SetWindowLongPtr window -4 (make-callback WndProc))

               (SetPixelFormat hDC PixelFormat pfd)
               (let ((hRC (gl:CreateContext hDC)))
                  (gl:MakeCurrent hDC hRC)
                  (print-to stderr "OpenGL version: " (glGetString GL_VERSION))
                  (print-to stderr "OpenGL vendor: " (glGetString GL_VENDOR))
                  (print-to stderr "OpenGL renderer: " (glGetString GL_RENDERER))
               ;(gl:MakeCurrent #f #f)
                  (mail 'opengl ['set-context [hDC hRC window]])
                  (await (mail 'opengl ['get-context])) ; синхронизация

                  (ShowWindow window 5)
                  [hDC hRC window])))

         (define (native:enable-context context)
            (let ((dc   (ref context 1))
                  (glrc (ref context 2)))
               (gl:MakeCurrent dc glrc)))

         (define (native:disable-context context)
            (gl:MakeCurrent #f #f))

         (setq PeekMessage      (user32 fft-int "PeekMessageA"     fft-void* fft-void* fft-int fft-int fft-int))
         (setq TranslateMessage (user32 fft-int "TranslateMessage" fft-void*))
         (setq GetMessage       (user32 fft-int "GetMessageA"      fft-void* fft-void* fft-int fft-int))
         (setq DispatchMessage  (user32 fft-int "DispatchMessageA" fft-void*))

         (define (native:process-events context handler)
            (let ((MSG (make-bytevector 48))) ; 28 for x32
            (let loop ()
               (if (= 1 (PeekMessage MSG #f 0 0 1))
                  (let*((w (size nullptr))
                        (message (bytevector->int32 MSG w)))      ; 4 for x32
                     ;(print message ": " message)
                     (cond
                        ((and (eq? message 273) ; WM_COMMAND
                              (eq? (+ (<< (ref MSG (+ 0 (* w 2))) 0)
                                      (<< (ref MSG (+ 1 (* w 2))) 8)) 2)) ; wParam, IDCANCEL
                           (handler ['quit])) ; EXIT
                        ;; ((eq? message 15) ; WM_PAINT
                        ;;    (define rect '(0 0 0 0))
                        ;;    (define window (bytevector->void* MSG 0))
                        ;;    (GetWindowRect window rect))
                           ;(print "WM_PAINT: " rect)

                        ;; ((and (eq? message 256) ; WM_KEYDOWN
                        ;;       (eq? (+ (<< (ref MSG (+ 0 (* w 2))) 0)
                        ;;               (<< (ref MSG (+ 1 (* w 2))) 8)) #x51)) ; Q key
                        ;;    (handler ['keyboard Q]))
                        (else
                           (TranslateMessage MSG)
                           (DispatchMessage MSG)
                           (loop))))))))

         ; ---
         (setq SetWindowText (user32 fft-int "SetWindowTextW" fft-void* type-string-wide))

         (define (gl:SetWindowTitle context title)
            (let ((window (ref context 3)))
               (SetWindowText window title)))

         ; ---
         (setq MoveWindow (user32 fft-int "MoveWindow" fft-void* fft-int fft-int fft-int fft-int fft-int))

         (define (gl:SetWindowSize context width height)
            (let ((window (ref context 3))
                  (rect '(0 0 0 0)))
               (GetWindowRect window rect)
               (MoveWindow window (list-ref rect 0) (list-ref rect 1) width height 0)))

         ; ---
         (setq ShowCursor (user32 fft-int "ShowCursor" fft-int))
         (define (gl:HideCursor context)
            (ShowCursor 0))

         ; ---
         (setq GetCursorPos (user32 fft-int "GetCursorPos" fft-int&))
         (setq ScreenToClient (user32 fft-int "ScreenToClient" type-vptr fft-int&))

         (define (gl:GetMousePos context)
            (let ((window (ref context 3))
                  (pos '(0 0)))
               (GetCursorPos pos)
               (ScreenToClient window pos)
               (cons (lref pos 0) (lref pos 1))))

      ))

   (else
      (begin (runtime-error "Unknown platform OS" *uname*))))

(begin
   ; internal function
   (define (gl:hide-cursor)
      (gl:HideCursor (await (mail 'opengl ['get 'context]))))
)

; -=( opengl coroutine )=------------------------------------
(cond-expand
   ((or Android Emscripten)
      (begin
         (coroutine 'opengl (lambda ()
         (let this ((dictionary {
               'resize-handler (lambda (w h) (glViewport 0 0 w h))}))
            (let*((envelope (wait-mail))
                  (sender msg envelope))
               (case msg
                  ; low level interface:
                  (['set key value]
                     (this (put dictionary key value)))
                  (['get key]
                     (mail sender (get dictionary key #false))
                     (this dictionary))
                  (['debug]
                     (mail sender dictionary)
                     (this dictionary))

                  (['set-renderer renderer]
                     (this (put dictionary 'renderer renderer)))
                  (['get-renderer]
                     (mail sender (get dictionary 'renderer #f))
                     (this dictionary))
                  (['set-context context] ; ignore
                     (this dictionary))
                  (['get-context]
                     (mail sender (get dictionary 'context #f))
                     (this dictionary))
                  (else
                     (print-to stderr "Unknown opengl server command " msg)
                     (this dictionary))
               )))))))
   (else
      (begin
         ; =============================================
         ; automation
         (coroutine 'opengl (lambda ()
         (let this ((dictionary {
               'resize-handler (lambda (w h) (glViewport 0 0 w h))}))
         (cond
            ; блок обработки сообщений
            ((check-mail) => (lambda (e) ; can be (and (eq? something 0) (check-mail)) =>
               (let*((sender msg e))
                  (case msg
                     ; low level interface:
                     (['set key value]
                        (this (put dictionary key value)))
                     (['get key]
                        (mail sender (get dictionary key #false))
                        (this dictionary))
                     (['debug]
                        (mail sender dictionary)
                        (this dictionary))

                     (['finish]  ; wait for OpenGL window closing (just no answer for interact)
                        ;(glFinish)

                        (unless (get dictionary 'renderer #f)
                           ; рендерера нет, значит оновим буфер
                           (gl:SwapBuffers (get dictionary 'context #f)))
                           ; рендерер есть, но режим интерактивный? тогда вернем управление юзеру
                           ;(if *interactive* ;(or (zero? (length (command-line))) (string-eq? (car (command-line)) "-"))
                           ;   (mail sender 'ok)))
                        (this (put dictionary 'customer sender)))

                     ; context
                     (['set-context context]
                        (this (put dictionary 'context context)))
                     (['get-context]
                        (mail sender (get dictionary 'context #f))
                        (this dictionary))

                     ; set-window-title
                     (['set-window-title title]
                        (gl:SetWindowTitle (get dictionary 'context #f) title)
                        (this dictionary))

                     ; set-window-size
                     (['set-window-size width height]
                        (gl:SetWindowSize (get dictionary 'context #f) width height)
                        ; сразу выставим вьюпорт в размер окна
                        (glViewport 0 0 width height)
                        (this dictionary))

                     ; renderer
                     (['set-renderer renderer]
                        (this (put dictionary 'renderer renderer)))
                     (['get-renderer]
                        (mail sender (get dictionary 'renderer #f))
                        (this dictionary))

                     ; renderer
                     (['set-resize-handler resize-handler]
                        (if resize-handler
                           (resize-handler (ref STATE 3) (ref STATE 4)))
                        (this (put dictionary 'resize-handler resize-handler)))

                     ; events
                     (['resize width height]
                        (set-ref! STATE 1 0) ; save current window dimensions
                        (set-ref! STATE 2 0)
                        (set-ref! STATE 3 width)
                        (set-ref! STATE 4 height)

                        (let ((resize-handler (get dictionary 'resize-handler #f)))
                           (if resize-handler (resize-handler width height)))
                        (this dictionary))

                     (else
                        (print-to stderr "Unknown opengl server command " msg)
                        (this dictionary))))))
            ; блок непосредственно рабочего цикла окна
            (else
               ; обработаем сообщения (todo: не более чем N за раз)
               (let ((context (get dictionary 'context #f)))
                  (if context ; todo: добавить обработку кнопок
                     (native:process-events context (lambda (event)
                        (case event
                           (['quit] (halt 0))
                           (['keyboard key]
                              ((get dictionary 'keyboard-handler (lambda (key) #f)) key))
                           (['mouse button x y]
                              ((get dictionary 'mouse-handler (lambda (b x y) #f)) button x y))
                           (['resize width height] ; post resize event
                              (mail 'opengl ['resize width height]))
                           (else
                              (print "unknown event: " event)))))))
               ; проделаем все действия
               (let ((renderer (get dictionary 'renderer #f)))
                  (when renderer
                     (case-apply renderer
                        (list 0)
                        (list 1 (gl:GetMousePos (get dictionary 'context #f))))
                     (gl:SwapBuffers (get dictionary 'context #f))))
               ;; (let*((dictionary
               ;;       ; 1. draw (if renderer exists)
               ;;       (or (call/cc (lambda (return)
               ;;             (let ((renderer (get dictionary 'renderer #f)))
               ;;                (if renderer
               ;;                   ; есть чем рисовать - рисуем
               ;;                   (let ((userdata (apply renderer (get dictionary 'userdata #null))))
               ;;                      (gl:SwapBuffers (get dictionary 'context #f))
               ;;                      (return
               ;;                         (put dictionary 'userdata userdata)))))))
               ;;          dictionary))
               ;;       (dictionary
               ;;       ; 2. think (if thinker exists)
               ;;       (or (call/cc (lambda (return)
               ;;             (let ((thinker (get dictionary 'thinker #f)))
               ;;                (if thinker
               ;;                   dictionary))))
               ;;          dictionary))
               ;;       )

                  ; done.
                  (sleep 1)
                  (this dictionary)))))))))

(begin

; -=( main )=--------------------------
; force window creation.
(native:create-context "Ol: OpenGL Window")

; -----------------------------
(define (gl:set-renderer renderer)
   (mail 'opengl ['set-renderer renderer]))

(define (gl:set-window-title title)
   (mail 'opengl ['set-window-title title]))

(define (gl:set-window-size width height)
   (mail 'opengl ['set-window-size width height]))

(define (gl:finish)
   (await (mail 'opengl ['finish])))

(define hook:exit (lambda args (gl:finish)))

; -----------------------------
;; (define gl:Color (case-lambda
;;    ((r g b)
;;       (glColor3f r g b))))


(define (gl:set-mouse-handler handler)
   (mail 'opengl ['set 'mouse-handler handler]))

(define (gl:set-keyboard-handler handler)
   (mail 'opengl ['set 'keyboard-handler handler]))

(define (gl:set-resize-handler handler)
   (mail 'opengl ['set-resize-handler handler]))

))
