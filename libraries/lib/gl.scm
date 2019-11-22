(define-library (lib gl)
(import
   (otus lisp) (otus ffi)
   (lib gl config))

(cond-expand
   (Android
      (import (OpenGL ES platform)))
   ((or Linux Windows Emscripten)
      (import (OpenGL platform)))
   (else
      (begin (runtime-error "Unsupported platform:" *uname*))))


(export
   gl:set-window-title gl:set-window-size
   gl:set-context-version ; recreate OpenGL with version
   gl:set-userdata gl:get-userdata
   gl:set-renderer
   gl:set-mouse-handler
   gl:set-keyboard-handler
      vkEnter vkEsc vkUp vkDown vkLeft vkRight
      vkQ vkW vkE vkR vkT vkY vkU vkI vkO vkP
      vkA vkS vkD vkF vkG vkH vkJ vkK vkL
      vkZ vkX vkC vkV vkB vkN vkM
      vkStar vkPlus vkMinus vkEqual
   gl:set-expose-handler
   gl:finish ; if renderer exists - wait for window close, else just glFinish

   gl:window-dimensions

   gl:hide-cursor
   *atexit*)

(begin
   (setq x32? (eq? (size nullptr) 4))
;;    (setq OS (ref (uname) 1))

;;    (setq win32? (string-ci=? OS "Windows"))
;;    (setq linux? (string-ci=? OS "Linux"))
;;    (setq x32? (eq? (size nullptr) 4))

;; ; check the platform
;; (or win32? linux?
;;    (runtime-error "Unsupported platform" OS))

; -- some debug staff -----
;; (define (vector->vptr vec offset)
;;    (let ((vptr (vm:cast 0 type-vptr)))
;;       (for-each (lambda (i)
;;             (set-ref! vptr i (ref vec (+ i offset))))
;;          (iota (size nullptr)))
;;       vptr))
;; (define (vector-set-int! vec offset int)
;;    (for-each (lambda (i)
;;          (set-ref! vec (+ offset i) (band #xFF (>> int (* i 8)))))
;;       (iota (size nullptr)))
;;    vec)
;; (define (vector-set-vptr! vec offset vptr)
;;    (for-each (lambda (i)
;;          (set-ref! vec (+ offset i) (ref vptr i)))
;;       (iota (size nullptr)))
;;    vec)

; --

   (define WIDTH  (get config 'width  854))
   (define HEIGHT (get config 'height 480))

;; (define CONFIG (pairs->ff '( ; todo: move to config
;;    (red   .  8)
;;    (green .  8)
;;    (blue  .  8)
;;    (depth . 24)
;;    ;
;;    ; alpha
;;    ; stencil
;;    ; version?
;; )))

; assume that window size can not be large than 16777215 for x32 build
;                                  and 72057594037927935 for x64 build.
   (define gl:window-dimensions [0 0 WIDTH HEIGHT]); x y width height
   (define STATE gl:window-dimensions) ;[0 0 WIDTH HEIGHT]) ; current window state
)

; ===================================================
(cond-expand
   ; -=( Android )=------------------------------------------
   (Android
      (begin
         (define (native:create-context title)
            (print-to stderr "OpenGL ES version: " (glGetString GL_VERSION))
            (print-to stderr "OpenGL ES vendor: " (glGetString GL_VENDOR))
            (print-to stderr "OpenGL ES renderer: " (glGetString GL_RENDERER))
            (print-to stderr "OpenGL ES extensions: " (glGetString GL_EXTENSIONS))

            (define width '(1184))
            (define height '(672))
            ;; (eglQuerySurface display surface #x3057 width) ;EGL_WIDTH
            ;; (eglQuerySurface display surface #x3056 height) ;EGL_HEIGHT

            (set-ref! gl:window-dimensions 3 (car width))
            (set-ref! gl:window-dimensions 4 (car height))

            (glViewport 0 0 (car width) (car height)))

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

         ;(set-ref! )
   ))

   ; -=( Linux )=------------------------------------------
   (Linux
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
                  ;; ; *unless (eq? 4 (class (int32->ol XVisualInfo 24))) (halt "TrueColor visual required for this program") ; offsetof(XVisualInfo, class)
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
                           8  (get config 'red 5) ; GLX_RED_SIZE
                           9  (get config 'green 6) ; GLX_GREEN_SIZE
                          10  (get config 'blue 5) ; GLX_BLUE_SIZE
                          12  (get config 'depth 24) ; GLX_DEPTH_SIZE
                           5 ; GLX_DOUBLEBUFFER
                           0)))); None

               ; common code
               (XSelectInput display window 32773) ; ExposureMask | KeyPressMask | ButtonPressMask
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
            (let loop ((XEvent (make-bytevector (if x32? 96 192)))) ; 96 for x32
               (if (> (XPending display) 0)
                  (begin
                     (XNextEvent display XEvent)
                     (case (int32->ol XEvent 0)
                        (2 ; KeyPress
                           (handler (tuple 'keyboard (int32->ol XEvent (if x32? 52 84))))) ; offsetof(XKeyEvent, keycode)
                        (3 #f) ; KeyRelease
                        (4 ; ButtonPress
                           (let ((x (int32->ol XEvent (if x32? 32 64)))
                                 (y (int32->ol XEvent (if x32? 36 68)))
                                 (button (int32->ol XEvent (if x32? 52 84))))
                              (handler (tuple 'mouse button x y))))
                        (5 #f) ; ButtonRelease
                        (12; Expose
                           (let ((x (int32->ol XEvent (if x32? 20 40)))
                                 (y (int32->ol XEvent (if x32? 24 44)))
                                 (w (int32->ol XEvent (if x32? 28 48)))
                                 (h (int32->ol XEvent (if x32? 32 52))))
                              ;(print "x: " x ", y: " y ", width: " w ", height: " h)
                              (handler (tuple 'expose x y w h))))
                        (else ;
                           (print "Unknown window event: " (int32->ol XEvent 0))))
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
                     (XStoreName display window (c-string title)))
                  (type-string-wide (let ((title (string->bytes title)))
                     (XChangeProperty display window
                        (XInternAtom display (c-string "_NET_WM_NAME") 0)
                        (XInternAtom display (c-string "UTF8_STRING") 0)
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
                  ;(? (XAllocNamedColor (ref context 1) cmap (c-string "black") black dummy))
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
            (exec ffi initialize_gl4es (cons fft-int #null) #null)

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

         (define (native:create-context title)
            (let*((window (CreateWindowEx
                     #x00040100 (c-string "#32770") (c-string title) ; WS_EX_APPWINDOW|WS_EX_WINDOWEDGE, #32770 is system classname for DIALOG
                     #x06cf0000 ; WS_OVERLAPPEDWINDOW | WS_CLIPSIBLINGS | WS_CLIPCHILDREN
                     0 0 WIDTH HEIGHT ; x y width height
                     #false ; no parent window
                     #false ; no menu
                     #false ; instance
                     #false))
                  (pfd (make-bytevector '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                          00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))
                  (hDC (GetDC window))
                  (PixelFormat (ChoosePixelFormat hDC pfd)))
               (SetPixelFormat hDC PixelFormat pfd)
               (let ((hRC (gl:CreateContext hDC)))
                  (gl:MakeCurrent hDC hRC)
                  (print-to stderr "OpenGL version: " (glGetString GL_VERSION))
                  (print-to stderr "OpenGL vendor: " (glGetString GL_VENDOR))
                  (print-to stderr "OpenGL renderer: " (glGetString GL_RENDERER))
               ;(gl:MakeCurrent #f #f)
                  (mail 'opengl (tuple 'set-context (tuple hDC hRC window)))
                  (interact 'opengl (tuple 'get-context)) ; синхронизация

                  (ShowWindow window 5)
                  (tuple hDC hRC window))))

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
                        (message (+ (<< (ref MSG (+ 0 (* w 1)))  0)      ; 4 for x32
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
                           (loop))))))))

         ; ---
         (setq SetWindowText (user32 fft-int "SetWindowTextW" fft-void* type-string-wide))

         (define (gl:SetWindowTitle context title)
            (let ((window (ref context 3)))
               (SetWindowText window (c-string title))))

         ; ---
         (setq GetWindowRect (user32 fft-int "MoveWindow" fft-void* (fft& fft-int)))
         (setq MoveWindow (user32 fft-int "MoveWindow" fft-void* fft-int fft-int fft-int fft-int fft-int))

         (define (gl:SetWindowSize context width height)
            (let ((window (ref context 3))
                  (rect '(0 0 0 0)))
               (GetWindowRect window rect)
               (MoveWindow window (list-ref rect 0) (list-ref rect 1) width height 0)))

         ; ---
         (define (gl:HideCursor context)
            #false)

         ; ---
         (define (gl:GetMousePos context)
            #false)

      ))

   (else
      (begin (runtime-error "Unknown platform OS" *uname*))))

(begin

; internal function


(define (gl:hide-cursor)
   (gl:HideCursor (interact 'opengl (tuple 'get 'context))))

)

; -=( opengl coroutine )=------------------------------------
(cond-expand
   ((or Android Emscripten)
      (begin
         (fork-server 'opengl (lambda ()
            (print "starting 'opengl coroutine")
         (let this ((dictionary #empty))
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
               )))))))
   (else
      (begin
         ; =============================================
         ; automation
         (fork-server 'opengl (lambda ()
         (let this ((dictionary {
               'expose-handler glViewport}))
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
                           ;(if *interactive* ;(or (zero? (length *vm-args*)) (string-eq? (car *vm-args*) "-"))
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
                     (['set-expose-handler expose-handler]
                        (if expose-handler
                           (expose-handler (ref STATE 1) (ref STATE 2) (ref STATE 3) (ref STATE 4)))
                        (this (put dictionary 'expose-handler expose-handler)))

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
                           (['keyboard key]
                              ((get dictionary 'keyboard-handler (lambda (key) #f)) key))
                           (['mouse button x y]
                              ((get dictionary 'mouse-handler (lambda (x y) #f)) button x y))
                           (['expose x y width height]
                              (set-ref! STATE 1 x) ; save current window dimensions
                              (set-ref! STATE 2 y)
                              (set-ref! STATE 3 width)
                              (set-ref! STATE 4 height)

                              (let ((expose-handler (get dictionary 'expose-handler #f)))
                                 (if expose-handler (expose-handler x y width height))))
                           (else
                              (print "unknown event: " event)))))))
               ; проделаем все действия
               (let ((renderer (get dictionary 'renderer #f)))
                  (if renderer (begin
                     ;; (print "renderer: " renderer)
                     ;; (print "context: " (get dictionary 'context #f))
                     ;; (print "mouse: " (gl:GetMousePos (ref (get dictionary 'context #f) 3)))
                     (renderer (gl:GetMousePos (get dictionary 'context #f)))
                     (gl:SwapBuffers (get dictionary 'context #f)))))
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
; userdata
(fork-server 'opengl-userdata (lambda ()
(let this ((dictionary #empty))
   (let*((envelope (wait-mail))
         (sender msg envelope))
      (case msg
         ; low level interaction interface
         (['set key data]
            (this (put dictionary key data)))
         (['get key]
            (mail sender (getf dictionary key))
            (this dictionary))
         (['debug] ; *debug interface
            (mail sender dictionary)
            (this dictionary))
         (else
            (print-to stderr "opengl-userdata: unknown message " msg)
            (this dictionary)))))))

; -=( main )=--------------------------
; force window creation.
(native:create-context "Ol: OpenGL Window")

; -----------------------------
(define (gl:set-userdata userdata)
   (mail 'opengl-userdata (tuple 'set 'userdata userdata)))
(define (gl:get-userdata)
   (interact 'opengl-userdata (tuple 'get 'userdata)))

(define (gl:set-renderer renderer)
   (mail 'opengl (tuple 'set-renderer renderer)))

(define (gl:set-window-title title)
   (mail 'opengl (tuple 'set-window-title title)))

(define (gl:set-window-size width height)
   (mail 'opengl (tuple 'set-window-size width height)))

(define (gl:finish)
   (interact 'opengl (tuple 'finish)))

(define *atexit* gl:finish))

; -=( 3.0+ )=-------------------------
; Higher OpenGL versions support
(cond-expand
   ((or Android Emscripten) ; TODO: dev only, please rename to Android
      (begin
         (define (gl:set-context-version major minor)
            #false)
   ))
   (Linux
      (import (OpenGL GLX ARB create_context))
      (begin
         (define (gl:set-context-version major minor)
            (let*((context (interact 'opengl (tuple 'get-context))) ;#(display screen window cx)
                  (display screen window cx context)
                  ; this functions requires GLX 1.3+
                  (glXChooseFBConfig (GL_LIBRARY fft-void* "glXChooseFBConfig" fft-void* fft-int fft-int* fft-int&)))
                  ;(glXGetVisualFromFBConfig (GLX fft-void* "glXGetVisualFromFBConfig" fft-void* fft-void*))
               ;; (print "display: " display)
               ;; (print "screen: " screen)

               (define visual_attribs (list
                  #x8012    1 ; GLX_X_RENDERABLE
                  #x8010    1 ; GLX_DRAWABLE_TYPE GLX_WINDOW_BIT
                  #x22 #x8002 ; GLX_X_VISUAL_TYPE GLX_TRUE_COLOR
                  #x8011    1 ; GLX_RENDER_TYPE GLX_RGBA_BIT
                  8  (get config 'red 5)    ; GLX_RED_SIZE
                  9  (get config 'green 6)  ; GLX_GREEN_SIZE
                  10 (get config 'blue 5)  ; GLX_BLUE_SIZE
                  12 (get config 'depth 24); GLX_DEPTH_SIZE
                  5         1 ; GLX_DOUBLEBUFFER
                  0))

               (define fbcount (box 0))
               (define fbc*
                  (glXChooseFBConfig display screen visual_attribs fbcount))
               ;; (print "fbcount: " (unbox fbcount))
               (define fbc (vptr->bytevector fbc* (* (size nullptr) (unbox fbcount))))
               (define bestFbc (extract-void* fbc 0))
               ;; (define vi (glXGetVisualFromFBConfig display bestFbc))

               (define contextAttribs (list
                  GLX_CONTEXT_MAJOR_VERSION_ARB  major
                  GLX_CONTEXT_MINOR_VERSION_ARB  minor
                  0))
               (define new_cx (glXCreateContextAttribsARB display bestFbc NULL 1 contextAttribs))
               (define new_context (tuple display screen window new_cx))

               ; disable and destroy old context
               (native:disable-context context) ; todo: destroy
               ; set new context
               (mail 'opengl (tuple 'set-context new_context))
               (native:enable-context new_context)
               #true))))
   (Windows
      (import (OpenGL WGL ARB create_context))
      (begin
         (define (gl:set-context-version major minor)
            #false)))
)
(begin

; -----------------------------
;; (define gl:Color (case-lambda
;;    ((r g b)
;;       (glColor3f r g b))))


(define (gl:set-mouse-handler handler)
   (mail 'opengl (tuple 'set 'mouse-handler handler)))

(define (gl:set-keyboard-handler handler)
   (mail 'opengl (tuple 'set 'keyboard-handler handler)))
(define vkEnter 36) (define vkEsc 9)
(define vkAlt 64) (define vkShift 62)
(define vkUp 111) (define vkDown 116) (define vkLeft 113) (define vkRight 114)
(define vkQ 24) (define vkW 25) (define vkE 26) (define vkR 27) (define vkT 28) (define vkY 29) (define vkU 30) (define vkI 31) (define vkO 32) (define vkP 33)
(define vkA 38) (define vkS 39) (define vkD 40) (define vkF 41) (define vkG 42) (define vkH 43) (define vkJ 44)(define vkK 45) (define vkL 46)
(define vkZ 52) (define vkX 53) (define vkC 54) (define vkV 55) (define vkB 56) (define vkN 57) (define vkM 58)
(define vkStar 63) (define vkPlus 86) (define vkMinus 82) (define vkEqual 21)

(define (gl:set-expose-handler handler)
   (mail 'opengl (tuple 'set-expose-handler handler)))

))