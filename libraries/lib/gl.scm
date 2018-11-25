(define-library (lib gl)
(import
   (otus lisp) (otus ffi)
   (OpenGL) (lib gl config))

(export
   gl:set-window-title gl:set-window-size
   gl:set-context-version ; recreate OpenGL with version
   gl:set-userdata gl:get-userdata
   gl:set-renderer
   gl:set-mouse-handler
   gl:set-keyboard-handler
   gl:finish ; if renderer exists - wait for window close, else just glFinish

   gl:hide-cursor

   ; this library automatically creates window
   gl:enable-context gl:disable-context
   *atexit*)

(begin
   (setq OS (ref (uname) 1))

   (setq win32? (string-ci=? OS "Windows"))
   (setq linux? (string-ci=? OS "Linux"))
   (setq x32? (eq? (size nullptr) 4))

; check the platform
(or win32? linux?
   (runtime-error "Unsupported platform" OS))

; -- some debug staff -----
(define (vector->vptr vec offset)
   (let ((vptr (vm:cast 0 type-vptr)))
      (for-each (lambda (i)
            (set-ref! vptr i (ref vec (+ i offset))))
         (iota (size nullptr)))
      vptr))
(define (vector-set-int! vec offset int)
   (for-each (lambda (i)
         (set-ref! vec (+ offset i) (band #xFF (>> int (* i 8)))))
      (iota (size nullptr)))
   vec)
(define (vector-set-vptr! vec offset vptr)
   (for-each (lambda (i)
         (set-ref! vec (+ offset i) (ref vptr i)))
      (iota (size nullptr)))
   vec)

; --

(define WIDTH  (get config 'width  854))
(define HEIGHT (get config 'height 480))
(define CONFIG (list->ff '( ; todo: move to config
   (red   .  8)
   (green .  8)
   (blue  .  8)
   (depth . 24)
)))

; ===================================================
(define gl:enable-context (cond
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

(define gl:disable-context (cond
   (win32?  (lambda (context)
                  (gl:MakeCurrent #f #f)))
   (linux?  (lambda (context)
               (let ((display (ref context 1)))
                  (gl:MakeCurrent display #f #f))))))

(define gl:create-context (cond
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
               (pfd (make-bytevector '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
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
            (mail 'opengl (tuple 'set-context (tuple hDC hRC window)))
            (interact 'opengl (tuple 'get-context)) ; синхронизация

            (ShowWindow window 5)
            (tuple hDC hRC window)))))))
   ; -=( linux )=---------------------------------------------------------------------
   (linux?
      (let ((libX11 (load-dynamic-library "libX11.so.6"))
            (libGLX (load-dynamic-library "libGL.so.1")))
      (let ((XOpenDisplay  (libX11 type-vptr "XOpenDisplay" type-string))
            (XDefaultScreen(libX11 fft-int "XDefaultScreen" type-vptr))
            (XRootWindow   (libX11 fft-int "XRootWindow" type-vptr fft-int))
            (XBlackPixel   (libX11 type-vptr "XBlackPixel" type-vptr fft-int))
            (XWhitePixel   (libX11 type-vptr "XWhitePixel" type-vptr fft-int))
            (XCreateColormap (libX11 type-vptr "XCreateColormap" type-vptr fft-int type-vptr fft-int))
            (XCreateSimpleWindow (libX11 type-vptr "XCreateSimpleWindow"
                              type-vptr fft-int ; display, parent
                              fft-int fft-int fft-unsigned-int fft-unsigned-int ; x y width height
                              fft-unsigned-int  ; border width
                              type-vptr ; border
                              type-vptr ; background
                           ))
            (XCreateWindow (libX11 type-vptr "XCreateWindow"
                              type-vptr fft-int ; display, parent
                              fft-int fft-int fft-unsigned-int fft-unsigned-int ; x y width height
                              fft-unsigned-int  fft-int ; border_width, depth
                              fft-unsigned-int type-vptr ; class, visual
                              fft-unsigned-long ; valuemask
                              type-vptr)) ; XSetWindowAttributes* attributes

            (XSelectInput (libX11 fft-int "XSelectInput" type-vptr type-vptr fft-long))
            (XMapWindow   (libX11 fft-int "XMapWindow" type-vptr type-vptr))
            (XStoreName   (libX11 fft-int "XStoreName" type-vptr type-vptr type-string))
            ; glx
            (glXQueryExtension(libGLX fft-int "glXQueryExtension" type-vptr fft-int* fft-int*))
            (glXChooseVisual  (libGLX type-vptr "glXChooseVisual" type-vptr fft-int fft-int*))
            (glXCreateContext (libGLX type-vptr "glXCreateContext" type-vptr type-vptr type-vptr fft-int)))
      (lambda (title)
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
               ;; (XVisualInfo (vptr->vector vi 64)) ; sizeof(XVisualInfo) = 64
               ;; ; *unless (eq? 4 (class (int32->ol XVisualInfo 24))) (halt "TrueColor visual required for this program") ; offsetof(XVisualInfo, class)
               ;; ;(cx (gl:CreateContext display vi #false 1))

               ;; (visual (vector->vptr XVisualInfo 0)) ;
               ;; (root (XRootWindow display screen))
               ;; (colormap (XCreateColormap display root visual 0)) ; 0 == AllocNone

               ;; ; ...
               ;; (XSetWindowAttributes (list->vector (repeat 0 112))) ; sizeof(XSetWindowAttributes)
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
                        8  (get CONFIG 'red 5) ; GLX_RED_SIZE
                        9  (get CONFIG 'green 5) ; GLX_GREEN_SIZE
                       10  (get CONFIG 'blue 5) ; GLX_BLUE_SIZE
                       12  (get CONFIG 'depth 24) ; GLX_DEPTH_SIZE
                        5 ; GLX_DOUBLEBUFFER
                        0)))); None

            ; common code
            (XSelectInput display window 32773) ; ExposureMask | KeyPressMask | ButtonPressMask
            (XStoreName display window title)
            (XMapWindow display window)
            (let ((cx (gl:CreateContext display vi #false 1)))
               (gl:MakeCurrent display window cx)
               (print "OpenGL version: " (glGetString GL_VERSION))
               (print "OpenGL vendor: " (glGetString GL_VENDOR))
               (print "OpenGL renderer: " (glGetString GL_RENDERER))
               ;(gl:MakeCurrent display #f #f)
               (mail 'opengl (tuple 'set-context (tuple display screen window cx)))
               (interact 'opengl (tuple 'get-context)) ; синхронизация

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
      (lambda (context handler)
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
                        (loop)))))))))))
   (linux?
      (let ((libX11 (load-dynamic-library "libX11.so.6")))
      (let ((XPending  (libX11 fft-int "XPending" type-vptr))
            (XNextEvent(libX11 fft-int "XNextEvent" type-vptr type-vptr)))
      (lambda (context handler)
         (let ((display (ref context 1)))
         (let loop ((XEvent (make-bytevector 192))) ; 96 for x32
            (if (> (XPending display) 0)
               (begin
                  (XNextEvent display XEvent)
                  (case (int32->ol XEvent 0)
                     (2 ; KeyPress
                        (handler (tuple 'keyboard (int32->ol XEvent (if x32? 52 84))))) ; offsetof(XKeyEvent, keycode)
                     (4 ; ButtonPress
                        (let ((x (int32->ol XEvent (if x32? 32 64)))
                              (y (int32->ol XEvent (if x32? 36 68)))
                              (button (int32->ol XEvent (if x32? 52 84))))
                           (handler (tuple 'mouse button x y))))
                     (else ;
                        (print ".event.")))
                  (loop XEvent)))))))))
   (else
      (runtime-error "Unknown platform" OS))))

; internal function
(define gl:SetWindowTitle (cond
   (win32?
      (let ((user32 (load-dynamic-library "user32.dll")))
      (let ((SetWindowText (user32 fft-int "SetWindowTextW" fft-void* type-string-wide)))
         (lambda (context title)
            (let ((window (ref context 3)))
               (SetWindowText window (c-string title)))))))
   (linux?
      (let ((libX11 (load-dynamic-library "libX11.so.6")))
      (let ((XStoreName (libX11 fft-int "XStoreName" type-vptr type-vptr type-string)))
         (lambda (context title)
            (let ((display (ref context 1))
                  ;(screen  (ref context 2))
                  (window  (ref context 3))
                  (cx      (ref context 4)))
               (XStoreName display window (c-string title)))))))))

(define gl:SetWindowSize (cond
   (win32?
      (let ((user32 (load-dynamic-library "user32.dll")))
      (let ((GetWindowRect (user32 fft-int "MoveWindow" fft-void* (fft& fft-int)))
            (MoveWindow (user32 fft-int "MoveWindow" fft-void* fft-int fft-int fft-int fft-int fft-int)))
         (lambda (context width height)
            (let ((window (ref context 3))
                  (rect '(0 0 0 0)))
               (GetWindowRect window rect)
               (MoveWindow window (list-ref rect 0) (list-ref rect 1) width height 0))))))
   (linux?
      (let ((libX11 (load-dynamic-library "libX11.so.6")))
      (let ((XResizeWindow (libX11 fft-int "XResizeWindow" type-vptr type-vptr fft-int fft-int)))
         (lambda (context width height)
            (let ((display (ref context 1))
                  (window  (ref context 3)))
               (XResizeWindow display window width height))))))))

(define gl:HideCursor (cond
   (win32?
      #false)
   (linux?
      (let ((libX11 (load-dynamic-library "libX11.so.6")))
      (let ((XDefineCursor (libX11 fft-void "XDefineCursor" type-vptr type-vptr type-vptr))
            (XDefaultColormap (libX11 fft-int "XDefaultColormap" type-vptr fft-int))
            (XAllocNamedColor (libX11 fft-void "XAllocNamedColor" type-vptr fft-int type-string type-vptr type-vptr))
            (XCreateBitmapFromData (libX11 type-vptr "XCreateBitmapFromData" type-vptr type-vptr type-vptr fft-unsigned-int fft-unsigned-int))
            (XCreatePixmapCursor (libX11 type-vptr "XCreatePixmapCursor" type-vptr type-vptr type-vptr type-vptr type-vptr fft-unsigned-int fft-unsigned-int))
            (XFreeCursor (libX11 type-vptr "XFreeCursor" type-vptr type-vptr))
            (XFreePixmap (libX11 type-vptr "XFreePixmap" type-vptr type-vptr)))
         (lambda (context)
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
               (XFreePixmap (ref context 1) bm_no))))))))

(define (gl:hide-cursor)
   (gl:HideCursor (interact 'opengl (tuple 'get 'context))))

; internal:
(define gl:GetMousePos (cond
   (win32?
      #false)
   (linux?
      (let ((libX11 (load-dynamic-library "libX11.so.6")))
      (let ((XGetInputFocus (libX11 fft-void "XGetInputFocus" type-vptr (fft* type-vptr) (fft& fft-int)))
            (XQueryPointer (libX11 fft-int "XQueryPointer" type-vptr type-vptr (fft* type-vptr) (fft* type-vptr) (fft& fft-int) (fft& fft-int) (fft& fft-int) (fft& fft-int) (fft& fft-unsigned-int))))
         (lambda (context)
            (let ((window (make-vptr))
                  (revert_to_return (box 0)))
               (XGetInputFocus (ref context 1) window revert_to_return)
               (if (equal? window (ref context 3))
                  (let ((root (make-vptr))
                        (child (make-vptr))
                        (root_x (box 0)) (root_y (box 0))
                        (x (box 0)) (y (box 0)) (mask (box 0)))
                     (if (eq? 1 (XQueryPointer (ref context 1) (ref context 3) root child root_x root_y x y mask))
                        (cons (unbox x) (unbox y))))))))))))
; =============================================
; automation
(fork-server 'opengl (lambda ()
(let this ((dictionary #empty))
(cond
   ; блок обработки сообщений
   ((check-mail) => (lambda (e) ; can be (and (eq? something 0) (check-mail)) =>
      (let*((sender msg e))
         ;(print "envelope: " envelope)
         (tuple-case msg
            ; low level interface:
            ((set key value)
               (this (put dictionary key value)))
            ((get key)
               (mail sender (get dictionary key #false))
               (this dictionary))
            ((debug)
               (mail sender dictionary)
               (this dictionary))

            ((finish)  ; wait for OpenGL window closing (just no answer for interact)
               ;(glFinish)

               (unless (get dictionary 'renderer #f)
                  ; рендерера нет, значит оновим буфер
                  (gl:SwapBuffers (get dictionary 'context #f))
                  ; рендерер есть, но режим интерактивный? тогда вернем управление юзеру
                  (if (or (zero? (length *vm-args*)) (string-eq? (car *vm-args*) "-"))
                     (mail sender 'ok)))
               (this (put dictionary 'customer sender)))

            ; context
            ((set-context context)
               (this (put dictionary 'context context)))
            ((get-context)
               (mail sender (get dictionary 'context #f))
               (this dictionary))

            ; set-window-title
            ((set-window-title title)
               (gl:SetWindowTitle (get dictionary 'context #f) title)
               (this dictionary))

            ; set-window-size
            ((set-window-size width height)
               (gl:SetWindowSize (get dictionary 'context #f) width height)
               (this dictionary))

            ; renderer
            ((set-renderer renderer)
               (this (put dictionary 'renderer renderer)))
            ((get-renderer)
               (mail sender (get dictionary 'renderer #f))
               (this dictionary))

            ; userdata
            ((set-userdata userdata)
               (this (put dictionary 'userdata userdata)))
            ((get-userdata)
               (mail sender (get dictionary 'userdata #f))
               (this dictionary))

            (else
               (print-to stderr "Unknown opengl server command " msg)
               (this dictionary))))))
   ; блок непосредственно рабочего цикла окна
   (else
      ; обработаем сообщения (todo: не более чем N за раз)
      (let ((context (get dictionary 'context #f)))
         (if context ; todo: добавить обработку кнопок
            (gl:ProcessEvents context (lambda (event)
               (tuple-case event
                  ((keyboard key)
                     ((get dictionary 'keyboard-handler (lambda (x) #f)) key))
                  ((mouse button x y)
                     ((get dictionary 'mouse-handler (lambda (x) #f)) button x y))
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
         (this dictionary))))))

; userdata
(fork-server 'opengl-userdata (lambda ()
(let this ((dictionary #empty))
   (let*((envelope (wait-mail))
         (sender msg envelope))
      (tuple-case msg
         ; low level interaction interface
         ((set key data)
            (this (put dictionary key data)))
         ((get key)
            (mail sender (getf dictionary key))
            (this dictionary))
         ((debug) ; *debug interface
            (mail sender dictionary)
            (this dictionary))
         (else
            (print-to stderr "opengl-userdata: unknown message " msg)
            (this dictionary)))))))

; -=( main )=--------------------------
; force window creation.
(gl:create-context "Ol: OpenGL window")

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

(define *atexit* gl:finish)

; -=( 3.0+ )=-------------------------
; Higher OpenGL versions support
(import (OpenGL GLX ARB create_context))
(import (OpenGL WGL ARB create_context))

(define gl:set-context-version (cond
   (win32? (lambda (major minor)
         #false))
   (linux? (lambda (major minor)
      (let*((context (interact 'opengl (tuple 'get-context))) ;#(display screen window cx)
            (display screen window cx context)
            ; this functions requires GLX 1.3+
            (glXChooseFBConfig (GLX fft-void* "glXChooseFBConfig" fft-void* fft-int fft-int* fft-int&)))
            ;(glXGetVisualFromFBConfig (GLX fft-void* "glXGetVisualFromFBConfig" fft-void* fft-void*))
         ;; (print "display: " display)
         ;; (print "screen: " screen)

         (define visual_attribs (list
            #x8012    1 ; GLX_X_RENDERABLE
            #x8010    1 ; GLX_DRAWABLE_TYPE GLX_WINDOW_BIT
            #x22 #x8002 ; GLX_X_VISUAL_TYPE GLX_TRUE_COLOR
            #x8011    1 ; GLX_RENDER_TYPE GLX_RGBA_BIT
             8  (get CONFIG 'red 5) ; GLX_RED_SIZE
             9  (get CONFIG 'green 5) ; GLX_GREEN_SIZE
            10  (get CONFIG 'blue 5) ; GLX_BLUE_SIZE
            12  (get CONFIG 'depth 24) ; GLX_DEPTH_SIZE
             5        1 ; GLX_DOUBLEBUFFER
            0))

         (define fbcount (box 0))
         (define fbc*
            (glXChooseFBConfig display screen visual_attribs fbcount))
         ;; (print "fbcount: " (unbox fbcount))
         (define fbc (vptr->vector fbc* (* (size nullptr) (unbox fbcount))))
         (define bestFbc (extract-void* fbc 0))
         ;; (define vi (glXGetVisualFromFBConfig display bestFbc))

         (define contextAttribs (list
            GLX_CONTEXT_MAJOR_VERSION_ARB  major
            GLX_CONTEXT_MINOR_VERSION_ARB  minor
            0))
         (define new_cx (glXCreateContextAttribsARB display bestFbc NULL 1 contextAttribs))
         (define new_context (tuple display screen window new_cx))

         ; disable and destroy old context
         (gl:disable-context context) ; todo: destroy
         ; set new context
         (mail 'opengl (tuple 'set-context new_context))
         (gl:enable-context new_context)
         #true)))))

; -----------------------------
;; (define gl:Color (case-lambda
;;    ((r g b)
;;       (glColor3f r g b))))


(define (gl:set-mouse-handler handler)
   (mail 'opengl (tuple 'set 'mouse-handler handler)))

(define (gl:set-keyboard-handler handler)
   (mail 'opengl (tuple 'set 'keyboard-handler handler)))


))