(setq X11 (or (load-dynamic-library "libX11.so")
              (load-dynamic-library "libX11.so.6")))

; context manipulations
(setq XOpenDisplay   (X11 fft-void* "XOpenDisplay" type-string))
(setq XDefaultScreen (X11 fft-int "XDefaultScreen" fft-void*))
(setq XRootWindow    (X11 fft-void* "XRootWindow" fft-void* fft-int))
(setq XBlackPixel    (X11 fft-void* "XBlackPixel" fft-void* fft-int))
(setq XWhitePixel    (X11 fft-void* "XWhitePixel" fft-void* fft-int))
(setq XCreateSimpleWindow (X11 fft-void* "XCreateSimpleWindow" fft-void* fft-void* ; display, parent
                              fft-int fft-int fft-unsigned-int fft-unsigned-int ; x y width height
                              fft-unsigned-int  ; border-width
                              fft-void* ; border
                              fft-void*)) ; background
(setq XSelectInput   (X11 fft-void "XSelectInput" fft-void* fft-void* fft-long))
(setq XMapWindow     (X11 fft-void "XMapWindow" fft-void* fft-void*))
(setq XStoreName     (X11 fft-void "XStoreName" fft-void* fft-void* type-string))
(setq XKeycodeToKeysym  (X11 fft-unsigned-long "XKeycodeToKeysym" fft-void* fft-unsigned-char fft-int))

(setq GLX GL_LIBRARY)
(setq glXChooseVisual   (GLX fft-void* "glXChooseVisual" fft-void* fft-int fft-int*))
(setq glXCreateContext  (GLX fft-void* "glXCreateContext" fft-void* fft-void* fft-void* fft-int))
(setq glXMakeCurrent    (GLX fft-int "glXMakeCurrent" fft-void* fft-void* fft-void*))
(setq glXSwapBuffers    (GLX fft-void "glXSwapBuffers" fft-void* fft-void*))

; functions
(define (native:create-context title)
   (let*((dpy (XOpenDisplay #f))
         (screen (XDefaultScreen dpy))
         (window (XCreateSimpleWindow dpy
                     (XRootWindow dpy screen)
                     0 0 WIDTH HEIGHT 1
                     (XBlackPixel dpy screen)
                     (XWhitePixel dpy screen)))
         (vi (glXChooseVisual dpy screen (list
               4 ; GLX_RGBA
               8  (get config 'red #xFFFFFFFF)   ; GLX_RED_SIZE = GLX_DONT_CARE
               9  (get config 'green #xFFFFFFFF) ; GLX_GREEN_SIZE = GLX_DONT_CARE
               10 (get config 'blue #xFFFFFFFF)  ; GLX_BLUE_SIZE = GLX_DONT_CARE
               12 (get config 'depth 24)         ; GLX_DEPTH_SIZE
               5 ; GLX_DOUBLEBUFFER
               0)))) ; None

      ; common code
      (XSelectInput dpy window #b100000000000000101) ; StructureNotifyMask | KeyPressMask | ButtonPressMask
      (XStoreName dpy window title)
      (XMapWindow dpy window)
      (let ((cx (glXCreateContext dpy vi #false 1)))
         (glXMakeCurrent dpy window cx)
         (print-to stderr "OpenGL version: " (glGetString GL_VERSION))
         (print-to stderr "OpenGL vendor: " (glGetString GL_VENDOR))
         (print-to stderr "OpenGL renderer: " (glGetString GL_RENDERER))

         [dpy screen window cx])))

(define (native:enable-context context)
   (vector-apply context (lambda (dpy screen window cx)
      (glXMakeCurrent dpy window cx))))

(define (native:disable-context context)
   (vector-apply context (lambda (dpy screen window cx)
      (glXMakeCurrent dpy #false #f))))

(define (native:swap-buffers context)
   (vector-apply context (lambda (dpy screen window cx)
      (glXSwapBuffers dpy window))))

; -=( process events )=--------------------------------
(setq XPending      (X11 fft-int "XPending" type-vptr))
(setq XNextEvent    (X11 fft-void "XNextEvent" type-vptr type-bytevector))

(import (prefix (lib x11 config) x11))
(import (lib gamepad))

(define (native:process-events context handler)
   (vector-apply context (lambda (dpy screen window cx)
      ; x11 events
      (define XEvent (make-bytevector (x11config '|sizeof XEvent|)))
      (let loop ()
         (when (> (XPending dpy) 0)
            (XNextEvent dpy XEvent)
            ; https://tronche.com/gui/x/xlib/events/types.html
            (case (bytevector->int32 XEvent 0)
               (2 ; KeyPress
                  (handler ['keyboard (XKeycodeToKeysym dpy
                        (bytevector->int32 XEvent (x11config '|XKeyEvent.keycode|)) 0)]))
               (3 #f) ; KeyRelease, skip
               (4 ; ButtonPress
                  (let ((x (bytevector->int32 XEvent (x11config '|XButtonEvent.x|)))
                        (y (bytevector->int32 XEvent (x11config '|XButtonEvent.y|)))
                        (button (bytevector->int32 XEvent (x11config '|XButtonEvent.button|))))
                     (handler ['mouse button x y])))
               (5 #f) ; ButtonRelease
               ; others
               (17 #f); DestroyNotify
               (19 #f) ; MapNotify
               (21 #f) ; ReparentNotify
               (22 ; ConfigureNotify
                  (let ((w (bytevector->int32 XEvent (x11config '|XConfigureEvent.width|)))
                        (h (bytevector->int32 XEvent (x11config '|XConfigureEvent.height|))))
                     (mail 'opengl ['resize w h])))
               (else #f))
            (loop)))

      ; gamepad events
      (let loop ()
         (define ev (read-event))
         (if ev (vector-apply ev (lambda (value evtype abnumber)
            (handler ['gamepad evtype abnumber value])
            (loop)))))
   )))

; ---
(setq XInternAtom (X11 type-vptr "XInternAtom" type-vptr type-string fft-int))
(setq XChangeProperty (X11 fft-void "XChangeProperty" type-vptr type-vptr type-vptr type-vptr fft-int fft-int type-string fft-int))

(define (gl:SetWindowTitle context title)
   (vector-apply context (lambda (dpy screen window cx)
      (case (type title)
         (type-string
            (XStoreName dpy window title))
         (type-string-wide
            (XChangeProperty dpy window ; BUG: is not working
               (XInternAtom dpy "_NET_WM_NAME" 0)
               (XInternAtom dpy "UTF8_STRING" 0)
               8 0 ;PropModeReplace
               title 0))))))

; ---
(setq XResizeWindow (X11 fft-int "XResizeWindow" type-vptr type-vptr fft-int fft-int))

(define (gl:SetWindowSize context width height)
   (let ((display (ref context 1))
         (window  (ref context 3)))
      (XResizeWindow display window width height)))

; ---
(setq XDefineCursor (X11 fft-void "XDefineCursor" type-vptr type-vptr type-vptr))
(setq XDefaultColormap (X11 fft-int "XDefaultColormap" type-vptr fft-int))
(setq XAllocNamedColor (X11 fft-void "XAllocNamedColor" type-vptr fft-int type-string type-vptr type-vptr))
(setq XCreateBitmapFromData (X11 type-vptr "XCreateBitmapFromData" type-vptr type-vptr type-vptr fft-unsigned-int fft-unsigned-int))
(setq XCreatePixmapCursor (X11 type-vptr "XCreatePixmapCursor" type-vptr type-vptr type-vptr type-vptr type-vptr fft-unsigned-int fft-unsigned-int))
(setq XFreeCursor (X11 type-vptr "XFreeCursor" type-vptr type-vptr))
(setq XFreePixmap (X11 type-vptr "XFreePixmap" type-vptr type-vptr))

(define (gl:HideCursor context)
   (vector-apply context (lambda (dpy screen window cx)
      (let*((bm_no_data '(0 0 0 0 0 0 0 0))
            (cmap (XDefaultColormap dpy screen))
            ; sizeof XColor is 12 for x64 and 9 for x86
            (black (make-bytevector 12))
            (dummy (make-bytevector 12))
            ;(? (XAllocNamedColor dpy cmap "black" black dummy))
            (bm_no (XCreateBitmapFromData dpy window black 8 8))
            (cursor (XCreatePixmapCursor dpy bm_no bm_no black black 0 0)))
         (XDefineCursor dpy window cursor)
         (XFreeCursor dpy cursor)
         (XFreePixmap dpy bm_no)))))

; ---
(setq XGetInputFocus (X11 fft-void "XGetInputFocus" type-vptr (fft* type-vptr) (fft& fft-int)))
(setq XQueryPointer (X11 fft-int "XQueryPointer" type-vptr type-vptr (fft* type-vptr) (fft* type-vptr) (fft& fft-int) (fft& fft-int) (fft& fft-int) (fft& fft-int) (fft& fft-unsigned-int)))

(define (gl:GetMousePos context)
   (vector-apply context (lambda (dpy screen window cx)
   (let ((vptr (make-vptr))
         (revert_to_return (box 0)))
      (XGetInputFocus dpy vptr revert_to_return)
      (if (equal? vptr window) ; we'r focused?
         (let ((root (make-vptr))
               (child (make-vptr))
               (root_x (box 0)) (root_y (box 0))
               (x (box 0)) (y (box 0)) (mask (box 0)))
            (if (eq? (XQueryPointer dpy window root child root_x root_y x y mask) 1)
               (cons (unbox x) (unbox y)))))))))
