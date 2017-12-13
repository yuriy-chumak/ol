#!/usr/bin/ol
;http://wili.cc/blog/ogl3-glx.html
(import (lib opengl))
(define Context (gl:Create "9. OpenGL 4.0"))

(import (OpenGL version-3-0))

; let's init context
(define (vector->int32 vector) (+
   (<< (ref vector 0)  0)
   (<< (ref vector 1)  8)
   (<< (ref vector 2) 16)
   (<< (ref vector 3) 24)))

(define major (vm:new-raw-object type-vector-raw '(0 0 0 0)))
(define minor (vm:new-raw-object type-vector-raw '(0 0 0 0)))
(glGetIntegerv GL_MAJOR_VERSION major)
(glGetIntegerv GL_MINOR_VERSION minor)

(print (vector->int32 major) "." (vector->int32 minor))




(define width 640)
(define height 480)

;(main)
#|
(define display (XOpenDisplay #f)) ;"Couldn't open X11 display"


(define (glx:query-version)
   (let ((glx_major (vm:new-raw-object type-vector-raw '(0 0 0 0)))
         (glx_minor (vm:new-raw-object type-vector-raw '(0 0 0 0))))
      (or
         (glXQueryVersion display glx_major glx_minor)
         (runtime-error "Can't get glX version" null))
      (cons (vector->int32 glx_major)
            (vector->int32 glx_minor))))

(define glx_version (glx:query-version))
(print "glX version " glx_version)
; todo: compare with '(1 . 3)


(define screen (XDefaultScreen display))

(define num (vm:new-raw-object type-vector-raw '(0 0 0 0)))
(define fbc
(glXChooseFBConfig display screen
   (vm:new-raw-object type-vector-raw '(
      8 0 0 0  1 0 0 0 ; GLX_RED_SIZE
      9 0 0 0  1 0 0 0 ; GLX_GREEN_SIZE
     10 0 0 0  1 0 0 0 ; GLX_BLUE_SIZE
     11 0 0 0  1 0 0 0 ; GLX_ALPHA_SIZE
      5 0 0 0 ; GLX_DOUBLEBUFFER

      0 0 0 0))
   num))
(print (vector->int32 num) ":" (IN fbc 4))

(define vi (glXChooseVisual display screen
   (vm:new-raw-object type-vector-raw '(
      4 0 0 0 ; GLX_RGBA
      5 0 0 0 ; GLX_DOUBLEBUFFER
      8 0 0 0  1 0 0 0 ; GLX_RED_SIZE
      9 0 0 0  1 0 0 0 ; GLX_GREEN_SIZE
     10 0 0 0  1 0 0 0 ; GLX_BLUE_SIZE

      0 0 0 0)))); None
(define fbc0 (vm:new-raw-object type-port (vector->list (IN fbc 4))))
(print "fbc0: " fbc0)

;(XFree fbc)


(define glXCreateContextAttribs  (glGetProcAddress type-port (c-string "glXCreateContextAttribsARB") type-port type-port type-int+ type-int+ type-vector-raw))
(print "glXCreateContextAttribsARB: " glXCreateContextAttribs)

(define context (glXCreateContextAttribs display fbc0 0 1 (vm:new-raw-object type-vector-raw '(
   #x91 #x20 0 0  2 0 0 0 ; GLX_CONTEXT_MAJOR_VERSION_ARB
   #x92 #x20 0 0  1 0 0 0 ; GLX_CONTEXT_MINOR_VERSION_ARB
;   #x94 #x20 0 0  2 0 0 0 ; GLX_CONTEXT_FLAGS_ARB, GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB
   0 0 0 0))))

;(runtime-error "Ok." context)


(define window (XCreateSimpleWindow display (XRootWindow display screen)
   0 0 width height 1
   (XBlackPixel display screen) (XWhitePixel display screen)))

(XSelectInput display window ExposureMask)
(XMapWindow display window)
(XStoreName display window "1. Creating an OpenGL Window")

(define cx context)


;(init)
(glXMakeCurrent display window cx)

(print "OpenGL version: " (glGetString GL_VERSION))
(print "OpenGL vendor: " (glGetString GL_VENDOR))
(print "OpenGL renderer: " (glGetString GL_RENDERER))


(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(glXMakeCurrent display #f #f)


;(loop)
(let ((XEvent (vm:new-raw-object type-vector-raw (repeat 0 192))))
(let loop ()
   (let process-events ()
      (if (> (XPending display) 0)
         (begin
            (XNextEvent display XEvent)
            (process-events))))

   ;(draw)
   (glXMakeCurrent display window cx)
   (glClear GL_COLOR_BUFFER_BIT)

   (glXSwapBuffers display window)
   (glXMakeCurrent display #f #f)
(loop)))
|#
;(done)
(print "Ok.")
