#!/usr/bin/ol

(import (EGL version-1-1)
   (lib x11) (owl io)
   (otus ffi))

(define width 640)
(define height 480)

;(main)
(define x11Display (XOpenDisplay #f))
(define eglDisplay (eglGetDisplay x11Display))

(define egl-major (make-32bit-array 1))
(define egl-minor (make-32bit-array 1))
(eglInitialize eglDisplay egl-major egl-minor)

(print "egl version: " (car egl-major) "." (car egl-minor))

(print "EGL_VERSION: " (eglQueryString eglDisplay EGL_VERSION))
(print "EGL_VENDOR: " (eglQueryString eglDisplay EGL_VENDOR))
(print "EGL_EXTENSIONS: " (eglQueryString eglDisplay EGL_EXTENSIONS))

(runtime-error "Ok" '())


(define screen (XDefaultScreen display))

(define window (XCreateSimpleWindow display (XRootWindow display screen)
   0 0 width height 1
   (XBlackPixel display screen) (XWhitePixel display screen)))

(XSelectInput display window ExposureMask)
(XMapWindow display window)
(XStoreName display window "1. Creating an OpenGL Window")

(define vi (glXChooseVisual display screen
   (vm:new-raw-object type-vector-raw '(
      4 0 0 0 ; GLX_RGBA
      5 0 0 0  1 0 0 0 ; GLX_DOUBLEBUFFER
      8 0 0 0  1 0 0 0 ; GLX_RED_SIZE
      9 0 0 0  1 0 0 0 ; GLX_GREEN_SIZE
     10 0 0 0  1 0 0 0 ; GLX_BLUE_SIZE

      0 0 0 0)))); None
(define cx (glXCreateContext display vi 0 1))


;(init)
(glXMakeCurrent display window cx)

(print "OpenGL version: " (glGetString GL_VERSION))
(print "OpenGL vendor: " (glGetString GL_VENDOR)) 
(print "OpenGL renderer: " (glGetString GL_RENDERER))

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(glXMakeCurrent display null null)


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
   (glXMakeCurrent display null null)
(loop)))

;(done)
(print "Ok.")
