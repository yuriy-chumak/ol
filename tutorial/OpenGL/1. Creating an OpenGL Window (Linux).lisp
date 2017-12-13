#!/usr/bin/ol

(import (OpenGL version-1-0)
   (lib x11) (owl io))

(define width 640)
(define height 480)


;(main)
(define display (XOpenDisplay #false))
(define screen (XDefaultScreen display))
(define root (XRootWindow display screen))

(define vi (or
   (glXChooseVisual display screen (list
      GLX_RGBA
      GLX_DOUBLEBUFFER
      GLX_RED_SIZE 1
      GLX_GREEN_SIZE 1
      GLX_BLUE_SIZE 1
      GLX_DEPTH_SIZE 1
      0))
   (runtime-error "Can't get visual for display" #f)))

(define window (XCreateSimpleWindow display root
   0 0 width height 1
   (XBlackPixel display screen) (XWhitePixel display screen)))


(define cx (or
   (glXCreateContext display vi #false 1)
   (runtime-error "Can't create gl context" #f)))

(XFree vi)

(XMapWindow display window)
(XSelectInput display window ExposureMask)
(XStoreName display window (c-string "1. Creating an OpenGL Window"))


;(init)
(glXMakeCurrent display window cx)

(print "OpenGL version: " (glGetString GL_VERSION))
(print "OpenGL vendor: " (glGetString GL_VENDOR))
(print "OpenGL renderer: " (glGetString GL_RENDERER))

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(glXMakeCurrent display #false #false)


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

;(done)
(print "Ok.")
