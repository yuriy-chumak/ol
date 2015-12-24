#!/usr/bin/ol

(import (OpenGL version-1-0)
   (lib x11) (owl io))

(define width 640)
(define height 480)

;(main)
(define display (XOpenDisplay 0))
(define screen (XDefaultScreen display))

(define vi (glXChooseVisual display screen
   (raw type-vector-raw '(
      4 0 0 0 ; GLX_RGBA
      5 0 0 0  1 0 0 0 ; GLX_DOUBLEBUFFER
      8 0 0 0  1 0 0 0 ; GLX_RED_SIZE
      9 0 0 0  1 0 0 0 ; GLX_GREEN_SIZE
     10 0 0 0  1 0 0 0 ; GLX_BLUE_SIZE
     12 0 0 0  1 0 0 0 ; GLX_DEPTH_SIZE

      0 0 0 0)))); None
(define cx (glXCreateContext display vi 0 1))

(define window (XCreateSimpleWindow display (XRootWindow display screen)
   0 0 width height 1
   (XBlackPixel display screen) (XWhitePixel display screen)))
   
(XSelectInput display window ExposureMask)
(XMapWindow display window)


;(init)
(glXMakeCurrent display window cx)

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)
(gluPerspective 45 (/ width height) 0.1 100)

(glEnable GL_DEPTH_TEST)

(glXMakeCurrent display null null)

(define sphere (gluNewQuadric))
(gluQuadricDrawStyle sphere GLU_LINE)

;(loop)
(let ((XEvent (raw type-vector-raw (repeat 0 192))))
(let loop ((x 1) (dx 0.02) (y 3) (dy 0.03))
   (let process-events ()
      (if (> (XPending display) 0)
         (begin
            (XNextEvent display XEvent)
            (process-events))))

      (glXMakeCurrent display window cx)
   (glClear (fx:or GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt x y 5
      0 0 0
      0 1 0)

   (gluSphere sphere 1 16 8)

   (glXSwapBuffers display window)
   (glXMakeCurrent display null null)

   (let ((nx (if (or (> x 2) (< x -2)) (- dx) dx))
         (ny (if (or (> y 4) (< y -4)) (- dy) dy)))
      (loop (+ x nx) nx (+ y ny) ny))))

;(done)
(gluDeleteQuadric sphere)

(print "Ok.")
