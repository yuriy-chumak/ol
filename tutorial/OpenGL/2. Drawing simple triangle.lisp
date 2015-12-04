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

(glXMakeCurrent display null null)

;(loop)
(let ((XEvent (raw type-vector-raw (repeat 0 192))))
(let loop ()
   ; http://www-h.eng.cam.ac.uk/help/tpl/graphics/X/X11R5/node25.html
   (let process-events ()
      (if (> (XPending display) 0)
         (begin
            (XNextEvent display XEvent)
            (process-events))))

   (glXMakeCurrent display window cx)
   (glClear GL_COLOR_BUFFER_BIT)

   (glColor3f 0.2 0.5 0.2)
   (glBegin GL_TRIANGLES)
      (glVertex2f -0.2 -0.2)
      (glVertex2f +0.2 -0.2)
      (glVertex2f -0.0 +0.3)
   (glEnd)

   (glXSwapBuffers display window)
   (glXMakeCurrent display null null)
(loop)))

;(done)
(print "Ok.")
