#!/bin/ol

; http://www.geeks3d.com/20120102/programming-tutorial-simple-x11-x-window-code-sample-for-linux-and-mac-os-x/

; http://www.eg.bucknell.edu/~cs367/glx/xintro.html
(import (owl pinvoke) (owl io)
   (lib x11)
   (OpenGL version-1-0)
)

(define OR (lambda args (fold bor 0 args)))

;(main)
(define dpy (XOpenDisplay 0))
(define s (XDefaultScreen dpy))

(define vi (glXChooseVisual dpy s
   (raw type-vector-raw '(
      4 0 0 0 ; GLX_RGBA
      5 0 0 0  1 0 0 0 ; GLX_DOUBLEBUFFER
      8 0 0 0  1 0 0 0 ; GLX_RED_SIZE
      9 0 0 0  1 0 0 0 ; GLX_GREEN_SIZE
     10 0 0 0  1 0 0 0 ; GLX_BLUE_SIZE
   
      0 0 0  0  )))); None
(define cx (glXCreateContext dpy vi 0 1))



(define win (XCreateSimpleWindow dpy (XRootWindow dpy s)
   10 10 200 200 1
   (XBlackPixel dpy s) (XWhitePixel dpy s)))
   
(XSelectInput dpy win (OR ExposureMask KeyPressMask))
(XMapWindow dpy win)


(define XEvent (raw type-vector-raw (repeat 0 192)))
(let loop ()
   (XNextEvent dpy XEvent)
   (if (= (refb XEvent 0) 12)
      (begin
         (glXMakeCurrent dpy win cx)
         (print "x")
         (glClearColor 0 0.4 0 1)
         (glClear #x00004000)
         
         (glMatrixMode GL_PROJECTION)
         (glLoadIdentity)
         (glOrtho  0 1
                   0 1
                  -1 1)
         (glMatrixMode GL_MODELVIEW)
         (glLoadIdentity)
         
         (glBegin GL_LINES)
         (let loop ((x 0))
            (glVertex2f x 0)
            (glVertex2f x 1)
            (if (< x 1)
               (loop (+ x 1/8))))
         (let loop ((y 0))
            (glVertex2f 0 y)
            (glVertex2f 1 y)
            (if (< y 1)
               (loop (+ y 1/8))))
         (glEnd)
         
         
         (glXSwapBuffers dpy win)
         (glXMakeCurrent dpy null null)
      ))
   (loop))
;(loop)

;    if ((e.type == ClientMessage) && 
;        (static_cast<unsigned int>(e.xclient.data.l[0]) == WM_DELETE_WINDOW))
;    {
;      break;
;    }

(print s)
(print "Ok.")
