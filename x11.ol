#!/bin/ol

; http://www.geeks3d.com/20120102/programming-tutorial-simple-x11-x-window-code-sample-for-linux-and-mac-os-x/

; http://www.eg.bucknell.edu/~cs367/glx/xintro.html
(import (owl pinvoke) (owl io)
   (OpenGL version-1-0)
)

;(define % (dlopen '() RTLD_LAZY)) ; dummy - how to get own handle
(define GL (dlopen "libGL.so" RTLD_LAZY))

; opengl: https://gist.github.com/gszauer/da038dec2a7ffc288c41
(define glXChooseVisual  (dlsym GL type-port "glXChooseVisual" type-port type-int+ type-vector-raw))
(define glXCreateContext (dlsym GL type-port "glXCreateContext" type-port type-port type-int+ type-int+))
(define glXMakeCurrent   (dlsym GL type-int+ "glXMakeCurrent"  type-port type-port type-port))
(define glXSwapBuffers   (dlsym GL type-int+ "glXSwapBuffers"  type-port type-port))

;(print (syscall 59 "ls" "-l" #f))

(define OR (lambda args (fold bor 0 args)))

(define % (dlopen "libX11.so" RTLD_LAZY))
(define XOpenDisplay (dlsym % type-port "XOpenDisplay" type-int+))
(define XDefaultScreen (dlsym % type-int+ "XDefaultScreen" type-port))

(define XRootWindow (dlsym % type-port "XRootWindow" type-port type-int+))
(define XBlackPixel (dlsym % type-port "XBlackPixel" type-port type-int+))
(define XWhitePixel (dlsym % type-port "XWhitePixel" type-port type-int+))

(define XCreateWindow (dlsym % type-port "XCreateWindow"
   type-port ; display
   type-port ; parent Window
   type-int+ type-int+ type-int+ type-int+ ; x y width height
   type-int+ ; border width
   type-int+ ; depth
   type-int+ ; class
   type-port ; visual
   type-int+ ; valuemask
   type-port ; attributes
   ))
(define XCreateSimpleWindow (dlsym % type-port "XCreateSimpleWindow"
   type-port type-port ; display, parent Window
   type-int+ type-int+ type-int+ type-int+ ; x y width height
   type-int+ ; border width
   type-int+ ; border
   type-int+ ; background
   ))
   
; http://tronche.com/gui/x/xlib/events/mask.html
(define ExposureMask (<< 1 15))
(define KeyPressMask (<< 1 0))
(define XSelectInput (dlsym % type-int+ "XSelectInput" type-port type-port type-int+))

(define XMapWindow (dlsym % type-int+ "XMapWindow" type-port type-port))
(define XNextEvent (dlsym % type-int+ "XNextEvent" type-port type-vector-raw))

;(define Colormap type-port)
;(define XCreateColormap (dlsym % Colormap "XCreateColormap" type-port type-port type-port type-fix+))


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
   10 10 660 200 1
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
         (glClearColor 0 0 1 1)
         (glClear #x00004000)
;         (glFlush)
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
