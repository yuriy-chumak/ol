; http://standards.freedesktop.org/wm-spec/wm-spec-1.5.html
; http://www-h.eng.cam.ac.uk/help/tpl/graphics/X/X11R5/node25.html
(define-library (lib x11)
  (export
    XOpenDisplay
    XDefaultScreen
    XRootWindow
    XBlackPixel
    XWhitePixel
    XCreateWindow
    XCreateSimpleWindow
    ExposureMask
    KeyPressMask
    XSelectInput
    XMapWindow
    XNextEvent XPending
    XStoreName
    XFree


   ; GLX (WGL: Windows, CGL: Mac OS X, EGL)
   glXQueryVersion
   glXChooseVisual glXCreateContext glXMakeCurrent glXSwapBuffers
   glXChooseFBConfig glXGetVisualFromFBConfig; glXCreateContextAttribs

   GLX_RGBA
   GLX_DOUBLEBUFFER
   GLX_RED_SIZE GLX_GREEN_SIZE GLX_BLUE_SIZE GLX_DEPTH_SIZE

   vector->int32
  )

  (import
      (r5rs core) (owl io)
      (owl list) (owl string)
      (owl math)
      (otus ffi))
  (begin

(define X11 (dlopen "libX11.so"))
(define XOpenDisplay  (dlsym X11 type-void* "XOpenDisplay" type-string))
(define XDefaultScreen (dlsym X11 type-int+ "XDefaultScreen" type-void*))
(define XRootWindow   (dlsym X11 type-void* "XRootWindow" type-void* type-int+))
(define XFree (dlsym X11 type-fix+ "XFree" type-void*))

(define XBlackPixel (dlsym X11 type-int+ "XBlackPixel" type-void* type-int+))
(define XWhitePixel (dlsym X11 type-int+ "XWhitePixel" type-void* type-int+))

(define XCreateWindow (dlsym X11 type-void* "XCreateWindow"
   type-void* ; display
   type-void* ; parent Window
   type-int+ type-int+ type-int+ type-int+ ; x y width height
   type-int+ ; border width
   type-int+ ; depth
   type-int+ ; class
   type-void* ; visual
   type-int+ ; valuemask
   type-void* ; attributes
   ))
(define XCreateSimpleWindow (dlsym X11 type-void* "XCreateSimpleWindow"
   type-void* type-void* ; display, parent Window
   type-int+ type-int+ type-int+ type-int+ ; x y width height
   type-int+ ; border width
   type-int+ ; border
   type-int+ ; background
   ))

; http://tronche.com/gui/x/xlib/events/mask.html
(define ExposureMask (<< 1 15))
(define KeyPressMask (<< 1 0))
(define XSelectInput (dlsym X11 type-int+ "XSelectInput" type-void* type-void* type-int+))

(define XMapWindow (dlsym X11 type-int+ "XMapWindow" type-void* type-void*))
(define XNextEvent (dlsym X11 type-int+ "XNextEvent" type-void* type-vector-raw))
(define XPending   (dlsym X11 type-int+ "XPending"   type-void*))

(define XStoreName (dlsym X11 type-int+ "XStoreName" type-void* type-void* type-string))

;(define Colormap type-void*)
;(define XCreateColormap (dlsym X11 Colormap "XCreateColormap" type-void* type-void* type-void* type-fix+))

; http://stackoverflow.com/questions/1157364/intercept-wm-delete-window-on-x11
;(define XInternAtom (dlsym X11 type-void* "XInternAtom" type-void* type-string type-fix+))
;(define XSetWMProtocols (

(define int  type-int+)
(define int* (vm:or type-int+ #x40))
(define bool type-fix+)
; -=( wgl )=------------------------------------------------------------
; opengl: https://gist.github.com/gszauer/da038dec2a7ffc288c41
(define GLX (dlopen "libGLX.so"))
   (define glXQueryVersion  (dlsym GLX type-fix+ "glXQueryVersion" type-void* type-vector-raw type-vector-raw))

   (define glXChooseVisual  (dlsym GLX type-void* "glXChooseVisual" type-void* int int*))
      (define GLX_RGBA         4)
      (define GLX_DOUBLEBUFFER 5)
      (define GLX_RED_SIZE     8)
      (define GLX_GREEN_SIZE   9)
      (define GLX_BLUE_SIZE   10)
      (define GLX_DEPTH_SIZE  12)
   (define glXCreateContext (dlsym GLX type-void* "glXCreateContext" type-void* type-void* type-void* bool))
   (define glXMakeCurrent   (dlsym GLX bool "glXMakeCurrent"  type-void* type-void* type-void*))
   (define glXSwapBuffers   (dlsym GLX type-int+ "glXSwapBuffers"  type-void* type-void*))

   (define glXChooseFBConfig(dlsym GLX type-void* "glXChooseFBConfig" type-void* type-int+ type-vector-raw type-vector-raw)) ; minimal 1.3
   (define glXGetVisualFromFBConfig (dlsym GLX type-void* "glXGetVisualFromFBConfig" type-void* type-void*))



   (define (vector->int32 vector)
      (+ (<< (ref vector 0) 0)
         (<< (ref vector 1) 8)
         (<< (ref vector 2) 16)
         (<< (ref vector 3) 24)))
))
