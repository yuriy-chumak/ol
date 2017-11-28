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
(define XOpenDisplay  (dlsym X11 fft-void* "XOpenDisplay" type-string))
(define XDefaultScreen (dlsym X11 type-int+ "XDefaultScreen" fft-void*))
(define XRootWindow   (dlsym X11 fft-void* "XRootWindow" fft-void* type-int+))
(define XFree (dlsym X11 type-fix+ "XFree" fft-void*))

(define XBlackPixel (dlsym X11 type-int+ "XBlackPixel" fft-void* type-int+))
(define XWhitePixel (dlsym X11 type-int+ "XWhitePixel" fft-void* type-int+))

(define XCreateWindow (dlsym X11 fft-void* "XCreateWindow"
   fft-void* ; display
   fft-void* ; parent Window
   type-int+ type-int+ type-int+ type-int+ ; x y width height
   type-int+ ; border width
   type-int+ ; depth
   type-int+ ; class
   fft-void* ; visual
   type-int+ ; valuemask
   fft-void* ; attributes
   ))
(define XCreateSimpleWindow (dlsym X11 fft-void* "XCreateSimpleWindow"
   fft-void* fft-void* ; display, parent Window
   type-int+ type-int+ type-int+ type-int+ ; x y width height
   type-int+ ; border width
   type-int+ ; border
   type-int+ ; background
   ))

; http://tronche.com/gui/x/xlib/events/mask.html
(define ExposureMask (<< 1 15))
(define KeyPressMask (<< 1 0))
(define XSelectInput (dlsym X11 type-int+ "XSelectInput" fft-void* fft-void* type-int+))

(define XMapWindow (dlsym X11 type-int+ "XMapWindow" fft-void* fft-void*))
(define XNextEvent (dlsym X11 type-int+ "XNextEvent" fft-void* type-vector-raw))
(define XPending   (dlsym X11 type-int+ "XPending"   fft-void*))

(define XStoreName (dlsym X11 type-int+ "XStoreName" fft-void* fft-void* type-string))

;(define Colormap fft-void*)
;(define XCreateColormap (dlsym X11 Colormap "XCreateColormap" fft-void* fft-void* fft-void* type-fix+))

; http://stackoverflow.com/questions/1157364/intercept-wm-delete-window-on-x11
;(define XInternAtom (dlsym X11 fft-void* "XInternAtom" fft-void* type-string type-fix+))
;(define XSetWMProtocols (

(define int  type-int+)
(define int* (vm:or type-int+ #x40))
(define bool type-fix+)
; -=( wgl )=------------------------------------------------------------
; opengl: https://gist.github.com/gszauer/da038dec2a7ffc288c41
(define GLX (dlopen "libGLX.so"))
   (define glXQueryVersion  (dlsym GLX type-fix+ "glXQueryVersion" fft-void* type-vector-raw type-vector-raw))

   (define glXChooseVisual  (dlsym GLX fft-void* "glXChooseVisual" fft-void* int int*))
      (define GLX_RGBA         4)
      (define GLX_DOUBLEBUFFER 5)
      (define GLX_RED_SIZE     8)
      (define GLX_GREEN_SIZE   9)
      (define GLX_BLUE_SIZE   10)
      (define GLX_DEPTH_SIZE  12)
   (define glXCreateContext (dlsym GLX fft-void* "glXCreateContext" fft-void* fft-void* fft-void* bool))
   (define glXMakeCurrent   (dlsym GLX bool "glXMakeCurrent"  fft-void* fft-void* fft-void*))
   (define glXSwapBuffers   (dlsym GLX type-int+ "glXSwapBuffers"  fft-void* fft-void*))

   (define glXChooseFBConfig(dlsym GLX fft-void* "glXChooseFBConfig" fft-void* type-int+ type-vector-raw type-vector-raw)) ; minimal 1.3
   (define glXGetVisualFromFBConfig (dlsym GLX fft-void* "glXGetVisualFromFBConfig" fft-void* fft-void*))



   (define (vector->int32 vector)
      (+ (<< (ref vector 0) 0)
         (<< (ref vector 1) 8)
         (<< (ref vector 2) 16)
         (<< (ref vector 3) 24)))
))
