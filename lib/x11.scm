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


   ; GLX (WGL: Windows, CGL: Mac OS X, EGL)
   glXQueryVersion
   glXChooseVisual glXCreateContext glXMakeCurrent glXSwapBuffers
   glXChooseFBConfig glXGetVisualFromFBConfig; glXCreateContextAttribs

   vector->int32
  )

  (import
      (r5rs core) (owl io)
      (owl list) (owl string)
      (owl math)
      (otus pinvoke))
  (begin

(define % (dlopen "libX11.so" RTLD_LAZY))
(define XOpenDisplay (dlsym % type-void* "XOpenDisplay" type-string))
(define XDefaultScreen (dlsym % type-int+ "XDefaultScreen" type-void*))

(define XRootWindow (dlsym % type-void* "XRootWindow" type-void* type-int+))
(define XBlackPixel (dlsym % type-void* "XBlackPixel" type-void* type-int+))
(define XWhitePixel (dlsym % type-void* "XWhitePixel" type-void* type-int+))

(define XCreateWindow (dlsym % type-void* "XCreateWindow"
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
(define XCreateSimpleWindow (dlsym % type-void* "XCreateSimpleWindow"
   type-void* type-void* ; display, parent Window
   type-int+ type-int+ type-int+ type-int+ ; x y width height
   type-int+ ; border width
   type-int+ ; border
   type-int+ ; background
   ))

; http://tronche.com/gui/x/xlib/events/mask.html
(define ExposureMask (<< 1 15))
(define KeyPressMask (<< 1 0))
(define XSelectInput (dlsym % type-int+ "XSelectInput" type-void* type-void* type-int+))

(define XMapWindow (dlsym % type-int+ "XMapWindow" type-void* type-void*))
(define XNextEvent (dlsym % type-int+ "XNextEvent" type-void* type-vector-raw))
(define XPending   (dlsym % type-int+ "XPending"   type-void*))

(define XStoreName (dlsym % type-int+ "XStoreName" type-void* type-void* type-string))

;(define Colormap type-void*)
;(define XCreateColormap (dlsym % Colormap "XCreateColormap" type-void* type-void* type-void* type-fix+))

; http://stackoverflow.com/questions/1157364/intercept-wm-delete-window-on-x11
;(define XInternAtom (dlsym % type-void* "XInternAtom" type-void* type-string type-fix+))
;(define XSetWMProtocols (

(define type-int* (vm:or type-int+ #x40))
; -=( wgl )=------------------------------------------------------------
; opengl: https://gist.github.com/gszauer/da038dec2a7ffc288c41
(define GL (dlopen "libGL.so" RTLD_LAZY))
   (define glXQueryVersion  (dlsym GL type-fix+ "glXQueryVersion" type-void* type-vector-raw type-vector-raw))

   (define glXChooseVisual  (dlsym GL type-void* "glXChooseVisual" type-void* type-int+ type-vector-raw))
   (define glXCreateContext (dlsym GL type-void* "glXCreateContext" type-void* type-void* type-int+ type-int+))
   (define glXMakeCurrent   (dlsym GL type-int+ "glXMakeCurrent"  type-void* type-void* type-void*))
   (define glXSwapBuffers   (dlsym GL type-int+ "glXSwapBuffers"  type-void* type-void*))

   (define glXChooseFBConfig(dlsym GL type-void* "glXChooseFBConfig" type-void* type-int+ type-vector-raw type-vector-raw)) ; minimal 1.3
   (define glXGetVisualFromFBConfig (dlsym GL type-void* "glXGetVisualFromFBConfig" type-void* type-void*))



   (define (vector->int32 vector)
      (+ (<< (ref vector 0) 0)
         (<< (ref vector 1) 8)
         (<< (ref vector 2) 16)
         (<< (ref vector 3) 24)))
))
