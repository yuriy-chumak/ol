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
)

  (import
      (r5rs core) (owl io)
      (owl list) (owl string)
      (owl math)
      (otus ffi))
(begin

(define X11 (load-dynamic-library "libX11.so"))
(define XOpenDisplay  (X11 fft-void* "XOpenDisplay" type-string))
(define XDefaultScreen (X11 fft-int "XDefaultScreen" fft-void*))
(define XRootWindow   (X11 fft-void* "XRootWindow" fft-void* fft-int))
(define XFree (X11 fft-int "XFree" fft-void*))

(define XBlackPixel (X11 fft-int "XBlackPixel" fft-void* fft-int))
(define XWhitePixel (X11 fft-int "XWhitePixel" fft-void* fft-int))

(define XCreateWindow (X11 fft-void* "XCreateWindow"
   fft-void* ; display
   fft-void* ; parent Window
   fft-int fft-int fft-int fft-int ; x y width height
   fft-int ; border width
   fft-int ; depth
   fft-int ; class
   fft-void* ; visual
   fft-int ; valuemask
   fft-void* ; attributes
   ))
(define XCreateSimpleWindow (X11 fft-void* "XCreateSimpleWindow"
   fft-void* fft-void* ; display, parent Window
   fft-int fft-int fft-int fft-int ; x y width height
   fft-int ; border width
   fft-int ; border
   fft-int ; background
   ))

; http://tronche.com/gui/x/xlib/events/mask.html
(define ExposureMask (<< 1 15))
(define KeyPressMask (<< 1 0))
(define XSelectInput (X11 fft-int "XSelectInput" fft-void* fft-void* fft-int))

(define XMapWindow (X11 fft-int "XMapWindow" fft-void* fft-void*))
(define XNextEvent (X11 fft-int "XNextEvent" fft-void* type-vptr))
(define XPending   (X11 fft-int "XPending"   fft-void*))

(define XStoreName (X11 fft-int "XStoreName" fft-void* fft-void* type-string))

;(define Colormap fft-void*)
;(define XCreateColormap (X11 Colormap "XCreateColormap" fft-void* fft-void* fft-void* fft-int))

; http://stackoverflow.com/questions/1157364/intercept-wm-delete-window-on-x11
;(define XInternAtom (X11 fft-void* "XInternAtom" fft-void* type-string fft-int))
;(define XSetWMProtocols (

(define int  fft-int)
(define int* (fft* fft-int))
(define bool fft-int)
; -=( wgl )=------------------------------------------------------------
; opengl: https://gist.github.com/gszauer/da038dec2a7ffc288c41
(define GLX (load-dynamic-library "libGL.so.1"))
   (define glXQueryVersion  (GLX fft-int "glXQueryVersion" fft-void* type-vptr type-vptr))

   (define glXChooseVisual  (GLX fft-void* "glXChooseVisual" fft-void* int int*))
      (define GLX_RGBA         4)
      (define GLX_DOUBLEBUFFER 5)
      (define GLX_RED_SIZE     8)
      (define GLX_GREEN_SIZE   9)
      (define GLX_BLUE_SIZE   10)
      (define GLX_DEPTH_SIZE  12)
   (define glXCreateContext (GLX fft-void* "glXCreateContext" fft-void* fft-void* fft-void* bool))
   (define glXMakeCurrent   (GLX bool "glXMakeCurrent"  fft-void* fft-void* fft-void*))
   (define glXSwapBuffers   (GLX fft-int "glXSwapBuffers"  fft-void* fft-void*))

   (define glXChooseFBConfig(GLX fft-void* "glXChooseFBConfig" fft-void* fft-int type-vptr type-vptr)) ; minimal 1.3
   (define glXGetVisualFromFBConfig (GLX fft-void* "glXGetVisualFromFBConfig" fft-void* fft-void*))
))
