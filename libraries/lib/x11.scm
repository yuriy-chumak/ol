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

   True False None

   GLX_RGBA
   GLX_DOUBLEBUFFER
   GLX_RED_SIZE GLX_GREEN_SIZE GLX_BLUE_SIZE GLX_DEPTH_SIZE

   GLX_CONFIG_CAVEAT
   GLX_DONT_CARE
   GLX_X_VISUAL_TYPE
   GLX_TRANSPARENT_TYPE
   GLX_TRANSPARENT_INDEX_VALUE
   GLX_TRANSPARENT_RED_VALUE
   GLX_TRANSPARENT_GREEN_VALUE
   GLX_TRANSPARENT_BLUE_VALUE
   GLX_TRANSPARENT_ALPHA_VALUE
   GLX_WINDOW_BIT
   GLX_PIXMAP_BIT
   GLX_PBUFFER_BIT
   GLX_AUX_BUFFERS_BIT
   GLX_FRONT_LEFT_BUFFER_BIT
   GLX_FRONT_RIGHT_BUFFER_BIT
   GLX_BACK_LEFT_BUFFER_BIT
   GLX_BACK_RIGHT_BUFFER_BIT
   GLX_DEPTH_BUFFER_BIT
   GLX_STENCIL_BUFFER_BIT
   GLX_ACCUM_BUFFER_BIT
   GLX_NONE
   GLX_SLOW_CONFIG
   GLX_TRUE_COLOR
   GLX_DIRECT_COLOR
   GLX_PSEUDO_COLOR
   GLX_STATIC_COLOR
   GLX_GRAY_SCALE
   GLX_STATIC_GRAY
   GLX_TRANSPARENT_RGB
   GLX_TRANSPARENT_INDEX
   GLX_VISUAL_ID
   GLX_SCREEN
   GLX_NON_CONFORMANT_CONFIG
   GLX_DRAWABLE_TYPE
   GLX_RENDER_TYPE
   GLX_X_RENDERABLE
   GLX_FBCONFIG_ID
   GLX_RGBA_TYPE
   GLX_COLOR_INDEX_TYPE
   GLX_MAX_PBUFFER_WIDTH
   GLX_MAX_PBUFFER_HEIGHT
   GLX_MAX_PBUFFER_PIXELS
   GLX_PRESERVED_CONTENTS
   GLX_LARGEST_PBUFFER
   GLX_WIDTH
   GLX_HEIGHT
   GLX_EVENT_MASK
   GLX_DAMAGED
   GLX_SAVED
   GLX_WINDOW
   GLX_PBUFFER
   GLX_PBUFFER_HEIGHT
   GLX_PBUFFER_WIDTH
   GLX_RGBA_BIT
   GLX_COLOR_INDEX_BIT
   GLX_PBUFFER_CLOBBER_MASK
)

  (import
      (scheme core) (owl io)
      (owl list) (owl string)
      (owl math)
      (otus ffi))
(begin

; x11 types
(define Display* fft-void*)
(define Window fft-void*)
(define Visual* fft-void*)
(define XSetWindowAttributes* fft-void*)
(define XEvent* fft-void*)

(define X11 (or (load-dynamic-library "libX11.so")
                (load-dynamic-library "libX11.so.6")))

; functions
(define XOpenDisplay  (X11 Display* "XOpenDisplay" type-string))
(define XDefaultScreen(X11 fft-int "XDefaultScreen" Display*))
(define XRootWindow   (X11 Window "XRootWindow" Display* fft-int))
(define XFree         (X11 fft-int "XFree" fft-void*))

(define XBlackPixel (X11 fft-unsigned-long "XBlackPixel" Display* fft-int))
(define XWhitePixel (X11 fft-unsigned-long "XWhitePixel" Display* fft-int))

(define XCreateWindow (X11 Window "XCreateWindow"
   Display* ; display
   Window ; parent Window
   fft-int fft-int fft-unsigned-int fft-unsigned-int ; x y width height
   fft-unsigned-int ; border width
   fft-int ; depth
   fft-unsigned-int ; class
   Visual* ; visual
   fft-unsigned-long ; valuemask
   XSetWindowAttributes* ; attributes
   ))
(define XCreateSimpleWindow (X11 fft-void* "XCreateSimpleWindow"
   Display* Window ; display, parent Window
   fft-int fft-int fft-unsigned-int fft-unsigned-int ; x y width height
   fft-unsigned-int ; border width
   fft-unsigned-long ; border
   fft-unsigned-long ; background
   ))

; http://tronche.com/gui/x/xlib/events/mask.html
(define ExposureMask (<< 1 15))
(define KeyPressMask (<< 1 0))
(define XSelectInput (X11 fft-int "XSelectInput" Display* Window fft-long))

(define XMapWindow (X11 fft-int "XMapWindow" Display* Window))
(define XPending   (X11 fft-int "XPending"   Display*))

(define XStoreName (X11 fft-int "XStoreName" Display* Window type-string))

; events
; event types: (X.h)
   (setq KeyPress 2)
   (setq KeyRelease 3)
;#define ButtonPress<---><---->4
;#define ButtonRelease<-><---->5
;#define MotionNotify<--><---->6
;#define EnterNotify<---><---->7
;#define LeaveNotify<---><---->8
;#define FocusIn<-><----><---->9
;#define FocusOut<><---->10
;#define KeymapNotify<--><---->11
;#define Expose<--><----><---->12
;#define GraphicsExpose<><---->13
;#define NoExpose<><---->14
;#define VisibilityNotify<---->15
;#define CreateNotify<--><---->16
;#define DestroyNotify<-><---->17
;#define UnmapNotify<---><---->18
;#define MapNotify><---->19
;#define MapRequest<----><---->20
;#define ReparentNotify<><---->21
;#define ConfigureNotify><---->22
;#define ConfigureRequest<---->23
;#define GravityNotify<-><---->24
;#define ResizeRequest<-><---->25
;#define CirculateNotify><---->26
;#define CirculateRequest<---->27
;#define PropertyNotify<><---->28
;#define SelectionClear<><---->29
;#define SelectionRequest<---->30
;#define SelectionNotify><---->31
;#define ColormapNotify<><---->32
;#define ClientMessage<-><---->33
;#define MappingNotify<-><---->34
;#define GenericEvent<--><---->35
;#define LASTEvent><---->36<-->/* must be bigger than any event # */

(define XNextEvent (X11 fft-int "XNextEvent" Display* XEvent*))



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
