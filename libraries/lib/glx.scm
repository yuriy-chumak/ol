; based on
; Mesa 3-D graphics library
(define-library (lib glx)
(export
   GLX_VERSION_1_1
   GLX_VERSION_1_2
   GLX_VERSION_1_3
   GLX_VERSION_1_4

   GLX_EXTENSION_NAME

   ; Tokens for glXChooseVisual and glXGetConfig:
   GLX_USE_GL
   GLX_BUFFER_SIZE
   GLX_LEVEL
   GLX_RGBA
   GLX_DOUBLEBUFFER
   GLX_STEREO
   GLX_AUX_BUFFERS
   GLX_RED_SIZE
   GLX_GREEN_SIZE
   GLX_BLUE_SIZE
   GLX_ALPHA_SIZE
   GLX_DEPTH_SIZE
   GLX_STENCIL_SIZE
   GLX_ACCUM_RED_SIZE
   GLX_ACCUM_GREEN_SIZE
   GLX_ACCUM_BLUE_SIZE
   GLX_ACCUM_ALPHA_SIZE

   ; Error codes returned by glXGetConfig:
   GLX_BAD_SCREEN
   GLX_BAD_ATTRIBUTE
   GLX_NO_EXTENSION
   GLX_BAD_VISUAL
   GLX_BAD_CONTEXT
   GLX_BAD_VALUE
   GLX_BAD_ENUM

;; /*
;;  * GLX 1.1 and later:
;;  */
;; #define GLX_VENDOR		1
;; #define GLX_VERSION		2
;; #define GLX_EXTENSIONS 		3

;; /*
;;  * GLX 1.3 and later:
;;  */
;; #define GLX_CONFIG_CAVEAT		0x20
;; #define GLX_DONT_CARE			0xFFFFFFFF
;; #define GLX_X_VISUAL_TYPE		0x22
;; #define GLX_TRANSPARENT_TYPE		0x23
;; #define GLX_TRANSPARENT_INDEX_VALUE	0x24
;; #define GLX_TRANSPARENT_RED_VALUE	0x25
;; #define GLX_TRANSPARENT_GREEN_VALUE	0x26
;; #define GLX_TRANSPARENT_BLUE_VALUE	0x27
;; #define GLX_TRANSPARENT_ALPHA_VALUE	0x28
;; #define GLX_WINDOW_BIT			0x00000001
;; #define GLX_PIXMAP_BIT			0x00000002
;; #define GLX_PBUFFER_BIT			0x00000004
;; #define GLX_AUX_BUFFERS_BIT		0x00000010
;; #define GLX_FRONT_LEFT_BUFFER_BIT	0x00000001
;; #define GLX_FRONT_RIGHT_BUFFER_BIT	0x00000002
;; #define GLX_BACK_LEFT_BUFFER_BIT	0x00000004
;; #define GLX_BACK_RIGHT_BUFFER_BIT	0x00000008
;; #define GLX_DEPTH_BUFFER_BIT		0x00000020
;; #define GLX_STENCIL_BUFFER_BIT		0x00000040
;; #define GLX_ACCUM_BUFFER_BIT		0x00000080
;; #define GLX_NONE			0x8000
;; #define GLX_SLOW_CONFIG			0x8001
;; #define GLX_TRUE_COLOR			0x8002
;; #define GLX_DIRECT_COLOR		0x8003
;; #define GLX_PSEUDO_COLOR		0x8004
;; #define GLX_STATIC_COLOR		0x8005
;; #define GLX_GRAY_SCALE			0x8006
;; #define GLX_STATIC_GRAY			0x8007
;; #define GLX_TRANSPARENT_RGB		0x8008
;; #define GLX_TRANSPARENT_INDEX		0x8009
;; #define GLX_VISUAL_ID			0x800B
;; #define GLX_SCREEN			0x800C
;; #define GLX_NON_CONFORMANT_CONFIG	0x800D
;; #define GLX_DRAWABLE_TYPE		0x8010
;; #define GLX_RENDER_TYPE			0x8011
;; #define GLX_X_RENDERABLE		0x8012
;; #define GLX_FBCONFIG_ID			0x8013
;; #define GLX_RGBA_TYPE			0x8014
;; #define GLX_COLOR_INDEX_TYPE		0x8015
;; #define GLX_MAX_PBUFFER_WIDTH		0x8016
;; #define GLX_MAX_PBUFFER_HEIGHT		0x8017
;; #define GLX_MAX_PBUFFER_PIXELS		0x8018
;; #define GLX_PRESERVED_CONTENTS		0x801B
;; #define GLX_LARGEST_PBUFFER		0x801C
;; #define GLX_WIDTH			0x801D
;; #define GLX_HEIGHT			0x801E
;; #define GLX_EVENT_MASK			0x801F
;; #define GLX_DAMAGED			0x8020
;; #define GLX_SAVED			0x8021
;; #define GLX_WINDOW			0x8022
;; #define GLX_PBUFFER			0x8023
;; #define GLX_PBUFFER_HEIGHT              0x8040
;; #define GLX_PBUFFER_WIDTH               0x8041
;; #define GLX_RGBA_BIT			0x00000001
;; #define GLX_COLOR_INDEX_BIT		0x00000002
;; #define GLX_PBUFFER_CLOBBER_MASK	0x08000000

;; /*
;;  * GLX 1.4 and later:
;;  */
;; #define GLX_SAMPLE_BUFFERS              0x186a0 /*100000*/
;; #define GLX_SAMPLES                     0x186a1 /*100001*/


;; typedef struct __GLXcontextRec *GLXContext;
;; typedef XID GLXPixmap;
;; typedef XID GLXDrawable;
;; /* GLX 1.3 and later */
;; typedef struct __GLXFBConfigRec *GLXFBConfig;
;; typedef XID GLXFBConfigID;
;; typedef XID GLXContextID;
;; typedef XID GLXWindow;
;; typedef XID GLXPbuffer;


   ; GLX (WGL: Windows, CGL: Mac OS X, EGL)
   glXQueryVersion
   ;; glXChooseVisual glXCreateContext glXMakeCurrent glXSwapBuffers
   ;; glXChooseFBConfig glXGetVisualFromFBConfig; glXCreateContextAttribs

   ;; True False None

   ;; GLX_RGBA
   ;; GLX_DOUBLEBUFFER
   ;; GLX_RED_SIZE GLX_GREEN_SIZE GLX_BLUE_SIZE GLX_DEPTH_SIZE

   ;; GLX_CONFIG_CAVEAT
   ;; GLX_DONT_CARE
   ;; GLX_X_VISUAL_TYPE
   ;; GLX_TRANSPARENT_TYPE
   ;; GLX_TRANSPARENT_INDEX_VALUE
   ;; GLX_TRANSPARENT_RED_VALUE
   ;; GLX_TRANSPARENT_GREEN_VALUE
   ;; GLX_TRANSPARENT_BLUE_VALUE
   ;; GLX_TRANSPARENT_ALPHA_VALUE
   ;; GLX_WINDOW_BIT
   ;; GLX_PIXMAP_BIT
   ;; GLX_PBUFFER_BIT
   ;; GLX_AUX_BUFFERS_BIT
   ;; GLX_FRONT_LEFT_BUFFER_BIT
   ;; GLX_FRONT_RIGHT_BUFFER_BIT
   ;; GLX_BACK_LEFT_BUFFER_BIT
   ;; GLX_BACK_RIGHT_BUFFER_BIT
   ;; GLX_DEPTH_BUFFER_BIT
   ;; GLX_STENCIL_BUFFER_BIT
   ;; GLX_ACCUM_BUFFER_BIT
   ;; GLX_NONE
   ;; GLX_SLOW_CONFIG
   ;; GLX_TRUE_COLOR
   ;; GLX_DIRECT_COLOR
   ;; GLX_PSEUDO_COLOR
   ;; GLX_STATIC_COLOR
   ;; GLX_GRAY_SCALE
   ;; GLX_STATIC_GRAY
   ;; GLX_TRANSPARENT_RGB
   ;; GLX_TRANSPARENT_INDEX
   ;; GLX_VISUAL_ID
   ;; GLX_SCREEN
   ;; GLX_NON_CONFORMANT_CONFIG
   ;; GLX_DRAWABLE_TYPE
   ;; GLX_RENDER_TYPE
   ;; GLX_X_RENDERABLE
   ;; GLX_FBCONFIG_ID
   ;; GLX_RGBA_TYPE
   ;; GLX_COLOR_INDEX_TYPE
   ;; GLX_MAX_PBUFFER_WIDTH
   ;; GLX_MAX_PBUFFER_HEIGHT
   ;; GLX_MAX_PBUFFER_PIXELS
   ;; GLX_PRESERVED_CONTENTS
   ;; GLX_LARGEST_PBUFFER
   ;; GLX_WIDTH
   ;; GLX_HEIGHT
   ;; GLX_EVENT_MASK
   ;; GLX_DAMAGED
   ;; GLX_SAVED
   ;; GLX_WINDOW
   ;; GLX_PBUFFER
   ;; GLX_PBUFFER_HEIGHT
   ;; GLX_PBUFFER_WIDTH
   ;; GLX_RGBA_BIT
   ;; GLX_COLOR_INDEX_BIT
   ;; GLX_PBUFFER_CLOBBER_MASK
)

(import
   (otus lisp)
   (otus ffi))
(begin
   (setq Display* fft-void*)
   (setq X11 (or
      (load-dynamic-library "libX11.so")
      (load-dynamic-library "libX11.so.6")
      (lambda args #false)))
   (setq XOpenDisplay (if X11 (X11 Display* "XOpenDisplay" type-string)))

   (setq GLX (or
      (load-dynamic-library "libGL.so.1")
      (lambda args #false)))
   (setq glXQueryVersion (GLX fft-int "glXQueryVersion" Display* fft-int& fft-int&))

   (setq major (box 0))
   (setq minor (box 0))
   (if glXQueryVersion (glXQueryVersion (XOpenDisplay #false) major minor))
   (setq major (unbox major))
   (setq minor (unbox minor))


   (define GLX_VERSION_1_1 (>= (+ minor (* major 10)) 11))
   (define GLX_VERSION_1_2 (>= (+ minor (* major 10)) 12))
   (define GLX_VERSION_1_3 (>= (+ minor (* major 10)) 13))
   (define GLX_VERSION_1_4 (>= (+ minor (* major 10)) 14))

   (define GLX_EXTENSION_NAME "GLX")

   ; Tokens for glXChooseVisual and glXGetConfig:
   (define GLX_USE_GL             1)
   (define GLX_BUFFER_SIZE        2)
   (define GLX_LEVEL              3)
   (define GLX_RGBA               4)
   (define GLX_DOUBLEBUFFER       5)
   (define GLX_STEREO             6)
   (define GLX_AUX_BUFFERS        7)
   (define GLX_RED_SIZE           8)
   (define GLX_GREEN_SIZE         9)
   (define GLX_BLUE_SIZE         10)
   (define GLX_ALPHA_SIZE        11)
   (define GLX_DEPTH_SIZE        12)
   (define GLX_STENCIL_SIZE      13)
   (define GLX_ACCUM_RED_SIZE    14)
   (define GLX_ACCUM_GREEN_SIZE  15)
   (define GLX_ACCUM_BLUE_SIZE   16)
   (define GLX_ACCUM_ALPHA_SIZE  17)

   ; Error codes returned by glXGetConfig:
   (define GLX_BAD_SCREEN		1)
   (define GLX_BAD_ATTRIBUTE	2)
   (define GLX_NO_EXTENSION	3)
   (define GLX_BAD_VISUAL		4)
   (define GLX_BAD_CONTEXT		5)
   (define GLX_BAD_VALUE      6)
   (define GLX_BAD_ENUM		   7)

;; (define Window fft-void*)
;; (define Visual* fft-void*)
;; (define XSetWindowAttributes* fft-void*)
;; (define XEvent* fft-void*)

;; (define X11 (load-dynamic-library "libX11.so"))

;; ; functions
;; (define XOpenDisplay  (X11 Display* "XOpenDisplay" type-string))
;; (define XDefaultScreen(X11 fft-int "XDefaultScreen" Display*))
;; (define XRootWindow   (X11 Window "XRootWindow" Display* fft-int))
;; (define XFree         (X11 fft-int "XFree" fft-void*))

;; (define XBlackPixel (X11 fft-unsigned-long "XBlackPixel" Display* fft-int))
;; (define XWhitePixel (X11 fft-unsigned-long "XWhitePixel" Display* fft-int))

;; (define XCreateWindow (X11 Window "XCreateWindow"
;;    Display* ; display
;;    Window ; parent Window
;;    fft-int fft-int fft-unsigned-int fft-unsigned-int ; x y width height
;;    fft-unsigned-int ; border width
;;    fft-int ; depth
;;    fft-unsigned-int ; class
;;    Visual* ; visual
;;    fft-unsigned-long ; valuemask
;;    XSetWindowAttributes* ; attributes
;;    ))
;; (define XCreateSimpleWindow (X11 fft-void* "XCreateSimpleWindow"
;;    Display* Window ; display, parent Window
;;    fft-int fft-int fft-unsigned-int fft-unsigned-int ; x y width height
;;    fft-unsigned-int ; border width
;;    fft-unsigned-long ; border
;;    fft-unsigned-long ; background
;;    ))

;; ; http://tronche.com/gui/x/xlib/events/mask.html
;; (define ExposureMask (<< 1 15))
;; (define KeyPressMask (<< 1 0))
;; (define XSelectInput (X11 fft-int "XSelectInput" Display* Window fft-long))

;; (define XMapWindow (X11 fft-int "XMapWindow" Display* Window))
;; (define XPending   (X11 fft-int "XPending"   Display*))

;; (define XStoreName (X11 fft-int "XStoreName" Display* Window type-string))

;; ; events
;; ; event types: (X.h)
;;    (setq KeyPress 2)
;;    (setq KeyRelease 3)
;; ;#define ButtonPress<---><---->4
;; ;#define ButtonRelease<-><---->5
;; ;#define MotionNotify<--><---->6
;; ;#define EnterNotify<---><---->7
;; ;#define LeaveNotify<---><---->8
;; ;#define FocusIn<-><----><---->9
;; ;#define FocusOut<><---->10
;; ;#define KeymapNotify<--><---->11
;; ;#define Expose<--><----><---->12
;; ;#define GraphicsExpose<><---->13
;; ;#define NoExpose<><---->14
;; ;#define VisibilityNotify<---->15
;; ;#define CreateNotify<--><---->16
;; ;#define DestroyNotify<-><---->17
;; ;#define UnmapNotify<---><---->18
;; ;#define MapNotify><---->19
;; ;#define MapRequest<----><---->20
;; ;#define ReparentNotify<><---->21
;; ;#define ConfigureNotify><---->22
;; ;#define ConfigureRequest<---->23
;; ;#define GravityNotify<-><---->24
;; ;#define ResizeRequest<-><---->25
;; ;#define CirculateNotify><---->26
;; ;#define CirculateRequest<---->27
;; ;#define PropertyNotify<><---->28
;; ;#define SelectionClear<><---->29
;; ;#define SelectionRequest<---->30
;; ;#define SelectionNotify><---->31
;; ;#define ColormapNotify<><---->32
;; ;#define ClientMessage<-><---->33
;; ;#define MappingNotify<-><---->34
;; ;#define GenericEvent<--><---->35
;; ;#define LASTEvent><---->36<-->/* must be bigger than any event # */

;; (define XNextEvent (X11 fft-int "XNextEvent" Display* XEvent*))



;; ;(define Colormap fft-void*)
;; ;(define XCreateColormap (X11 Colormap "XCreateColormap" fft-void* fft-void* fft-void* fft-int))

;; ; http://stackoverflow.com/questions/1157364/intercept-wm-delete-window-on-x11
;; ;(define XInternAtom (X11 fft-void* "XInternAtom" fft-void* type-string fft-int))
;; ;(define XSetWMProtocols (

;; (define int  fft-int)
;; (define int* (fft* fft-int))
;; (define bool fft-int)
;; ; -=( wgl )=------------------------------------------------------------
;; ; opengl: https://gist.github.com/gszauer/da038dec2a7ffc288c41

;;    (define glXChooseVisual  (GLX fft-void* "glXChooseVisual" fft-void* int int*))
;;       (define GLX_RGBA         4)
;;       (define GLX_DOUBLEBUFFER 5)
;;       (define GLX_RED_SIZE     8)
;;       (define GLX_GREEN_SIZE   9)
;;       (define GLX_BLUE_SIZE   10)
;;       (define GLX_DEPTH_SIZE  12)
;;    (define glXCreateContext (GLX fft-void* "glXCreateContext" fft-void* fft-void* fft-void* bool))
;;    (define glXMakeCurrent   (GLX bool "glXMakeCurrent"  fft-void* fft-void* fft-void*))
;;    (define glXSwapBuffers   (GLX fft-int "glXSwapBuffers"  fft-void* fft-void*))

;;    (define glXChooseFBConfig(GLX fft-void* "glXChooseFBConfig" fft-void* fft-int type-vptr type-vptr)) ; minimal 1.3
;;    (define glXGetVisualFromFBConfig (GLX fft-void* "glXGetVisualFromFBConfig" fft-void* fft-void*))
))
