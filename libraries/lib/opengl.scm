(define-library (lib opengl)
   (import
      (otus lisp) (otus ffi)
      (OpenGL version-1-2))

   (export
      (exports (OpenGL version-1-2))
      gl:set-renderer
      gl:set-window-title
      gl:set-userdata
      gl:finish ; if renderer exists - wait for window close, else just glFinish

      gl:Create ; create window + context
      gl:Enable gl:Disable

      gl:SwapBuffers
      gl:ProcessEvents)

(begin
(define OS (ref (uname) 1))

(define win32? (string-ci=? OS "Windows"))
(define linux? (string-ci=? OS "Linux"))
(define x32? (eq? (vm:wordsize) 4))

; check the platform
(or win32? linux?
   (runtime-error "Unsupported platform" OS))

; -- some debug staff -----
(define (vector->vptr vec offset)
   (let ((vptr (vm:cast 0 type-vptr)))
      (for-each (lambda (i)
            (set-ref! vptr i (ref vec (+ i offset))))
         (iota (vm:wordsize)))
      vptr))
(define (vector-set-int! vec offset int)
   (for-each (lambda (i)
         (set-ref! vec (+ offset i) (band #xFF (>> int (* i 8)))))
      (iota (vm:wordsize)))
   vec)
(define (vector-set-vptr! vec offset vptr)
   (for-each (lambda (i)
         (set-ref! vec (+ offset i) (ref vptr i)))
      (iota (vm:wordsize)))
   vec)

; --

(define WIDTH 640)
(define HEIGHT 480)

; ===================================================
(define gl:Enable (cond
   (win32?  (lambda (context)
               (let ((dc   (ref context 1))
                     (glrc (ref context 2)))
                  (gl:MakeCurrent dc glrc))))
   (linux?  (lambda (context)
               (let ((display (ref context 1))
                    ;(screen  (ref context 2))
                     (window  (ref context 3))
                     (cx      (ref context 4)))
                  (gl:MakeCurrent display window cx))))))

(define gl:Disable (cond
   (win32?  (lambda (context)
                  (gl:MakeCurrent #f #f)))
   (linux?  (lambda (context)
               (let ((display (ref context 1)))
                  (gl:MakeCurrent display #f #f))))))

(define gl:Create (cond
   ; -=( win32 )=---------------------------------------------------------------------
   (win32?
      (let ((user32 (load-dynamic-library "user32.dll"))
            (gdi32  (load-dynamic-library "gdi32")))
      (let ((CreateWindowEx   (user32 fft-void* "CreateWindowExA" fft-int type-string type-string fft-int fft-int fft-int fft-int fft-int fft-void* fft-void* fft-void* fft-void*))
            (GetDC            (user32 fft-void* "GetDC" fft-void*))
            (ShowWindow       (user32 fft-int "ShowWindow" fft-void* fft-int))
            (ChoosePixelFormat(gdi32  fft-int "ChoosePixelFormat" fft-void* fft-void*))
            (SetPixelFormat   (gdi32  fft-int "SetPixelFormat" fft-void* fft-int fft-void*)))
      (lambda (title)
         (let*((window (CreateWindowEx
                  #x00040100 (c-string "#32770") (c-string title) ; WS_EX_APPWINDOW|WS_EX_WINDOWEDGE, #32770 is system classname for DIALOG
                  #x06cf0000 ; WS_OVERLAPPEDWINDOW | WS_CLIPSIBLINGS | WS_CLIPCHILDREN
                  0 0 WIDTH HEIGHT ; x y width height
                  #false ; no parent window
                  #false ; no menu
                  #false ; instance
                  #false))
               (pfd (vm:new-raw-object type-vector-raw '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                                          00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))
               (hDC (GetDC window))
               (PixelFormat (ChoosePixelFormat hDC pfd)))
            (SetPixelFormat hDC PixelFormat pfd)
         (let ((hRC (gl:CreateContext hDC)))
            (gl:MakeCurrent hDC hRC)
            (print "OpenGL version: " (glGetString GL_VERSION))
            (print "OpenGL vendor: " (glGetString GL_VENDOR))
            (print "OpenGL renderer: " (glGetString GL_RENDERER))
           ;(gl:MakeCurrent #f #f)
            (mail 'opengl (tuple 'set-context (tuple 'hDC hRC window)))
            (interact 'opengl (tuple 'get-context)) ; синхронизация

            (ShowWindow window 5)
            (tuple hDC hRC window)))))))
   ; -=( linux )=---------------------------------------------------------------------
   (linux?
      (let ((libX11 (load-dynamic-library "libX11.so"))
            (libGLX (load-dynamic-library "libGL.so.1")))
      (let ((XOpenDisplay  (libX11 type-vptr "XOpenDisplay" type-string))
            (XDefaultScreen(libX11 fft-int "XDefaultScreen" type-vptr))
            (XRootWindow   (libX11 fft-int "XRootWindow" type-vptr fft-int))
            (XBlackPixel   (libX11 type-vptr "XBlackPixel" type-vptr fft-int))
            (XWhitePixel   (libX11 type-vptr "XWhitePixel" type-vptr fft-int))
            (XCreateColormap (libX11 type-vptr "XCreateColormap" type-vptr fft-int type-vptr fft-int))
            (XCreateSimpleWindow (libX11 type-vptr "XCreateSimpleWindow"
                              type-vptr type-vptr ; display, parent
                              fft-int fft-int fft-unsigned-int fft-unsigned-int ; x y width height
                              fft-unsigned-int  ; border width
                              type-vptr ; border
                              type-vptr ; background
                           ))
            (XCreateWindow (libX11 type-vptr "XCreateWindow"
                              type-vptr fft-int ; display, parent
                              fft-int fft-int fft-unsigned-int fft-unsigned-int ; x y width height
                              fft-unsigned-int  fft-int ; border_width, depth
                              fft-unsigned-int type-vptr ; class, visual
                              fft-unsigned-long ; valuemask
                              type-vptr)) ; XSetWindowAttributes* attributes

            (XSelectInput (libX11 fft-int "XSelectInput" type-vptr type-vptr fft-long))
            (XMapWindow   (libX11 fft-int "XMapWindow" type-vptr type-vptr))
            (XStoreName   (libX11 fft-int "XStoreName" type-vptr type-vptr type-string))
            ; memcpy (hack)
            (memcpy ((load-dynamic-library #false) fft-void "memcpy" fft-void* fft-void* fft-int))
            (memcpy2 ((load-dynamic-library #false) fft-void "memcpy" (fft* fft-void*) fft-void* fft-int))
            ; glx
            (glXQueryExtension(libGLX fft-int "glXQueryExtension" type-vptr fft-int* fft-int*))
            (glXChooseVisual  (libGLX type-vptr "glXChooseVisual" type-vptr fft-int fft-int*))
            (glXCreateContext (libGLX type-vptr "glXCreateContext" type-vptr type-vptr type-vptr fft-int)))
      (lambda (title)
         ;; Double Buffer Solution:
         ;; (let*((display (XOpenDisplay #false))
         ;;       (screen  (XDefaultScreen display))
         ;;       (window  (XCreateSimpleWindow display (XRootWindow display screen)
         ;;                   0 0 WIDTH HEIGHT 1
         ;;                   (XBlackPixel display screen)
         ;;                   (XWhitePixel display screen)))
         ;;       (vi (glXChooseVisual display screen
         ;;             '( 4 ; GLX_RGBA
         ;;                8  5 ; GLX_RED_SIZE
         ;;                9  6 ; GLX_GREEN_SIZE
         ;;               10  5 ; GLX_BLUE_SIZE
         ;;               12  24 ; GLX_DEPTH_SIZE
         ;;                5 ; GLX_DOUBLEBUFFER
         ;;                0)))); None
         ;;    (XSelectInput display window 32769) ; ExposureMask
         ;;    (XStoreName display window title)
         ;;    (XMapWindow display window)
         ;;    (let ((cx (gl:CreateContext display vi #false 1)))
         ;;       (gl:MakeCurrent display window cx)
         ;;       (print "OpenGL version: " (glGetString GL_VERSION))
         ;;       (print "OpenGL vendor: " (glGetString GL_VENDOR))
         ;;       (print "OpenGL renderer: " (glGetString GL_RENDERER))

         ;;       (mail 'opengl (tuple 'set-context (tuple display screen window cx)))
         ;;       (interact 'opengl (tuple 'get-context)) ; синхронизация

         ;;       (tuple display screen window cx)))))))
         (let*((display (XOpenDisplay #false))
               (screen  (XDefaultScreen display))
               ; (unless (glxQueryExtension display #f #f) (halt "X server has no OpenGL GLX extension")
               (vi (glXChooseVisual display screen
                     '( 4 ; GLX_RGBA
                        8  1 ; GLX_RED_SIZE
                        9  1 ; GLX_GREEN_SIZE
                       10  1 ; GLX_BLUE_SIZE
                       12  24 ; GLX_DEPTH_SIZE
                        0))); None
               (XVisualInfo (vptr->vector vi 64)) ; sizeof(XVisualInfo) = 64
               ; *unless (eq? 4 (class (int32->ol XVisualInfo 24))) (halt "TrueColor visual required for this program") ; offsetof(XVisualInfo, class)

               (visual (vector->vptr XVisualInfo 0)) ;
               (root (XRootWindow display screen))
               (colormap (XCreateColormap display root visual 0)) ; 0 == AllocNone

               ; ...
               (XSetWindowAttributes (list->vector (repeat 0 112))) ; sizeof(XSetWindowAttributes)
               (_ (vector-set-vptr! XSetWindowAttributes 96 colormap))
               (_ (vector-set-int!  XSetWindowAttributes 24 0)) ; border_pixel
               (_ (vector-set-int!  XSetWindowAttributes 72 163844)); event_mask (ExposureMask | ButtonPressMask | StructureNotifyMask)

               (window (XCreateWindow display root
                           0 0 WIDTH HEIGHT 0
                           24 1; vi->depth InputOutput 
                           visual
                           10248 ; CWBorderPixel | CWColormap | CWEventMask
                           XSetWindowAttributes)))
            (XSelectInput display window 32769) ; ExposureMask
            (XStoreName display window title)
            (XMapWindow display window)
            (let ((cx (gl:CreateContext display vi #false 1)))
               (gl:MakeCurrent display window cx)
               (print "OpenGL version: " (glGetString GL_VERSION))
               (print "OpenGL vendor: " (glGetString GL_VENDOR))
               (print "OpenGL renderer: " (glGetString GL_RENDERER))

               (mail 'opengl (tuple 'set-context (tuple display screen window cx)))
               (interact 'opengl (tuple 'get-context)) ; синхронизация

               (tuple display screen window cx)))))))
   (else
      (runtime-error "Unknown platform" OS))))


(define gl:ProcessEvents (cond ; todo: add "onClose" handler
   (win32?
      (let ((user32 (load-dynamic-library "user32.dll")))
      (let ((PeekMessage      (user32 fft-int "PeekMessageA"     fft-void* fft-void* fft-int fft-int fft-int))
            (TranslateMessage (user32 fft-int "TranslateMessage" fft-void*))
            (GetMessage       (user32 fft-int "GetMessageA"      fft-void* fft-void* fft-int fft-int))
            (DispatchMessage  (user32 fft-int "DispatchMessageA" fft-void*)))
      (lambda (context)
         (let ((MSG (vm:new-raw-object type-vector-raw 48))) ; 28 for x32
         (let loop ()
            (if (= 1 (PeekMessage MSG #f 0 0 1))
               (let*((w (vm:wordsize))
                     (message (+ (<< (ref MSG (+ 0 (* w 1)))  0)      ; 4 for x32
                                 (<< (ref MSG (+ 1 (* w 1)))  8)
                                 (<< (ref MSG (+ 2 (* w 1))) 16)
                                 (<< (ref MSG (+ 3 (* w 1))) 24))))
                  ;(print message ": " MSG)
                  (cond
                     ((and (eq? message 273) ; WM_COMMAND
                           (eq? (+ (<< (ref MSG (+ 0 (* w 2))) 0)
                                   (<< (ref MSG (+ 1 (* w 2))) 8)) 2)) ; wParam, IDCANCEL
                        24) ; EXIT
                     ((and (eq? message 256) ; WM_KEYDOWN
                           (eq? (+ (<< (ref MSG (+ 0 (* w 2))) 0)
                                   (<< (ref MSG (+ 1 (* w 2))) 8)) #x51)) ; Q key
                        24) ;
                     (else 
                        (TranslateMessage MSG)
                        (DispatchMessage MSG)
                        (loop)))))))))))
   (linux?
      (let ((libX11 (load-dynamic-library "libX11.so")))
      (let ((XPending  (libX11 fft-int "XPending" type-vptr))
            (XNextEvent(libX11 fft-int "XNextEvent" type-vptr type-vptr)))
      (lambda (context)
         (let ((display (ref context 1)))
         (let loop ((XEvent (vm:new-raw-object type-vector-raw 192))) ; 96 for x32
            (if (> (XPending display) 0)
               (begin
                  (XNextEvent display XEvent)
                  (if (eq? (int32->ol XEvent 0) 2) ; KeyPress
                     (int32->ol XEvent (if x32? 52 84)) ; offsetof(XKeyEvent, keycode)
                     (loop XEvent))))))))))
   (else
      (runtime-error "Unknown platform" OS))))

; internal function
(define gl:SetWindowTitle (cond
   (win32?
      (let ((user32 (load-dynamic-library "user32.dll")))
      (let ((SetWindowText   (user32 fft-int "SetWindowTextW"   fft-void* type-string-wide)))
         (lambda (context title)
            (let ((window (ref context 3)))
               (SetWindowText window title))))))
   (linux?
      (let ((libX11 (load-dynamic-library "libX11.so")))
      (let ((XStoreName   (libX11 fft-int "XStoreName" type-vptr type-vptr type-string)))
         (lambda (context title)
            (let ((display (ref context 1))
                  ;(screen  (ref context 2))
                  (window  (ref context 3))
                  (cx      (ref context 4)))
               (XStoreName display window title))))))))

; =============================================
; automation
(fork-server 'opengl (lambda ()
(let this ((dictionary #empty))
(cond
   ; блок обработки сообщений
   ((check-mail) => (lambda (e) ; can be (and (eq? something 0) (check-mail)) =>
      (let*((sender msg e))
         ;(print "envelope: " envelope)
         (tuple-case msg
            ((debug)
               (mail sender dictionary)
               (this dictionary))

            ((finish)  ; wait for OpenGL window closing (just no answer for interact)
               ;(glFinish)
               (gl:SwapBuffers (get dictionary 'context #f))

               ; возможно надо вернуть управление юзеру?
               (unless (get dictionary 'renderer #f)
                  ; нечего ждать к отрисовке, рендерера то нет
                  (mail sender 'ok)
                  ; рендерер есть, но режим интерактивный?
                  (if (or (zero? (length *vm-args*)) (string-eq? (car *vm-args*) "-"))
                     (mail sender 'ok)))
               (this (put dictionary 'customer sender)))

            ; context
            ((set-context context)
               (this (put dictionary 'context context)))
            ((get-context)
               (mail sender (get dictionary 'context #f))
               (this dictionary))

            ; set-window-title
            ((set-window-title title)
               (gl:SetWindowTitle (get dictionary 'context #f) title)
               (this dictionary))

            ; renderer
            ((set-renderer renderer)
               (this (put dictionary 'renderer renderer)))
            ((get-renderer)
               (mail sender (get dictionary 'renderer #f))
               (this dictionary))

            ; userdata
            ((set-userdata userdata)
               (this (put dictionary 'userdata userdata)))
            ((get-userdata)
               (mail sender (get dictionary 'userdata #f))
               (this dictionary))

            (else
               (print-to stderr "Unknown opengl server command " msg)
               (this dictionary))))))
   ; блок непосредственно рабочего цикла окна
   (else
      ; обработаем сообщения (todo: не более чем N за раз)
      (let ((context (get dictionary 'context #f)))
         (if context ; todo: добавить обработку кнопок
            (gl:ProcessEvents context)))
      ; проделаем все действия
      (let*((dictionary
            ; 1. draw (if renderer exists)
            (or (call/cc (lambda (return)
                  (let ((renderer (get dictionary 'renderer #f)))
                     (if renderer
                        ; есть чем рисовать - рисуем
                        (let ((userdata (apply renderer (get dictionary 'userdata #null))))
                           (gl:SwapBuffers (get dictionary 'context #f))
                           (return
                              (put dictionary 'userdata userdata)))))))
               dictionary))
            (dictionary
            ; 2. think (if thinker exists)
            (or (call/cc (lambda (return)
                  (let ((thinker (get dictionary 'thinker #f)))
                     (if thinker
                        dictionary))))
               dictionary))
            )
         ; done.
         (sleep 1)
         (this dictionary)))))))

; force create window. please, change the window behaviour using exported functions
(gl:Create "Ol: OpenGL window")

; -----------------------------
(define (gl:set-userdata . userdata)
   (mail 'opengl (tuple 'set-userdata userdata)))

(define (gl:set-renderer renderer)
   (mail 'opengl (tuple 'set-renderer renderer)))

(define (gl:set-window-title title)
   (mail 'opengl (tuple 'set-window-title title)))

(define (gl:finish)
   (interact 'opengl (tuple 'finish)))

; ====================================================================================================
;; (define (gl:run context init renderer)
;; (let ((context (if (string? context) (gl:Create context) context)))

;;    (gl:Enable context)
;;    (let ((userdata (init)))
;;    (gl:Disable context)

;;    (call/cc (lambda (return)
;;    (let this ((userdata userdata))
;;       (let ((message (gl:ProcessEvents context)))
;;          (if (eq? message 24)
;;             (return message)))

;;       (gl:Enable context)
;;       (let ((userdata (if renderer (apply renderer userdata) userdata)))
;;       (gl:SwapBuffers context)
;;       (gl:Disable context)

;;       (this userdata))))))))

;(define gl:run (lambda args
;   (let run ((title #f) (init #f) (draw #f) (args args) (selector #f))
;      (if (null? args)
;         (gl:run title init draw)
;      (cond
;      ((eq? (car args) 'init)
;         (run title (cadr args) draw (cddr args) selector))
;      ((eq? (car args) 'draw)
;         (run title init (cadr args) (cddr args) selector))
;      (else
;         (if selector
;            (selector title init draw args)
;            (run title init draw args (lambda (title init draw args)
;               (run (car args) init draw (cdr args) (lambda (title init draw args)
;                  (run title (car args) draw (cdr args) (lambda (title init draw args)
;                     (run title init (car args) (cdr args) #f))))))))))))))

;;  (lambda args
;;    (let run ((title #f) (init #f) (draw #f) (args args))
;;       (if (null? args)
;;         (gl:run title init draw)
;;       (cond
;;       ((eq? (car args) 'init)
;;          (run title (cadr args) draw (cddr args)))
;;       ((eq? (car args) 'draw)
;;          (run title init (cadr args) (cddr args)))
;;       (else (cond
;;          ((eq? title #f)
;;             (run (car args) init draw (cdr args)))
;;          ((eq? init #f)
;;             (run title (car args) draw (cdr args)))
;;          ((eq? draw #f)
;;             (run title init (car args) (cdr args))))))))))


))

;(define gl:run2 (lambda args
;   (print args)))

;         (gl:ProcessEvents context)
;      (let ((XEvent (vm:new-raw-object fft-void* (repeat 0 192))))
;         (let process-events ((unused 0))
;            (if (> (XPending display) 0)
;               (process-events (XNextEvent display XEvent))))

#| Working single-buffer X11 example:

#include <iostream>
using namespace std;
 
#include <X11/Xlib.h>
#include <GL/glx.h>
 
Display* g_display;
 
int main(int argc, char* argv[])
{
  g_display = XOpenDisplay(NULL);
  if(g_display == NULL)
  {
    cerr << "Failed to open display" << endl;
    return 1;
  }
 
  int errorBase, eventBase;
  if(!glXQueryExtension(g_display, &errorBase, &eventBase))
  {
    cerr << "Failed to query glx" << endl;
    return 1;
  }
 
  int major, minor;
  if(!glXQueryVersion(g_display, &major, &minor)
      || major < 1 || (major == 1 && minor < 3))
  {
    cerr << "glx 1.3 required, only " << major << "."
         << minor << " found." << endl;
    return 1;
  }
 
  int num;
  int attribs[] = { GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
                    GLX_DOUBLEBUFFER, False,
                    //GLX_X_RENDERABLE, True,
                    //GLX_RED_SIZE, 1, GLX_GREEN_SIZE, 1, GLX_BLUE_SIZE, 1,
                    GLX_RENDER_TYPE, GLX_RGBA_BIT,
                    None };
 
  GLXFBConfig* configs = glXChooseFBConfig(g_display,
      XDefaultScreen(g_display), attribs, &num);
  if(configs == NULL || num < 1)
  {
    cerr << "No config found" << endl;
    return 1;
  }
  GLXFBConfig config = configs[0];
  XFree(configs);
 
  XVisualInfo* vis = glXGetVisualFromFBConfig(g_display, config);
  if(vis == NULL)
  {
    cerr << "Couldn't get visual" << endl;
    return 1;
  }
 
 
  int x = 100, y = 100, wid = 640, hyt = 150;
  XSetWindowAttributes swa;
  swa.event_mask = ExposureMask | StructureNotifyMask;
//  Visual* t = XDefaultVisual(g_display, 0);


  swa.colormap = XCreateColormap(g_display, XRootWindow(g_display, vis->screen),
      vis->visual, AllocNone); //XDefaultColormap(g_display, vis->screen);
  Visual* t = XDefaultVisual(g_display, 0);
  Window win = XCreateWindow(g_display, XRootWindow(g_display, vis->screen),
      x, y, wid, hyt, 0, 24, InputOutput, vis->visual, //CopyFromParent,
     CWEventMask | CWColormap, &swa);

//  Window win = XCreateWindow(g_display, XRootWindow(g_display, vis->screen),
//      x, y, wid, hyt, 0, vis->depth, InputOutput, vis->visual,
//      CWEventMask, &swa);
 
  GLXWindow glXWin = glXCreateWindow(g_display, config, win, NULL);
 
  GLXContext glXContext = glXCreateNewContext(g_display, config,
      GLX_RGBA_TYPE, NULL, True);
  if(glXContext == NULL)
  {
    cerr << "Failed to create glXContext" << endl;
    return 1;
  }
 
 
  XMapWindow(g_display, win);
  glXMakeContextCurrent(g_display, glXWin, glXWin, glXContext);
 
  Atom a = XInternAtom(g_display, "WM_DELETE_WINDOW", False);
  XSetWMProtocols(g_display, win, &a, 1);
 

   glClearColor(1, 0, 1, 1);
        glClear(GL_COLOR_BUFFER_BIT);
        glFlush();
//        glXSwapBuffers(g_display, glXWin);
 
  bool done = false;
  XEvent event;
  while(!done)
  {
    XNextEvent(g_display, &event);
    switch(event.type)
    {
      case Expose:
        if(event.xexpose.count > 1)
          break;
        break;
      case ClientMessage:
        done = true;
        break;
    }
  }
 
  glXMakeContextCurrent(g_display, None, None, NULL);
  glXDestroyContext(g_display, glXContext);
  glXDestroyWindow(g_display, glXWin);
  XCloseDisplay(g_display);
}

|#