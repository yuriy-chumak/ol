; properties
(define setup:autorender-mode #true)


(import (EGL 1.3))

(define (native:create-context title)
   ;(print "Checking WebGL support...")
   (let ((major '(0))
         (minor '(0))
         (numConfigs '(0))
         (attribList (list
            EGL_RED_SIZE 5 ; red
            EGL_GREEN_SIZE 6 ; green
            EGL_BLUE_SIZE 5 ; blue

            EGL_ALPHA_SIZE 8 ; alpha
            EGL_DEPTH_SIZE 8 ; depth

            ;; EGL_SAMPLE_BUFFERS 1 ; sample buffers
            EGL_NONE))

         (config (make-vptr-array 1))
         (contextAttribs (list
            EGL_CONTEXT_CLIENT_VERSION 2
            EGL_NONE)))
   (print) ; empty print, some king of 'flag' in output

   ; display
   (define display (eglGetDisplay #false))
   ;;  assert(display != EGL_NO_DISPLAY);
   ;;  assert(eglGetError() == EGL_SUCCESS);

   ; surface
   (eglInitialize display major minor)
   (print "eglInitialize: " (car major) "." (car minor))
   ;; todo: check EGL version

   (eglGetConfigs display config 0 numConfigs)
   (print "eglGetConfigs: " (car numConfigs))
   ;; assert(numConfigs > 0)

   (eglChooseConfig display attribList config 1 numConfigs)
   (define surface (eglCreateWindowSurface display (car config) 2 #false)) ; temp "2" instead of XCreateWindow

   (define context (eglCreateContext display (car config) EGL_NO_CONTEXT contextAttribs))

   ; gl2es part
   ; TODO: remove it
   (define gl2es (dlopen))
   ;; ;; (define LIBGL_BEGINEND #xA10D)  (glHint LIBGL_BEGINEND 0)
   (define Init (dlsym gl2es "Init"))
   (Init)
   
   (print-to stderr "OpenGL version: " (glGetString GL_VERSION))
   (print-to stderr "OpenGL vendor: " (glGetString GL_VENDOR))
   (print-to stderr "OpenGL renderer: " (glGetString GL_RENDERER))
   ;; (print-to stderr "OpenGL extensions: " (glGetString GL_EXTENSIONS))

   (define width '(1184))  (define height '(672))
   (eglQuerySurface display surface EGL_WIDTH width)
   (eglQuerySurface display surface EGL_HEIGHT height)

   (print "widht: " width ", height: " height)
   (set-ref! gl:window-dimensions 3 (car width))
   (set-ref! gl:window-dimensions 4 (car height))

   (glViewport 0 0 (car width) (car height))
   [display surface context]
))

(define (native:enable-context context) #true)
(define (native:disable-context context) #false)
(define (native:swap-buffers context) #f) ;(glFinish))


(define (native:process-events context handler)
   ;; TBD.
   #false)

(define (os:SetWindowTitle context title)
   ;; TBD.
   #false)
(define (os:SetWindowSize context width height) #false)
(define (os:HideCursor context) #false)
(define (os:GetMousePos context) #false)
