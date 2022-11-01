(setq gl4es (load-dynamic-library "libgl4es.so"))
(setq glGetIntegerv (gl4es fft-void "glGetIntegerv" fft-int (fft& fft-int)))

(setq THIS (load-dynamic-library "libmain.so")) ; android shared code (todo: move to gl4es)
(setq anlProcessEvents (THIS fft-void "anlProcessEvents"))
(setq anlSwapBuffers (THIS fft-void "anlSwapBuffers"))

; functions
(define (native:create-context title)
   ; context already created, just notify opengl

   ;; hack to get window size
   (define viewport '(0 0 0 0))
   (define GL_VIEWPORT  #x0BA2)
   (glGetIntegerv GL_VIEWPORT viewport)
   (set-ref! gl:window-dimensions 3 (list-ref viewport 2))
   (set-ref! gl:window-dimensions 4 (list-ref viewport 3))

   ;; ;; (eglQuerySurface display surface #x3057 width) ;EGL_WIDTH
   ;; ;; (eglQuerySurface display surface #x3056 height) ;EGL_HEIGHT

   ; android printing to the stdin means "DEBUG" logcat message
   (print-to stdin "OpenGL version: " (glGetString GL_VERSION))
   (print-to stdin "OpenGL vendor: " (glGetString GL_VENDOR))
   (print-to stdin "OpenGL renderer: " (glGetString GL_RENDERER))

   #true)

(define (native:enable-context context)
   #true)
(define (native:disable-context context)
   #true)

(define (native:swap-buffers context)
   (anlSwapBuffers))

(define (native:process-events context handler)
   (anlProcessEvents))

; -=( gl functions )=--------------------------------

(define (gl:SetWindowTitle context title)
   #false)

(define (gl:SetWindowSize context width height)
   #false)

(define (gl:HideCursor context)
   #false)

(define (gl:GetMousePos context)
   ;; TODO: aniGetMousePos, etc....
   #false)
