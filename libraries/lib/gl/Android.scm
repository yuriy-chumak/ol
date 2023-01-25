;; (setq gl4es (load-dynamic-library "libgl4es.so"))
;; (setq gl2es (load-dynamic-library "libgl2es.so"))
;; (setq GLESv2 (load-dynamic-library "libGLESv2.so"))

(setq THIS (load-dynamic-library "libmain.so")) ; android shared code (todo: move to gl4es)
(setq anlPollEvents (THIS fft-void "anlPollEvents"))
(setq anlNextEvent (THIS fft-unsigned-int "anlNextEvent"))
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

   (define vr '(0))
   (define GL_VR  #x10C33)
   (glGetIntegerv GL_VR vr)

   ;; ;; (eglQuerySurface display surface #x3057 width) ;EGL_WIDTH
   ;; ;; (eglQuerySurface display surface #x3056 height) ;EGL_HEIGHT

   ; android printing to the stdin means "DEBUG" logcat message
   (print-to stdin "OpenGL version: " (glGetString GL_VERSION))
   (print-to stdin "OpenGL vendor: " (glGetString GL_VENDOR))
   (print-to stdin "OpenGL renderer: " (glGetString GL_RENDERER))
   (print-to stdin "OpenGL VR: " (eq? (car vr) 1))

   ; switch to vr mode
   (when (eq? (car vr) 1)
      (define vr-begin (THIS fft-void "begin"))
      (define vr-update (THIS fft-void "update" fft-int))
      (define vr-flush (THIS fft-void "flush"))
      (define vr-end (THIS fft-void "end"))

      (mail 'opengl ['set 'vr-begin vr-begin])
      (mail 'opengl ['set 'vr-update vr-update])
      (mail 'opengl ['set 'vr-flush vr-flush])
      (mail 'opengl ['set 'vr-end vr-end])

      (mail 'opengl ['set 'vr-mode #true]))

   #true)

(define (native:enable-context context)
   #true)
(define (native:disable-context context)
   #true)

(define (native:swap-buffers context)
   (anlSwapBuffers))

(define (native:process-events context handler)
   (anlPollEvents) ; collect events
   ; process events:
   (let loop ()
      (define event (anlNextEvent))
      (define t (>> event 16))
      (define v (band event #xFFFF))
      (unless (zero? t)
         (case t
            (2 ; AKEY_EVENT_ACTION_DOWN
                  (handler ['keyboard v]))
            (3 ; AKEY_EVENT_ACTION_UP
                  #false)
            (else #f))
         (loop))))

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
