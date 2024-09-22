; properties
(define setup:autorender-mode #false)

;; (setq gl4es (load-dynamic-library "libgl4es.so"))
;; (setq gl2es (load-dynamic-library "libgl2es.so"))
;; (setq GLESv2 (load-dynamic-library "libGLESv2.so"))

(setq THIS (load-dynamic-library "libandroid.so")) ; android shared code (todo: move to gl4es)
;; (setq anlPollEvents (THIS fft-void "anlPollEvents"))
;; (setq anlNextEvent (THIS fft-unsigned-int "anlNextEvent"))
;; (setq anlSwapBuffers (THIS fft-void "anlSwapBuffers"))

; functions
(define (native:create-context title)
   ; context already created, just notify opengl

   (print "native:create-context('" title "')")
   ; initialize gl2es library
   ((GL_LIBRARY fft-void "initialize_gl2es"))

   ;; hack to get window size
   (define viewport '(0 0 0 0))
   (define GL_VIEWPORT  #x0BA2)
   (glGetIntegerv GL_VIEWPORT viewport)
   (set-ref! gl:window-dimensions 3 (list-ref viewport 2))
   (set-ref! gl:window-dimensions 4 (list-ref viewport 3))

   (print "viewport: " viewport)

   (define vr '(0))
   (define GL_VR  #x10C33)
   (glGetIntegerv GL_VR vr)

   ;; ;; (eglQuerySurface display surface #x3057 width) ;EGL_WIDTH
   ;; ;; (eglQuerySurface display surface #x3056 height) ;EGL_HEIGHT

   ; android printing to the stdin means "DEBUG" logcat message
   (print "OpenGL version: " (glGetString GL_VERSION))
   (print "OpenGL vendor: " (glGetString GL_VENDOR))
   (print "OpenGL renderer: " (glGetString GL_RENDERER))
   (print "OpenGL VR: " (eq? (car vr) 1))

   ;; ; switch to vr mode
   ;; (when (eq? (car vr) 1)
   ;;    (define vr-begin (THIS fft-void "begin"))
   ;;    (define vr-update (THIS fft-void "update" fft-int))
   ;;    (define vr-flush (THIS fft-void "flush"))
   ;;    (define vr-end (THIS fft-void "end"))

   ;;    (mail 'opengl ['set 'vr-begin vr-begin])
   ;;    (mail 'opengl ['set 'vr-update vr-update])
   ;;    (mail 'opengl ['set 'vr-flush vr-flush])
   ;;    (mail 'opengl ['set 'vr-end vr-end])

   ;;    (mail 'opengl ['set 'vr-mode #true]))
   (print "end")
   #true)

(define (native:enable-context context)
   (print "native:enable-context('" context "')")
   #true)
(define (native:disable-context context)
   (print "native:disable-context('" context "')")
   #true)

(define (native:swap-buffers context)
   #false)
   ;; (anlSwapBuffers))

(define (native:process-events context handler)
   #false)
   ;; (anlPollEvents) ; collect events
   ;; ; process events:
   ;; (let loop ()
   ;;    (define event (anlNextEvent))
   ;;    (define t (>> event 16))
   ;;    (define v (band event #xFFFF))
   ;;    (unless (zero? t)
   ;;       (case t
   ;;          (2 ; AKEY_EVENT_ACTION_DOWN
   ;;                (handler ['keyboard v]))
   ;;          (3 ; AKEY_EVENT_ACTION_UP
   ;;                #false)
   ;;          (else #f))
   ;;       (loop))))

; -=( gl functions )=--------------------------------

(define (os:SetWindowTitle context title)
   #false)

(define (os:SetWindowSize context width height)
   #false)

(define (os:HideCursor context)
   #false)

(define (os:GetMousePos context)
   ;; TODO: aniGetMousePos, etc....
   #false)
