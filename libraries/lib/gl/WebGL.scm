      (import (EGL version-1-1))

         (define (native:create-context title)
            ;(print "Checking WebGL support...")
            (let ((major (make-32bit-array 1))
                  (minor (make-32bit-array 1))
                  (numConfigs (make-32bit-array 1))
                  (attribList '(
                     #x3024 5 ; red
                     #x3023 6 ; green
                     #x3022 5 ; blue
                     #x3021 8 ; alpha
                     #x3025 8 ; depth
                     ;#x3026 ; stencil
                     #x3032 0 ; sample buffers
                     #x3038)) ; EGL_NONE
                  (config (make-vptr-array 1))
                  (contextAttribs '(
                     #x3098 2 #x3038 #x3038))) ; EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE, EGL_NONE
            (print) ; empty print, some king of 'flag' in output

            (define display (eglGetDisplay #false))
            (mail 'opengl ['set-display display])

            (eglInitialize display major minor)
            (print "eglInitialize: " (car major) "." (car minor))

            (eglGetConfigs display config 0 numConfigs)
            (print "eglGetConfigs: " (car numConfigs))

            (eglChooseConfig display attribList config (car numConfigs) numConfigs)
            (define surface (eglCreateWindowSurface display (car config) 2 #false)) ; temp "2" instead of XCreateWindow
            (mail 'opengl ['set-surface surface])

            (define context (eglCreateContext display (car config) EGL_NO_CONTEXT contextAttribs))

            ; gl2es part
            (define gl4es (dlopen))
            (define LIBGL_BEGINEND #xA10D)  (glHint LIBGL_BEGINEND 0)
            (define initialize_gl4es (dlsym gl4es "initialize_gl4es"))
            
            (ffi initialize_gl4es (cons fft-int #null) #null)

            (print-to stderr "OpenGL version: " (glGetString GL_VERSION))
            (print-to stderr "OpenGL vendor: " (glGetString GL_VENDOR))
            (print-to stderr "OpenGL renderer: " (glGetString GL_RENDERER))
            ;; (print-to stderr "OpenGL extensions: " (glGetString GL_EXTENSIONS))

            ;; (define width '(1184))
            ;; (define height '(672))
            ;; ;; (eglQuerySurface display surface #x3057 width) ;EGL_WIDTH
            ;; ;; (eglQuerySurface display surface #x3056 height) ;EGL_HEIGHT

            ;; (set-ref! gl:window-dimensions 3 (car width))
            ;; (set-ref! gl:window-dimensions 4 (car height))

            ;; (glViewport 0 0 (car width) (car height)))
            context
         ))

         (define (native:enable-context context)
            #false)
         (define (native:disable-context context)
            #false)
         (define (native:process-events context handler)
            #false)

         (define (gl:SetWindowTitle context title)
            #false)
         (define (gl:SetWindowSize context width height)
            #false)
         (define (gl:HideCursor context)
            #false)
         (define (gl:GetMousePos context)
            #false)
