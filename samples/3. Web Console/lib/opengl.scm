(define-library (lib opengl)
   (export
      (exports (EGL version-1-1))
      (exports (OpenGL ES version-1-1))
      (exports (otus ffi))

      opengl:init)

   (import
      (r5rs core)
      (EGL version-1-1)
      (OpenGL ES version-1-1)
      (owl interop) (owl ff) (owl io)
      (otus ffi))

   (begin

(fork-server 'opengl (lambda ()
(let this ((dictionary #empty))
(let* ((envelope (wait-mail))
       (sender msg envelope))
   (tuple-case msg
      ((set-display display)
         (this (put dictionary 'display display)))
      ((get-display)
         (mail sender (get dictionary 'display 0))
         (this dictionary))

      ((set-surface surface)
         (this (put dictionary 'surface surface)))
      ((get-surface)
         (mail sender (get dictionary 'surface 0))
         (this dictionary))

      ((set-context context)
         (this (put dictionary 'context context)))
      ((get-context)
         (mail sender (get dictionary 'context 0))
         (this dictionary))

      (else
         (print-to stderr "Unknown opengl server command " msg)
         (this dictionary)))))))

(define opengl:init
(let*((init (lambda (title)
               (let ((major '(0))
                     (minor '(0))
                     (numConfigs '(0))
                     (attribList '(
                        #x3024 5 ; red
                        #x3023 6 ; green
                        #x3022 5 ; blue
                        #x3021 8 ; alpha
                        #x3025 8 ; depth
                        ;#x3026 ; stencil
                        #x3032 1 ; sample buffers
                        #x3038)) ; EGL_NONE
                     (config (make-vptr-array 1))
                     (contextAttribs '(
                        #x3098 2 #x3038 #x3038))) ; EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE, EGL_NONE
               (print) ; empty print, some king of 'flag' in output

               (define display (eglGetDisplay #false))
               (mail 'opengl (tuple 'set-display display))

               (eglInitialize display major minor)
               (print "eglInitialize: " (car major) "/" (car minor))

               (eglGetConfigs display #f 0 numConfigs)
               (print "eglGetConfigs: " (car numConfigs))

               (eglChooseConfig display attribList config (car numConfigs) numConfigs)
               (define surface (eglCreateWindowSurface display (car config) 2 #false))                 ; temp "2" instead of XCreateWindow
               (mail 'opengl (tuple 'set-surface surface))

               (define context (eglCreateContext display (car config) EGL_NO_CONTEXT contextAttribs))
               (mail 'opengl (tuple 'set-context context))

               #true))))
   (case-lambda
      (() (init ""))
      ((title) (init title)))))

))