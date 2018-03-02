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
      ((get-display display)
         (mail sender (get dictionary 'display 0))
         (this dictionary))

      (else
         (print-to stderr "Unknown opengl server command " msg)
         (this dictionary)))))))

(define opengl:init
(let ((init (lambda (title)
               (let*((NULL #false)
                     (major '(0))
                     (minor '(0))
                     (display (eglGetDisplay NULL))

               (mail 'opengl (tuple 'set-display 123))
               #t)))

   (case-lambda
      (() (init ""))
      ((title) (init title)))))

))