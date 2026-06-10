(define-library (lib gl 3 context)
(import
   (scheme base)
   (owl math) (otus async)
   (lib gl)
   (lib gl config)
   (otus ffi))

(export
   gl:set-context-version ; recreate OpenGL with version
)

; -=( 3.0+ )=-------------------------
; Higher OpenGL versions support
(cond-expand
   ((or Android Emscripten) ; TODO: dev only, please rename to Android
      (begin
         (define (gl:set-context-version major minor)
            #false)
   ))
   (Linux
      (import (OpenGL GLX ARB create_context))
      (import (owl io))
      (begin
         (define (gl:set-context-version major minor)
            (let*((context (await (mail 'opengl ['get 'context]))) ;#(display screen window cx)
                  (display screen window cx context)
                  ; this functions requires GLX 1.3+, TODO: handle case without glXChooseFBConfig
                  (glXChooseFBConfig (GL_LIBRARY fft-void* "glXChooseFBConfig" fft-void* fft-int fft-int* fft-int&)))
                  ;(glXGetVisualFromFBConfig (GLX fft-void* "glXGetVisualFromFBConfig" fft-void* fft-void*))
               ;; (print "display: " display)
               ;; (print "screen: " screen)

               (define visual_attribs (list
                  #x8012    1 ; GLX_X_RENDERABLE
                  #x8010    1 ; GLX_DRAWABLE_TYPE GLX_WINDOW_BIT
                  #x22 #x8002 ; GLX_X_VISUAL_TYPE GLX_TRUE_COLOR
                  #x8011    1 ; GLX_RENDER_TYPE GLX_RGBA_BIT
                  8  (config 'red 5)    ; GLX_RED_SIZE
                  9  (config 'green 6)  ; GLX_GREEN_SIZE
                  10 (config 'blue 5)  ; GLX_BLUE_SIZE
                  12 (config 'depth 24); GLX_DEPTH_SIZE
                  5         1 ; GLX_DOUBLEBUFFER
                  0))

               (define fbcount (box 0))
               (define fbc*
                  (glXChooseFBConfig display screen visual_attribs fbcount))
               ;; (print "fbcount: " (unbox fbcount))
               (define fbc (vptr->bytevector fbc* (* (size nullptr) (unbox fbcount))))
               (define bestFbc (bytevector->void* fbc 0))
               ;; (define vi (glXGetVisualFromFBConfig display bestFbc))

               (print (config 'exact-context))
               (define contextAttribs (append
                  (list
                     GLX_CONTEXT_MAJOR_VERSION  major
                     GLX_CONTEXT_MINOR_VERSION  minor)
                  ; поддерживать ли fixed-function функции
                  (if (config 'core-profile #f)
                     (list #x9126 1) ;GLX_CONTEXT_PROFILE_MASK_ARB GLX_CONTEXT_CORE_PROFILE_BIT_ARB
                     (list))
                  ; 
                  (if (config 'exact-context #f)
                     (list #x2094 2) ;GLX_CONTEXT_FLAGS_ARB GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB
                     (list))
                  (list 0)
               ))
               (define new_cx (glXCreateContextAttribs display bestFbc NULL 1 contextAttribs))
               (define new_context [display screen window new_cx])

               ; disable and destroy old context
               (native:disable-context context) ; todo: destroy
               ; set new context
               (mail 'opengl ['set 'context new_context])
               (native:enable-context new_context)
               #true))))
   (Windows
      ;(import (OpenGL WGL ARB create_context))
      (begin
         (define (gl:set-context-version major minor)
            #false)))
)

(begin ))
