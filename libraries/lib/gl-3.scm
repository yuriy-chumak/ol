(define-library (lib gl-3)
(import
   (scheme base)
   (owl math) (otus async)
   (lib gl config)
   (lib gl-2)
   (OpenGL 3))
(export
   gl:set-context-version ; recreate OpenGL with version

   (exports (lib gl-2))
   (exports (OpenGL 3)))

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
      (begin
         (define (gl:set-context-version major minor)
            (let*((context (await (mail 'opengl ['get 'context]))) ;#(display screen window cx)
                  (display screen window cx context)
                  ; this functions requires GLX 1.3+
                  (glXChooseFBConfig (GL_LIBRARY fft-void* "glXChooseFBConfig" fft-void* fft-int fft-int* fft-int&)))
                  ;(glXGetVisualFromFBConfig (GLX fft-void* "glXGetVisualFromFBConfig" fft-void* fft-void*))

               (define visual_attribs (list
                  #x8012    1 ; GLX_X_RENDERABLE
                  #x8010    1 ; GLX_DRAWABLE_TYPE GLX_WINDOW_BIT
                  #x22 #x8002 ; GLX_X_VISUAL_TYPE GLX_TRUE_COLOR
                  #x8011    1 ; GLX_RENDER_TYPE GLX_RGBA_BIT
                  8  (config 'red 8)    ; GLX_RED_SIZE
                  9  (config 'green 8)  ; GLX_GREEN_SIZE
                  10 (config 'blue 8)   ; GLX_BLUE_SIZE
                  12 (config 'depth 24) ; GLX_DEPTH_SIZE
                  5         1 ; GLX_DOUBLEBUFFER
                  ; if we need a sample buffers:
                  ;; 100000 1
                  ;; 100001 4 ; count of samples
                  0))

               (define fbcount (box 0))
               (define fbc*
                  (glXChooseFBConfig display screen visual_attribs fbcount))
               (define fbc (vptr->bytevector fbc* (* (size nullptr) (unbox fbcount))))
               (define firstFbc (bytevector->void* fbc 0))

               (define contextAttribs (list
                  GLX_CONTEXT_MAJOR_VERSION_ARB  major
                  GLX_CONTEXT_MINOR_VERSION_ARB  minor
                  0))
               (define new_cx (glXCreateContextAttribsARB display firstFbc NULL 1 contextAttribs))
               (define new_context [display screen window new_cx])

               ; change old to new context
               (native:disable-context context) ; todo: destroy old one
               (mail 'opengl ['set 'context new_context])
               (native:enable-context new_context)
               #true))))
   (Windows
      ;(import (OpenGL WGL ARB create_context))
      (begin
         ; TODO: https://community.khronos.org/t/opengl-3-x-and-qt-framework/58357/6
         (define (gl:set-context-version major minor)
            #false)))
)

(begin
   #true))
