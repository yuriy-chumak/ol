; Platform independent OpenGL Library part
(define-library (OpenGL ES platform)
(export

   ; Platform Types
   ; /usr/include/KHR/khrplatform.h
   khronos_int8_t    ;   signed  8 bit
   khronos_uint8_t   ; unsigned  8 bit
   khronos_int32_t   ;   signed 32 bit

   khronos_intptr_t  ;   signed same number of bits as a pointer
   khronos_ssize_t   ;   signed size

   khronos_float_t   ;   signed 32 bit floating point

   ; minimal required GL function set
   glGetString
      GL_VENDOR
      GL_RENDERER
      GL_VERSION
      GL_EXTENSIONS
   glViewport

   ;; ; WGL/GLX/CGL/EGL/... universal functions
   ;; gl:GetProcAddress

   ;; gl:CreateContext ; context creation
   gl:MakeCurrent
   gl:SwapBuffers
   gl:QueryExtension

   ; internal variables
   GL
   GL_LIBRARY ; deprecated

   (exports (otus lisp))
   (exports (otus ffi)))

; ============================================================================
; == implementation ==========================================================
(import (otus lisp) (otus ffi))

(begin
   ; KHR/khrplatform
   (define khronos_int8_t fft-signed-char)
   (define khronos_uint8_t fft-unsigned-char)
   (define khronos_int16_t fft-signed-short)
   (define khronos_uint16_t fft-unsigned-short)
   (define khronos_int32_t fft-signed-int)
   (define khronos_uint32_t fft-unsigned-int)

   (define khronos_intptr_t fft-signed-long)
   (define khronos_ssize_t fft-signed-long) ;todo: khronos_ssize_t -cond-expand windows and windows-x64

   (define khronos_float_t fft-float))

(cond-expand
   ; -=( Linux )=--------------------------------------
   ((or Linux Android)
      (begin
         (define GL (or
            (load-dynamic-library "libGLESv1_CM.so")
            (runtime-error "No OpenGL ES v1 installed:" "Maybe install libgles1-mesa package?")))

         (setq EGL (load-dynamic-library "libEGL.so"))
         (setq EGLBoolean fft-int)
         (setq EGLint fft-int32) (setq EGLint* (fft* EGLint)) (setq EGLint& (fft& EGLint))

         (setq EGLDisplay fft-void*)
         (setq EGLConfig  fft-void*)  (setq EGLConfig* (fft* EGLConfig))   (setq EGLConfig& (fft& EGLConfig))
         (setq EGLSurface fft-void*)
         (setq EGLContext fft-void*)

         (setq eglMakeCurrent   (EGL EGLBoolean "eglMakeCurrent" EGLDisplay EGLSurface EGLSurface EGLContext))
         (setq eglSwapBuffers   (EGL EGLBoolean "eglSwapBuffers" EGLDisplay EGLSurface))

         (define (gl:MakeCurrent context)
            (eglMakeCurrent (ref context 1) (ref context 2) (ref context 2) (ref context 4)))

         (define (gl:SwapBuffers context)
            (eglSwapBuffers (ref context 1) (ref context 2)))

         ; GL
         (setq GLint fft-int)
         (setq GLsizei fft-int)
         (setq GLvoid fft-void)
         (setq GLenum fft-unsigned-int)

         (define glGetString (GL GLvoid "glGetString" type-string GLenum))
            (define GL_VENDOR     #x1F00)
            (define GL_RENDERER   #x1F01)
            (define GL_VERSION    #x1F02)
            (define GL_EXTENSIONS #x1F03)
         (define glViewport (GL GLvoid "glViewport" GLint GLint GLsizei GLsizei))
   ))

   (else
      (begin
         (runtime-error "Unsupported platform: " *uname*))))

(begin
   (define GL_LIBRARY GL)

   (import (owl regex))
   (setq split (string->regex "c/ /"))
   (define (gl:QueryExtension extension)
      (for-each (λ (s) (display-to stderr s)) (list "Checking " extension " support...")) ; debug info
      (let ((extensions (split (or
               (glGetString GL_EXTENSIONS)
               ; if no extensions - use empty string:
               ""))))
         (if (member extension extensions)
            (begin (print " ok.") #true)
            (begin (print " not found.") #false))))

))