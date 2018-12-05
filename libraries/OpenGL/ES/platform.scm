; Platform independent OpenGL Library part
(define-library (OpenGL ES platform)
(export

   ;; ; GL types
   ;; ; https://www.opengl.org/wiki/OpenGL_Type
   ;; GLenum                     ; unsigned 32-bit
   ;; GLboolean GLboolean*       ; unsigned byte (GL_TRUE or GL_FALSE)
   ;; GLbitfield                 ; unsigned 32-bit
   ;; GLbyte                     ;   signed  8-bit
   ;; GLshort  GLshort*          ;   signed 16-bit
   ;; GLint    GLint*   GLint&   ;   signed 32-bit
   ;; GLsizei                    ;   signed 32-bit
   ;; GLubyte  GLubyte*          ; unsigned  8-bit
   ;; GLushort GLushort*         ; unsigned 16-bit
   ;; GLuint   GLuint*  GLuint&  ; unsigned 32-bit

   ;; GLfloat  GLfloat*  ; floating 32-bit
   ;; GLclampf           ; floating 32-bit (clamped to the range [0,1])
   ;; GLdouble GLdouble* ; floating 64-bit
   ;; GLclampd           ; floating 64-bit (clamped to the range [0,1])

   ;; GLvoid   GLvoid*       ; void, void*

   ;; ; minimal required GL function set
   ;; glGetString
   ;;    GL_VENDOR
   ;;    GL_RENDERER
   ;;    GL_VERSION
   ;;    GL_EXTENSIONS
   glViewport

   ;; ; WGL/GLX/CGL/EGL/... universal functions
   ;; gl:GetProcAddress

   ;; gl:CreateContext ; context creation
   gl:MakeCurrent
   gl:SwapBuffers
   ;; gl:QueryExtension

   ; internal variables
   GL
   GL_LIBRARY ; deprecated

   (exports (otus lisp))
   (exports (otus ffi)))

; ============================================================================
; == implementation ==========================================================
(import (otus lisp) (otus ffi))

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

         (define glViewport (GL GLvoid "glViewport" GLint GLint GLsizei GLsizei))
   ))

   (else
      (begin
         (runtime-error "Unsupported platform: " *uname*))))

(begin
   (setq khronos_int8_t fft-signed-char)
   (setq khronos_uint8_t fft-unsigned-char)
   (setq khronos_int16_t fft-signed-short)
   (setq khronos_uint16_t fft-unsigned-short)
   (setq khronos_int32_t fft-signed-int)
   (setq khronos_uint32_t fft-unsigned-int)

   (setq khronos_float_t fft-float)
   ;khronos_ssize_t -cond-expand windows and windows-x64
   ;khronos_intptr_t

   (define GL_LIBRARY GL)

))