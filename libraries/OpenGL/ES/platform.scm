; Platform independent OpenGL Library part
(define-library (OpenGL ES platform)
(export

   ; GL types
   ; /usr/include/GLES/gl.h
   GLvoid   GLvoid*
   GLchar
   GLenum
   GLboolean
   GLbitfield
   GLbyte
   GLshort
   GLint
   GLsizei
   GLubyte  GLubyte*
   GLushort GLushort*
   GLuint   GLuint*   GLuint&
   GLfloat  GLfloat*
   GLclampf
   GLfixed
   GLclampx

   GLintptr
   GLsizeiptr

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
   ; Platform Types
   ; /usr/include/KHR/khrplatform.h
   (setq khronos_int8_t fft-signed-char)
   (setq khronos_uint8_t fft-unsigned-char)
   (setq khronos_int16_t fft-signed-short)
   (setq khronos_uint16_t fft-unsigned-short)
   (setq khronos_int32_t fft-signed-int)
   (setq khronos_uint32_t fft-unsigned-int)

   (setq khronos_intptr_t fft-signed-long)
   (setq khronos_ssize_t fft-signed-long) ;todo: khronos_ssize_t -cond-expand windows and windows-x64

   (setq khronos_float_t fft-float)

   ; GL ES types
   (define GLchar      fft-char)
   (define GLenum      fft-unsigned-int)
   (define GLboolean   fft-unsigned-int)
   (define GLbitfield  fft-unsigned-int)
   (define GLbyte      khronos_int8_t)
   (define GLshort     fft-short)
   (define GLint       fft-int)
   (define GLsizei     fft-int)
   (define GLubyte     khronos_uint8_t)  (define GLubyte*  (fft* GLubyte))
   (define GLushort    fft-unsigned-int) (define GLushort* (fft* GLushort))
   (define GLuint      fft-unsigned-int) (define GLuint*   (fft* GLuint))   (define GLuint&  (fft& GLuint))
   (define GLfloat     khronos_float_t)  (define GLfloat*  (fft* GLfloat))
   (define GLclampf    khronos_float_t)
   (define GLfixed     khronos_int32_t)
   (define GLclampx    khronos_int32_t)

   (define GLintptr    khronos_intptr_t)
   (define GLsizeiptr  khronos_ssize_t)

   (define GLvoid fft-void)
   (define GLvoid* fft-void*)
)

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

         (define glGetString (GL type-string "glGetString" GLenum))
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
      (display-to stderr (fold string-append "" (list "Checking " extension " support..."))) ; debug info
      (let ((extensions (split (or
               (glGetString GL_EXTENSIONS)
               ; if no extensions - use empty string:
               ""))))
         (if (member extension extensions)
            (begin (print " ok.") #true)
            (begin (print " not found.") #false))))

))