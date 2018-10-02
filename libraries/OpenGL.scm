; Platform independent OpenGL Library part
(define-library (OpenGL)
(export

   ; GL types
   ; https://www.opengl.org/wiki/OpenGL_Type
   GLenum                     ; unsigned 32-bit
   GLboolean GLboolean*       ; unsigned byte (GL_TRUE or GL_FALSE)
   GLbitfield                 ; unsigned 32-bit
   GLbyte                     ;   signed  8-bit
   GLshort                    ;   signed 16-bit
   GLint    GLint*   GLint&   ;   signed 32-bit
   GLsizei                    ;   signed 32-bit
   GLubyte  GLubyte*          ; unsigned  8-bit
   GLushort GLshort*          ; unsigned 16-bit
   GLuint   GLuint*  GLuint&  ; unsigned 32-bit

   GLfloat  GLfloat*  ; floating 32-bit
   GLclampf           ; floating 32-bit (clamped to the range [0,1])
   GLdouble GLdouble* ; floating 64-bit
   GLclampd           ; floating 64-bit (clamped to the range [0,1])

   GLvoid   GLvoid*       ; void, void*

   ; minimal GL function set
   glGetString
      GL_VENDOR
      GL_RENDERER
      GL_VERSION
      GL_EXTENSIONS

   ; WGL/GLX/CGL/EGL/... universal functions
   gl:GetProcAddress

   gl:CreateContext ; context creation
   gl:MakeCurrent
   gl:SwapBuffers
   gl:QueryExtension

   ; internal variables
   GL GLX WGL GDI EGL
   GL_LIBRARY ; deprecated

   (exports (otus lisp))
   (exports (otus ffi)))

; ============================================================================
; == implementation ==========================================================
(import (otus lisp) (otus ffi))

(begin
   ; https://en.wikipedia.org/wiki/Uname
   (setq OS (ref (uname) 1))

   (setq win32? (string-ci=? OS "Windows"))
   (setq linux? (string-ci=? OS "Linux"))
   (setq apple? (string-ci=? OS "Darwin"))

   (setq GL_LIBRARY (load-dynamic-library
      (cond
         (win32? "opengl32")
         (linux? "libGL.so.1")
         ;"HP-UX"
         ;"SunOS"
         ;"Darwin"
         ;"FreeBSD"
         ;"CYGWIN_NT-5.2-WOW64"
         ;"MINGW32_NT-5.2"
         ;...
         (else
            (runtime-error "Unsupported platform" OS)))))

   (define GL GL_LIBRARY)

   (define GLX (if linux? GL_LIBRARY))
   (define WGL (if win32? GL_LIBRARY))
   (define GDI (if win32? (load-dynamic-library "gdi32.dll")))
   (define EGL #false) ;TODO

   ; -------------------------------------------------------------------------
   ; -- types
   (define GLvoid   fft-void)
   (define GLvoid*  fft-void*)

   (define GLenum     fft-unsigned-int)
   (define GLboolean  fft-unsigned-char)
   (define GLbitfield fft-unsigned-int)

   (define GLbyte   fft-signed-char)
   (define GLshort  fft-short)
   (define GLint    fft-int)
   (define GLsizei  fft-int)
   (define GLubyte  fft-unsigned-char)
   (define GLushort fft-unsigned-short)
   (define GLuint   fft-unsigned-int)

   (define GLfloat  fft-float)
   (define GLclampf fft-float)
   (define GLdouble fft-double)
   (define GLclampd fft-double)

   (define GLubyte* type-string) ; todo: ?

   ; pointers
   (define GLboolean* (fft* GLboolean))
   (define GLshort*   (fft* GLshort))
   (define GLint*     (fft* GLint))
   (define GLuint*    (fft* GLuint))
   (define GLfloat*   (fft* GLfloat))
   (define GLdouble*  (fft* GLdouble))

   ; references
   (define GLint&     (fft& GLint))
   (define GLuint&    (fft& GLuint))
   ; -------------------------------------------------------------------------

   ; Strings GL_VENDOR and GL_RENDERER together uniquely specify a platform.
   ; They do not change from release to release and    ; should be used by platform-recognition algorithms.
   (define GL_VENDOR     #x1F00)
   (define GL_RENDERER   #x1F01)
   (define GL_VERSION    #x1F02)
   (define GL_EXTENSIONS #x1F03)
   (define glGetString (GL type-string "glGetString" fft-unsigned-int))

; -------------------------------------------------------------------------
; WGL context creation https://www.GL.org/wiki/Creating_an_OpenGL_Context_(WGL)
; GLX context creation https://www.GL.org/wiki/Tutorial:_OpenGL_3.0_Context_Creation_(GLX)

; поддержка расширений
(setq GetProcAddress (cond ; internal function
   (win32? (WGL type-vptr "wglGetProcAddress" type-string))
   (linux? (GLX type-vptr "glXGetProcAddress" type-string))
   (else (lambda args #false)))) ; silently return #false

(define (gl:GetProcAddress type name . prototype)
   (let ((rtty (cons type prototype))
         (function (GetProcAddress (c-string name))))
      (if function
         (lambda args
            (exec ffi function rtty args)))))

; нужные функции платформ для создания/уничтожения контекста
(define Display* type-vptr)
(define XVisualInfo* type-vptr)
;glXChooseVisual      ChoosePixelFormat
;glXCopyContext       wglCopyContext

;glXCreateContext     wglCreateContext
(define gl:CreateContext (cond
   (win32? (WGL type-vptr "wglCreateContext" fft-void*))
   (linux? (GLX type-vptr "glXCreateContext" Display* XVisualInfo* fft-void* fft-int))))
   ;apple? (GL fft-void* "CGLCreateContext" ...)

(define gl:MakeCurrent (cond
   (win32? (WGL fft-int "wglMakeCurrent" fft-void* fft-void*))
   (linux? (GLX fft-int "glXMakeCurrent" fft-void* fft-void* fft-void*))))


(define gl:SwapBuffers (cond
   (win32?
      (let ((SwapBuffers (GDI fft-int "SwapBuffers"    fft-void*)))
         (lambda (context)
            (SwapBuffers (ref context 1)))))
   (linux?
      (let ((SwapBuffers (GLX type-vptr "glXSwapBuffers" fft-void* fft-void*)))
         (lambda (context)
            (SwapBuffers (ref context 1) (ref context 3)))))
   (else   (runtime-error "SwapBuffers: Unknown platform" OS))))

;(define gl:GetString glGetString)

; поддержка расширений (включая GLX):
(import (owl string))

(define (gl:QueryExtension extension)
   (let ((extensions
            (cond
               ; GLX, Linux
               ((and (> (size extension) 3) (string-eq? (substring extension 0 4) "GLX_"))
                  (let ((libX11 (load-dynamic-library "libX11.so.6")))
                  (let ((XOpenDisplay  (if libX11 (libX11 type-vptr "XOpenDisplay" type-string)))
                        (XDefaultScreen(if libX11 (libX11 fft-int "XDefaultScreen" type-vptr)))
                        (glXQueryExtensionsString(if GLX (GLX type-string "glXQueryExtensionsString" type-vptr fft-int))))
                  (unless glXQueryExtensionsString
                     "-" ; no extensions list available
                     (let*((display (XOpenDisplay #false))
                           (screen  (XDefaultScreen display)))
                        (glXQueryExtensionsString display screen))))))
               ; WGL, Windows
               ; (( tbd.
               (else
                  (or (glGetString GL_EXTENSIONS) "-")))))

   (let ((string (append '(#\space) (string->bytes extensions) '(#\space)))
         (substr (append '(#\space) (string->bytes extension) '(#\space))))
   (for-each (λ (s) (display-to stderr s)) (list "Checking " extension " support...")) ; debug info
   (if ; fasl manual findstr implementation
      (let iter ((string string))
         (or
            (let loop ((one string) (two substr))
               (if (null? two)
                  #true
                  (if (not (null? one))
                     (if (eq? (car one) (car two))
                        (loop (cdr one) (cdr two))))))
            (if (not (null? string))
               (iter (cdr string)))))
      (begin (print " ok.") #true)
      (begin (print " not found.") #false)))))


(define gl:ExtensionSupported? gl:QueryExtension)
))
