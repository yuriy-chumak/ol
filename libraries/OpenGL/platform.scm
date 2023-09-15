(define-library (OpenGL platform)
   (version 1.2)
   (license MIT/LGPL3)
   (description
      "Platform-specific types and definitions for OpenGL")

(export
   (exports (otus ffi))

   ; GL types
   ; https://www.opengl.org/wiki/OpenGL_Type
   GLenum   GLenum*           ; unsigned 32-bit
   GLboolean GLboolean*       ; unsigned byte (GL_TRUE or GL_FALSE)
   GLbitfield                 ; unsigned 32-bit
   GLbyte   GLbyte*           ;   signed  8-bit
   GLshort  GLshort*          ;   signed 16-bit
   GLint    GLint*   GLint&   ;   signed 32-bit
   GLsizei  GLsizei*          ;   signed 32-bit
   GLubyte  GLubyte*          ; unsigned  8-bit
   GLushort GLushort*         ; unsigned 16-bit
   GLuint   GLuint*  GLuint&  ; unsigned 32-bit

   GLfloat  GLfloat*  ; floating 32-bit
   GLclampf           ; floating 32-bit (clamped to the range [0,1])
   GLdouble GLdouble* ; floating 64-bit
   GLclampd           ; floating 64-bit (clamped to the range [0,1])

   GLvoid   GLvoid*       ; void, void*

   ; minimal required GL functions set
   glGetString
      GL_VENDOR
      GL_RENDERER
      GL_VERSION
      GL_EXTENSIONS
   glHint
   glViewport

   ; WGL/GLX/CGL/EGL/... platform functions
   gl:GetProcAddress ; * ol internal
   gl:QueryExtension ; * ol specific

   GL_LIBRARY)

; ============================================================================
; == implementation ==========================================================
(import
   (otus lisp)
   (otus ffi))

; = OS DEPENDENT part ===============
; https://en.wikipedia.org/wiki/Uname
(cond-expand
   ; -=( Linux )=--------------------------------------
   (Linux
      (begin
         (define GL_LIBRARY (or
            (load-dynamic-library "libGL.so")
            (load-dynamic-library "libGL.so.1")))

         (setq GLX GL_LIBRARY)
         (setq GetProcAddress (GLX type-vptr "glXGetProcAddress" type-string)) ))
   ; -=( Windows )=--------------------------------------
   (Windows
      (begin
         (define GL_LIBRARY
            (load-dynamic-library "opengl32.dll"))

         (setq WGL GL_LIBRARY)
         (setq GetProcAddress (WGL type-vptr "wglGetProcAddress" type-string)) ))
   ; -=( Android )=-----------
   (Android
      (begin
         (define GL_LIBRARY
            (load-dynamic-library "libgl2es.so"))

         (setq GL2 GL_LIBRARY)
         (setq GetProcAddress (GL2 type-vptr "gl2GetProcAddress" type-string)) ))

         ;; (setq GL2 (dlopen "libgl2es.so"))
         ;; (print "GL2: " GL2)
         ;; (print "GetProcAddress: " GetProcAddress)
         ;; ;; ))
         ;; (define (GetProcAddress name)
         ;;    (dlsym GL2 (string-append name))) ))
   ; -=( WebGL )=-----------
   (Emscripten
      (begin
         (setq GL_LIBRARY
            (load-dynamic-library #false))
         (print "GL_LIBRARY: " GL_LIBRARY)

;;          ;; old gl4es hack: add "gl4es_" prefix to all opengl functions
;;          (define (GL_LIBRARY type name . prototype)
;;             (let ((rtti (cons type prototype))
;;                   (function (dlsym gl4es (string-append "gl4es_" name))))
;;                (if function
;;                   (lambda args
;;                      (ffi function rtti args)))))


         (setq GL2 GL_LIBRARY)
         (setq GetProcAddress (GL2 type-vptr "gl2GetProcAddress" type-string))
         (print "GetProcAddress: " GetProcAddress)
         
;;          ; unsupported
;;          (define (gl:CreateContext . args) #false)
;;          (define (gl:MakeCurrent . args) #false)
;;          (define (gl:SwapBuffers . args) #false)
   ))

;;    ; -=( macOS )=--------------------------------------
;;    (Darwin
;;       (begin
;;          (define GL_LIBRARY (load-dynamic-library "/System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib"))

;;          (setq THIS (load-dynamic-library #f))
;;          (setq NSLookupAndBindSymbol (THIS fft-void* "NSLookupAndBindSymbol"))

;;          (setq GetProcAddress (lambda (name)
;;             (NSLookupAndBindSymbol (string-append "_" name))))

;;          (define (gl:CreateContext . args)
;;             (runtime-error "CreateContext is not supported, use libSDL instead" #null))
;;          (define (gl:MakeCurrent . args)
;;             (runtime-error "MakeCurrent is not supported, use libSDL instead" #null))
;;          (define (gl:SwapBuffers . args)
;;             (runtime-error "SwapBuffers is not supported, use libSDL instead" #null))
;;    ))

   ; -=( Unknown )=--
   ;"HP-UX"
   ;"SunOS"
   ;"FreeBSD"
   ;"CYGWIN_NT-5.2-WOW64"
   ;"MINGW32_NT-5.2"
   ;...
   (else (begin
      (runtime-error "Unsupported platform" (uname)))))

; ============================================================================
(begin
   (define GL GL_LIBRARY)

   ; -------------------------------------------------------------------------
   ; -- types
   (define GLvoid   fft-void)
   (define GLvoid*  fft-void*)

   (define GLenum     fft-unsigned-int)
   (define GLboolean  fft-unsigned-char) ;?
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
   (define GLenum*    (fft* GLenum))
   (define GLbyte*    (fft* GLbyte))
   (define GLshort*   (fft* GLshort))
   (define GLushort*  (fft* GLushort))
   (define GLint*     (fft* GLint))
   (define GLuint*    (fft* GLuint))
   (define GLfloat*   (fft* GLfloat))
   (define GLdouble*  (fft* GLdouble))
   (define GLsizei*   (fft* GLsizei))

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

   ; Some basic functions
   (define glGetString (GL type-string "glGetString" fft-unsigned-int))
   (define glHint (GL fft-void "glHint" GLenum GLenum))
   (define glViewport (GL GLvoid "glViewport" GLint GLint GLsizei GLsizei))
)

; ----------------------------------
; поддержка расширений (включая GLX):
(cond-expand
   (Linux
      (begin
         (setq X11 (or (load-dynamic-library "libX11.so")
                       (load-dynamic-library "libX11.so.6")))
         (setq XOpenDisplay   (X11 type-vptr "XOpenDisplay" type-string))
         (setq XDefaultScreen (X11 fft-int "XDefaultScreen" type-vptr))
         (define glXQueryExtensionsString
            (let*((glXQueryExtensionsString (GLX type-string "glXQueryExtensionsString" type-vptr fft-int))
                  (display (XOpenDisplay #false))
                  ; WSL workaround: XDefaultScreen can be core dumped if no display
                  (screen  (if display (XDefaultScreen display))))
               (if glXQueryExtensionsString
                  (case-lambda
                     ((display screen)
                        (glXQueryExtensionsString display screen))
                     (()
                        (glXQueryExtensionsString display screen)))
                  (lambda args #false))))))
   (else
      (begin
         (define glXQueryExtensionsString (lambda args #false)))))

; public interface
(begin

   (define (gl:GetProcAddress type name . prototype)
      (let ((rtti (cons type prototype))
            (function (GetProcAddress name)))
         (if function
            (lambda args
               (ffi function rtti args)))))

   (define (gl:QueryExtension extension)
      ;; (display-to stderr (string-append
      ;;    "Checking " extension " support..."))
      (let ((extensions (c/ / (or ; split by space character
               (cond
                  ; GLX, Linux
                  ((and (>= (size extension) 4) (string-eq? (substring extension 0 4) "GLX_"))
                     (glXQueryExtensionsString))
                  ; all others
                  (else
                     (glGetString GL_EXTENSIONS)))
               ; if no extensions - use empty string:
               ""))))
         (if (member extension extensions)
            (begin (print-to stderr "Checking " extension " support... ok.") #true)
            (begin (print-to stderr "Checking " extension " support... not found.") #false))))

))
