(define-library (OpenAL platform)
   (version 1.0)
   (license MIT/LGPL3)
   (description
      "Platform-specific types and definitions for OpenAL")

(export
   (exports (otus ffi))

   ; AL types
   ALboolean
   ALchar    ALchar*
   ALbyte
   ALubyte
   ALshort
   ALushort
   ALint     ALint*   ALint&
   ALuint    ALuint*  ALuint&
   ALsizei
   ALenum
   ALfloat
   ALdouble

   ALvoid
   ALvoid*

   ; minimal required AL functions set
   alGetString
      AL_VENDOR
      AL_RENDERER
      AL_VERSION
      AL_EXTENSIONS

   ; WGL/GLX/CGL/EGL/... platform functions
   al:GetProcAddress ; * ol internal
   al:QueryExtension ; * ol specific

   AL_LIBRARY)

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
         (define AL_LIBRARY (or
            (load-dynamic-library "libopenal.so")
            (load-dynamic-library "libopenal.so.1"))) ))
   ; -=( Windows )=--------------------------------------
   (Windows
      (begin
         (define AL_LIBRARY
            (load-dynamic-library "openal32.dll")) ))
   ;; ; -=( Android )=-----------
   ;; Android
   ;; Emscripten
   ;; Darwin

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
   (define AL AL_LIBRARY)

   ; -------------------------------------------------------------------------
   ; -- types
   (define ALboolean fft-char)
   (define ALchar    fft-char)  (define  ALchar* type-string)
   (define ALbyte    fft-char)
   (define ALubyte   fft-unsigned-char)
   (define ALshort   fft-short)
   (define ALushort  fft-unsigned-short)
   (define ALint     fft-int)
      (define ALint*  (fft* ALint))
      (define ALint&  (fft& ALint))
   (define ALuint    fft-unsigned-int)
      (define ALuint* (fft* ALuint))
      (define ALuint& (fft& ALuint))
   (define ALsizei   fft-int)
   (define ALenum    fft-int)
   (define ALfloat   fft-float)  ; 32-bit IEEE754 floating-point
   (define ALdouble  fft-double) ; 64-bit IEEE754 floating-point

   (define ALvoid    fft-void)
   (define ALvoid*   type-vptr)

   (setq GetProcAddress (AL type-vptr "alGetProcAddress" type-string))

   ; -------------------------------------------------------------------------

   ; Strings AL_VENDOR and AL_RENDERER together uniquely specify a platform.
   ; They do not change from release to release.
   (define AL_VENDOR     #xB001)
   (define AL_RENDERER   #xB003)
   (define AL_VERSION    #xB002)
   (define AL_EXTENSIONS #xB004)

   ; Some basic functions
   (define alGetString (AL ALchar* "alGetString" ALenum))

   (define (al:GetProcAddress type name . prototype)
      (let ((rtti (cons type prototype))
            (function (GetProcAddress name)))
         (if function
            (lambda args
               (ffi function rtti args)))))

   (define (al:QueryExtension extension)
      (let ((extensions (c/ / (or ; split by space character
                  (alGetString AL_EXTENSIONS)
                  "")))) ; if no extensions - use empty string
         (if (member extension extensions)
            then (print-to stderr "Checking " extension " support... ok.") #true
            else (print-to stderr "Checking " extension " support... not found.") #false)))

))
