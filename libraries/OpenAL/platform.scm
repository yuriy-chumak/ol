(define-library (OpenAL platform)
   (version 1.0)
   (license MIT/LGPL3)
   (description
      "Platform-specific types and definitions for OpenAL")

(export
   (exports (otus ffi))

   ; AL types
   ALboolean
   ALbyte
   ALubyte
   ALshort
   ALushort
   ALint     ALint*   ALint&
   ALuint    ALuint*  ALuint&
   ALsizei
   ALenum
   ALfloat            ALfloat&
   ALdouble

   ALvoid    ALvoid*

   ; minimal required AL set
   alGetString
      AL_VENDOR
      AL_RENDERER
      AL_VERSION
      AL_EXTENSIONS

   ; AL helpers
   al:GetProcAddress  ; * ol internal
   alc:GetProcAddress ; * ol internal
   al:QueryExtension  ; * ol specific

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
   (setq AL AL_LIBRARY)

   ; -------------------------------------------------------------------------
   ; -- types
   (define ALboolean fft-char)
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
   (define ALfloat&  (fft& fft-float))
   (define ALdouble  fft-double) ; 64-bit IEEE754 floating-point

   (define ALvoid    fft-void)
   (define ALvoid*   type-vptr)

   ; -- ALC --
   (setq ALC AL)

   (define ALCdevice* type-vptr)
   (define ALCcontext* type-vptr)

   (define ALCboolean fft-char)
   (define ALCchar    fft-char)
      (define ALCchar* type-string)
   (define ALCbyte    fft-char)
   (define ALCubyte   fft-unsigned-char)
   (define ALCshort   fft-short)
   (define ALCushort  fft-unsigned-short)
   (define ALCint     fft-int)
      (define ALCint*  (fft* ALCint))
      (define ALCint&  (fft& ALCint))
   (define ALCuint    fft-unsigned-int)
      (define ALCuint*  (fft* ALCuint))
      (define ALCuint&  (fft& ALCuint))
   (define ALCsizei   fft-int)
   (define ALCenum    fft-int)
   (define ALCfloat   fft-float)
   (define ALCdouble  fft-double)

   (define ALCvoid    fft-void)
   (define ALCvoid*   type-vptr)

   ; -------------------------------------------------------------------------
   ; Strings AL_VENDOR and AL_RENDERER together uniquely specify a platform.
   ; They do not change from release to release.
   (define AL_VENDOR     #xB001)
   (define AL_RENDERER   #xB003)
   (define AL_VERSION    #xB002)
   (define AL_EXTENSIONS #xB004)
   (define ALC_EXTENSIONS #x1006)

   ; Minimal required AL/ALC function set
   (define alGetString (AL type-string "alGetString" ALenum))
   (define alcGetString (ALC type-string "alcGetString" ALCdevice* ALenum))
   (define alcGetCurrentContext (ALC ALCcontext* "alcGetCurrentContext"))
   (define alcGetContextsDevice (ALC ALCdevice* "alcGetContextsDevice" ALCcontext*))

   (setq alGetProcAddress (AL type-vptr "alGetProcAddress" type-string))
   (define (al:GetProcAddress type name . prototype)
      (let ((rtti (cons type prototype))
            (function (alGetProcAddress name)))
         (if function
            (lambda args
               (ffi function rtti args)))))

   (setq alcGetProcAddress (ALC type-vptr "alcGetProcAddress" type-string))
   (define (alc:GetProcAddress type name . prototype)
      (let ((rtti (cons type prototype))
            (function (alcGetProcAddress name)))
         (if function
            (lambda args
               (ffi function rtti args)))))

   (define (al:QueryExtension extension)
      (let ((extensions (c/ / (or ; split by space character
               (cond
                  ; ALC
                  ((and (>= (size extension) 4) (string-eq? (substring extension 0 4) "ALC_"))
                     (define context (alcGetCurrentContext))
                     (define device (alcGetContextsDevice context))
                     (alcGetString device ALC_EXTENSIONS))
                  ; all others
                  (else
                     (alGetString AL_EXTENSIONS)))
               "")))) ; use empty string if no extensions
         (if (member extension extensions)
            then (print-to stderr "Checking " extension " support... ok.") #true
            else (print-to stderr "Checking " extension " support... not found.") #false)))

))
