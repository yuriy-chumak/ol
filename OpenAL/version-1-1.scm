; OpenAL
;
; Version 1.1
; Published June 2005 (by authors)
;
; Version 1.0 Draft Edition
; Published June 2000 (by Loki Software)

(define-library (OpenAL version-1-1)
   (export
    AL_VERSION_1_0
    AL_VERSION_1_1

      AL_LIBRARY  ;internal variable

      ; AL base types
      ;cl_char     ;   signed  8-bit
      ;cl_uchar    ; unsigned  8-bit
      ;cl_short    ;   signed 16-bit
      ;cl_ushort   ; unsigned 16-bit
      ;cl_int      ;   signed 32-bit
      ;cl_uint     ; unsigned 32-bit
      ;cl_long     ;   signed 64-bit
      ;cl_ulong    ; unsigned 64-bit
      ;
      ;cl_half     ; unsigned 16-bit
      ;cl_float    ; floating 32-bit
      ;cl_double   ; floating 64-bit

      alGetError

      alcOpenDevice
      alcCreateContext
      alcMakeContextCurrent

      alGenSources
      alGenBuffers

      alSourcei
      alBufferData

      alSourcePlay

      ; Source:
      AL_BUFFER AL_LOOPING

   )
; ============================================================================
; == implementation ==========================================================
   (import
      (r5rs core) (owl math) (owl io) (owl string)
      (otus pinvoke)
      (owl interop) (owl list))

(begin
;   (define CL_VERSION_1_0 1)
;
;   (define type-int64 44)
;
;   (define cl_char   type-fix+)
;   (define cl_uchar  type-fix+)
;   (define cl_short  type-fix+)
;   (define cl_ushort type-fix+)
;   (define cl_int    type-int+)  (define cl_int*  type-vector-raw)
;   (define cl_uint   type-int+)  (define cl_uint* type-vector-raw)
;   (define cl_long   type-int64)
;   (define cl_ulong  type-int64)
;   (define cl_half   type-fix+) ;?
;   (define cl_float  type-float)
;   (define cl_double type-double)

; https://en.wikipedia.org/wiki/Uname
(define uname (syscall 63 #f #f #f))

(define win32? (string-ci=? (ref uname 1) "Windows"))
(define linux? (string-ci=? (ref uname 1) "Linux"))
(define apple? (string-ci=? (ref uname 1) "Darwin"))

(define AL_LIBRARY (c-string
   (cond
      (win32? "openal32")
      (linux? "libAL.so")
      ;"HP-UX"
      ;"SunOS"
      ;"Darwin"
      ;"FreeBSD"
      ;"CYGWIN_NT-5.2-WOW64"
      ;"MINGW32_NT-5.2"
      ;...
      (else   (runtime-error "Unknown platform" uname)))))

(define AL_VERSION_1_0 1)
(define AL_VERSION_1_1 1)

(define ALboolean type-fix+)
(define ALchar    type-fix+)
(define ALbyte    type-fix+)
(define ALubyte   type-fix+) ;unsigned
(define ALshort   type-fix+)
(define ALushort  type-fix+) ;unsigned
(define ALint     type-int+)
(define ALuint    type-int+) ;unsigned
(define ALuint*   type-vector-raw)
(define ALenum    type-int+) ; 32-bit
(define ALfloat   type-float)
(define ALdouble  type-double)
(define ALvoid    type-void)
(define ALvoid*   type-vptr)

(define $ (or
   (dlopen AL_LIBRARY)
   (runtime-error "Can't load OpenAL library")))

(define AL_NONE 0)
(define AL_FALSE 0)
(define AL_TRUE 1)

; AL_SOURCE_RELATIVE
; ...
(define AL_LOOPING #x1007)
; ...
;

   (define AU_ULAW_8 1)   ; 8-bit ISDN u-law
   (define AU_PCM_8  2)   ; 8-bit linear PCM (signed)
   (define AU_PCM_16 3)   ; 16-bit linear PCM (signed, big-endian)
   (define AU_PCM_24 4)   ; 24-bit linear PCM
   (define AU_PCM_32 5)   ; 32-bit linear PCM
   (define AU_FLOAT_32 6) ; 32-bit IEEE floating point
   (define AU_FLOAT_64 7) ; 64-bit IEEE floating point
   (define AU_ALAW_8 27)  ; 8-bit ISDN a-law


; ===================================================
   (define alGetError   (dlsym $ type-int+ "alGetError"))

   (define alcOpenDevice   (dlsym $ type-vptr "alcOpenDevice"  type-vptr))
   (define alcCreateContext (dlsym $ type-vptr "alcCreateContext" type-vptr type-vptr))
   (define alcMakeContextCurrent (dlsym $ type-int+ "alcMakeContextCurrent" type-vptr))

   (define alGenSources (dlsym $ type-vptr "alGenSources" type-int+ ALuint*))
   (define alGenBuffers (dlsym $ type-vptr "alGenBuffers" type-int+ ALuint*))

   (define alSourcei (dlsym $ type-void "alSourcei" type-int+ type-int+ type-int+))
     (define AL_BUFFER      #x1009)
   (define alBufferData (dlsym $ type-void "alBufferData" type-int+ type-int+ type-vector-raw type-int+ type-int+))

   (define alSourcePlay (dlsym $ type-void "alSourcePlay" type-int+))

))