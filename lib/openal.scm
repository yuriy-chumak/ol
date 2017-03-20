(define-library (lib openal)
 (import
  (otus lisp) (otus pinvoke)
  (owl parse) (lang sexp) (owl lazy))

 (export
      AL_VERSION_1_0 AL_VERSION_1_1
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

      ; High level methods
      al:decode-fd
   )

(begin
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


   ; al:decode-fd
   (define (syntax-fail pos info lst)
      (print "snd heared fail: " info)
      (print ">>> " pos "-" (runes->string lst) " <<<")
      '(() (())))


   (define get-int32-big-endian
      (let-parses ((a get-byte)
                   (b get-byte)
                   (c get-byte)
                   (d get-byte))
         (+ (<< a 24)
            (<< b 16)
            (<< c  8)
            d)))


   ; .snd parser:
   (define snd-parser
      ; http://sox.sourceforge.net/AudioFormats-11.html
      ; 11.2 The NeXT/Sun audio file format
      (let-parses (
            (header      get-int32-big-endian) ; ".snd", #x2E736E64
            ; todo: add parsing depend of field "header"
            (data-offset get-int32-big-endian)
            (data-size   get-int32-big-endian)
            (encoding    get-int32-big-endian)
            (sample-rate get-int32-big-endian)
            (channels    get-int32-big-endian))
         (tuple data-offset data-size encoding sample-rate channels)))

   (define exp_lut (tuple 0 132 396 924 1980 4092 8316 16764))

   (define AL_FORMAT_MONO8    #x1100)
   (define AL_FORMAT_MONO16   #x1101)
   (define AL_FORMAT_STEREO8  #x1102)
   (define AL_FORMAT_STEREO16 #x1103)


   (define (al:decode-fd buffer fd)
      (snd-parser (port->byte-stream fd)
         ; ok
         (lambda (in backtrack file pos)
            (print "data offset: " (ref file 1))
            (print "stream len: "  (ref file 2))
            (print "encoding: "    (ref file 3))
            (print "sample-rate: " (ref file 4))
            (print "channels: "    (ref file 5))
      
            ; if encoding = AU_ULAW_8 then bits-per-sample = 16 and codec = _alutCodecULaw
      
            (let*((data-offset (ref file 1))
                  (data-size   (ref file 2))
                  (in (ldrop in (- data-offset 24)))
                  (data (vm:raw type-vector-raw (* data-size 2))))
      
               ; let's prepare the data:
               ; todo:
               ;(case (ref file 3) ; encoding
               ;   (1
      
               ; mulaw2linear:
               (let loop ((i 0) (j 0) (in in))
               (if (less? i data-size)
                  (cond
                     ((null? in) in)
                     ((pair? in)
                        (let*((byte (bxor (car in) #xFF))  ; neg byte
                              (sign (band byte #x80))
                              (exponent (band (>> byte 4) #x07))
                              (mantissa (band byte #x0F))
                              (sample (+
                                 (ref exp_lut (+ exponent 1))
                                 (<< mantissa (+ exponent 3))))
                              (sample (if (eq? sign 0)
                                 sample
                                 (bxor (- sample 1 )#xFFFF)))) ; binary -(short)sample
      
                           (set-ref! data j (band sample #xFF))
                           (set-ref! data (+ j 1) (>> sample 8))
                           (loop (+ i 1) (+ j 2) (cdr in))))
                     (else ; function?
                        (loop i j (force in))))))
           
               ;(print data)
               (print (size data))
               (print (ref file 4))
      
               (alBufferData (ref buffer 0)
                  (case (ref file 5) ; channels count
                     (1 AL_FORMAT_MONO16)
                     (2 AL_FORMAT_STEREO16))
                  data  (size data)  (ref file 4))
            #t))
      
         ; fail
         (λ (pos info)
            (print "fail"))
         0))

))

; =================================================================================

(import (lib openal))
(import (owl parse) (lang sexp) (owl lazy))

(define device (alcOpenDevice null))
(define context (alcCreateContext device null))
(alcMakeContextCurrent context)

(define buffer (vm:raw type-vector-raw 4))
(alGenBuffers 1 buffer)
(print "buffer id: " buffer)

(al:decode-fd buffer (open-input-file "cave.snd"))

(define source (vm:raw type-vector-raw 4))
(alGenSources 1 source)
(print "source id: " source)

(alSourcei (ref source 0) AL_BUFFER (ref buffer 0))
(alSourcei (ref source 0) AL_LOOPING 1)

(alSourcePlay (ref source 0))
(print "play error: " (alGetError))

(display "> ")
(read)
