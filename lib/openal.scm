(define-library (lib openal)
 (import
  (otus lisp) (otus pinvoke)
  (owl parse) (lang sexp))

 (export
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
      AL_BUFFER
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

(define $ (or
   (dlopen AL_LIBRARY)
   (runtime-error "Can't load OpenAL library")))

   (define ALuint* type-vector-raw)

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

(define AL_FORMAT_MONO8    #x1100)
(define AL_FORMAT_MONO16   #x1101)
(define AL_FORMAT_STEREO8  #x1102)
(define AL_FORMAT_STEREO16 #x1103)

; =================================================================================

(import (lib openal))
(import (owl parse) (lang sexp))

(define device (alcOpenDevice null))
(define context (alcCreateContext device null))
(alcMakeContextCurrent context)

(define buffer (vm:raw type-vector-raw 4))
(alGenBuffers 1 buffer)
(print "buffer id: " buffer)


      ; .snd parser:
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
;(define (+1 x) (+ x 1))

(snd-parser (port->byte-stream (open-input-file "waveform.snd"))
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
            ;in (ldrop data (- data-offset (* 4 5)))
            (data (vm:raw type-vector-raw (* data-size 2))))

         ; let's prepare the data:
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

                     ;(print "byte: " byte)
                     ;(print "sign: " sign)
                     ;(print "exponent: " exponent)
                     ;(print "mantissa: " mantissa)
                     ;(print "sample: " sample)

                     (set-ref! data j (band sample #xFF))
                     (set-ref! data (+ j 1) (>> sample 8))
                     (loop (+ i 1) (+ j 2) (cdr in))))
               (else ; function?
                  (loop i j (force in))))))
     
         ;(print data)
         (print (size data))
         (print (ref file 4))

         (alBufferData (ref buffer 0) AL_FORMAT_MONO16  data  (size data)  (ref file 4))
         (print "buffer-data error: " (alGetError))
      #t))

   ; fail
   (λ (pos info)
      (print "fail"))
   0)

(define source (vm:raw type-vector-raw 4))
(alGenSources 1 source)
(print "source id: " source)

(alSourcei (ref source 0) AL_BUFFER (ref buffer 0))

(alSourcePlay (ref source 0))
(print "play error: " (alGetError))

(display "> ")
(read)

;
;
;(let ((file (open-input-file "waveform.lisp")))
;(let*((dataOffset (
  ; loadFile
  ; will load "loadAUFile"
;  ALvoid *data;
;  size_t length;
;  ALint numChannels;
;  ALint bitsPerSample;
;  ALfloat sampleFrequency;


;  Int32BigEndian dataOffset;    /* byte offset to data part, minimum 24 */
;  Int32BigEndian len;           /* number of bytes in the data part, -1 = not known */
;  Int32BigEndian encoding;      /* encoding of the data part, see AUEncoding */
;  Int32BigEndian sampleFrequency;       /* number of samples per second */
;  Int32BigEndian numChannels;   /* number of interleaved channels */
;  size_t length;
;  Codec *codec;
;  char *data;
;  ALint bitsPerSample;
;
;  if (!_alutInputStreamReadInt32BE (stream, &dataOffset) ||
;      !_alutInputStreamReadInt32BE (stream, &len) ||
;      !_alutInputStreamReadInt32BE (stream, &encoding) ||
;      !_alutInputStreamReadInt32BE (stream, &sampleFrequency) ||
;      !_alutInputStreamReadInt32BE (stream, &numChannels))
;    {
;      return AL_FALSE;
;    }



;(print (ref buffer 0))


;stream = _alutInputStreamConstructFromFile
;return _alutCreateBufferFromInputStream(stream)




;(read)
