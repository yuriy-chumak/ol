(define-library (lib openal)
 (import
  (otus lisp) (otus pinvoke)
  (owl parse) (lang sexp))

 (export
      alcOpenDevice
      alcCreateContext
      alcMakeContextCurrent

      alGenSources
      alGenBuffers

      alSourcei
      alBufferData

      alSourcePlay
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
   (define alcOpenDevice   (dlsym $ type-vptr "alcOpenDevice"  type-vptr))
   (define alcCreateContext (dlsym $ type-vptr "alcCreateContext" type-vptr type-vptr))
   (define alcMakeContextCurrent (dlsym $ type-vptr "alcMakeContextCurrent" type-vptr))

   (define alGenSources (dlsym $ type-vptr "alGenSources" type-int+ ALuint*))
   (define alGenBuffers (dlsym $ type-vptr "alGenBuffers" type-int+ ALuint*))

   (define alSourcei (dlsym $ type-void "alSourcei" type-int+ type-int+ type-int+))
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

(define source (vm:raw type-vector-raw '(0)))
(alGenSources 1 source)
(define buffer (vm:raw type-vector-raw '(0)))
(alGenBuffers 1 buffer)

(print "source id: " source)
(print "buffer id: " buffer)



      ; .snd parser:
      (define (syntax-fail pos info lst)
         (print "http parser fail: " info)
         (print ">>> " pos "-" (runes->string lst) " <<<")
         '(() (())))


      (define get-int32-big-endian
         (let-parses ((a get-byte)
                      (b get-byte)
                      (c get-byte)
                      (d get-byte))
            (+ (<< (+ (<< (+ (<< a 8) b) 8) c) 8) d)))


      (define snd-parser
         (let-parses (
               (header get-int32-big-endian)
               (data-offset get-int32-big-endian)
               (len get-int32-big-endian)
               (encoding get-int32-big-endian)
               (sample-frequency get-int32-big-endian)
               (num-channels get-int32-big-endian)
               (data (get-greedy* get-byte)))

            (tuple data-offset len encoding sample-frequency num-channels data))) ;(vm:raw type-vector-raw data))))


(define exp_lut (tuple 0 132 396 924 1980 4092 8316 16764))

(let ((file (file->exp-stream "waveform.snd" #f snd-parser syntax-fail)))
   (print "data offset: " (ref (car file) 1))
   (print "stream len: " (ref (car file) 2))
   (print "encoding: " (ref (car file) 3))
   (print "sample-frequency: " (ref (car file) 4))
   (print "num-channels: " (ref (car file) 5))
   (print (type (ref (car file) 6)))

   ; AU_ULAW_8:
   ; (bits-per-sample 16)

   (let ((mulaw2linear
      (map (lambda (mulawbyte)
         (let*((mulawbyte (bxor mulawbyte #xFF))
               (sign (band mulawbyte #x80))
               (exponent (band (>> mulawbyte 4) #x07))
               (mantissa (band mulawbyte #x0F))

               (sample (+
                          (ref exp_lut (+ exponent 1))
                          (<< mantissa (+ exponent 3)))))
            (if (eq? sign 0)
               sample
               (bor #x80 (- sample 1)))))
         (ref (car file) 6))))



         (let ((data (vm:raw type-vector-raw (reverse
                          (fold (lambda (state x)
                             (cons (band x #xFF)
                             (cons (>> x 8)
                             state))) #null mulaw2linear)))))

;             (print (vm:raw type-vector-raw buffer))
             (print "buffer id: " (ref buffer 0))
             ;(print "data: " data)
             (print "data size: " (size data))
             (print "len: " (ref (car file) 2))
             (print "frequency: " (ref (car file) 4))
             (alBufferData (ref buffer 0) AL_FORMAT_MONO16  data  (size data)  (ref (car file) 4))
         ;alSetData

         )
      #t)


   (print "source id: " (ref source 0))
   (alSourcePlay (ref source 0))
      ;...

;   ; we got codec 1, let's transform it!
;   ;mulaw2linear
;static int16_t
;mulaw2linear (uint8_t mulawbyte)
;  static const int16_t exp_lut[8] = {
;    0, 132, 396, 924, 1980, 4092, 8316, 16764
;  };
;  int16_t sign, exponent, mantissa, sample;
;  mulawbyte = ~mulawbyte;
;  sign = (mulawbyte & 0x80);
;  exponent = (mulawbyte >> 4) & 0x07;
;  mantissa = mulawbyte & 0x0F;
;  sample = exp_lut[exponent] + (mantissa << (exponent + 3));
;  if (sign != 0)
;    {
;      sample = -sample;
;    }
;  return sample;



)
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
