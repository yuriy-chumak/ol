#!/usr/bin/ol

(define-library (lib openal)
 (import
  (otus lisp)
  (OpenAL version-1-1)
  (otus pinvoke)
  (owl parse) (lang sexp) (owl lazy))

 (export
    (exports (OpenAL version-1-1))

      ; High level methods
      al:decode-fd
   )

(begin

   ; al:decode-fd
   (define (invalid-format pos info lst)
      (print "snd heared fail: " info)
      (print ">>> " pos "-" (runes->string lst) " <<<")
      #f) ;'(() (())))


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

(print "OpenAL version: " (alGetString AL_VERSION))
(print "OpenAL vendor: " (alGetString AL_VENDOR))
(print "OpenAL renderer: " (alGetString AL_RENDERER))
;(print (alGetString AL_EXTENSIONS))


(define buffer (vm:raw type-vector-raw 4))
(alGenBuffers 1 buffer)
(print "buffer id: " buffer)

(al:decode-fd buffer (open-input-file "waveform.snd"))

(define source (vm:raw type-vector-raw 4))
(alGenSources 1 source)
(print "source id: " source)

(alSourcei (ref source 0) AL_BUFFER (ref buffer 0))
(alSourcei (ref source 0) AL_LOOPING 1)

(alSourcePlay (ref source 0))
(print "play error: " (alGetError))

(display "> ")
(read)
