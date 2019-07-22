(define-library (lib openal)
 (import
  (otus lisp)
  (OpenAL version-1-1)
  (owl parse) (lang sexp))

 (export
   (exports (OpenAL version-1-1))

      ; High level methods
      al:decode-fd
      al:decode-file
   )

(begin
   (define AU_ULAW_8   1) ;  8-bit ISDN u-law
   (define AU_PCM_8    2) ;  8-bit linear PCM (signed)
   (define AU_PCM_16   3) ; 16-bit linear PCM (signed, big-endian)
   (define AU_PCM_24   4) ; 24-bit linear PCM
   (define AU_PCM_32   5) ; 32-bit linear PCM
   (define AU_FLOAT_32 6) ; 32-bit IEEE floating point
   (define AU_FLOAT_64 7) ; 64-bit IEEE floating point
   (define AU_ALAW_8  27) ;  8-bit ISDN a-law


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
         [data-offset data-size encoding sample-rate channels]))

   (define exp_lut [0 132 396 924 1980 4092 8316 16764])

   (define (al:decode-fd buffer fd)
      (snd-parser (port->bytestream fd)
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
                  (data (make-bytevector (* data-size 2))))

               ; let's prepare the data:
               ; todo:
               (case (ref file 3) ; encoding
                  ; mu-law
                  (AU_ULAW_8
                     (let loop ((i 0) (j 0) (in in))
                     (if (less? i data-size) ; sanity check
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
                                       (bxor (- sample 1) #xFFFF)))) ; binary -(short)sample

                                 (set-ref! data j (band sample #xFF))
                                 (set-ref! data (+ j 1) (>> sample 8))
                                 (loop (+ i 1) (+ j 2) (cdr in))))
                           (else ; function?
                              (loop i j (force in)))))))

                  ; a-law
                  (AU_ALAW_8
                     (let loop ((i 0) (j 0) (in in))
                     (if (less? i data-size) ; sanity check
                        (cond
                           ((null? in) in)
                           ((pair? in)
                              (let*((byte (bxor (car in) #x55))
                                    (sign (band byte #x80))
                                    (t (<< (band byte #x0F) 4))
                                    (seg (>> (band byte #x70) 4))
                                    (sample
                                       (case seg
                                          (0
                                             (+ t 8))
                                          (1
                                             (+ t #x108))
                                          (else
                                             (<< (+ t #x108) (- seg 1)))))
                                    (sample (if (eq? sign 0)
                                       sample
                                       (bxor (- sample 1) #xFFFF)))) ; binary -(short)sample

                                 (set-ref! data j (band sample #xFF))
                                 (set-ref! data (+ j 1) (>> sample 8))
                                 (loop (+ i 1) (+ j 2) (cdr in))))
                           (else ; function?
                              (loop i j (force in)))))))

                  ; pcm-8s
                  (AU_PCM_8
                     (let loop ((i 0) (in in))
                     (if (less? i data-size)
                        (cond
                           ((null? in) in)
                           ((pair? in)
                              (let*((byte (car in))
                                    (sample (+ byte 128)))

                                 (set-ref! data i (band sample #xFF)) ; band can be safely removed
                                 (loop (+ i 1) (cdr in))))
                           (else ; function?
                              (loop i (force in)))))))

                  (else
                     (runtime-error "Unknown snd format" (ref file 3))))
           
               ;(print data)
               ;(print (size data))
               ;(print (ref file 4))
      
               (alBufferData (list-ref buffer 0)
                  (case (ref file 5) ; channels count
                     (1 AL_FORMAT_MONO16)
                     (2 AL_FORMAT_STEREO16))
                  data  (size data)  (ref file 4))
            #t))
      
         ; fail
         (λ (pos info)
            (print "fail"))
         0))

   (define (al:decode-file buffer filename)
      (al:decode-fd buffer (open-input-file filename)))

))
