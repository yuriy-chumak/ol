(define-library (file glTF)
   (export
      read-glTF-file
      glTF-parser
   )
   (import
      (scheme base)
      (scheme file)
      (owl io)
      (owl ff) (owl string)
      (prefix (otus base64) base64:)
      (owl parse)
      (file json))

(begin
   (setq vref vector-ref)

   (define skip32 (let-parse* (
         (b0 byte) (b1 byte) (b2 byte) (b3 byte))
      0))

   (define glTF-parser
      (either
         ; binary glTF
         (let-parse* (
               ; 12-byte header
               (header (word "glTF" #t))
               (version skip32)
               (flen skip32)
               ; Chunk 0 (JSON)
               (clen skip32)
               (ctype (word "JSON" #t))
               (json json-parser)
               (spaces (greedy* (imm #\space)))
               ; Chunk 1 (BIN)
               (skips (lazy+ byte))
               (ctype (word "BIN\0" #t))
               (bin (greedy+ byte)) ) ; todo: read as bytevector
            ; replace buffers[0] with buffer data
            ; assert "(size buffers) == 1"
            (put json 'buffers (vector-map (lambda (buffer)
                  (if (buffer 'uri #f)
                     buffer
                     (put buffer 'buffer
                        (make-bytevector bin))))
               (json 'buffers))))
         ; text glTF
         json-parser))
         
   ; read and compile glTF file
   (define (read-glTF-file filename)
      ; read the stream
      (define glTF (call-with-input-file filename (lambda (port)
         (define data (try-parse glTF-parser (port->bytestream port) #f))
         (when data
            (car data)))))

      ; binary buffers
      (define buffers (vector-map (lambda (buffer)
            (define source (buffer 'uri #f))
            (if source
               (cond
                  ((m/^data:application\/octet-stream;base64,/ source)
                     (make-bytevector (base64:decode (substring source 37))))
                  (else
                     (file->bytevector source)))
            else
               (buffer 'buffer)))
         (glTF 'buffers)))

      (ff-replace glTF {
         'buffers buffers
      }))

))
