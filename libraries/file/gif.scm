(define-library (file gif)
(import
   (otus lisp)
   (file parser))

(export
   gif-parser
   read-gif-file
   read-gif-stream
   
   gif->rgb24)

(begin

   (define read-le2
      (let-parse* (
            (l byte)
            (h byte))
         (+ l (<< h 8))))

   (define gif-parser
      (let-parse* (
            ; header:
            (? (get-word "GIF" 'gif))
            (? (any-of
                  (get-word "87a" 'a87)
                  (get-word "89a" 'a89)))
            (width read-le2)
            (height read-le2)
            ; flags:
            (fdsz byte)
            ; Global Color Table Flag = (fdsz >> 7) & 1
            ; Color Resolution        = (fdsz >> 4) & 7
            (color-resolution (epsilon (+ (band (>> fdsz 4) #b111) 1)))
            ; Sort Flag               = (fdsz >> 3) & 1
            ; Global Color Table Size = (fdsz     ) & 7
            (color-table-size (epsilon (<< 1 (+ (band fdsz #b111) 1))))

            ; additional flags
            (background-color byte)
            (pixel-aspect-ratio byte) ; Aspect Ratio = (Pixel Aspect Ratio + 15) / 64

            ; color table
            (color-table (times color-table-size
                  (let-parse* (
                        (r byte) (g byte) (b byte))
                     [r g b])))

            ; extensions
            (extensions (greedy* (let-parse* (
                  (? (get-imm #\!)) ; Extension Introducer
                  (block (any-of
                        ; Application Extension
                        (let-parse* (
                              (mark (imm #xFF))    ; Extension Label
                              (block-size byte)    ; (imm 11)
                              (id (times 8 byte))
                              (aac (times 3 byte))
                              (sub-blocks (greedy* ; Data Sub-blocks
                                 (let-parse* (
                                       (block-size (byte-if (lambda (x) (> x 0))))
                                       (block (times block-size byte)))
                                    block)
                                 ))
                              (? (imm 0)) ) ; Block Terminator
                           {
                              'label "Application Extension"
                              'id (list->string id)
                           })
                        ; Graphic Control Extension
                        (let-parse* (
                              (mark (imm #xF9))    ; Extension Label
                              (block-size byte)

                              (? (times block-size byte))
                              (? (imm 0)) ) ; Block Terminator
                           {
                              'label "Graphic Control Extension"
                           })
                     ; ...
                  )))
               block)))

            ; first frame
            (image-separator (imm #\,))
            (x read-le2) ; Start of image from the left side of the screen
            (y read-le2) ; Start of image from the top of the screen
            (w read-le2) ; Width
            (h read-le2) ; Height
            (mmiip byte) ; TODO: support all fields
            (local-color-map (if (eq? (band mmiip #x80) #x80)
                  (times (<< 1 (+ (band mmiip #b111) 1)) ; local color map size
                        (let-parse* (
                              (r byte) (g byte) (b byte))
                           [r g b]))
                  (epsilon #false)))

            (key-size byte)
            (data (greedy+ (let-parse* (
                     (block-size (byte-if (lambda (x) (> x 0))))
                     (block (times block-size byte)))
                  block)))
            (? (imm 0))

            (? (imm #\;)) )
      {
         'file 'gif
         'width width
         'height height
         'fdsz fdsz
         'color-resolution color-resolution
         'background-color background-color

         'color-table (list->vector color-table)

         'mmiip mmiip
         'x x 'y y 'w w 'h h
         'local-color-map (list->vector local-color-map)
         'key-size key-size
         'data data
      }))

   (define (read-gif-stream stream)
      (when stream
         (define gif (try-parse gif-parser stream #f))
         (if gif (car gif))))

   (define (read-gif-port port)
      (when port
         (read-gif-stream (force (port->bytestream port)))))

   (define (read-gif-file filename)
      (read-gif-port
         (if (equal? filename "-")
            stdin
            (open-input-file filename)))) ; note: no need to close port


   (define (gif->rgb24 gif)
      (define key-size (gif 'key-size))

      (define clear (<< 1 key-size)) ; clear code
      (define stop (+ clear 1))

      ; 1. generate basic codes table
      (define dict (fold (lambda (ff key)
            (put ff key (list key)))
         {} (iota clear 0)))
      (define basic-dict dict)

      ; 2. convert raw byte stream into vector array
      (define stream (foldr (lambda (subdata stream)
                           (foldr (lambda (byte stream)
                                 (+ (<< stream 8) byte))
                           stream subdata))
                        0 (gif 'data)))

      ; 3. decode color data
      (define indices
      (let loop (
            (dict {}) (top (+ clear 2))
            (stream stream)
            (old #f) (out '()))

         (define width (ilog 2 (++ top)))
         (define mask (- (<< 1 width) 1))
         (define code (band stream mask))

         (define str (dict code #f))

         (cond
            ((= code clear) ; restart dictionary
               (loop basic-dict (+ clear 2) (>> stream width) #f out))
            ((= code stop)  ; decoding ended
               out)
            (old ; todo: cache (last old) and (last str)
               (if str
               then
                  (define new (cons (last str) old))
                  (loop (put dict top new)
                     (+ top 1)
                     (>> stream width)
                     str (cons str out))
               else
                  (define new (cons (last old) old))
                  (loop (put dict top new)
                     (+ top 1)
                     (>> stream width)
                     new (cons new out)) ))
            (else
               (loop dict top (>> stream width) str (cons str out)) ))))

      ; indices became in reversed order and in two levels,
      ; 012345 image represented as '((5) (4 3 2) (1) (0))
      (define bytes (make-bytevector (* (gif 'width) (gif 'height) 3)))
      (define colors (or (gif 'local-color-map #f) (gif 'color-table)))
      (let loop ((i (size bytes)) (indices indices))
         (unless (null? indices)
            (let subloop ((i i) (subindices (car indices)))
               (if (null? subindices)
                  (loop i (cdr indices))
               else
                  (define color (vector-ref colors (car subindices)))
                  (set-ref! bytes (- i 3) (ref color 1))
                  (set-ref! bytes (- i 2) (ref color 2))
                  (set-ref! bytes (- i 1) (ref color 3))
                  (subloop (- i 3) (cdr subindices))))))

      bytes)

))
