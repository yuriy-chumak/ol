(define-library (otus inflate)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (zlib inflate deflate rfc-1951))
   (description "
      RFC-1951 stream library (only inflate algorithm).")

(import
   (otus lisp))

(export
   inflate)

(begin

; https://www.nayuki.io/page/unspecified-edge-cases-in-the-deflate-standard

; return n bits from input binary stream
(define (bits n hold)
   (let loop ((hold hold))
      (vector-apply hold (lambda (v i l)
         (cond
            ((null? l)
               (print-to stderr "END OF STREAM"))
            ((pair? l)
               (if (not (less? i n))
                  (values (band v (- (<< 1 n) 1)) (vector (>> v n) (- i n) l))
                  ; если же не хватает бит, втянем из потока
                  (loop (vector
                     (bor v (<< (car l) i))
                     (+ i 8)
                     (cdr l)))))
            (else
               (loop (vector v i (l)))))))))

; code - huffman table (from make-huffman-table), hold - binary stream
; декодер входного потока по таблице хафмана
(define (huffman codes hold)
   (let loop ((code 1) (hold hold))
      (let*((bit hold (bits 1 hold))
            (code (bor (<< code 1) bit))
            (value (getf codes code))) ; todo: (codes code)
         (if value
            (values value hold)
            (loop code hold)))))

; конвертируем массив длин в таблицу хафмана
(define (make-huffman-table lengths)
   (let loop  ((codes #empty)
               (pairs (sort (lambda (a b) (< (car a) (car b))) ; отсортированный и
                  (filter (lambda (a) (not (eq? (car a) 0)))   ; отфильтрованный массив
                     (map (lambda (a b) (cons a b))            ; ( длина кода хаффмана . кодируемое ею значение )
                        (bytevector->list lengths) (iota (size lengths))))))
               (code 0) (len 0))
      (if (null? pairs)
         codes ; вернем коды
         (let*((nlen (caar pairs))
               (code (<< (+ code 1) (- nlen len))))
            (loop (put codes code (cdar pairs))
                  (cdr pairs) code nlen)))))

; decode encoded header lenthgs
(define (decode-huffman-lengths length hold codes)
   (define lengths (make-bytevector length 0))
   (let loop ((n 0) (hold hold) (len 0)) ; len is "previous code"
      (if (less? n length)
         (let*((code hold (huffman codes hold)))
            (cond
               ((less? code 16) ; 0..15
                  (set-ref! lengths n code)
                  (loop (+ n 1) hold code))
               ((less? code 17) ; 16
                  (let*((count hold (bits 2 hold))
                        (count (+ count 3)))
                     (for-each (lambda (i) (set-ref! lengths i len)) (iota count n))
                     (loop (+ n count) hold len)))
               ((less? code 18) ; 17
                  (let*((count hold (bits 3 hold))
                        (count (+ count 3)))
                     (loop (+ n count) hold 0)))
               ((less? code 19) ; 18
                  (let*((count hold (bits 7 hold))
                        (count (+ count 11)))
                     (loop (+ n count) hold 0)))))
            ; else (done, no more codes required)
            (values lengths hold))))

; эта функция возвращает поток в виде ленивого списка переменной длины
; где последний элемент списка либо лямбда на создание следующего списка, либо #null
; после того, как упакованный поток законится, будет возвращен исходный поток (за упакованным)
(define (inflate l)
   ; задаем буфер для декодированных даных
   ; максимальное смещение равняется 32768, максимальная длина блока - 258
   (define history (make-bytevector #x8000 0))

   ; декодер блока
   (define (make-block-decoder i hold)
      (lambda ()
      ; декодируем заголовок блока
      (let*((BFINAL hold (bits 1 hold))
            (BTYPE hold (bits 2 hold))
            (literal-or-shift-huffman shift-huffman hold (cond
               ((eq? BTYPE #b10)
                  (let*((HLIT hold (bits 5 hold))   ; nlen = (hold & 31) + 257
                        (HDIST hold (bits 5 hold))  ; ndist = (hold & 31) + 1
                        (HCLEN hold (bits 4 hold))) ; ncode = (hold & 15) + 4
                     (define nlen (+ HLIT 257))
                     (define ndist (+ HDIST 1))
                     (define nlens (+ HCLEN 4))

                     (define order '(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))
                     (define lens (make-bytevector (length order))) ; default is 0
                     ; читаем 19 кодов хаффмана для декодирования главной таблицы хаффмана
                     (let ((hold (fold (lambda (hold index)
                                          (let*((len hold (bits 3 hold)))
                                             (set-ref! lens index len)
                                             hold))
                                    hold
                                    (take order nlens)))) ; order
                     (define codes (make-huffman-table lens))

                           ; декодирование таблицы литералов-и-длин
                     (let*((lengths hold (decode-huffman-lengths nlen hold codes))
                           (literal-or-shift-huffman (make-huffman-table lengths))
                           ; декодирование таблицы сдвигов
                           (lengths hold (decode-huffman-lengths ndist hold codes))
                           (shift-huffman (make-huffman-table lengths)))

                     (values literal-or-shift-huffman shift-huffman hold)))))
               ; статическая таблица хаффмана (todo: вынести вовне как константу)
               ((eq? BTYPE #b01)
                  (let ((lengths (make-bytevector 288)))
                     (for-each (lambda (i) (set-ref! lengths i 8)) (iota 143 0))
                     (for-each (lambda (i) (set-ref! lengths i 9)) (iota 112 144))
                     (for-each (lambda (i) (set-ref! lengths i 7)) (iota 24 256))
                     (for-each (lambda (i) (set-ref! lengths i 8)) (iota 8 280))
                     (define literal-or-shift-huffman (make-huffman-table lengths))

                  (let ((lengths (make-bytevector 32 5)))
                     (define shift-huffman (make-huffman-table lengths))

                     (values literal-or-shift-huffman shift-huffman hold))))
               (else
                  (print-to stderr "UNKNOWN BLOCK!!!")))))

         ; и вот теперь, собственно
         ; декодирование основного потока
         (let loop ((i i) (hold hold))
            (let*((code hold (huffman literal-or-shift-huffman hold)))
               (cond
                  ((less? code 256)
                     (set-ref! history i code)
                     (cons code (lambda ()
                        (loop (band (+ i 1) #x7FFF) hold))))
                  ((eq? code 256) ; end of block
                     ;(print-to stderr "hold: " (ref hold 1) " " (ref hold 2) " > " (ref hold 3))
                     ;(print-to stderr "BFINAL: " BFINAL)
                     (if (eq? BFINAL 1)
                        (begin
                           ;(print-to stderr "END-OF-STREAM")
                           (ref hold 3)) ;#null
                        (make-block-decoder i hold)))

                  ((less? code 286) ; 257-264, ..., 285
                     (let*((length (- code 257)) ; +3
                           (clarification (lref (list 0 0 1 2 3 4 5 0) (>> length 2)))
                           (clarf hold (bits clarification hold))
                           (delta (lref '(3 4 5 6 7 8 9 10 ; 0 bits
                                          11 13 15 17  19 23 27 31  ; 1, 2 bits pairs
                                          35 43 51 59  67 83 99 115 ; 3, 4 bits pairs
                                          131 163 195 227  258) length)) ; 5 bits and special
                           ; декодированная длина
                           (length (+ delta clarf))

                           (shift hold (huffman shift-huffman hold))
                           (clarification (lref (cons 0 (iota 14)) (>> shift 1))) ; количество бит, уточняющих смещение
                           (clarf hold (bits clarification hold))
                           (delta (lref '(1 2 3 4 ; 0 bits
                                          5 7  9 13  17 25  33 49 ; 1, 2, 3, 4 bits pairs
                                          65 97  129 193  257 385 ; 5, 6, 7 bits pairs
                                          513 769  1025 1537      ; 8, 9
                                          2049 3073  4097 6145    ; 10, 11 / and 12, 13
                                          8193 12289  16385 24577) shift))
                           ; декодированное смещение
                           (shift (+ delta clarf))
                           (shift (- shift)))
                        ; lz77, копируем строку
                        (let copy ((i i) (p (band (+ i #x10000 shift) #x7FFF)) (n length) (out #null))
                           (if (eq? n 0)
                              ; вернем скопированную строку в виде '(c c c ... c c . lambda)
                              (let reverse ((old out) (new (lambda () (loop i hold))))
                                 (if (null? old)
                                    new
                                    (reverse (cdr old) (cons (car old) new))))
                              ; скопируем строку в историю и продоллжим копирование
                              (let ((code (ref history p)))
                                 (set-ref! history i code)
                                 (copy (band (+ i 1) #x7FFF) (band (+ p 1) #x7FFF) (- n 1) (cons code out)))))))
                  ; temp
                  (else
                     (print-to stderr "INVALID STREAM DETECTED")
                     #null)))))))

   ; вернем поток с декодированным первым байтом
   (force
      (make-block-decoder 0 [0 0 l])))

))
