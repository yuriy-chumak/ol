#!/usr/bin/ol

; https://www.nayuki.io/page/unspecified-edge-cases-in-the-deflate-standard

; return n bits from input binary stream
(define (bits n hold)
   (let loop ((hold hold))
      (tuple-apply hold (lambda (v i l)
         (unless (less? i n)
            (values (band v (- (<< 1 n) 1)) (tuple (>> v n) (- i n) l))
            ; если же не хватает бит, втянем из потока
            (loop (tuple
               (bor v (<< (car l) i))
               (+ i 8)
               (cdr l))))))))

; code - huffman table (from make-huffman-table), hold - binary stream
(define (huffman codes hold)
   (let loop ((code 1) (hold hold))
      (let*((bit hold (bits 1 hold))
            (code (bor (<< code 1) bit))
            (value (getf codes code)))
         (if value
            (values value hold)
            (loop code hold)))))

; converts lengths array to code->value huffman ff
; вот по таким правилам:
;  [если воспринимать коды как числа, у которых первый бит кода это старший разряд, то] все коды одной длины должны быть последовательными значениями, упорядоченными так же, как символы, которые они кодируют
;  [если воспринимать коды как строки бит, то] коды меньшей длины лексикографически предшествуют кодам большей длины
(define (make-huffman-table lengths)
   (let loop  ((codes #empty)
               (pairs (sort (lambda (a b) (< (car a) (car b))) ; отсортированный и
                  (filter (lambda (a) (not (eq? (car a) 0)))   ; отфильтрованный массив
                     (map (lambda (a b) (cons a b))            ; ( длина кода хаффмана . кодируемое ею значение )
                        (vector->list lengths) (iota (size lengths))))))
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

(define (inflate l)
   (define hold (tuple 0 0 l))
   (let*((BFINAL hold (bits 1 hold))
         (BTYPE hold (bits 2 hold)))
      (print "BFINAL: " BFINAL)
      (print "BTYPE: " BTYPE)

   (if (eq? BTYPE #b10)
      (let*((HLIT hold (bits 5 hold))   ; nlen = (hold & 31) + 257
            (HDIST hold (bits 5 hold))  ; ndist = (hold & 31) + 1
            (HCLEN hold (bits 4 hold))) ; ncode = (hold & 15) + 4;
         (define nlen (+ HLIT 257)) ;(print "nlen: " nlen)
         (define ndist (+ HDIST 1)) ;(print "ndist: " ndist)
         (define nlens (+ HCLEN 4)) ;(print "nlens: " nlens)
         ;(print "HLIT: " HLIT)
         ;(print "HDIST: " HDIST)
         ;(print "HCLEN: " HCLEN)

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
         (let*((lengths hold (decode-huffman-lengths nlen hold codes)))
            (define literal-or-shift-huffman (make-huffman-table lengths))
         ;(print "literal-or-shift-huffman: " literal-or-shift-huffman)

         ; декодирование таблицы сдвигов
         (let*((lengths hold (decode-huffman-lengths ndist hold codes)))
            (define shift-huffman (make-huffman-table lengths))
         ;(print "shift-huffman: " shift-huffman)


         ;(shutdown 1)
         ;(print "hold: " hold)

         ;(set-car! huffman-debug #t)
         ;(shutdown 1233)

         ; декодирование основного потока
         (define output (make-bytevector 65536 0))
         ;(print "hold: "hold)
         (let loop ((i 0) (hold hold))
            (let*((code hold (huffman literal-or-shift-huffman hold))
                  (copy (lambda (shift n hold)
                           (let copy ((i i) (p (- i shift)) (n n))
                              (if (eq? n 0)
                                 (loop i hold)
                                 (begin
                                    (set-ref! output i (ref output p))
                                    (copy (+ i 1) (+ p 1) (- n 1))))))))
               (cond
                  ((less? code 256)
                     (set-ref! output i code)
                     (loop (+ i 1) hold))
                  ((eq? code 256) ; end of block
                     ; вернем декодироанный поток в виде списка
                     (let loop ((i i) (out #null))
                        (if (eq? i 0)
                           out
                           (loop (- i 1) (cons (ref output (- i 1)) out)))))

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
                                          8193 12289  16285 24577) shift))
                           ; декодированное смещение
                           (shift (+ delta clarf)))
                        (copy shift length hold)))
                  ; temp
                  (else
                     (print "DECODER ERROR")
                     ; вернем декодироанный поток в виде списка
                     (let loop ((i i) (out #null))
                        (if (eq? i 0)
                           out
                           (loop (- i 1) (cons (ref output (- i 1)) out)))))))))))))))

;; (define stream (file->list "1_0"))

;; (let*((ID1 (car stream))
;;       (stream (cdr stream))
;;       (ID2 (car stream))
;;       (stream (cdr stream))
;;       (CM (car stream))
;;       (stream (cdr stream))
;;       (FLG (car stream))
;;       (stream (cdr stream))
;;       (stream (drop stream 4))
;;       (XFL (car stream))
;;       (stream (cdr stream))
;;       (OS (car stream))
;;       (stream (cdr stream)))
;;       ;(stream (drop stream 8)))
;;    (print "ID1: " ID1)
;;    (print "ID2: " ID2)
;;    (print "CM: " CM)
;;    (print "FLG: " FLG)
;;    (print "XFL: " XFL)
;;    (print "OS: " OS)

;;    (print (list->string (inflate stream))))

;;       ; CM
;;       ; FLG
;;       ; MTIME
;;       ; XFL
;;       ; OS

;; ;;       (CM (band CMF #b1111)) ; assert method == 8
;; ;;       (CINFO (>> CMF 4))

;; ;;       (FLG (cadr stream))
;; ;;       (FCHECK (band FLG #b11111))
;; ;;       (FDICT (band (>> FLG 5) 1))
;; ;;       (FLEVEL (band (>> FLG 6) 2)))
;; ;; ;;    ; todo: if FDICT then need to read FDICT
;; ;; ;;       ; dmax = 1 << len;

;; ;;    (print "CM: " CM)
;; ;;    (print "CINFO: " CINFO)
;; ;;    (print "FLG: " FLG)
;; ;;    (print "FDICT: " FDICT)
;; ;;    (print "FLEVEL: " FLEVEL)
;; ;; (let*((stream (cddr stream)))
;; ;;    #t))
