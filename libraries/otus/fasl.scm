(define-library (otus fasl)
   (version 2.0)
   (license MIT/LGPL3)
   (description "object serialization library")

;  fasl - FASt Lisp binary format
;

; limitations:
;
; * maximum size of marshalled object
;    is 16MB for x32
;   and 64PB for x64 (Petabytes)
; * maximum count of marshalled objects
;    is 16MB for x32
;   and 64PB for x64 (Petabytes)
; * symbols deserialzed uninterned

; protocol:
;  <obj> = 0 <type> <value>            -- immediate object
;        = 1 <type> <size> <field> ... -- allocated
;        = 2 <type> <size> <byte>  ... -- allocated, raw data
; now used
;     00 - imm
;     01 - alloc
;     10 - alloc raw
;     11 - free
;
;  <field> = 0 <type> <val>...<val> -- immediate
;          = <N> -- pointer to nth last object

;; todo: add a version which handles symbols and ff's specially
   (export
      encode2              ; obj -> (lazy) fasl list
      fasl-encode          ; obj -> (byte ... 0)
      serialize            ; same as fasl-encode

      decode2              ; (lazy) fasl list -> obj
      fasl-decode          ; (byte ...) -> obj
      deserialize)         ; slower than fasl-decode, but interns symbols

   (import
      (scheme core)

      (owl math)
      (owl ff)
      (owl lazy)
      (owl list)
      (owl rlist)
      (otus async))

   (begin

      (define enodata #false) ;; reason to fail if out of data (progressive readers want this)
      (define fasl-finale (list 0)) ; stream end marker

      (define (object->list obj)
         (let loop ((pos (size obj)) (tail #null))
            (if (eq? pos 0)
               tail
               (loop (-- pos) (cons (ref obj pos) tail)))))

      ;;;
      ;;; Encoder
      ;;;

      (setq add (lambda (a b)         ; * internal
         (values-apply (vm:add a b) (lambda (n carry) n))))

      (define (send-biggish-num num tail)
         (if (less? num 127)
            (cons (vm:cast num type-enum+) tail) ; highest chunk must have a bit 8 clear
            (cons (vm:ior (band num 127) 128)
                  (send-biggish-num (>> num 7) tail))))

      (define (send-number num tail)
         (if (less? num 128)
            (cons (vm:cast num type-enum+) tail)
            (cons (vm:ior (band num 127) 128)
                  (send-biggish-num (>> num 7) tail))))

      ;; encode the value
      (define (encode-value val tail)
         (cons 0
            (cons (type val)
               (send-number val tail))))

      ;; encode the integer number
      (define (encode-integer val tail)
         (cons 0
            (cons (if (positive? val) type-enum+ type-enum-)
               (send-number (abs val) tail))))


      (define (encode-fields lst pos index tail)
         (foldr (lambda (elem out)
               (cond
                  ((integer? elem)
                     (encode-integer elem out))
                  ; проверять на соответствие 24-битному размеру
                  ((value? elem)
                     (encode-value elem out))
                  (else
                     (let ((target (index elem #false)))
                        ; hack warning: objects cannot refer to themselves and the
                        ; heap is unidirectional, so pos - target > 0, so a 0 can
                        ; be safely used as the immediate marker, and pointers can
                        ; have the delta directly encoded as a natural, which always
                        ; starts with a nonzero byte when the natural > 0
                        (send-number (- pos target) out)))))
         tail lst))

      (define (copy-bytes out bvec p)
         (if (eq? p -1)
            out
            (copy-bytes (cons (ref bvec p) out) bvec (- p 1))))

      (define (copyreverse-bytes out bvec p)
         (let loop ((n 0) (out out))
            (if (eq? (++ p) n)
               out
               (loop (++ n) (cons (ref bvec n) out)))))

      (define (encode-inexact val tail copybytes)
         (let ((t (type val))
               (s (size val)))
            (cons* 2 t
               (send-number s
                  (copybytes tail val (-- s))))))


      ;; todo - pack type to this now that they fit 6 bits
      ;; encode the reference
      (define (encode-reference val pos index tail copybytes)
         (cond
            ((integer? val) ; сюда мы можем придти только если непосредственно энкодим число, как часть объекта оно сюда придти не может
               (encode-integer val tail))
            ((eq? (type val) type-inexact)
               (encode-inexact val tail copybytes))
            ((ref val 0) ; ==(raw object? val), 0 in ref works only for binary objects
               (let ((t (type val))
                     (s (size val)))
                  (cons* 2 t
                     (send-number s
                        (copy-bytes tail val (-- s))))))
            (else
               (let ((t (type val))
                     (s (size val)))
                  (cons* 1 t
                     (send-number s
                        (encode-fields (object->list val) pos index
                           tail)))))))

      ; -----
      ; collect all objects into dictionary

      (define (collect loop seen obj)
         (let ((seen (put seen obj 1)))
            (if (ref obj 0) ; == binary object, 0 in ref works only for blobs
               seen
            else
               (fold loop seen (object->list obj)))))


      (define (object-closure root)
         (let loop ((seen #empty) (obj root))
            (cond
               ; целые числа
               ((integer? obj)
                  seen)
               ; остальные константы
               ((value? obj) ; todo: проверять размер value < 24-bits 
                  seen)
               ; если хотим статистику - (ff-update seen obj (+ n 1))
               ((seen obj #false) => (lambda (n)
                  seen))
               ; прочие объектыs
               (else
                  (collect loop seen obj)))))

      ; ---------------------
      ; enumerate all objects
      (define (index-closure clos)
         (ff-fold
            (λ (fc key _)
               (let* ((fp clos fc))
                  ; no more than 16777216 object for x32 is allowed
                  (cons (++ fp) (ff-update clos key fp))))
            (cons 0 clos) clos))


      ; root -> byte-stream processor
      (define (encode-object obj tail)
         ; special case for endian-dependent bytevectors
         (define copybytes
            (let ((one (vm:cast 1 type-vptr)))
               (cond
                  ((eq? (ref one 0) 1)
                     copy-bytes)
                  ((eq? (ref one (-- (size one))) 1)
                     copyreverse-bytes)
                  ((eq? (ref one 1) 1)
                     (runtime-error "middle-endian unsupported" #null))
                  (else
                     (runtime-error "unknown-endian unsupported" #null)))))
         ; generate subobjects index
         (let ((index (cdr (index-closure (object-closure obj)))))
            (let loop ((kvs (ff-iter index)))
               (cond
                  ((null? kvs) tail)
                  ((pair? kvs)
                     (let ((kv (car kvs)))
                        (encode-reference (car kv) (cdr kv) index
                           (lambda () (loop (cdr kvs))) copybytes)))
                  (else
                     (loop (force kvs)))))))

      ; object -> serialized lazy list
      (define (encode2 obj)
         (cond
            ;; todo: add inexacts
            ((integer? obj) ; todo: remove from fasl2-encode
               (encode-integer obj #null))
            ((value? obj) ; todo: remove from fasl2-encode
               (encode-value obj #null))
            (else
               (encode-object obj fasl-finale))))

      ; object -> serialized list
      (define (fasl-encode obj)
         (force-ll (encode2 obj)))

      (define serialize fasl-encode)

      ;; ;;;
      ;; ;;; Decoder
      ;; ;;;

      ; process lazy list
      (define (grab ll fail)
         (cond
            ((null? ll) (fail enodata))
            ((pair? ll) (values (cdr ll) (car ll)))
            (else (grab (force ll) fail))))

      (define (get-number ll fail)
         (let gnloop ((ll ll) (value 0) (shift 0))
            (let* ((ll byte (grab ll fail)))
               (if (eq? (vm:and byte 128) 0) ; leaf case
                  (values ll (bor value (<< byte shift)))
               else
                  (define data (band byte 127))
                  (gnloop ll (bor value (<< data shift))
                     ; (+ shift 7) speedup:
                     (values-apply (vm:add shift 7) (lambda (n carry) n)))))))

      (define (decode-value ll fail)
         (let*((ll atype (grab ll fail))
               (ll value (get-number ll fail)))
            (values ll (cond
               ((value? value)
                  (vm:cast value atype))
               ((eq? atype type-enum+)
                  (vm:cast value type-int+))
               ((eq? atype type-enum-)
                  (vm:cast value type-int-))
               (else
                  (fail "invalid value type"))))))

      (define nan "not here") ; eq?-unique

      (define (get-bytes ll n fail tail)
         (if (eq? n 0)
            (values ll tail)
            (let* ((ll byte (grab ll fail)))
               (get-bytes ll (-- n) fail (cons byte tail)))))

      (define (decode-fields ll index size fail fields)
         (if (eq? size 0)
            (values ll (reverse fields))
         else
            (let* ((ll fst (grab ll fail)))
               (if (eq? fst 0) ; a value
                  (let* ((ll val (decode-value ll fail)))
                     (decode-fields ll index (-- size) fail (cons val fields)))
               else ; a reference
                  (let*((ll pos (get-number (cons fst ll) fail)))
                     (if (eq? pos 0)
                        (fail "bad reference"))
                     (define val (rget index (-- pos) nan))
                     (if (eq? val nan)
                        (fail "bad reference"))
                     (decode-fields ll index (-- size) fail (cons val fields)))))))

      ;; index is a "random accessible list"
      (define (decode-object ll index fail copybytes)
         (cond
            ((null? ll)
               ;; no terminal 0, treat as a bug
               (fail "no terminal zero"))
            ((pair? ll)
               (let* ((kind ll ll))
                  (cond
                     ; regular object
                     ((eq? kind 1)
                        (let*((ll type (grab ll fail))
                              (ll size (get-number ll fail))
                              (ll fields (decode-fields ll index size fail #null))
                              (obj (vm:make type fields)))
                           (decode-object ll (rcons obj index) fail copybytes)))
                     ; bitstream object
                     ((eq? kind 2)
                        (let*((ll type (grab ll fail))
                              (ll size (get-number ll fail))
                              (ll rbytes (get-bytes ll size fail null))
                              (obj (vm:alloc type ((if (eq? type type-inexact) copybytes reverse) rbytes))))
                           (decode-object ll (rcons obj index) fail copybytes)))
                     ;; fasl stream end marker
                     ((eq? kind 0)
                        ;; object done
                        (values ll (rcar index)))
                     (else
                        (fail (list "unknown object tag: " kind))))))
            (else
               (decode-object (force ll) index fail copybytes))))

      (define (decode-or ll err copybytes) ; -> ll obj | null (err why)
         (let*/cc ret
               ((fail (λ (why) (ret #false (err why)))))
            (cond
               ((null? ll)
                  (fail enodata))
               ((pair? ll)
                  ; a leading 0 is special and means the stream has a value or a number
                  (if (eq? (car ll) 0)
                     (decode-value (cdr ll) fail)
                     (decode-object ll #null fail copybytes)))
               (else
                  (decode-or (force ll) err copybytes)))))

      (define failed "fail") ;; a unique object

      ;; ;; ll fail → val | fail
      ; serialized lazy list -> object
      (define (decode2 ll fail)
         ; special case for endian-dependent bytevectors
         (define copybytes
            (let ((one (vm:cast 1 type-vptr)))
               (cond
                  ((eq? (ref one 0) 1)
                     reverse)
                  ((eq? (ref one (-- (size one))) 1)
                     (lambda (x)
                        x)) ; same
                  ((eq? (ref one 1) 1)
                     (runtime-error "middle-endian unsupported" #null))
                  (else
                     (runtime-error "unknown-endian unsupported" #null)))))

         (let* ((ll ob (decode-or ll (λ (why) failed) copybytes)))
            (if (eq? ob failed)
               fail
            else ob)))

      (define fasl-decode decode2)

      ; deserialize
      (define (procedure? o)
         (case (type o)
            (type-procedure #true)
            (type-closure #true)
            (type-constructor #true)))

      (define (deserialize ll fail)
         (define obj (fasl-decode ll fail))
         (if (eq? obj fail)
            fail
         else
            (let loop ((obj obj))
               (if (reference? obj)
               then
                  (cond
                     ((symbol? obj)
                        (await (mail 'intern (ref obj 1))))
                     ((pair? obj)
                        (cons
                           (loop (car obj))
                           (loop (cdr obj))))
                     ((or (vector? obj)
                          (procedure? obj))
                        (vm:make (type obj)
                           (let subloop ((pos (size obj)) (tail #null))
                              (if (eq? pos 0)
                                 tail
                                 (subloop (-- pos) (cons (loop (ref obj pos)) tail))))))
                     ((ff? obj)
                        (alist->ff (loop (ff->alist obj))))
                     (else
                        obj))
               else
                  obj))))
))
