;;;
;;; Object serialization and reconstruction
;;;

;; fasl - FASt Lisp format

;; limitations:
;
; * maximum size of marshalled object
;    is 16MB for x32
;   and 64PB for x64 (Petabytes)
; * maximum count of marshalled objects
;    is 16MB for x32
;   and 64PB for x64 (Petabytes)

;; todo: add a version which handles symbols and ff's specially

;; fixme: encoder which returns the number of objects is probably no longer needed

; protocol
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

(define-library (otus fasl)

   (export
      fasl2-encode          ; obj -> (byte ... 0)

      ;; fasl-decode          ; (byte ...) -> obj, input can be lazy

      fasl2-save

      ;; decode-or            ; (byte ...) fail → object | (fail reason)
      ;; encode               ; obj -> (byte ... 0), n-alloc-objs (mainly for bootstrapping)
      ;; objects-below        ; obj -> (obj ...), all allocated objects below obj
      ;; decode-stream        ; ll failval → (ob ...) | (ob .. failval)
      ;; sub-objects          ; obj wanted? -> ((obj . n-occurrences) ...)
      )

   (import
      (scheme core)
      (scheme vector)

      (otus blobs)
      (owl math)
      (owl ff)
      (owl io) (owl string) (scheme bytevector) (owl math fp); temp
      (owl lazy)
      (owl list)
      (owl rlist))

   (begin
      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      (define enodata #false) ;; reason to fail if out of data (progressive readers want this)
      (define fasl-finale (list 0)) ; stream end marker

      (define low7 #b1111111)


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

      ;; todo - pack type to this now that they fit 6 bits
      ;; encode the reference
      (define (encode-reference val pos index tail)
         (cond
            ((ref val 0) ; ==(raw object? val), 0 in ref works only for binary objects
               (let ((t (type val))
                     (s (size val)))
                  (ilist 2 t
                     (send-number s
                        (copy-bytes tail val (- s 1))))))
            ; все остальные числа
            ((integer? val) ; сюда мы можем придти только если непосредственно энкодим число, как часть объекта оно сюда придти не может
               (encode-integer val tail))
            (else
               (let ((t (type val))
                     (s (size val)))
                  (ilist 1 t
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
         (let ((index (cdr (index-closure (object-closure obj)))))
            (let loop ((kvs (ff-iter index)))
               (cond
                  ((null? kvs) tail)
                  ((pair? kvs)
                     (let ((kv (car kvs)))
                        (encode-reference (car kv) (cdr kv) index
                           (lambda () (loop (cdr kvs))))))
                  (else
                     (loop (force kvs)))))))

      ; object -> serialized lazy list
      (define (encode2 obj)
         (cond
            ((integer? obj) ; todo: remove from fasl2-encode
               (encode-integer obj #null))
            ((value? obj) ; todo: remove from fasl2-encode
               (encode-value obj #null))
            (else
               (encode-object obj fasl-finale))))

      ; object -> serialized list
      (define (fasl2-encode obj)
         (force-ll (encode2 obj)))

      ;; ;;;
      ;; ;;; Decoder
      ;; ;;;

      ; process lazy list
      (define (grab ll fail)
         ;; (print "grab")
         (cond
            ((null? ll) (fail enodata))
            ((pair? ll) (values (cdr ll) (car ll)))
            (else (grab (force ll) fail))))

      (define (get-number ll fail)
         (let gnloop ((ll ll) (value 0) (shift 0))
            ;; (print "gnloop/ value: " value ", shift: " shift)
            (let* ((ll byte (grab ll fail)))
               (if (eq? (vm:and byte 128) 0) ; leaf case
               then
                  ;; (print "byte: " byte)
                  (define data (band byte 127))
                  ;; (print "data: " data)
                  ;; (print "shifted: " (<< data shift))
                  ;; (print "result: " (bor value (<< byte shift)))
                  (values ll (bor value (<< byte shift)))
               else
                  ;; (print "byte: " byte)
                  (define data (band byte 127))
                  ;; (print "data: " data)
                  ;; (print "shifted: " (<< data shift))
                  ;; (print "result: " (bor value (<< byte shift)))
                  (gnloop ll (bor value (<< data shift)) (+ shift 7))))))

      (define (decode-value ll fail)
         ;; (print "        decode-value")
         (let*((ll atype (grab ll fail))
               ;; (_ (print "         type: " atype))
               (ll value (get-number ll fail)))
            ;; (print "        dv: " value ", type: " (type value))
            (values ll (cond
               ((value? value)
                  ;; (print "        value")
                  ;; (print "        dv: " (vm:cast value atype))
                  (vm:cast value atype))
               ((eq? atype type-enum+)
                  ;; (print "        enum+")
                  ;; (print "        dv: " (vm:cast value type-int+))
                  (vm:cast value type-int+))
               ((eq? atype type-enum-)
                  ;; (print "        enum-")
                  ;; (print "        dv: " (vm:cast value type-int-))
                  (vm:cast value type-int-))
               (else
                  ;; (print "        else")
                  (fail "invalid value type"))))))

      (define nan "not here") ; eq?-unique

      (define (get-bytes ll n fail tail)
         (if (eq? n 0)
            (values ll tail)
            (let* ((ll byte (grab ll fail)))
               (get-bytes ll (-- n) fail (cons byte tail)))))

      (define (decode-fields ll index size fail fields)
         ;; (print "    decode-fields, fields: " fields)
         ;; (print "      /size: " size)
         (if (eq? size 0)
         then
            ;; (print "          " fields " -> " (reverse fields))
            (values ll (reverse fields))
         else
            (let* ((ll fst (grab ll fail)))
               ;; (print "      field reference: " fst)
               (if (eq? fst 0) ; a value
                  (let* ((ll val (decode-value ll fail)))
                     ;; (print "      val: " val)
                     (decode-fields ll index (-- size) fail (cons val fields)))
               else ; a reference
                  (let*((ll pos (get-number (cons fst ll) fail)))
                     ;; (print "      pos: " pos)
                     (if (eq? pos 0)
                        (fail "bad reference"))
                     (define val (rget index (-- pos) nan))
                     ;; (print "      val: " val)
                     (if (eq? val nan)
                        (fail "bad reference"))
                     (decode-fields ll index (-- size) fail (cons val fields)))))))

      ;; ; → ll value | (fail reason)
      ;; index is a "random accessible list"
      (define (decode-object ll index fail)
         ;; (print "decode-object, index: " index)
         (cond
            ((null? ll)
               ;; no terminal 0, treat as a bug
               (fail "no terminal zero"))
            ((pair? ll)
               (let* ((kind ll ll))
                  ;; (print "  kind: " kind)
                  (cond
                     ; regular object
                     ((eq? kind 1)
                        (let*((ll type (grab ll fail))
                              ;; (_ (print "    type: " type))
                              (ll size (get-number ll fail))
                              ;; (_ (print "    size: " size))
                              (ll fields (decode-fields ll index size fail #null))
                              ;; (_ (print "    fields: " fields))
                              (obj (vm:make type fields)))
                           ;; (print "    obj: " obj)
                           (decode-object ll (rcons obj index) fail)))
                     ; bitstream object
                     ((eq? kind 2)
                        (let*((ll type (grab ll fail))
                              (ll size (get-number ll fail))
                              (ll rbytes (get-bytes ll size fail null))
                              (obj (vm:makeb type (reverse rbytes))))
                           ;; (print "    obj: " obj)
                           (decode-object ll (rcons obj index) fail)))
                     ;; fasl stream end marker
                     ((eq? kind 0)
                        ;; object done
                        (values ll (rcar index)))
   ;;                ((eq? (band kind 3) 3) ; shortcut allocated
   ;;                   (lets
   ;;                      ((type (>> kind 2))
   ;;                       (ll size (get-nat ll fail 0))
   ;;                       (foo (if (> size 65535) (fail "bad raw object size")))
   ;;                       (ll rbytes (get-bytes ll size fail null))
   ;;                       (obj (vm:makeb type (reverse rbytes))))
   ;;                      (decode-object ll (rcons obj got) fail)))
                     (else
                        (fail (list "unknown object tag: " kind))))))
            (else
               (decode-object (force ll) index fail))))

      (define (decode-or ll err) ; -> ll obj | null (err why)
         ;; (print "decode-or : " ll)
         (let*/cc ret
               ((fail (λ (why) (ret #false (err why)))))
            (cond
               ((null? ll)
                  (fail enodata))
               ((pair? ll)
                  ; a leading 0 is special and means the stream has a value or a number
                  (if (eq? (car ll) 0)
                     (decode-value (cdr ll) fail)
                     (decode-object ll #null fail)))
               (else
                  (decode-or (force ll) err)))))

      (define failed "fail") ;; a unique object

      ;; ;; ll fail → val | fail
      ; serialized lazy list -> object
      (define (decode2 ll fail)
         (let* ((ll ob (decode-or ll (λ (why) failed))))
            (cond
               ((eq? ob failed)
                  fail)
               ((null? ll) ob)
               (else
                  fail))))

      ;; ;; byte-stream → (ob ...) | (ob ... err)
      (define fasl2-decode decode2)

      ; ---------------------------
      (define (fasl2-save obj path)
         (bytevector->file
            (list->bytevector (fasl2-encode obj))
            path))

      (define (fasl2-load path fail-val)
         (let ((bs (file->bytestream path)))
            (if bs
               (fasl2-decode bs fail-val)
               fail-val)))

))
