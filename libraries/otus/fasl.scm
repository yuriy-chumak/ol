;;;
;;; Object serialization and reconstruction
;;;

;; fasl - FASt Lisp format

;; todo: removing dependencies to bignum math would increase speed
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
;     11 - free -> use as tag for allocs where the type fits 6 bits (not in use atm)
;
;  <field> = 0 <type> <val> -- immediate
;          = <N> -- pointer to nth last object (see hack warning below)

(define-library (otus fasl)

   (export
      fasl2-encode          ; obj -> (byte ... 0)
      ;; fasl-encode-cooked   ; obj cook -> (byte ... 0), with (cook alloc-obj) -> alloc-obj'
      ;; fasl-encode-stream   ; obj cook -> (bvec ...) stream

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
      ;; (define (special? obj)
      ;;    (and
      ;;       (eq? (type obj) type-int+)
      ;;       (or
      ;;          (null? (cdr obj)) ; большие числа не могут состоять из одного элемента (это маленькие числа)
      ;;          (eq? (car obj) 0)))) ; большие числа не могут начинаться с нуля


      (define (object->list obj)
         (let loop ((pos (size obj)) (tail #null))
            (if (eq? pos 0)
               tail
               (loop (-- pos) (cons (ref obj pos) tail)))))

      ;; (define (object2->list obj)
      ;;    (let loop ((pos (size obj)) (tail #null))
      ;;       ;; (print "pos: " pos)
      ;;       (if (eq? pos 0)
      ;;          tail
      ;;          (let*((item (ref obj pos))
      ;;                (item (if (special? item)
      ;;                      then ; сюда мы не должны попадать
      ;;                         (print "This is invalid number !!! -- " pos " / " (ref item 1))
      ;;                         item
      ;;                      else
      ;;                         item)))
      ;;          (loop (-- pos) (cons item tail))))))

      ;;;
      ;;; Encoder
      ;;;

      (setq add (lambda (a b)         ; * internal
         (values-apply (vm:add a b) (lambda (n carry) n))))

      (define (send-biggish-num num done)
         (if (less? num 127)
            (cons num done) ; highest chunk must have a bit 8 clear
            (cons (vm:ior (band num 127) 128)
                  (send-biggish-num (>> num 7) done))))

      (define (send-number num tail)
         (if (less? num 128)
            (cons num tail)
            (cons (vm:ior (band num 127) 128)
                  (send-biggish-num (>> num 7) tail))))

      ;; encode the value
      (define (encode-value val tail)
         ;; (print "encoding value " val)
         (cons 0
            (cons (type val)
               (send-number val tail))))

      ;; encode the integer number
      (define (encode-integer val tail)
         ;; (print "encode integer: " val)
         (cons 0
            (cons (if (positive? val) type-enum+ type-enum-)
               (send-number (abs val) tail))))


      (define (render-fields lst pos index tail)
         ;; (display "   render-fields( ")
         ;; (for-each (lambda (l)
         ;;       (write l)
         ;;       (display " "))
         ;;    lst)
         ;; (print ") / {" pos "}")
         (foldr (lambda (elem out)
               (cond
                  ;; ; we should skip special integer case: *big-num* and similars
                  ;; ; "специальные" числа
                  ;; ((special? elem)
                  ;;    (print "special " (type elem) " " (size elem) " - " (vm:cast type-enum+ (car elem)) " . " (cdr elem))
                  ;;    (let ((target (index elem))) ; assert elem is already index
                  ;;       ;; (print "special number! pos: " pos " -> " target)
                  ;;       (send-number (- pos target) out)))
                  ((integer? elem)
                     ; note: position number будет потерян
                     (encode-integer elem out))
                  ; проверять на соответствие 24-битному размеру
                  ((value? elem)
                     (encode-value elem out))
                  (else
                     (let ((target (get index elem "bug")))
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
         ;; (display "encode reference: ")
         ;; (write val)
         ;; (print ", pos: " pos ", tail: " tail)
         (cond
            ((ref val 0) ; ==(raw object? val), 0 in ref works only for binary objects
               ;; (print "encoding raw obj: " val)
               (let ((t (type val))
                     (s (size val)))
                  (ilist 2 t
                     (send-number s
                        (copy-bytes tail val (- s 1))))))
            ;; ; специальные "числа", вроде *int-max*
            ;; ((special? val)
            ;;    ;; (print "encoding SPECIAL number")
            ;;    (let ((t (type val))
            ;;          (s (size val)))
            ;;       (ilist 1 t
            ;;          (send-number s
            ;;             (render-fields (object->list val) pos index
            ;;                tail)))))
            ; все остальные числа
            ((integer? val) ; сюда мы можем придти только если непосредственно энкодим число, как часть объекта оно сюда придти не может
               ;; (print "encoding number: " val)
               (encode-integer val tail))
            (else
               ;; (print "encoding regular object: " val)
               (let ((t (type val))
                     (s (size val)))
                  (ilist 1 t
                     (send-number s
                        (render-fields (object->list val) pos index
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
               ;; ; специальные "числа"
               ;; ((special? obj)
               ;;    (collect loop seen obj))
               ; обычные числа
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
                  ; TODO: для оптимизации, если не ожидать больше 16777216 объектов, можно использовать ++
                  (cons (+ fp 1) (ff-update clos key fp))))
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
      (define (encode obj)
         (cond
            ;; ((special? obj)
            ;;    (encode-object obj fasl-finale))
            ((integer? obj)
               (encode-integer obj fasl-finale))
            ((value? obj)
               (encode-value obj fasl-finale))
            (else
               (encode-object obj fasl-finale))))

      ; object -> serialized list
      (define (fasl2-encode obj)
         (force-ll (encode obj)))

      ; dump the data, but cook each allocated value with cook just before dumping
      ; (to allow for example changing code from functions without causing
      ;  them to move in the heap, which would break object order)
      ;; (define (fasl-encode-cooked obj cook)
      ;;    (force-ll (encode obj cook)))

      ;; (define chunk-size 4096)

      ;; (define (chunk-stream bs n buff)
      ;;    (cond
      ;;       ((eq? n chunk-size)
      ;;          (cons
      ;;             (make-bytevector (reverse buff))
      ;;             (chunk-stream bs 0 null)))
      ;;       ((null? bs)
      ;;          (if (null? buff)
      ;;             null
      ;;             (list (make-bytevector (reverse buff)))))
      ;;       ((pair? bs)
      ;;          (lets ((n _ (vm:add n 1)))
      ;;             (chunk-stream (cdr bs) n (cons (car bs) buff))))
      ;;       (else
      ;;          (chunk-stream (bs) n buff))))

      ;; (define (fasl-encode-stream obj cook)
      ;;    (chunk-stream (encode obj cook) 0 null))

      ;; ;;;
      ;; ;;; Decoder
      ;; ;;;

      ;; (define (grab ll fail)
      ;;    (cond
      ;;       ((null? ll) (fail enodata))
      ;;       ((pair? ll) (values (cdr ll) (car ll)))
      ;;       (else (grab (ll) fail))))

      ;; (define (get-nat ll fail top)
      ;;    (lets ((ll b (grab ll fail)))
      ;;       (if (eq? 0 (vm:and b 128)) ; leaf case
      ;;          (values ll (bor (<< top 7) b))
      ;;          (get-nat ll fail (bor (<< top 7) (band b low7))))))

      ;; (define (decode-immediate ll fail)
      ;;    (lets
      ;;       ((ll type (grab ll fail))
      ;;        (ll val  (get-nat ll fail 0)))
      ;;       (values ll (vm:cast val type))))

      ;; (define nan "not here") ; eq?-unique

      ;; (define (get-fields ll got size fail out)
      ;;    (if (eq? size 0)
      ;;       (values ll (reverse out))
      ;;       (lets ((ll fst (grab ll fail)))
      ;;          (if (eq? fst 0)
      ;;             (lets ((ll val (decode-immediate ll fail)))
      ;;                (get-fields ll got (- size 1) fail (cons val out)))
      ;;             (lets
      ;;                ((ll pos (get-nat (cons fst ll) fail 0)))
      ;;                (if (eq? pos 0)
      ;;                   (fail "bad reference")
      ;;                   (let ((val (rget got (- pos 1) nan)))
      ;;                      (if (eq? val nan)
      ;;                         (fail "bad reference")
      ;;                         (get-fields ll got (- size 1) fail (cons val out))))))))))

      ;; (define (get-bytes ll n fail out)
      ;;    (if (eq? n 0)
      ;;       (values ll out)
      ;;       (lets ((ll byte (grab ll fail)))
      ;;          (get-bytes ll (- n 1) fail (cons byte out)))))

      ;; ; → ll value | (fail reason)
      ;; (define (decoder ll got fail)
      ;;    (cond
      ;;       ((null? ll)
      ;;          ;; no terminal 0, treat as a bug
      ;;          (fail "no terminal zero"))
      ;;       ((pair? ll)
      ;;          (lets ((kind ll ll))
      ;;             (cond
      ;;                ((eq? kind 1) ; allocated, type SIZE
      ;;                   (lets
      ;;                      ((ll type (grab ll fail))
      ;;                       (ll size (get-nat ll fail 0))
      ;;                       (ll fields (get-fields ll got size fail null))
      ;;                       (obj (vm:make type #|size|# fields)))
      ;;                      (decoder ll (rcons obj got) fail)))
      ;;                ((eq? kind 2) ; raw, type SIZE byte ...
      ;;                   (lets
      ;;                      ((ll type (grab ll fail))
      ;;                       (ll size (get-nat ll fail 0))
      ;;                       (foo (if (> size 65535) (fail "bad raw object size")))
      ;;                       (ll rbytes (get-bytes ll size fail null))
      ;;                       (obj (vm:makeb type (reverse rbytes))))
      ;;                      (decoder ll (rcons obj got) fail)))
      ;;                ((eq? kind 0) ;; fasl stream end marker
      ;;                   ;; object done
      ;;                   (values ll (rcar got)))
      ;;                ((eq? (band kind 3) 3) ; shortcut allocated
      ;;                   (lets
      ;;                      ((type (>> kind 2))
      ;;                       (ll size (get-nat ll fail 0))
      ;;                       (foo (if (> size 65535) (fail "bad raw object size")))
      ;;                       (ll rbytes (get-bytes ll size fail null))
      ;;                       (obj (vm:makeb type (reverse rbytes))))
      ;;                      (decoder ll (rcons obj got) fail)))
      ;;                (else
      ;;                   (fail (list "unknown object tag: " kind))))))
      ;;       (else
      ;;          (decoder (ll) got fail))))

      ;; (define (decode-or ll err) ; -> ll obj | null (err why)
      ;;    (let*/cc ret
      ;;          ((fail (λ (why) (ret null (err why)))))
      ;;       (cond
      ;;          ((null? ll) (fail enodata))
      ;;          ((pair? ll)
      ;;             ; a leading 0 is special and means the stream has no allocated objects, just one immediate one
      ;;             (if (eq? (car ll) 0)
      ;;                (decode-immediate (cdr ll) fail)
      ;;                (decoder ll null fail)))
      ;;          (else
      ;;             (decode-or (ll) err)))))
      ;;    ;; (call/cc ; setjmp2000
      ;;    ;;    (λ (ret)
      ;;    ;;       (lets ((fail (λ (why) (ret null (err why)))))

      ;; ;; decode a full (possibly lazy) list of data, and succeed only if it exactly matches a fasl-encoded object

      ;; (define failed "fail") ;; a unique object

      ;; ;; ll fail → val | fail
      ;; (define (decode ll fail-val)
      ;;    (lets ((ll ob (decode-or ll (λ (why) failed))))
      ;;       (cond
      ;;          ((eq? ob failed) fail-val)
      ;;          ((null? (force-ll ll)) ob)
      ;;          (else fail-val))))

      ;; ;; byte-stream → (ob ...) | (ob ... err)
      ;; (define (decode-stream ll err)
      ;;    (cond
      ;;       ((pair? ll)
      ;;          (lets ((ll ob (decode-or ll (λ (why) failed))))
      ;;             (if (eq? ob failed)
      ;;                (list err)
      ;;                (pair ob (decode-stream ll err)))))
      ;;       ((null? ll) null)
      ;;       (else (decode-stream (ll) err))))

      ;; (define fasl-decode decode)

      (define (fasl2-save obj path)
         (define port (open-output-file path))
         (when port
            (write-bytes port (fasl2-encode obj))
            (close-port port)))

))


;; ;; (import (otus fasl))
;; ;(print (send-number 123456123456123456123456123456123456123456123456 #f))

;; ;(print (fasl-encode 'xyz))
;; (print
;;    (force-ll (encode 1111111111111111111)))
