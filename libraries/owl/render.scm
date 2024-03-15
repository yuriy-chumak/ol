
;; making printable representations

(define-library (owl render)

   (import
      (scheme core)
      (scheme list)
      (owl string)
      (owl list-extra)
      (owl symbol)
      (owl ff)
      (owl rlist)
      (otus async)
      (owl lazy)
      (owl math)
      (scheme bytevector)
      (scheme vector)
      (otus blobs)
      (only (owl math) render-number number?)
      (only (owl string) render-string string? string->list))

   (export
      make-serializer  ;; names → ((obj tl) → (byte ... . tl))
      render)          ;; obj tl            → (byte ... . tl)

   (begin
      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      (define lp #\()
      (define rp #\))

      ; hack: do not include full (owl math fp) library and save 1k for image
      ;       we use only this three functions:

      ; (display), (print)
      (define (make-renderer meta)
         (define (render obj tl)
            (cond
               ((number? obj)
                  (render-number obj tl 10))

               ((string? obj)
                  (render-string obj tl))

               ((symbol? obj)
                  (render-symbol obj tl))

               ((pair? obj)
                  (cons #\(
                     (cdr
                        (let loop ((obj obj) (tl (cons #\) tl)))
                           (cond
                              ((null? obj) tl)
                              ((pair? obj)
                                 (cons #\space
                                    (render (car obj) (loop (cdr obj) tl))))
                              (else
                                 (cons* #\space #\. #\space (render obj tl))))))))

               ((vector? obj)
                  (cons #\# (render (vector->list obj) tl)))

               ((eq? obj #null)
                  (cons* #\( #\) tl))

               ((eq? obj #true)  (cons* #\# #\t #\r #\u #\e tl))
               ((eq? obj #false) (cons* #\# #\f #\a #\l #\s #\e tl))
               ((eq? obj #empty) (cons* #\# #\e #\m #\p #\t #\y tl)) ;; don't print as #()
               ((eq? obj #eof)   (cons* #\# #\e #\o #\f tl))

               ((function? obj)
                  (render "#function" tl))
                  ;; anonimas
                  ;(let ((symp (interact 'intern ['get-name obj])))
                  ;   (if symp
                  ;      (cons* #\# #\< (render symp (cons #\> tl)))
                  ;      (render "#<function>" tl))))

               ((bytevector? obj)
                  (cons* #\# #\u #\8 (render (bytevector->list obj) tl)))

               ((ff? obj) ;; fixme: ff not parsed yet this way
                  (cons* #\# #\f #\f (render (ff->alist obj) tl)))

               ((port? obj) (cons* #\# #\< #\f #\d #\space (render (vm:cast obj type-enum+) (cons #\> tl))))

               ((eq? (type obj) type-const) ; ???
                  (render-number (vm:cast obj type-enum+) tl 16))

               ((eq? (type obj) type-vptr)
                  (append (string->list "#vptr") tl))

               ((blob? obj)
                  (cons #\# (render (blob->list obj) tl)))


; disabled, because records currently unload
;               ((record? obj)
;                  (cons* #\# #\{
;                     (render (ref obj 1) ;; type tag object
;                        (fold
;                           (λ (tl pos) (cons 32 (render (ref obj pos) tl)))
;                           (cons #\} tl)
;                           (lrange (size obj) -1 1)))))

               ((rlist? obj) ;; fixme: rlist not parsed yet
                  (cons* #\# #\r (render (rlist->list obj) tl)))

               (else
                  (cons* #\# #\w #\t #\f #\? tl)))) ;; What This Format?
         render)

      (define render
         (make-renderer #empty))

      ;;; serialize suitably for parsing, not yet sharing preserving

      ;; hack: positive id = not written yet, negative = written, so just output a reference

      ; laziness changes:
      ;  - use explicit CPS to 'return'
      ;  - emit definition on first encounter
      ;; (define print-stats [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])
      ;; (define (@ n)
      ;;    (set-ref! print-stats n (++ (ref print-stats n))))

      ; todo: add regex printing
      ; (write)
      (define (make-ser names datum?)
         (define (ser sh obj k)
            (cond
               ; most likely
               ((symbol? obj)
                  (render-symbol obj (delay (k sh))))

               ; lists
               ((pair? obj)
                  (cons #\(
                     (let loop ((sh sh) (obj obj))
                        (cond
                           ((null? obj)
                              ;; run of the mill list end
                              (lcons #\) (k sh)))
                           ((pair? obj)
                              ;; render car, then cdr
                              (ser sh (car obj)
                                 (λ (sh)
                                    (delay
                                       (if (null? (cdr obj))
                                          (loop sh (cdr obj))
                                          (cons #\space (loop sh (cdr obj))))))))
                           (else
                              ;; improper list
                              (cons* #\. #\space
                                 (ser sh obj
                                    (λ (sh) (lcons #\) (k sh)))))))))) ;(

               ; numbers
               ((and datum?
                     (inexact? obj))  ; write, not write-simple
                  (cond
                     ((equal? obj +nan.0) (cons* #\+ #\n #\a #\n #\. #\0 (delay (k sh))))
                     ((equal? obj +inf.0) (cons* #\+ #\i #\n #\f #\. #\0 (delay (k sh))))
                     ((equal? obj -inf.0) (cons* #\- #\i #\n #\f #\. #\0 (delay (k sh))))
                     (else
                        (cons* #\# #\i (render-number obj (delay (k sh)) 10)))))

               ; todo: datum? and rational? and denom is 10, 100, 1000, ... - write with dot
               ((number? obj)
                  (render-number obj (delay (k sh)) 10))

               ((string? obj)
                  (cons #\"
                     (render-quoted-string obj  ;; <- all eager now
                        (lcons #\" (k sh)))))

               ((vector? obj)
                  (cons #\# (cons #\(
                     (let loop ((sh sh) (n 1))
                        (cond
                           ((less? (size obj) n)
                              (lcons #\) (k sh)))
                           (else
                              (ser sh (ref obj n) ; render car, then cdr
                                 (λ (sh)
                                    (delay
                                       (if (eq? n (size obj))
                                          (loop sh (+ n 1))
                                          (cons #\space (loop sh (+ n 1)))))))))))))

               ((eq? obj #null)
                  (cons* #\( #\) (delay (k sh))))

               ((eq? obj #true)  (cons* #\# #\t #\r #\u #\e (delay (k sh))))
               ((eq? obj #false) (cons* #\# #\f #\a #\l #\s #\e (delay (k sh))))
               ((eq? obj #empty) (cons* #\# #\f #\f #\( #\) (delay (k sh))))
               ((eq? obj #eof)   (cons* #\# #\e #\o #\f (delay (k sh))))

               ;; render name is one is known, just function otherwise
               ;; todo: print `(foo ,map ,+ -) instead of '(foo #<map> <+> -) ; ?, is it required
               ((function? obj)
                  (let ((name (get names obj #f)))
                     (foldr render (delay (k sh))
                        (if name
                           (list "#<" name ">")
                        else
                           (list "#function")))))

               ((bytevector? obj)
                  (cons* #\# #\u #\8
                     (ser sh (bytevector->list obj) k))) ;; todo: should convert incrementally

               ((ff? obj)
                  (cons* #\# #\f #\f
                     (ser sh (ff->alist obj) k)))

               ((port? obj)
                  (render obj (λ () (k sh))))

               ((eq? (type obj) type-vptr)
                  (cons* #\# #\v #\p #\t #\r (delay (k sh))))

               ((rlist? obj) ;; fixme: rlist not parsed yet
                  (cons* #\# #\r (ser sh (rlist->list obj) k)))

               ((blob? obj)
                  (cons #\#
                     (ser sh (blob->list obj) k))) ;; <- should convert incrementally!

               (else
                  (cons* #\# #\w #\t #\f #\? (delay (k sh))))))
         ser)

      (define (const? x)
         (eq? (type x) type-const))

      (define (self-quoting? val datum?)
         (cond
            ((symbol? val)
               #false)
            ((pair? val)
               (if datum? #true #false))
            (else
               #true)))

      ;; could drop val earlier to possibly gc it while rendering
      (define (maybe-quote val lst datum?)
         (if (self-quoting? val datum?)
            lst
            (cons #\' lst)))

      ;; a value worth checking for sharing in datum labeler
      (define (shareable? x)
         (not (or (function? x) (symbol? x) (port? x))))


      (define (partial-object-closure root pred)
         (define (clos seen obj)
            (cond
               ((value? obj) seen)
               ((not (pred obj)) seen)
               ((getf seen obj) =>
                  (λ (n) (ff-update seen obj (+ n 1))))
               (else
                  (let ((seen (put seen obj 1)))
                     (if (ref obj 0) ; ==(blob? obj), 0 in ref works only for blobs
                        seen
                        (fold clos seen (vector->list obj)))))))
         (clos empty root))

      ;; don't return ff, type of which is changing atm
      (define (sub-objects root pred)
         (ff->alist
            (partial-object-closure root pred)))

      ;; val → ff of (ob → node-id)
      (define (label-shared-objects val)
         (lets
            ((refs (sub-objects val shareable?))
             (shares
               (fold
                  (λ (shared p)
                     (lets ((ob refs p))
                        (if (eq? refs 1)
                           shared
                           (cons ob shared))))
                  #null refs)))
            (let loop ((out empty) (shares shares) (n 1))
               (if (null? shares)
                  out
                  (loop (put out (car shares) n) (cdr shares) (+ n 1))))))

      (define (make-lazy-serializer names datum?)
         (let ((ser (make-ser names datum?)))
            (λ (val tl share?)
               (maybe-quote val
                  (ser
                     (if share? ;; O(n), allow skipping
                        (label-shared-objects val)
                        empty)
                     val (λ (sh) tl))
                  datum?))))

      (define (make-serializer names datum?)
         (let ((serialize-lazy (make-lazy-serializer names datum?)))
            (λ (val tl)
               (force-ll
                  (serialize-lazy val tl #true)))))

))
