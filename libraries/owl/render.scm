
;; making printable representations

(define-library (owl render)

   (import
      (scheme core)
      (owl string)
      (owl list)
      (owl list-extra)
      (owl symbol)
      (owl ff)
      (owl tuple)
      (owl rlist)
      (owl interop)
      (owl lazy)
      (owl math)
      (scheme bytevector)
      (scheme vectors)
      (otus blobs)
      (only (owl fasl) sub-objects)
      (only (owl math) render-number number?)
      (only (owl string) render-string string? string->list))

   (export
      make-serializer    ;; names → ((obj tl) → (byte ... . tl))
      ;serialize       ;; obj tl        → (byte ... . tl), eager, always shared
      ;serialize-lazy  ;; obj tl share? → (byte ... . tl), lazy, optional sharing
      render          ;; obj tl        → (byte ... . tl) -- deprecated
      )

   (begin
      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      (define lp #\()
      (define rp #\))

      ; hack: do not include full (owl math fp) library and save 1k for image
      ;       we use only this three functions:

      ;; this could be removed?
      (define (make-renderer meta)
         (define (render obj tl)
            (cond
               ((null? obj)
                  (ilist #\( #\) tl))

               ((number? obj)
                  (render-number obj tl 10))

               ((string? obj)
                  (render-string obj tl))

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
                                 (ilist #\space #\. #\space (render obj tl))))))))

               ((symbol? obj)
                  (render (symbol->string obj) tl))

               ((bytevector? obj)
                  (ilist #\# #\u #\8 (render (bytevector->list obj) tl)))

               ((blob? obj)
                  (cons #\# (render (blob->list obj) tl)))

               ((vector? obj)
                  (cons #\# (render (vector->list obj) tl)))
                  ;; (ilist #\# #\(
                  ;;    (render (ref obj 1)
                  ;;       (fold
                  ;;          (λ (tl pos) (cons #\space (render (ref obj pos) tl)))
                  ;;          (cons #\) tl)
                  ;;          (lrange (size obj) -1 1)))))

               ((function? obj)
                  (render "#<function>" tl))
                  ;; anonimas
                  ;(let ((symp (interact 'intern ['get-name obj])))
                  ;   (if symp
                  ;      (ilist #\# #\< (render symp (cons #\> tl)))
                  ;      (render "#<function>" tl))))


; disabled, because records currently unload
;               ((record? obj)
;                  (ilist #\# #\{
;                     (render (ref obj 1) ;; type tag object
;                        (fold
;                           (λ (tl pos) (cons 32 (render (ref obj pos) tl)))
;                           (cons #\} tl)
;                           (lrange (size obj) -1 1)))))

               ((rlist? obj) ;; fixme: rlist not parsed yet
                  (ilist #\# #\r (render (rlist->list obj) tl)))

               ((ff? obj) ;; fixme: ff not parsed yet this way
                  (ilist #\# #\f #\f (render (ff->alist obj) tl)))

               ((eq? obj #true)  (ilist #\# #\t #\r #\u #\e tl))
               ((eq? obj #false) (ilist #\# #\f #\a #\l #\s #\e tl))
               ((eq? obj #empty) (ilist #\# #\e #\m #\p #\t #\y tl)) ;; don't print as #()
               ((eq? obj #eof)   (ilist #\# #\e #\o #\f tl))

               ((port? obj) (ilist #\# #\< #\f #\d #\space (render (vm:cast obj type-fix+) (cons #\> tl))))

               ((eq? (type obj) type-const)
                  (render-number (vm:cast obj type-fix+) tl 16))

               (else
                  (append (string->list "#<WTF>") tl)))) ;; What This Format?
         render)

      (define render
         (make-renderer #empty))

      ;;; serialize suitably for parsing, not yet sharing preserving

      ;; hack: positive id = not written yet, negative = written, so just output a reference

      ; laziness changes:
      ;  - use explicit CPS to 'return'
      ;  - emit definition on first encounter

      (define (make-ser names)
         (define (ser sh obj k)
            (cond

               ;((getf sh obj) =>
               ;   (λ (id)
               ;      (if (< id 0) ;; already written, just refer
               ;         (ilist #\# (render (abs id) (pair #\# (k sh))))
               ;         (ilist #\#
               ;            (render id
               ;               (ilist #\# #\=
               ;                  (ser (del sh obj) obj
               ;                     (λ (sh)
               ;                        (delay
               ;                           (k (put sh obj (- 0 id))))))))))))

               ((null? obj)
                  (ilist #\' #\( #\) (k sh)))

               ((number? obj)
                  (render-number obj (delay (k sh)) 10))

               ((string? obj)
                  (cons #\" ;"
                     (render-quoted-string obj  ;; <- all eager now
                        (pair #\" (k sh))))) ;"

               ((pair? obj)
                  (cons #\(
                     (let loop ((sh sh) (obj obj))
                        (cond
                           ((null? obj)
                              ;; run of the mill list end
                              (pair #\) (k sh)))
                           ;((getf sh obj) =>
                           ;   (λ (id)
                           ;      (ilist #\. #\space #\#
                           ;         (render (abs id)
                           ;            (cons #\#
                           ;               (if (< id 0)
                           ;                  (pair 41 (k sh))
                           ;                  (pair #\=
                           ;                     (ser (del sh obj) obj
                           ;                        (λ (sh)
                           ;                           (pair 41
                           ;                              (k
                           ;                                 (put sh obj
                           ;                                    (- 0 id)))))))))))))
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
                              (ilist #\. #\space
                                 (ser sh obj
                                    (λ (sh) (pair 41 (k sh))))))))))

               ((boolean? obj)
                  (append
                     (string->list (if obj "#true" "#false"))
                     (delay (k sh))))

               ((symbol? obj)
                  (render (symbol->string obj) (delay (k sh))))

               ((bytevector? obj)
                  (ilist #\# #\u #\8
                     (ser sh (bytevector->list obj) k))) ;; <- should convert incrementally!

               ((blob? obj)
                  (cons #\#
                     (ser sh (blob->list obj) k))) ;; <- should convert incrementally!

               ((vector? obj)
                  (cons #\# (cons #\(
                     (let loop ((sh sh) (n 1))
                        (cond
                           ((less? (size obj) n)
                              (pair #\) (k sh)))
                           (else
                              (ser sh (ref obj n) ; render car, then cdr
                                 (λ (sh)
                                    (delay
                                       (if (eq? n (size obj))
                                          (loop sh (+ n 1))
                                          (cons #\space (loop sh (+ n 1)))))))))))))

               ;; render name is one is known, just function otherwise
               ;; todo: print `(foo ,map ,+ -) instead of '(foo #<map> <+> -) ; ?, is it required
               ((function? obj)
                  (foldr render (delay (k sh)) (list "#<" (get names obj "function") ">")))

               ((rlist? obj) ;; fixme: rlist not parsed yet
                  (ilist #\# #\r (ser sh (rlist->list obj) k)))

               ((eq? obj #empty) ;; don't print as #()
                  (ilist #\# #\e #\m #\p #\t #\y (delay (k sh))))

               ((ff? obj) ;; fixme: ff not parsed yet this way
                  (ilist #\# #\f #\f (ser sh (ff->alist obj) k)))

               ((port? obj)   (render obj (λ () (k sh))))
               ((eof? obj)    (render obj (λ () (k sh))))

               (else
                  (append (string->list "#<WTF>") (delay (k sh))))))
         ser)

      (define (self-quoting? val)
         (or  ; note, all immediates are
            (number? val) (string? val) (boolean? val) (function? val)
            (port? val) (vector? val) (bytevector? val) (null? val)
            (rlist? val) (empty? val)))

      ;; could drop val earlier to possibly gc it while rendering
      (define (maybe-quote val lst)
         (if (self-quoting? val)
            lst
            (cons #\' lst)))

      ;; a value worth checking for sharing in datum labeler
      (define (shareable? x)
         (not (or (function? x) (symbol? x) (port? x))))

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
                  null refs)))
            (let loop ((out empty) (shares shares) (n 1))
               (if (null? shares)
                  out
                  (loop (put out (car shares) n) (cdr shares) (+ n 1))))))

      (define (make-lazy-serializer names)
         (let ((ser (make-ser names)))
            (λ (val tl share?)
               (maybe-quote val
                  (ser
                     (if share? ;; O(n), allow skipping
                        (label-shared-objects val)
                        empty)
                     val (λ (sh) tl))))))

      (define (make-serializer names)
         (let ((serialize-lazy (make-lazy-serializer names)))
            (λ (val tl)
               (force-ll
                  (serialize-lazy val tl #true)))))

))
