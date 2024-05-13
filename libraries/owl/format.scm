(define-library (owl format)

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
      (only (owl math) format-number number?))

   (export
      make-writer  ;; names → ((obj tl) → (byte ... . tl)) ; TODO: remove

      write-format-ff ; write, write-simple
      write-formatter
      print-format-ff ; display, print
      print-formatter

      format
      formatter)

   (begin
      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      (define lp #\()
      (define rp #\))

      ;; helper function
      (define (regex? x)
         (and (eq? (type x) type-closure)
              (eq? (size x) 3)
              ; advanced checks
              (eq? (type (ref x 1)) type-procedure)
              (string?   (ref x 2))
              ;(eq?    (type (ref x 3)) type-closure)
         ))

      ; hack: do not include full (owl math fp) library and save 1k for image
      ;       we use only this three functions:

      (define (formatter this obj k)
         (cond
            ((this (type obj) #f) => (lambda (format)
               (format this obj k)))
            (else
               (cons* #\# #\w #\t #\f #\? k))))

      ;;; serialize suitably for parsing, not yet sharing preserving

      ;; hack: positive id = not written yet, negative = written, so just output a reference

      ; laziness changes:
      ;  - use explicit CPS to 'return'
      ;  - emit definition on first encounter
      ;; (define print-stats [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])
      ;; (define (@ n)
      ;;    (set-ref! print-stats n (++ (ref print-stats n))))

      ; default receipes
      (define (cook-string this obj tl)
         (format-string obj tl))

      (define (cook-quoted-string this obj tl)
         (cons #\"
            (format-quoted-string obj
               (cons* #\" tl))))

      (define (cook-number this obj tl)
         (format-number obj tl 10))

      (define (cook-symbol this obj tl)
         (format-symbol obj tl))

      (define (cook-const this obj k)
         (case obj
            (#null  (cons* #\( #\) k))
            (#true  (cons* #\# #\t #\r #\u #\e k))
            (#false (cons* #\# #\f #\a #\l #\s #\e k))
            (#empty (cons* #\# #\f #\f #\( #\) k))
            (#eof   (cons* #\# #\e #\o #\f k))
            (else   (cons* #\# #\? #\? k))))

      (define (cook-function this obj k)
         (if (regex? obj)
            (format-string (ref obj 2) k)
         else
            (let*((name ((this 'names {}) obj #f)))
               (format-string (if name
                     (string-append "#<" name ">")
                     "#function") k))))

      (define (cook-ff this obj k)
         (cons* #\# #\f #\f
            (formatter this (ff->alist obj) k)))


      ; receipe book
      ; (write), (write-simple)
      (define write-format-ff {
         type-symbol cook-symbol
         ; strings
         type-string cook-quoted-string
         type-string-wide cook-quoted-string
         type-string-dispatch cook-quoted-string
         ; true, false, etc.
         type-const cook-const
         ; numbers (todo: maybe use number? and (getf .. 'number))
         type-enum+ cook-number
         type-int+  cook-number
         type-enum- cook-number
         type-int-  cook-number
         type-rational cook-number
         type-inexact (lambda (this obj k)
               (if (this 'datum #f)
                  (cond ; write
                     ((equal? obj +nan.0) (cons* #\+ #\n #\a #\n #\. #\0 k))
                     ((equal? obj +inf.0) (cons* #\+ #\i #\n #\f #\. #\0 k))
                     ((equal? obj -inf.0) (cons* #\- #\i #\n #\f #\. #\0 k))
                     (else
                        (cons* #\# #\i (format-number obj k 10))))
               else ; write-simple
                  (format-number obj k 10)))
         type-complex cook-number
         ; functions
         type-procedure cook-function
         type-closure   cook-function
         type-bytecode  cook-function
         ; others
         type-port (lambda (this obj k)
               (cons* #\# #\< #\f #\d #\space
                  (format-number (vm:cast obj (if (value? obj) type-enum+ type-int+)) (cons* #\> k))))
         type-vptr (lambda (this obj k)
               (cons* #\# #\v #\p #\t #\r k))

         ; list
         type-pair (lambda (this obj k)
               (cons #\(
                  (let loop ((obj obj))
                     (cond
                        ((null? obj) ;; run of the mill list end
                           (cons* #\) k))
                        ((pair? obj)
                           ;; render car, then cdr
                           (formatter this (car obj)
                              (delay
                                 (if (null? (cdr obj))
                                    (loop (cdr obj))
                                    (cons #\space (loop (cdr obj)))))))
                        (else
                           ;; improper list
                           (cons* #\. #\space
                              (formatter this obj
                                 (λ () (cons* #\) k))))))))) ;(

         type-vector (lambda (this obj k)
               (cons* #\#
                  (formatter this (vector->list obj) k)))
               ;; (cons* #\# #\(
               ;;    (let loop ((n 1))
               ;;       (cond
               ;;          ((less? (size obj) n)
               ;;             (cons* #\) k))
               ;;          (else
               ;;             ((setup 'format) setup (ref obj n) ; render car, then cdr
               ;;                (delay
               ;;                   (if (eq? n (size obj))
               ;;                      (loop (+ n 1))
               ;;                      (cons #\space (loop (+ n 1)))))))))))

         type-bytevector (lambda (this obj k)
               (cons* #\# #\u #\8
                  (formatter this (bytevector->list obj) k))) ;; todo: should convert incrementally

         24 cook-ff  25 cook-ff  26 cook-ff  27 cook-ff

         ;; TODO:
            ;; ((rlist? obj) ;; fixme: rlist not parsed yet
            ;;    (cons* #\# #\r (write-formatter formatter (rlist->list obj) k)))

            ;; ((blob? obj)
            ;;    (cons #\#
            ;;       (write-formatter formatter (blob->list obj) k))) ;; <- should convert incrementally!
      })

      (define (write-formatter obj k)
         (formatter write-format-ff obj k))


      ; (display), (print)
      (define print-format-ff (ff-replace write-format-ff
      {
         type-inexact cook-number

         type-string cook-string
         type-string-wide cook-string
         type-string-dispatch cook-string

         type-pair (lambda (this obj k)
               (cons #\(
                  (cdr
                     (let loop ((obj obj) (tl (cons #\) k)))
                        (cond
                           ((null? obj) tl)
                           ((pair? obj)
                              (cons #\space
                                 (formatter this (car obj) (loop (cdr obj) tl))))
                           (else
                              (cons* #\space #\. #\space (formatter this obj tl))))))))
      }))

      ; 
      (define (print-formatter obj k)
         (formatter print-format-ff obj k))

      (define format print-formatter)


      ; ------------------------------
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

      (define (make-lazy-writer setup)
         (define datum? (setup 'datum #f))
         (define this (ff-replace write-format-ff setup))

         (λ (val tl) ; todo: remove (setup 'format)
            (maybe-quote val (formatter this val (λ () tl)) datum?)))

      (define (make-writer setup)
         (let ((serialize-lazy (make-lazy-writer setup)))
            (λ (val tl)
               (force-ll ; todo: change to "force" after scheme core be changed
                  (serialize-lazy val tl)))))

))
