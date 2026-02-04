(define-library (lang embed)
   (export
      make-entry)

   (import
      (scheme base)
      (owl ff) (owl list)
      (scheme vector)
      (owl io) (owl math)
      (otus symbols)
      (lang assemble)
      (lang threading))

(begin

   (define (symbols-of node)
      (define tag (list 'syms))

      (define (walk trail node)
         (cond
            ((value? node) trail)
            ((get trail node #false) trail)
            ((symbol? node)
               (let ((trail (put trail node 1)))
                  (put trail tag
                     (cons node (get trail tag null)))))
            ((ref node 0) ; (ref 0) works only for blobs
               (cond
                  ((eq? (type node) type-bytecode) #t)
                  ((eq? (type node) type-string) #t)
                  ((eq? (type node) type-port) #t)
                  ((eq? (type node) type-bytevector) #t)
                  ((eq? (type node) type-inexact) #t)
                  ((eq? (type node) type-vptr) #t)
                  (else (print "unknown raw object: " node)))
               trail)
            (else
               (fold walk
                  (put trail node #true)
                  (vector->list node)))))
      (define trail
         (walk (put empty tag null) node))

      (get
         (walk (put empty tag null) node)
         tag null))

   (define (code-refs seen obj)
      (cond
         ((value? obj) (values seen empty))
         ((bytecode? obj)
            (values seen (put empty obj 1)))
         ((get seen obj #false) =>
            (λ (here) (values seen here)))
         (else
            (let loop ((seen seen) (lst (vector->list obj)) (here empty))
               (if (null? lst)
                  (values (put seen obj here) here)
                  (let* ((seen this (code-refs seen (car lst))))
                     (loop seen (cdr lst)
                        (ff-union + this here))))))))
   (define (codes-of ob)
      (let* ((refs this (code-refs empty ob)))
         (ff-fold (λ (out x n) (cons (cons x x) out)) null this)))


   (define (make-entry main)
      (let ((symbols (symbols-of main))
            (codes   (codes-of   main)))
         (vm:new type-constructor
            (λ (args)
               (start-thread-controller
                  (list ; just 1 thread
                     [main-thread
                        (λ ()
                           (start-io-scheduler)
                           (fork-symbol-interner symbols) ; todo: rename to start-symbol-interner
                           (fork-bytecode-interner codes) ; todo: rename to start-bytecode-interner
                           (main args))] ))))))

))