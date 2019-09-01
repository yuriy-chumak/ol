(define-library (scheme base)
(export
   (exports (scheme core))
   (exports (scheme vectors))
   (exports (scheme bytevector))

      caaar caadr cadar caddr ; moved from (scheme cxr)
      cdaar cdadr cddar cdddr ; moved from (scheme cxr)

      ;; ; i/o from r6rs
      ;; current-input-port
      ;; current-output-port
      ;; current-error-port

      ; 6.4.  Pairs and lists
      assq assv assoc

      ; 6.13.2.  Input
      eof-object eof-object?
   )

   #| todo: this library should export these keywords:
      *
      +
      -
      ...
      /
      <
      <=
      =
      =>
      >
      >=
      abs
      and
      append
      apply
      assoc
      assq
      assv
      begin
      binary-port?
      boolean=?
      boolean?
      bytevector
      bytevector-append
      bytevector-copy
      bytevector-copy!
      bytevector-length
      bytevector-u8-ref
      bytevector-u8-set!
      bytevector?
      caar
      cadr
      call-with-current-continuation
      call-with-port
      call-with-values
      call/cc
      car
      case
      cdar
      cddr
      cdr
      ceiling
      char->integer
      char-ready?
      char<=?
      char<?
      char=?
      char>=?
      char>?
      char?
      close-input-port
      close-output-port
      close-port
      complex?
      cond
      cond-expand
      cons
      current-error-port
      current-input-port
      current-output-port
      define
      define-record-type
      define-syntax
      define-values
      denominator
      do
      dynamic-wind
      else
      + eof-object?
      equal?
      error
      error-object-message
      even?
      exact-integer-sqrt
      exact?
      features
      floor
      floor-remainder
      flush-output-port
      gcd
      get-output-string
      if
      include-ci
      inexact?
      input-port?
      integer?
      lcm
      let
      let*-values
      let-values
      letrec*
      list
      list->vector
      list-ref
      list-tail
      make-bytevector
      make-parameter
      make-vector
      max
      memq
      min
      negative?
      not
      number->string
      numerator
      open-input-bytevector
      open-output-bytevector
      or
      output-port?
      parameterize
      peek-u8
      positive?
      quasiquote
      quotient
      raise-continuable
      rationalize
      read-bytevector!
      read-error?
      read-string
      real?
      reverse
      set!
      set-cdr!
      string
      string->number
      string->utf8
      string-append
      + eof-object
      eq?
      eqv?
      error-object-irritants
      error-object?
      exact
      exact-integer?
      expt
      file-error?
      floor-quotient
      floor/
      for-each
      get-output-bytevector
      guard
      include
      inexact
      input-port-open?
      integer->char
      lambda
      length
      let*
      let-syntax
      letrec
      letrec-syntax
      list->string
      list-copy
      list-set!
      list?
      make-list
      make-string
      map
      member
      memv
      modulo
      newline
      null?
      number?
      odd?
      open-input-string
      open-output-string
      output-port-open?
      pair?
      peek-char
      port?
      procedure?
      quote
      raise
      rational?
      read-bytevector
      read-char
      read-line
      read-u8
      remainder
      round
      set-car!
      square
      string->list
      string->symbol
      string->vector
      string-copy
      string-copy!
      string-for-each
      string-map
      string-set!
      string<?
      string>=?
      string?
      symbol->string
      symbol?
      syntax-rules
      truncate
      truncate-remainder
      u8-ready?
      unquote
      utf8->string
      vector
      vector->string
      vector-copy
      vector-fill!
      vector-length
      vector-ref
      vector?
      with-exception-handler
      write-char
      write-u8
      string-fill!
      string-length
      string-ref
      string<=?
      string=?
      string>?
      substring
      symbol=?
      syntax-error
      textual-port?
      truncate-quotient
      truncate/
      unless
      unquote-splicing
      values
      vector->list
      vector-append
      vector-copy!
      vector-for-each
      vector-map
      vector-set!
      when
      write-bytevector
      write-string
      zero?
   |#
   (import
      (scheme core) (src vm)
      (scheme vector)
      (scheme bytevector)
      ;; (scheme dynamic-bindings) ; 4.2.6 Dynamic bindings (required coroutines)
      (owl io) (owl math))

   (begin

      ; due to frequent use, moved from (scheme cxr) to (scheme base)
      (define (caaar x) (car (car (car x))))
      (define (caadr x) (car (car (cdr x))))
      (define (cadar x) (car (cdr (car x))))
      (define (caddr x) (car (cdr (cdr x))))
      (define (cdaar x) (cdr (car (car x))))
      (define (cdadr x) (cdr (car (cdr x))))
      (define (cddar x) (cdr (cdr (car x))))
      (define (cdddr x) (cdr (cdr (cdr x))))


      ;; (define current-input-port (make-parameter stdin))
      ;; (define current-output-port (make-parameter stdout))
      ;; (define current-error-port (make-parameter stderr))

      ; 6.4  Pairs and lists

      ; procedure:  (assq obj alist)
      ; procedure:  (assv obj alist)
      ; procedure:  (assoc obj alist)
      ; procedure:  (assoc obj alist compare)

      ; These procedures find the first pair in alist whose car field
      ; is obj , and returns that pair. If no pair in alist has obj
      ; as its car, then #f (not the empty list) is returned.  The
      ; assq procedure uses eq? to compare obj with the car fields
      ; of the pairs in alist, while assv uses eqv? and assoc uses
      ; compare if given and equal? otherwise.
      (define (make-ass* comparer) ; helper
         (letrec ((f (lambda (obj list)
                        (unless (null? list)
                           (if (comparer (caar list) obj)
                              (car list)
                              (f obj (cdr list)))))))
            f))
      (define assq (make-ass* eq?))
      (define assv (make-ass* eqv?))
      (define assoc
         (case-lambda
            ((obj list)
               ((make-ass* equal?) obj list))
            ((obj list compare)
               ((make-ass* compare) obj list))))

      (assert (assoc 'oak
         '((pine . cones) (oak . acorns) (maple . seeds)))  ===> '(oak . acorns))
      (assert (assoc 'birch '((pine . cones)))              ===> #false)

      (assert (assq 'a '((a 1) (b 2) (c 3)))                ===> '(a 1))
      (assert (assq 'b '((a 1) (b 2) (c 3)))                ===> '(b 2))
      (assert (assq 'd '((a 1) (b 2) (c 3)))                ===> #false)
      (assert (assq '(a) '(((a)) ((b)) ((c))))              ===> #false)
      (assert (assq 5 '((2 3) (5 7) (11 13)))               ===> '(5 7)) ; * ol specific, (but in r7rs unspecified)
      (assert (assv 5 '((2 3) (5 7) (11 13)))               ===> '(5 7))

      ;; *********************
      ;; 6.10.  Control features
      ;
      ; This chapter describes various primitive procedures which control the flow of program
      ; execution in special ways. The procedure? predicate is also described here.

      ; procedure:  (procedure? obj)  * core
      ; procedure:  (apply proc arg1 ... args)  * builtin
      ; procedure:  (map proc list1 list2 ...)
      ; procedure:  (for-each proc list1 list2 ...)
      ; ...


      ; 6.13.2.  Input
      ; read
      (define (eof-object) #eof)
      (define (eof-object? o) (eq? o #eof))
))
