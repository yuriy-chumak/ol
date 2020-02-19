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

   ; -----------------------------
   ; r7rs small (scheme base) list
   *
   +
   -
      ;; ...
   /
   <
   <=
   =
      ;; =>
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
      ;; binary-port?
      ;; boolean=?
   boolean?
   bytevector
      ;; bytevector-append
      ;; bytevector-copy
      ;; bytevector-copy!
   bytevector-length
   bytevector-u8-ref
      ;; bytevector-u8-set!
   bytevector?
   caar
   cadr
   call-with-current-continuation
      ;; call-with-port
      ;; call-with-values
   call/cc
   car
   case
   cdar
   cddr
   cdr
   ceiling
      ;; char->integer
      ;; char-ready?
      ;; char<=?
      ;; char<?
      ;; char=?
      ;; char>=?
      ;; char>?
      ;; char?
      ;; close-input-port
      ;; close-output-port
   close-port
   complex?
   cond
;  cond-expand  * builtin, (lang eval)
   cons
      ;; current-error-port
      ;; current-input-port
      ;; current-output-port
   define
   define-record-type
;  define-syntax  * builtin (lang eval)
   define-values
   denominator
      ;; do
      ;; dynamic-wind
      ;; else ;?
   eof-object     ; 6.13.2  Input
   eof-object?    ; 6.13.2  Input
   eq?
   equal?
   eqv?
      ;; error
      ;; error-object-irritants
      ;; error-object-message
      ;; error-object?
   even?
   exact
      ;; exact-integer-sqrt
   exact-integer?
   exact?
   expt
      ;; features
      ;; file-error?
   floor
      ;; floor-quotient
      ;; floor-remainder
      ;; floor/
      ;; flush-output-port
   for-each
   gcd
      ;; get-output-bytevector
      ;; get-output-string
      ;; guard
   if
   include
   include-ci
   inexact
   inexact?
      ;; input-port-open?
      ;; input-port?
      ;; integer->char
   integer?
   lambda
   lcm
   length
   let
   let*
   let*-values
      ;; let-syntax
      ;; let-values
   letrec
      ;; letrec*
      ;; letrec-syntax
   list
      ;; list->string
   list->vector
      ;; list-copy
   list-ref
   list-set!
      ;; list-tail
   list?
   make-bytevector
      ;; make-list
      ;; make-parameter
      ;; make-string
   make-vector
   map
   max
   member
   memq
   memv
   min
   modulo
   negative?
      ;; newline
   not
   null?
      ;; number->string
   number?
   numerator
   odd?
      ;; open-input-bytevector
      ;; open-input-string
      ;; open-output-bytevector
      ;; open-output-string
   or
      ;; output-port-open?
      ;; output-port?
   pair?
      ;; parameterize
      ;; peek-char
      ;; peek-u8
   port?
   positive?
   procedure?
   quasiquote
   quote
   quotient
      ;; raise
      ;; raise-continuable
   rational?
      ;; rationalize
      ;; read-bytevector
      ;; read-bytevector!
      ;; read-char
      ;; read-error?
      ;; read-line
      ;; read-string
      ;; read-u8
   real?
   remainder
   reverse
   round
   set!
   set-car!
   set-cdr!
      ;; square
   string
   string->list
      ;; string->number
   string->symbol
      ;; string->utf8
   string->vector
   string-append
      ;; string-copy
      ;; string-copy!
      ;; string-fill!
      ;; string-for-each
   string-length
      ;; string-map
   string-ref
      ;; string-set!
   string<=?
   string<?
   string=?
   string>=?
   string>?
   string?
   substring
   symbol->string
   symbol=?
   symbol?
      ;; syntax-error
      ;; syntax-rules
      ;; textual-port?
      ;; truncate
      ;; truncate-quotient
      ;; truncate-remainder
      ;; truncate/
      ;; u8-ready?
      ;; unless
      ;; unquote
      ;; unquote-splicing
      ;; utf8->string
   values
   vector
   vector->list
      ;; vector->string
      ;; vector-append
      ;; vector-copy
      ;; vector-copy!
      ;; vector-fill!
      ;; vector-for-each
      ;; vector-length
      ;; vector-map
      ;; vector-ref
      ;; vector-set!
   vector?
   when
      ;; with-exception-handler
      ;; write-bytevector
      ;; write-char
      ;; write-string
      ;; write-u8
   zero?
)
   (import
      (scheme core) (src vm)
      (scheme vectors)
      (scheme bytevector)
      ;; (scheme dynamic-bindings) ; 4.2.6 Dynamic bindings (required coroutines)
      (owl io) (owl math) (owl math-extra)
      (owl string))

   (begin

      ; 4.1.7. Inclusion
      ;
      ; syntax: (include hstring1i hstring2i ...)  * not supported
      (setq include (lambda args
         (runtime-error "No include is allowed." "(use ,load instead)")))
      ; syntax: (include-ci hstring1i hstring2i ...)  * not supported
      (setq include-ci (lambda args
         (runtime-error "No include-ci is allowed." "(use ,load instead)")))

      ; 5.5  Record-type definitions
      ; syntax:  (define-record-type <name> <constructor> <pred> <field> ...)  * not supported
      (setq define-record-type (lambda (name constructor pred . fields)
         (runtime-error "No define-record-type is implemented." #null)))

      ; 6.4  Pairs and lists
      ; ...
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
