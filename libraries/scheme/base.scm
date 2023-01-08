(define-library (scheme base)
(export
   (exports (scheme core))
   (exports (scheme process-context))

   caaar caadr cadar caddr ; moved from (scheme cxr)
   cdaar cdadr cddar cdddr ; moved from (scheme cxr)

   ; -----------------------------------------
   ; r7rs small (scheme base) list
   *
   +
   -
;  ...            * reserved for use by Scheme
   /
   <
   <<     ; * ol specific
   <=
   =
;  =>             * reserved for use by Scheme
   >
   >>     ; * ol specific
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
   boolean=?
   boolean?
   bytevector
   bytevector-append
   bytevector-copy
   bytevector-copy!
   bytevector-length
   bytevector-u8-ref  ; [yc] why not bytevector-ref like vector-ref?
   bytevector-u8-set! ; [yc] why not bytevector-set! like vector-set!?
   bytevector?
   caar
   cadr
   call-with-current-continuation
      ;; call-with-port
   call-with-values
   call/cc
   car
   case
   cdar
   cddr
   cdr
   ceiling
   char->integer
      ;; char-ready?
   char<=?
   char<?
   char=?
   char>=?
   char>?
   char?
      ;; close-input-port
      ;; close-output-port
   close-port
   complex?
   cond
;  cond-expand    * builtin, (lang eval)
   cons cons*
   current-error-port
   current-input-port
   current-output-port
   define
   define-record-type
;  define-syntax  * builtin (lang eval)
   define-values
   denominator
      ;; do
      ;; dynamic-wind
;  else           * reserved for use by Scheme
   eof-object
   eof-object?
   eq?
   equal?
   eqv?
      ;; error
      ;; error-object-irritants
      ;; error-object-message
      ;; error-object?
   even?
   exact
   exact-integer-sqrt
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
   fold foldr ; * ol specific
   for-each
   gcd
      ;; get-output-bytevector
      ;; get-output-string
      ;; guard
   has?       ; * ol specific
   if
   include
   include-ci
   inexact
   inexact?
      ;; input-port-open?
      ;; input-port?
   integer->char
   integer?
   lambda
   lcm
   length
   let
   let*
      ;; let*-values
      ;; let-syntax
      ;; let-values
   letrec
      ;; letrec*
      ;; letrec-syntax
   list
   list->string
   list->vector
   list-copy
   list-ref
   list-set!
   list-tail
   list?
   make-bytevector
   make-list
      ;; make-parameter
   make-string
   make-vector
   map
   max
   member
   memq
   memv
   min
   modulo
   negative?
   newline
   not
   null?
   number->string
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
   raise
   raise-continuable
   rational?
   rationalize
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
   square
   sqrt ;; * not included in r7rs (scheme base), but we do because it's often used
   string
   string->list
   string->number  ; (scheme misc)
   string->symbol
   string->utf8
   string->vector
   string->bytes   ; * ol specific
   string-append
      ;; string-copy
      ;; string-copy!
      ;; string-fill!
      ;; string-for-each
   string-length
   string-map
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
   unless
;  unquote           * reserved for use by Scheme, used in (lang sexp)
;  unquote-splicing  * reserved for use by Scheme, used in (lang sexp)
   utf8->string
   values
   vector
   vector->list
   vector->string
   vector-append
   vector-copy
   vector-copy!
   vector-fill!
   vector-for-each
   vector-length
   vector-map
   vector-ref
   vector-set!
   vector?
   when
;  with-exception-handler  * (scheme exceptions)
   write-bytevector
   write-char
   write-string
   write-u8
   zero?
)
   (import
      (scheme core) (src vm)
      (scheme list)
      (scheme vector)
      (scheme bytevector)
      (scheme string)
      (scheme exceptions)
      ;; (scheme dynamic-bindings) ; 4.2.6 Dynamic bindings (coroutines required)
      (owl io) (owl math) (owl math-extra)
      (scheme inexact)
      (owl string)
      (scheme process-context))

   (begin
      ; * internal staff
      (setq base-profile-error (lambda (function module)
         (runtime-error "Base profile error:"
            (cons "Function" (cons function (cons "require to import" (cons module (cons "module." #null))))))))

      (define-syntax declare-external
         (syntax-rules (quote)
            ((declare-external function module)
               (setq function (lambda args
                  (base-profile-error (quote function) module))))))

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

      ; 6.2.7.  Numerical input and output

      ; procedure:  (number->string z)        * (scheme base)
      ; procedure:  (number->string z radix)  * (scheme base)
      (define number->string (case-lambda
         ((n)  (list->string (render-number n '() 10)))
         ((n radix)
               (list->string (render-number n '() radix)))))

      (assert (number->string 0)            ===> "0")
      (assert (number->string 1.2)          ===> "6/5")
      (assert (number->string 1.2 4)        ===> "12/11")
      (assert (number->string -77)          ===> "-77")
      (assert (number->string 7-4i)         ===> "7-4i")
      (assert (number->string +inf.0)       ===> "+inf.0")

      ; procedure:  (string->number z)        * (scheme misc)
      ; procedure:  (string->number z radix)  * (scheme misc)
      (declare-external string->number '(scheme misc))

      ; 6.3.  Booleans
      ; procedure: (boolean=? boolean1 boolean2 boolean3 ...)
      (define boolean=? (case-lambda
         ((a . b)
            (let loop ((b b))
               (if (null? b)
                  #true
                  (if (eq? (car b) a)
                     (loop (cdr b))))))
         (() #true)))

      ; 6.4.  Pairs and lists
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
      (define (make-ass* comparer) ; *internal staff
         (letrec ((f (lambda (obj alist)
                        (unless (null? alist)
                           (if (comparer (caar alist) obj)
                              (car alist)
                              (f obj (cdr alist)))))))
            f))
      (define assq (make-ass* eq?))
      (define assv (make-ass* eqv?))
      (define assoc
         (case-lambda
            ((obj alist)
               ((make-ass* equal?) obj alist))
            ((obj alist compare)
               ((make-ass* compare) obj alist))))

      (assert (assoc 'oak
         '((pine . cones) (oak . acorns) (maple . seeds)))  ===> '(oak . acorns))
      (assert (assoc 'birch '((pine . cones)))              ===> #false)

      (assert (assq 'a '((a 1) (b 2) (c 3)))                ===> '(a 1))
      (assert (assq 'b '((a 1) (b 2) (c 3)))                ===> '(b 2))
      (assert (assq 'd '((a 1) (b 2) (c 3)))                ===> #false)
      (assert (assq '(a) '(((a)) ((b)) ((c))))              ===> #false)
      (assert (assq 5 '((2 3) (5 7) (11 13)))               ===> '(5 7)) ; * ol specific, (but in r7rs unspecified)
      (assert (assv 5 '((2 3) (5 7) (11 13)))               ===> '(5 7))

      ; procedure:  (list-copy obj)
      ;
      ; Returns a newly allocated copy of the given obj if it is a
      ; list. Only the pairs themselves are copied; the cars of the
      ; result are the same (in the sense of eqv?) as the cars of list.
      (define (list-copy obj)
         (map (lambda (o) (vm:cast o (type o))) obj))

      ;; 6.6.  Characters

      ; procedure:  (char? obj)
      (define (char? o) (eq? (type o) type-enum+))

      ; * internal staff
      (define (compare cmp a b)
         (let loop ((a a) (b b))
            (or (null? b)
                (and (cmp a (car b))
                     (loop (car b) (cdr b))))))

      ; procedure:  (char=? char1 char2 ...)
      (define (char=? a . b)
         (compare eq? a b))

      ; procedure:  (char<? char1 char2 ...)
      (define (char<? a . b)
         (compare less? a b))

      ; procedure:  (char>? char1 char2 ...)
      (define (char>? a . b)
         (compare greater? a b))

      ; procedure:  (char<=? char1 char2 ...)
      (define (less-eq? a b)
         (or (less? a b) (eq? a b)))
      (define (char<=? a . b)
         (compare less-eq? a b))

      ; procedure:  (char>=? char1 char2 ...)
      (define (greater-eq? a b)
         (or (greater? a b) (eq? a b)))
      (define (char>=? a . b)
         (compare greater-eq? a b))

      (define char->integer idf)
      (define integer->char idf)

      ;; 6.6.  Characters (additional staff)



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


      ; 6.13.  Input and output
      ; read
      (define (eof-object) #eof)
      (define (eof-object? o) (eq? o #eof))

      (define (current-input-port) stdin)
      (define (current-output-port) stdout)
      (define (current-error-port) stderr)

      (define write-u8 (case-lambda
         ((u8)      (syscall 1 stdout (bytevector u8) 1))
         ((u8 port) (syscall 1   port (bytevector u8) 1))))

      (define write-char write-u8)

      (define write-bytevector (case-lambda
         ((bv)                 (syscall 1 stdout bv (size bv)))
         ((bv port)            (syscall 1   port bv (size bv)))
         ((bv port start)      (syscall 1   port (bytevector-copy bv start)     (- (size bv) start)))
         ((bv port start end)  (syscall 1   port (bytevector-copy bv start end) (- end start -1)))))

      (define write-string (case-lambda
         ((ss)                 (write-bytevector (string->utf8 ss)))
         ((ss port)            (write-bytevector (string->utf8 ss) port))
         ((ss port start)      (write-bytevector (string->utf8 ss) port start))
         ((ss port start end)  (write-bytevector (string->utf8 ss) port start end))))

      (define newline (case-lambda
         (() (print))
         ((port) (print-to port))))

))
