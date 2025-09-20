; minimal set of Scheme (including Ol subset)
(define-library (scheme core)
   (version 2.0)
   (license MIT/LGPL3)
   (keywords (scheme core ol))
   (description
      "Otus-Lisp scheme core library.")

   (import
      (src vm)           ; olvm сodes and primitives,
      ; that do not need to be exported by libraries:
      ;
      ; object creation/modification:
      ;   vm:new vm:make vm:alloc vm:cast
      ;   cons set-ref set-ref!
      ; other object manipulations:
      ;   car cdr ref type size eq? less?
      ; elementary math primitives:
      ;   integer:
      ;     vm:add vm:sub vm:mul vm:div vm:shr vm:shl
      ;   binary:
      ;     vm:and vm:ior vm:xor
      ; floating-point math (OLVM_INEXACTS and OLVM_BUILTIN_FMATH required):
      ;   vm:fp1 (fsqrt, fsin, fcos, ftan, fatan, flog, fexp, fasin, facos, ffloor)
      ;   vm:fp2 (fless?, fadd, fsub, fmul, fdiv, fatan2, flog2, fexpt)
      ; special:
      ;   vm:pin, vm:unpin, vm:deref
      ;   clock, syscall
      ; info:
      ;   vm:version vm:vmax vm:vsize vm:features
      ; associative arrays (fixed functions) support:
      ;   ff:red ff:black ff:toggle ff:red? ff:right?
      ; execution flow manipulation:
      ;   ff-apply vector-apply
      ;
      ; olvm exported functions:
      ;   apply apply/cc arity-error
      ;   call-with-current-continuation

      ; special forms declared in lang/env.scm:
      ;
      ; setq let-eval ifeq brae
      ; quote values lambda values-apply

      (srfi 16)  ; case-lambda, 4.2.9
      (srfi 87)  ; <= in cases
      (srfi 71)) ; (let* ((a b (values..

   ; -----------------------------------------------------------------
   ; internal ol staff
   (begin
      ; fast internal functions (-1 obj) and (+1 obj),
      ;  limitation - obj is atomic numbers,
      (setq -- (lambda (n)         ; * internal
         (values-apply (vm:sub n 1) (lambda (n carry) n))))
      (setq ++ (lambda (n)         ; * internal
         (values-apply (vm:add n 1) (lambda (n carry) n))))

      ; * ol specific: (runtime-error reason info)
      (setq runtime-error (case-lambda
         ((reason)
            (call-with-current-continuation
               (lambda (resume)
                  (vm:mcp resume 5 reason ""))))
         ((reason info)
            (call-with-current-continuation
               (lambda (resume)
                  (vm:mcp resume 5 reason info))))
         ((reason . info)
            (call-with-current-continuation
               (lambda (resume)
                  (vm:mcp resume 5 reason info))))
       ))

      ; * internal automation testing staff
      ; note: please be careful!
      ;       this is simplified 'assert' that uses 'eq?' before real 'equal?' be
      ;       defined to real implementation in 6.1
      ; todo: change ===> to the ==>, and ==== to the ===
      (define-syntax assert
         (syntax-rules (= ===> equal?)
            ((assert expression ===> expectation)
               (ifeq (equal? ((lambda (x) x) expression) expectation) #true
                  #true
                  (runtime-error "assertion error:"
                     (quote expression) "must be" (quote expectation))))
            ((assert expression = expectation)
               (ifeq (equal? ((lambda (x) x) expression) expectation) #true
                  #true
                  (runtime-error "assertion error:"
                     (quote expression) "must be" (quote expectation))))
            ((assert expression)
               (ifeq (equal? ((lambda (x) x) expression) #false) #true
                  (runtime-error "assertion error:"
                     (quote expression) "is not a true")
                  #true))))
      ; * 'equal?' stub
      ; note: allows basic assert implementation before real 'equal?' be
      ;       defined to real implementation in 6.1
      (setq equal? (lambda (a b)
         (ifeq a b #true #false)))


      ; Signaling errors in macro transformers (4.3.3)
      ;
      (define-syntax syntax-error
         (syntax-rules (runtime-error)
            ((syntax-error . staff)
               (runtime-error "syntax error: " (quote staff)))))

      ; * ol specific
      (setq error runtime-error) ; [yc] is it required?

      ; * internal staff
      (setq core-profile-error (lambda (function module)
         (runtime-error "Core profile error:"
            "Function" function "require to import" module "module.")))

      (setq make-vector ; #() and []
         (case-lambda
            ((v)
               (vm:make TVECTOR v))
            ((k fill)
               (vm:make TVECTOR k fill))))
   
      ; * Identity function
      (setq idf (lambda (x) x))

      ; not less
      (setq greater? (lambda (a b)
         (less? b a)))
   )

   ; =================================================================
   ; Scheme
   ;
   ; Revised(7) Report on the Algorithmic Language Scheme
   ;                  Dedicated to the Memory of ALGOL 60
   ;
   (begin

      ; =================================================================
      ; Programming languages should be designed not by piling feature on
      ; top of feature, but by removing the weaknesses and restrictions
      ; that make additional features appear necessary. Scheme demonstrates
      ; that a very small number of rules for forming expressions, with no
      ; restrictions on how they are composed, suffice to form a practical
      ; and efficient programming language that is flexible enough to support
      ; most of the major programming paradigms in use today.


      ;;; ---------------------------------------------------------------
      ;;; Chapter 4
      ;;; Expressions
      ;
      ; Expression types are categorized as primitive or derived.
      ; Primitive expression types include variables and procedure
      ; calls. Derived expression types are not semantically primitive,
      ; but can instead be defined as macros. With the exception
      ; of quasiquote, whose macro definition is complex,
      ; the derived expressions are classified as library features.
      ; Suitable definitions are given in section 7.3.

      ; 4.1  Primitive expression types

      ; 4.1.1  Variable references
      ;
      ; syntax:  <variable>                   * builtin
      ;
      ; An expression consisting of a variable (section 3.1) is a
      ; variable reference. The value of the variable reference is
      ; the value stored in the location to which the variable is
      ; bound. It is an error to reference an unbound variable.

      ; 4.1.2  Literal expressions
      ;
      ; syntax:  (quote <datum>)              * builtin
      ; syntax:  '<datum>                     * builtin
      ; syntax:  <constant>                   * builtin

      ; 4.1.3  Procedure calls
      ;
      ; syntax:  (<operator> <operand1> ...)  * builtin

      ; 4.1.4  Procedures
      ;
      ; syntax:  (lambda <formals> <body>)    * builtin
      (define-syntax λ                  ; * ol specific
         (syntax-rules ()
            ((λ . x) (lambda . x))))

      ; 4.1.5  Conditionals
      ;
      ; syntax:  (if <test> <consequent> <alternate>)
      ; syntax:  (if <test> <consequent>)
      ; auxiliary syntax: else            * ol specific
      ; todo: add (if (then ...) (else ...)) syntax
      (define-syntax if
         (syntax-rules (not eq? null? empty? then else begin)
            ((if val ok) (if val ok #f))
            ; true/false optimization
            ((if #true     ok otherwise) ok)
            ((if #false    ok otherwise) otherwise)
            ; expended if then else syntax:
            ((if val      ok else otherwise...) (if val ok (begin otherwise...)))
            ((if val then ok... else otherwise...) (if val (begin ok...) (begin otherwise...)))
            ((if val then ok...               ) (if val (begin ok...)))
            ; speedup, code size optimizations:
            ((if (not val) ok otherwise) (if val otherwise ok))
            ((if (eq? a b) ok otherwise) (ifeq a b ok otherwise))
            ((if (null? l) ok otherwise) (ifeq l #null ok otherwise))

            ((if (a . b)   ok otherwise) (let-eval (x) ((a . b)) (if x ok otherwise)))
            ((if val       ok otherwise) (ifeq val #false otherwise ok))))

      (assert (if (less? 2 3) 'yes 'no)               ===>  'yes)
      (assert (if (less? 3 2) 'yes 'no)               ===>  'no)

      ; 4.2.2  Conditionals

      ; syntax:  (and <test1> ...)
      (define-syntax and
         (syntax-rules ()
            ((and) #true)
            ((and a) a)
            ((and a . b)
               (if a (and . b) #false))))

      (assert (and (eq? 2 2) (less? 1 2))                 ===> #true)
      (assert (and (eq? 2 2) (less? 2 1))                 ===> #false)
      (assert (and 1 2 '(f g) 'c)                         ===> 'c)
      (assert (and)                                       ===> #true)

      ; syntax:  (or <test1> ...)
      (define-syntax or
         (syntax-rules ()
            ((or) #false)
            ((or (a . b) . c) ; additional optimization
               ((lambda (x) (or x . c))  (a . b)))
            ((or a) a)
            ((or a . b)
               (if a a (or . b)))))

      (assert (or (eq? 2 2) (less? 1 2))                  ===> #true)
      (assert (or (eq? 2 2) (less? 2 1))                  ===> #true)
      (assert (or #f #f #f)                               ===> #false)
      (assert (or #f 'c #f)                               ===> 'c)

      ; 4.1.6  Assignments
      ;
      ; syntax: (set! <variable> <expression>)  * not supported
      (setq set! (lambda (variable expression)
         (runtime-error "No set! is allowed." "(sometimes you can use set-ref!, check the docs)")))

      (setq box (lambda args args))
      (setq unbox car)

      ; 4.1.7. Inclusion
      ;
      ; syntax: (include hstring1i hstring2i ...)  * not supported, (scheme base)
      ; syntax: (include-ci hstring1i hstring2i ...)  * not supported, (scheme base)

      ; 4.2  Derived expression types
      ;
      ; The constructs in this section are hygienic, as discussed
      ; in section 4.3.  For reference purposes, section 7.3 gives
      ; macro definitions that will convert most of the constructs
      ; described in this section into the primitive constructs de-
      ; scribed in the previous section.

      ; 4.2.1  Binding constructs and sequencing
      ;
      ; The three binding constructs let, let*, and letrec give
      ; Scheme a block structure, like Algol 60. The syntax of the
      ; three constructs is identical, but they differ in the regions
      ; they establish for their variable bindings. In a let ex-
      ; pression, the initial values are computed before any of the
      ; variables become bound; in a let* expression, the bind-
      ; ings and evaluations are performed sequentially; while in a
      ; letrec expression, all the bindings are in effect while their
      ; initial values are being computed, thus allowing mutually
      ; recursive definitions.

      ; syntax:  (letrec <bindings> <body>)
      (define-syntax letrec
         (syntax-rules (begin)
            ((letrec ((?var ?val) ...) ?body) (let-eval (?var ...) (?val ...) ?body))
            ((letrec vars body ...) (letrec vars (begin body ...)))))

      ; syntax:  (let <bindings> <body>)
      ;          (let keyword <bindings> <body>) named let, see "4.2.4 Iteration"
      (define-syntax let
         (syntax-rules (letrec)
            ((let ((var val) ...) exp . rest)
               ((lambda (var ...) exp . rest) val ...))
            ((let keyword ((var init) ...) exp . rest)
               (letrec ((keyword (lambda (var ...) exp . rest))) (keyword init ...)))))

      ; syntax:  (begin <expression1> <expression2> ...)
      (define-syntax begin
         (syntax-rules (define define-values letrec)
            ((begin) #false) ; empty 
            ((begin exp) exp)

            ; super define, super define-values
            ((begin (define . a) (define . b) ... . rest)
               (begin 42 () (define . a) (define . b) ... . rest))
            ((begin (define-values (var ...) . body) . rest)
               (values-apply (begin . body) (lambda (var ...) (begin . rest))))

            ((begin 42 done (define ((op . args1) . args2) . body) . rest) ; list of variables
               (begin 42 done (define (op . args1) (lambda args2 . body)) . rest))
            ((begin 42 done (define (var . args) . body) . rest) ; function
               (begin 42 done (define var (lambda args . body)) . rest))
            ((begin 42 done (define var exp1 exp2 . expn) . rest) ; simple define
               (begin 42 done (define var (begin exp1 exp2 . expn)) . rest))
            ((begin 42 done (define var val) . rest) ; simplest define
               (begin 42 ((var val) . done) . rest))
            ((begin 42 done . exps)
               (begin 43 done () exps))

            ((begin 43 (a . b) c exps)
               (begin 43 b (a . c) exps))
            ((begin 43 () bindings exps)
               (letrec bindings (begin . exps)))

            ; define-values
            ((begin (define-values (var ...) gen) . rest)
               (values-apply gen (lambda (var ...) . rest)))

            ((begin first . rest)
               ((lambda (free) (begin . rest))  first))))

      ; library syntax:  (let* <bindings> <body>)
      (define-syntax let*
         (syntax-rules (<=)
            ((let* (((var ...) gen) . rest) . body)
               (values-apply gen (lambda (var ...) (let* rest . body))))
            ((let* ((var val) . rest-bindings) exp . rest-exps)
               ((lambda (var) (let* rest-bindings exp . rest-exps)) val))
            ; http://srfi.schemers.org/srfi-71/srfi-71.html
            ((let* ((var ... (op . args)) . rest-bindings) exp . rest-exps)
               (values-apply (op . args)
                  (lambda (var ...)
                     (let* rest-bindings exp . rest-exps))))
            ((let* ((var ... node) . rest-bindings) exp . rest-exps)
               (vector-apply node
                  (lambda (var ...)
                     (let* rest-bindings exp . rest-exps))))
            ((let* (((name ...) <= value) . rest) . code) ; currently unused, may be removed
               (vector-apply value
                  (lambda (name ...)
                     (let* rest . code))))
            ((let* ()) exp)
            ((let* () exp . rest)
               (begin exp . rest))))


      ; 5.3  Variable definitions

      ; syntax:  (define <variable> <expression>)
      ; syntax:  (define (<variable> <formals>) <body>)
      ; syntax:  (define (<variable> . <formal>) <body>)
      (define-syntax define
         (syntax-rules (lambda)
            ((define ((name . args) . more) . body)
               (define (name . args) (lambda more . body)))
            ((define (name . args) . body)
               (setq name (let-eval (name) ((lambda args . body)) name)))
            ((define name (lambda (var ...) . body))
               (setq name (let-eval (name) ((lambda (var ...) . body)) name)))
            ((define name val)
               (setq name val))
            ((define name a b . c)
               (define name (begin a b . c)))))

      ; 5.3.3  Multiple-value definitions

      (define-syntax define-values
         (syntax-rules (list)
            ((define-values (val ...) . body)
               (setq (val ...)
                  (let* ((val ... (begin . body)))
                     (list val ...))))))

       ; EXTENSION, unused!
;      (define-syntax define*
;         (syntax-rules (print list)
;            ((define* (op . args) . body)
;               (define (op . args)
;                  (print " * " (list (quote op) . args))
;                  .  body))
;            ((define* name (lambda (arg ...) . body))
;               (define* (name arg ...) . body))))

      ; -------------------------------------------------------------
      ; 6.1  Equivalence predicates
      ;
      ; A predicate is a procedure that always returns a boolean
      ; value (#t or #f). An equivalence predicate is the compu-
      ; tational analogue of a mathematical equivalence relation;
      ; it is symmetric, reflexive, and transitive. Of the equiva-
      ; lence predicates described in this section, eq? is the finest
      ; or most discriminating, equal? is the coarsest, and eqv?
      ; is slightly less discriminating than eq?.

      ; procedure:  (eq? obj1 obj2)   * builtin

      ; procedure:  (equal? obj1 obj2)
      ;
      ; The equal? procedure, when applied to pairs, vectors,
      ; strings and bytevectors, recursively compares them, return-
      ; ing #t when the unfoldings of its arguments into (possibly
      ; infinite) trees are equal (in the sense of equal?) as ordered
      ; trees, and #f otherwise. It returns the same as eqv? when
      ; applied to booleans, symbols, numbers, characters, ports,
      ; procedures, and the empty list. If two objects are eqv?,
      ; they must be equal? as well. In all other cases, equal?
      ; may return either #t or #f.
      (define (equal? a b)
         (if (eq? a b)
            #true
         (let ((sa (size a)))
            (if (and sa
                     (eq? (type a) (type b))
                     (eq? (size b) sa))
               (or
                  (eq? 0 sa)
                  (if (ref a 0) ; ==(bytestream? a), 0 in ref works only for bytestreams
                     ; comparing bytestreams
                     (let loop ((n (-- sa)))
                        (if (eq? (ref a n) (ref b n))
                           (if (eq? n 0)
                              #true
                              (loop (-- n)))))
                     ; recursive comparing objects
                     (let loop ((n sa))
                        (if (eq? n 0)
                           #true
                           (if (equal? (ref a n) (ref b n))
                              (loop (-- n)))))))))))

      ; procedure:  (eqv? obj1 obj2)
      ;
      ; The eqv? procedure defines a useful equivalence relation on
      ; objects. Briefly, it returns #t if obj 1 and obj 2 are normally
      ; regarded as the same object. This relation is left slightly
      ; open to interpretation, but the following partial specifica-
      ; tion of eqv? holds for all implementations of Scheme.
      (define (eqv? a b)
      ;  * obj 1 and obj 2 are both #t or both #f.
      ;  * obj 1 and obj 2 are both symbols and are the same sym-
      ;    bol according to the symbol=? procedure
      ;  * obj 1 and obj 2 are both characters and are the same
      ;    character according to the char=? procedure
      ;  * obj 1 and obj 2 are both the empty list
      ;  * obj 1 and obj 2 are pairs, vectors, bytevectors, records,
      ;    or strings that denote the same location in the store
      ;  * obj 1 and obj 2 are procedures whose location tags are
      ;    equal
         (if (eq? a b)
            #true
      ;  * obj 1 and obj 2 are both exact numbers and are numer-
      ;    ically equal (in the sense of =)
         (let ((typea (type a)))
         (if (or (eq? typea type-int+)
                 (eq? typea type-int-)
                 (eq? typea TRATIONAL)
                 (eq? typea TCOMPLEX))
            (and
               (eq? typea (type b))
               (eqv? (car a) (car b))
               (eqv? (cdr a) (cdr b)))
      ;  * obj 1 and obj 2 are both inexact numbers such that they
      ;    are numerically equal (in the sense of =) and they yield
      ;    the same results (in the sense of eqv?) when passed as
      ;    arguments to any other procedure that can be defined
      ;    as a finite composition of Scheme’s standard arith-
      ;    metic procedures, provided it does not result in a NaN
      ;    value.
         (if (eq? typea TINEXACT)
            (and
               (eq? typea (type b))
               (let loop ((n (-- (size a))))
                  (if (eq? (ref a n) (ref b n))
                     (if (eq? n 0)
                        #true
                        (loop (-- n)))))))))))

      (assert (eqv? 'a 'a)                  ===> #true) ; symbols
      (assert (eqv? '() '())                ===> #true) ; empty lists
      (assert (eqv? 2 2)                    ===> #true) ; atomic integer numbers
      (assert (eqv? 72057594037927936
                    72057594037927936)      ===> #true) ; long (not atomic) integer numbers
      (assert (eqv? 0.33 0.33)              ===> #true) ; rational numbers
      (assert (eqv? 7/3 14/6)               ===> #true) ; rational numbers
      (assert (eqv? 2+3i 2+3i)              ===> #true) ; complex numbers
      (assert (eqv? #i2 #i2)                ===> #true) ; inexact numbers
      (assert (eqv? +nan.0 +nan.0)          ===> #true) ; inexact NaNs
      (assert (let ((p (lambda (x) x)))
                 (eqv? p p))                ===> #true) ; same lambda
      (assert (let ((x '(a)))
                 (eqv? x x))                ===> #true) ; same list

      ; * ol specific cases (in r7rs unspecified)
      (assert (eqv? "" "")                  ===> #false); * ol specific, (in r7rs unspecified)

      ; The eqv? procedure returns #f if:

      ;  * obj 1 and obj 2 are of different types
      (assert (eqv? 'a 12)                  ===> #false)
      (assert (eqv? #f 'nil)                ===> #false)

      ;  * one of obj 1 and obj 2 is #t but the other is #f
      (assert (eqv? #t #f)                  ===> #false)

      ;  * obj 1 and obj 2 are symbols but are not the same symbol
      ;    according to the symbol=? procedure
      (assert (eqv? 'a 'b)                  ===> #false)

      ;  * one of obj 1 and obj 2 is an exact number but the other
      ;    is an inexact number
      (assert (eqv? 2 #i2)                  ===> #false)

      ;  * obj 1 and obj 2 are both exact numbers and are numer-
      ;    ically unequal
      (assert (eqv? 2 3)                    ===> #false) ; atomic number
      (assert (eqv? 72057594037927936
                    82057594037927936)      ===> #false) ; not an atomic numbers

      ;  * obj 1 and obj 2 are both inexact numbers such that ei-
      ;    ther they are numerically unequal (in the sense of =),
      ;    or they do not yield the same results (in the sense
      ;    of eqv?) when passed as arguments to any other pro-
      ;    cedure that can be defined as a finite composition of
      ;    Scheme’s standard arithmetic procedures, provided it
      ;    does not result in a NaN value. As an exception, the
      ;    behavior of eqv? is unspecified when both obj 1 and
      ;    obj 2 are NaN
      (assert (eqv? #i2 #i3)                ===> #false)
           ; todo: more examples

      ;  * obj 1 and obj 2 are characters for which the char=? pro-
      ;    cedure returns #f
      (assert (eqv? #\a #\A)                ===> #false)

      ;  * one of obj 1 and obj 2 is the empty list but the other is
      ;    not
      (assert (eqv? '() '(a))               ===> #false)

      ;  * obj 1 and obj 2 are pairs, vectors, bytevectors, records,
      ;    or strings that denote distinct locations
      (assert (eqv? (cons 1 2) (cons 1 2))  ===> #false)

      ;  * obj 1 and obj 2 are procedures that would behave dif-
      ;    ferently (return different values or have different side
      ;    effects) for some arguments
      (assert (eqv? (lambda () 1)
                    (lambda () 2))          ===> #false)
      (assert (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
                       (g (lambda () (if (eqv? f g) 'g 'both))))
                 (eqv? f g))                ===> #false)

      ; The following examples illustrate cases in which the above
      ; rules do not fully specify the behavior of eqv?. All that
      ; can be said about such cases is that the value returned by
      ; eqv? must be a boolean

      (assert (eqv? #() #())                ===> #false) ; * ol specific, (in r7rs unspecified)
      (assert (eqv? (lambda (x) x)
                    (lambda (x) x))         ===> #true)  ; * ol specific, (in r7rs unspecified), depends on (lang assemble)
      (assert (eqv? (lambda (x) x)
                    (lambda (y) y))         ===> #true)  ; * ol specific, (in r7rs unspecified), depends on (lang assemble)
      ;assert (eqv? 1.0e0 1.0e0)            ===> unspecified
      (assert (eqv? +nan.0 +nan.0)          ===> #true)  ; * ol specific, (in r7rs unspecified)

      (assert (eqv? '(a) '(a))              ===> #false) ; * ol specific, (in r7rs unspecified)
      (assert (eqv? "a" "a")                ===> #false) ; * ol specific, (in r7rs unspecified)
      (assert (eqv? '(b) (cdr '(a b)))      ===> #false) ; * ol specific, (in r7rs unspecified)
      (assert (letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
                       (g (lambda () (if (eqv? f g) 'both 'g))))
                 (eqv? f g))                ===> #false) ; * ol specific, (in r7rs unspecified)


      ; 4.2.2  Conditionals
      ;
      ; syntax: (cond <clause1> <clause2> ...)
      ; auxiliary syntax: else
      ; auxiliary syntax: =>
      (define-syntax cond
         (syntax-rules (else =>)
            ((cond) #false)
            ((cond (else exp . rest))
               (begin exp . rest))
            ((cond (clause => exp) . rest)
               (let ((fresh clause))
                  (if fresh
                     (exp fresh)
                     (cond . rest))))
            ((cond (clause exp . rest-exps) . rest)
               (if clause
                  (begin exp . rest-exps)
                  (cond . rest)))))

      (assert (cond ((less? 2 3) 'greater)
                    ((less? 3 2) 'less))                  ===>  'greater)
      (assert (cond ((less? 3 3) 'greater)
                    ((less? 3 3) 'less)
                    (else 'equal))                        ===>  'equal)
      (assert (cond ((car (cdr '((a 1) (b 2)))) => car)
                    (else #false))                        ===>  'b)

      ; syntax:  (case <key> <clause1> <clause2> ...)
      ; auxiliary syntax: else            * ol specific
      ; auxiliary syntax: []              * ol specific
      ; auxiliary syntax: ()              * ol specific
      ; auxiliary syntax: =>              * ol specific
      ; auxiliary syntax: else =>         * ol specific
      ; auxiliary syntax: else is         * ol specific
      ; todo: add (,symbol to evaluate
      (define-syntax case
         (syntax-rules (else list eqv? eq? and memv => make-vector is)
            ; precalculate case argument, if needed
            ((case (op . args) . clauses)
               (let ((fresh (op . args)))
                  (case fresh . clauses)))
            ((case thing) #false)
            ; http://srfi.schemers.org/srfi-87/srfi-87.html
            ((case thing ((a) => exp) . clauses)
               (if (eqv? thing (quote a))
                  (exp thing)
                  (case thing . clauses)))
            ; http://srfi.schemers.org/srfi-87/srfi-87.html
            ((case thing ((a ...) => exp) . clauses)
               (if (memv thing (quote (a ...)))
                  (exp thing)
                  (case thing . clauses)))
            ((case thing ((a) . body) . clauses)
               (if (eqv? thing (quote a))
                  (begin . body)
                  (case thing . clauses)))
            ; http://srfi.schemers.org/srfi-87/srfi-87.html
            ((case thing (else => func))
               (func thing))
            ((case thing (else is name . body)) ; * ol specific
               ((lambda (name) . body) thing))
            ((case thing (else . body))
               (begin . body))
            ; * ol specific
            ; case receives a vectors:
            ((case thing ((make-vector (list cp . args)) . body) . clauses)
               (if (eq? cp (ref thing 1))
                  (vector-apply thing
                     (lambda (_ . args)
                        . body))
                  (case thing . clauses)))

            ; http://srfi.schemers.org/srfi-87/srfi-93.html ?
            ((case thing ((a . b) . body) . clauses)
               (if (memv thing (quote (a . b)))
                  (begin . body)
                  (case thing . clauses)))
            ; (case (type foo) (type-foo thenfoo) (type-bar thenbar) ...)
            ((case thing (atom . then) . clauses)
               (if (eq? thing atom)
                  (begin . then)
                  (case thing . clauses)))))

      (assert (case 7
                 (1 'one) (7 'seven))          ===>  'seven)
      (assert (case 'x
                 (1 'one) (7 'seven))          ===>  #false)
      (assert (case 'x
                 (1 'one) (7 'seven)
                 (else 'nothing))              ===>  'nothing)
      (assert (case 'y
                 (1 'one) (7 'seven)
                 (else => (lambda (x) x)))     ===>  'y)
      (assert (case 'z
                 (1 'one) (7 'seven)
                 (else is x x))                ===>  'z)

      ; syntax:  when <test> <expression1> <expression2> ...
      (define-syntax when
         (syntax-rules ()
            ((when val) #false)
            ((when val . then) (if val (begin . then)))))

      (assert (when (less? 2 3) 'yes 'no)               ===>  'no)
      (assert (when (less? 3 2) 'yes 'no)               ===>  #false)

      ; syntax:  unless <test> <expression1> <expression2> ...
      (define-syntax unless
         (syntax-rules (not)
            ((unless val) #false)
            ((unless val . then) (if (not val) (begin . then)))))

      (assert (unless (less? 2 3) 'yes 'no)               ===>  #false)
      (assert (unless (less? 3 2) 'yes 'no)               ===>  'no)

      ; 4.2.4  Iteration       * moved to (scheme r5rs iteration)

      ; 4.2.5  Delayed evaluation
      ;
      ; syntax:  delay <expression>
      (define-syntax delay
         (syntax-rules ()
            ((delay . body)
               (lambda () (begin . body)))))

      ; syntax:  delay-force <expression>  * (scheme lazy)

      ; syntax:  force <expression>
      (define-syntax force
         (syntax-rules ()
            ((force promise) (promise))))

      ;; TODO: change to this:
         ;; (cond
         ;;    ((null? it) it)
         ;;    ((pair? it) (cons (car it) (force-ll (cdr it))))
         ;;    (else (force-ll (it)))))

      ; (promise? obj)  * (scheme lazy)
      ; (make-promise obj)  * (scheme lazy)

      ; 4.2.6. Dynamic bindings
      ; * declared in (scheme dynamic-bindings)

      ; 4.2.7. Exception handling
      ; todo: * declared in (scheme exceptions)
      ; todo: guard

      ; 4.2.8. Quasiquotation
      ;
      ; `(a ,(+ 1 2) ,(map abs '(4 -5 6)) b) ===> (a 3 (4 5 6) b)
      ; `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) ===> (a 3 4 5 6 b)
      (define-syntax quasiquote
         (syntax-rules (unquote unquote-splicing _work append)
                                                ;^
                                                ;'-- mine
            ((quasiquote _work () (unquote exp)) exp)
            ((quasiquote _work (a . b) (unquote exp))
               (list 'unquote (quasiquote _work b exp)))
            ((quasiquote _work d (quasiquote . e))
               (list 'quasiquote
                  (quasiquote _work (() . d) . e)))
            ((quasiquote _work () ((unquote-splicing exp) . tl))
               (append exp
                  (quasiquote _work () tl)))
            ((quasiquote _work d (a . b))
               (cons (quasiquote _work d a)
                     (quasiquote _work d b)))
            ((quasiquote _work d atom)
               (quote atom))
            ((quasiquote . stuff)
               (quasiquote _work () . stuff))))

      ; 4.2.9. Case-lambda
      ; * declared in (srfi 16)

      ; 4.3  Macros
      ; 4.3.1  Binding constructs for syntactic keywords
      ; 4.3.2  Pattern language
      ; 4.3.3  Signaling errors in macro transformers


      ;;; ---------------------------------------------------------------
      ;;; Chapter 5
      ;;; Program structure

      ; 5.1  Programs
      ; 5.2  Import declarations
      ; 5.3  Variable definitions

      ; 5.3.1  Top level definitions
      ; 5.3.2  Internal definitions

      ; 5.5  Record-type definitions
      ; syntax:  (define-record-type <name> <constructor> <pred> <field> ...)  * not supported

      ; 5.6  Libraries
      ; 5.6.1  Library Syntax
      ; * declared and implemented in (lang repl)

      ; 5.6.2. Library example
      ; todo: library example assert

      ; 5.7  The REPL

      ;;; ---------------------------------------------------------------
      ;;; Chapter 6
      ;;; Standard procedures
      ;
      ; This chapter describes Scheme's built-in procedures.
      ; 
      ; The initial (or ``top level'') Scheme
      ; environment starts out with a number of variables bound to locations containing useful values,
      ; most of which are primitive procedures that manipulate data. For example, the variable abs is
      ; bound to (a location initially containing) a procedure of one argument that computes the
      ; absolute value of a number, and the variable + is bound to a procedure that computes sums.
      ; Built-in procedures that can easily be written in terms of other built-in procedures are
      ; identified as ``library procedures''.


      ; 6.2  Numbers

      ; 6.2.1  Numerical types
      ;
      ; Mathematically, numbers are arranged into a tower of sub-
      ; types in which each level is a subset of the level above it:
      ;
      ; number
      ; complex number
      ; real number
      ; rational number
      ; integer

      type-enum+    ; * ol specific, short (value) positive integer number
      type-enum-    ; * ol specific, short (value) negative integer number
      type-int+     ; * ol specific, long positive integer number, TODO: change to type-integer+
      type-int-     ; * ol specific, long negative integer number, TODO: same
      (define type-rational         TRATIONAL)
      (define type-complex          TCOMPLEX)
      (define type-inexact          TINEXACT)

      ; 6.2.2  Exactness
      ; 6.2.3  Implementation restrictions
      ; 6.2.4  Implementation extensions
      ; 6.2.5  Syntax of numerical constants

      ; 6.2.6  Numerical operations

      ; procedure:  (integer? obj)
      (define (integer? a)
         (case (type a)
            (type-enum+ #true)
            (type-int+ #true)
            (type-enum- #true)
            (type-int- #true)))

      (assert (integer? 3+0i)               ===>  #t) ; imag part is 0
      (assert (integer? 3+4i)               ===>  #f) ; imag part is not a 0
      (assert (integer? 3.0)                ===>  #t)
      (assert (integer? 3.4)                ===>  #f)
      (assert (integer? 8/4)                ===>  #t)
      (assert (integer? 8/5)                ===>  #f)

      ; procedure:  (rational? obj)
      (define (rational? a)
      (or
         (integer? a)
         (eq? (type a) type-rational)))

      (assert (rational? -inf.0)            ===>  #f)
      (assert (rational? 6/10)              ===>  #t)
      (assert (rational? 6/3)               ===>  #t)
      (assert (rational? 3+4i)              ===>  #f)

      ; procedure:  (real? obj)
      ;  * if z is a complex number, then (real? z) is true if and
      ;    only if (zero? (imag-part z)) is true.
      (define (real? a)
      (or
         (rational? a)
         (eq? (type a) type-inexact)))
         ; next line is not required, because math lib automatically casts such complex numbers to the real
         ;  (and (eq? (type a) type-complex) (eq? (cdr a) 0))

      (assert (real? 3)                     ===>  #t)
      (assert (real? 3.4)                   ===>  #t)
      (assert (real? 1e10)                  ===>  #t)
      (assert (real? -2.5+0i)               ===>  #t)
      (assert (real? +inf.0)                ===>  #t)
      (assert (real? -inf.0)                ===>  #t)
      (assert (real? +nan.0)                ===>  #t)

      ; procedure:  (complex? obj)
      (define (complex? a)
      (or
         (real? a)
         (eq? (type a) type-complex)))

      (assert (complex? 3+4i)               ===>  #t)
      (assert (complex? 3)                  ===>  #t)
      (assert (complex? 3.4)                ===>  #t)

      ; Note: In many implementations the complex? procedure will
      ; be the same as number?, but unusual implementations may rep-
      ; resent some irrational numbers exactly or may extend the num-
      ; ber system to support some kind of non-complex numbers.
      ;
      ; procedure:  (number? obj)
      (define number? complex?)

      ; A Scheme number is exact if it was written as an exact
      ; constant or was derived from exact numbers using only
      ; exact operations. A number is inexact if it was written
      ; as an inexact constant, if it was derived using inexact
      ; ingredients, or if it was derived using inexact operations.
      ; Thus inexactness is a contagious property of a number.
      ;                            * note from 6.2.2. exactness

      ; In particular, an exact complex number has an exact real
      ; part and an exact imaginary part; all other complex numbers
      ; are inexact complex numbers.

      ; procedure:  (exact? z)
      (define (exact? z)
      (or (rational? z)
          (and
            (eq? (type z) type-complex)
            (rational? (car z))
            (rational? (cdr z)))))

      ; procedure:  (inexact? z)
      (define (inexact? z)
         (eq? (type z) type-inexact))

      ; procedure:  (exact-integer? z)
      (define exact-integer? integer?)

      ; procedure:  (finite? z)    * implemented in (scheme inexact)
      ; procedure:  (infinite? z)  * implemented in (scheme inexact)
      ; procedure:  (nan? z)       * implemented in (scheme inexact)

      ; procedure:  (= z1 z2 z3 ...)  * implemented in (owl math)
      ; procedure:  (< x1 x2 x3 ...)  * implemented in (owl math)
      ; procedure:  (> x1 x2 x3 ...)  * implemented in (owl math)
      ; procedure:  (<= x1 x2 x3 ...) * implemented in (owl math)
      ; procedure:  (>= x1 x2 x3 ...) * implemented in (owl math)

      ; procedure:  (zero? z)      * implemented in (owl math)
      ; procedure:  (positive? x)  * implemented in (owl math)
      ; procedure:  (negative? x)  * implemented in (owl math)
      ; procedure:  (odd? n)       * implemented in (owl math)
      ; procedure:  (even? n)      * implemented in (owl math)
      ; procedure:  (max x1 x2 ...)* implemented in (owl math)
      ; procedure:  (min x1 x2 ...)* implemented in (owl math)
      ; procedure:  (+ z1 ...)     * implemented in (owl math)
      ; procedure:  (* z1 ...)     * implemented in (owl math)
      ; procedure:  (- z)          * implemented in (owl math)
      ; procedure:  (- z1 z2 ...)  * implemented in (owl math)
      ; procedure:  (/ z)          * implemented in (owl math)
      ; procedure:  (/ z1 z2 ...)  * implemented in (owl math)
      ; procedure:  (abs x)        * implemented in (owl math)
      ; no floor/, no floor-quotient, no floor-remainder
      ; no truncate/, no truncate-quotient, no truncate-remainder
      ; procedure:  (quotient n1 n2)
      ; procedure:  (remainder n1 n2)
      ; procedure:  (modulo n1 n2) * implemented in (owl math)
      ; procedure:  (gcd n1 ...)   * implemented in (owl math)
      ; procedure:  (lcm n1 ...)   * implemented in (owl math)
      ; procedure:  (numerator q)
      ; procedure:  (denominator q)
      ; procedure:  (floor x)      * implemented in (owl math)
      ; procedure:  (ceiling x)    * implemented in (owl math)
      ; procedure:  (truncate x)   * implemented in (owl math)
      ; procedure:  (round x)      * implemented in (owl math)
      ; procedure:  (rationalize x y) * implemented in (owl math-extra)
      
      ; procedure:  (exp z)      * implemented in (scheme inexact)
      ; procedure:  (log z)      * implemented in (scheme inexact)
      ; procedure:  (log z1 z2)  * implemented in (scheme inexact)
      ; procedure:  (sin z)      * implemented in (scheme inexact)
      ; procedure:  (cos z)      * implemented in (scheme inexact)
      ; procedure:  (tan z)      * implemented in (scheme inexact)
      ; procedure:  (asin z)     * implemented in (scheme inexact)
      ; procedure:  (acos z)     * implemented in (scheme inexact)
      ; procedure:  (atan z)     * implemented in (scheme inexact)
      ; procedure:  (atan y x)   * implemented in (scheme inexact)
      ; procedure:  (square z)     * implemented in (owl math)
      ; procedure:  (sqrt z)     * implemented in (scheme inexact)
      ; procedure:  (exact-integer-sqrt k)    * implemented in (owl math-extra)
      ; procedure:  (make-rectangular x1 x2)  * implemented in (scheme complex)
      ; procedure:  (make-polar x1 x2)        * implemented in (scheme complex)
      ; procedure:  (real-part z)             * implemented in (scheme complex)
      ; procedure:  (imag-part z)             * implemented in (scheme complex)
      ; procedure:  (magnitude z)             * implemented in (scheme complex)
      ; procedure:  (angle z)                 * implemented in (scheme complex)

      ; procedure:  (inexact z)
      (define (inexact n) (vm:cast n type-inexact))

      ; procedure:  (exact z)
      (define (exact n) (vm:cast n type-rational))


      ; 6.2.7  Numerical input and output
      ; procedure:  (number->string z)        * (scheme base)
      ; procedure:  (number->string z radix)  * (scheme base)

      ; procedure:  (string->number z)        * (scheme misc)
      ; procedure:  (string->number z radix)  * (scheme misc)

      ; 6.2.8  Other data types
      ;
      ; This data types related to olvm, not a part of r7rs

      (define type-pair             TPAIR)    ; reference
      (define type-vector           TVECTOR)  ; reference
      (define type-string           TSTRING)  ; reference, bytevector of ansi chars
      (define type-symbol           TSYMBOL)  ; reference

      (define type-string-wide      TSTRINGWIDE) ; reference, vector of unicode chars

      (define type-rlist-spine      10) ; reference
      (define type-blob-leaf        11) ; reference

      (define type-port             12) ; value
      (define type-const            13) ; value

      (define type-rlist-node       14) ; reference
      (define type-blob-dispatch    15) ; reference

      (define type-bytecode         TBYTECODE)   ; reference, bytecode
      (define type-procedure        TPROCEDURE)  ; reference, pure function
      (define type-closure          TCLOSURE)    ; reference, function with closure(s)
      (define type-bytevector       TBYTEVECTOR) ; reference, bytevector
      (define type-constructor      TCONSTRUCTOR); reference, constructor

      (define type-superstring      21) ; reference

      (define type-thread-state     31) ; reference
      (define type-vptr             49) ; reference,  blob

      ; 6.3  Booleans
      ;
      ; Of all the standard Scheme values, only #f counts as false in conditional expressions.
      ; Except for #f, all standard Scheme values, including #t, pairs, the empty list, symbols,
      ; numbers, strings, vectors, and procedures, count as true.
      ;      Note: Programmers accustomed to other dialects of Lisp should be aware that Scheme
      ;            distinguishes both #f and the empty list from the symbol nil.
      ; Boolean constants evaluate to themselves, so they do not need to be quoted in programs.
      (assert #t                            ===>  #t)
      (assert #f                            ===>  #f)
      (assert (quote #f)                    ===>  #f)

      ; procedure:  (not obj)
      (define (not x)
         (if x #false #true))

      (assert (not #t)                      ===>  #f)
      (assert (not 3)                       ===>  #f)
      (assert (not '(3 . 0))                ===>  #f)
      (assert (not #f)                      ===>  #t)
      (assert (not '())                     ===>  #f)
      (assert (not cons)                    ===>  #f)
      (assert (not 'nil)                    ===>  #f)

      ; procedure:  (boolean? obj)
      (define (boolean? o)
         (cond
            ((eq? o #true) #true)
            ((eq? o #false) #true)
            (else #false)))

      (assert (boolean? #f)                 ===>  #t)
      (assert (boolean? 0)                  ===>  #f)
      (assert (boolean? '())                ===>  #f)

      ;; ; todo: Оставить здесь только самые базово необходимые вещи, все остальное переместить в:
      ;; ;   6.3.2 -> (scheme r5rs lists)
      ;; ;   6.3.3 -> (scheme r5rs symbols)
      ;; ;   6.3.4 -> (scheme r5rs characters)
      ;; ;   6.3.5 -> (scheme r5rs strings)
      ;; ;   6.3.6 -> (scheme r5rs vectors)

      ; 6.4  Pairs and lists
      ;
      ; A pair (sometimes called a dotted pair) is a record structure
      ; with two fields called the car and cdr fields (for historical
      ; reasons). Pairs are created by the procedure cons. The
      ; car and cdr fields are accessed by the procedures car and
      ; cdr. The car and cdr fields are assigned by the procedures
      ; set-car! and set-cdr!.
      ; ......

      ; procedure:  (pair? obj)
      (define (pair? o)
         (eq? (type o) type-pair))

      (assert (pair? '(a . b))              ===>  #t)
      (assert (pair? '(a b c))              ===>  #t)
      (assert (pair? '())                   ===>  #f)
      (assert (pair? #(a b))                ===>  #f) ; vectors will be defined later

      ; procedure:  (cons obj1 obj2)    * builtin

      ; procedure:  (car pair)          * builtin

      ; procedure:  (cdr pair)          * builtin

      ; procedure:  (set-car! pair obj)
      ;
      ; Stores obj in the car field of pair .
      ; Note: (ol specific) returns #F if attempt was failed
      (define (set-car! o v)
         (set-ref! o 1 v))

      ; procedure:  (set-cdr! pair obj)
      ;
      ; Stores obj in the cdr field of pair .
      ; Note: (ol specific) returns #F if attempt was failed
      (define (set-cdr! o v)
         (set-ref! o 2 v))

      ; procedure:  (caar pair)
      (define (caar pair) (car (car pair)))

      ; procedure:  (cadr pair)
      (define (cadr pair) (car (cdr pair)))

      ; procedure:  (cdar pair)
      (define (cdar pair) (cdr (car pair)))

      ; procedure:  (cddr pair)
      (define (cddr pair) (cdr (cdr pair)))

      ; procedure:  (caaar pair)  <- (scheme base)
      ; ...
      ; procedure:  (cdddr pair)  <- (scheme base)

      ; procedure:  (caaaar pair) <- (scheme cxr)
      ; ...
      ; procedure:  (cddddr pair) <- (scheme cxr)

      ; procedure:  (null? obj)
      (define (null? x)
         (eq? x #null))

      ; procedure:  (list? obj)
      (define (list? l)
         (cond
            ((null? l) #true)
            ((pair? l) (list? (cdr l)))
            (else #false)))

      ; procedure:  (make-list k)
      ; procedure:  (make-list k fill)
      ;
      ; Returns a newly allocated list of k elements. If a second
      ; argument is given, then each element is initialized to fill .
      ; Otherwise the initial contents of each element is unspeci-
      ; fied.
      (define make-list
         (define make (lambda (n fill)
            (let loop ((n n) (out '()))
               (if (eq? n 0)
                  out
                  (loop (-- n) (cons fill out))))))
         (case-lambda
            ((k)
               (make k #false))
            ((k fill)
               (make k fill))))

      ; procedure:  (list obj ...)
      (define-syntax list
         (syntax-rules ()
            ((list) '())
            ((list a . b)
               (cons a (list . b)))))

      ; question:
      ;  why list is a macro, not a procedure?
      ; answer:
      ;  the function unwraps to LD/LD/LD/LD/LD/MOV2/MOV2/MOV2/GOTO/JAX
      ;  sequnce, but macro unwraps to LD/CONS/LD/CONS/LD/CONS.
      ;  So, we have a good speedup, yeah?


      ; procedure:  (length list)
      ;  olvm notes: always returning fixnum, so can be checked by eq?, not only =
      (define (length l)
         (let loop ((n 0) (l l))
            (if (null? l)
               n
               (loop (++ n) (cdr l)))))

      (assert (length '(a b c))             ===>  3)
      (assert (length '(a (b) (c d e)))     ===>  3)
      (assert (length '())                  ===>  0)

      ; procedure:  (append list ...)
      (define append
         (letrec ((app (lambda (a b)
                     (if (null? a)
                        b
                        (cons (car a) (app (cdr a) b)))))
                  (appl (lambda (l)
                     (if (null? (cdr l))
                        (car l)
                        (app (car l) (appl (cdr l)))))))
            (case-lambda
               ((a b) (app a b))
               ((a b . cs) (app a (app b (appl cs))))
               ((a) a)
               (() '()))))

      (assert (append '(x) '(y))       ===> '(x y))
      (assert (append '(a) '(b c d))   ===> '(a b c d))
      (assert (append '(a (b)) '((c))) ===> '(a (b) (c)))
      (assert (append '(a b) '(c . d)) ===> '(a b c . d))
      (assert (append '() 'a)          ===> 'a)

      ; procedure:  (reverse list)
      (define (reverse list)
         (let loop ((old list) (new '()))
            (if (null? old)
               new
               (loop (cdr old) (cons (car old) new)))))

      ; procedure:  (list-tail list k)
      ;
      ; It is an error if list has fewer than k elements.
      ; Returns the sublist of list obtained by omitting the first k
      ; elements.
      (define (list-tail list k)
         (if (eq? k 0)
            list
            (list-tail (cdr list) (-- k))))

      ; procedure:  (list-ref list k)
      ;
      ; The list argument can be circular, but it is an error if list has
      ; k or fewer elements.
      (define (list-ref list k)
         (cond
            ((null? list) #false)    ; temporary instead of (syntax-error "lref: out of list" pos))
            ((eq? k 0) (car list))   ; use internal vm math, not math library
            (else (list-ref (cdr list) (-- k)))))

      ; procedure:  (list-set! list k obj)
      ;
      ; It is an error if k is not a valid index of list.
      ; The list-set! procedure stores obj in element k of
      ; list.
      (define (list-set! list k obj)
         (let ((tail (list-tail list k)))
            (if (pair? tail)
               (set-car! tail obj))))

      ; procedure:  (memq obj list)
      ; procedure:  (memv obj list)
      ; procedure:  (member obj list)
      ; procedure:  (member obj list compare)
      ;
      ; These procedures return the first sublist of list whose car
      ; is obj , where the sublists of list are the non-empty lists
      ; returned by (list-tail list k ) for k less than the length
      ; of list. If obj does not occur in list, then #f (not the empty
      ; list) is returned. The memq procedure uses eq? to compare
      ; obj with the elements of list, while memv uses eqv? and
      ; member uses compare, if given, and equal? otherwise.
      (define (make-mem* comparer) ; helper
         (letrec ((f (lambda (obj list)
                        (unless (null? list)
                           (if (pair? list)
                              (if (comparer obj (car list))
                                 list
                                 (f obj (cdr list)))))))) ; found
            f))

      (define memq (make-mem* eq?))
      (define memv (make-mem* eqv?))
      (define member
         (case-lambda
            ((obj list)
               ((make-mem* equal?) obj list))
            ((obj list compare)
               ((make-mem* compare) obj list))))

      (assert (case 6
                 ((2 3 5 7) 'prime)
                 ((1 4 6 8 9) 'composite))     ===> 'composite)
      (assert (case (car '(c d))
                 ((a e i o u) 'vowel)
                 ((w y) 'semivowel)
                 (else 'consonant))            ===> 'consonant)

      (assert (memq 'a '(a b c))               ===> '(a b c))
      (assert (memq 'b '(a b c))               ===> '(b c))
      (assert (memq 'd '(a b c))               ===> #false)
      (assert (memq '(a) '(b (a) c))           ===> #false)
      (assert (member '(a) '(b (a) c))         ===> '((a) c))
      (assert (member "a" '(3 "b" "c") less?)  ===> '("b" "c"))
      (assert (memq 101 '(100 101 102))        ===> '(101 102)) ; * ol specific, (in r7rs unspecified)
      (assert (memv 101 '(100 101 102))        ===> '(101 102))

      ; procedure:  (assq obj alist)           * moved to (scheme base)
      ; procedure:  (assv obj alist)           * moved to (scheme base)
      ; procedure:  (assoc obj alist)          * moved to (scheme base)
      ; procedure:  (assoc obj alist compare)  * moved to (scheme base)

      ; procedure:  (list-copy obj)            * moved to (scheme base)

      ; 6.5  Symbols
      ;

      ; procedure:  (symbol? obj)
      (define (symbol? o)
         (eq? (type o) type-symbol))

      ; procedure:  (symbol=? symbol1 symbol2 symbol3 ...)  * (scheme base)
      (define (symbol=? . os)
         (unless (null? os)
            (let loop ((o (cdr os)))
               (if (null? o)
                  #true
                  (and (equal? (car os) (car o)) (loop (cdr o)))))))

      ; procedure:  (symbol->string symbol)  * (owl string)
      ; procedure:  (string->symbol string)  * (owl string)

      ; 6.6  Characters   * (scheme base), (scheme char)
      ; Characters are objects that represent printed characters such as letters and digits.
      ; Characters are written using the notation #\<character> or #\<character name>.
      ;
      ; procedure:  (char? obj)                * (scheme base)
      ; procedure:  (char=? char1 char2 ...)   * (scheme base)
      ; procedure:  (char<? char1 char2 ...)   * (scheme base)
      ; procedure:  (char>? char1 char2 ...)   * (scheme base)
      ; procedure:  (char<=? char1 char2 ...)  * (scheme base)
      ; procedure:  (char>=? char1 char2 ...)  * (scheme base)

      ; procedure:  (char-ci=? char1 char2)    * (scheme char)
      ; procedure:  (char-ci<? char1 char2)    * (scheme char)
      ; procedure:  (char-ci>? char1 char2)    * (scheme char)
      ; procedure:  (char-ci<=? char1 char2)   * (scheme char)
      ; procedure:  (char-ci>=? char1 char2)   * (scheme char)
      ; procedure:  (char-alphabetic? char)    * (scheme char)
      ; procedure:  (char-numeric? char)       * (scheme char)
      ; procedure:  (char-whitespace? char)    * (scheme char)
      ; procedure:  (char-upper-case? letter)  * (scheme char)
      ; procedure:  (char-lower-case? letter)  * (scheme char)
      ; procedure:  (digit-value char)         * (scheme char)
      ; procedure:  (char->integer char)       * (scheme char)
      ; procedure:  (integer->char n)          * (scheme char)
      ; procedure:  (char-upcase char)         * (scheme char)
      ; procedure:  (char-downcase char)       * (scheme char)
      ; procedure:  (char-foldcase char)       * (scheme char)


      ; 6.3.5. Strings  * moved to (owl strings)
      ; Strings are sequences of characters. Strings are written as sequences of characters enclosed
      ; within doublequotes (`"'). A doublequote can be written inside a string only by escaping it
      ; with a backslash (\), as in "The word \"recursion\" has many meanings."
      ;
      ; procedure:  (string? obj)

      ; procedure:  (make-string k)
      ; procedure:  (make-string k char)
;      (define make-string
;         (case-lambda
;            ((k) (vm:vm:alloc type-string k))
;            ((k char) (app a (app b (appl cs appl) app) app))
;            ((a) a)
;            (() '()))))
;         ()
;         (list->string (repeat char n)))

      ; procedure:  (string char ...)
      ; procedure:  (string-length string)
;      (define (string-length str)
;         (case (type str)
;            (type-string          (size str))
;            (type-string-wide     (size str))
;            (type-string-dispatch (ref str 1))
;            ; todo: clarify the returning the runtime-error or simple #f
;            (else
;               (runtime-error "string-length: not a string: " str))))

      ; procedure:  (string-ref string k)
      ; procedure:  (string-set! string k char)
      ; procedure:  (string=? string1 string2)
      ; procedure:  (string-ci=? string1 string2)    * (scheme char)
      ; procedure:  (string<? string1 string2)
      ; procedure:  (string>? string1 string2)
      ; procedure:  (string<=? string1 string2)
      ; procedure:  (string>=? string1 string2)
      ; procedure:  (string-ci<? string1 string2)    * (scheme char)
      ; procedure:  (string-ci>? string1 string2)    * (scheme char)
      ; procedure:  (string-ci<=? string1 string2)   * (scheme char)
      ; procedure:  (string-ci>=? string1 string2)   * (scheme char)
      ; procedure:  (substring string start end)
      ; procedure:  (string-append string ...)
      ; procedure:  (string->list string)
      ; procedure:  (list->string list)
      ; procedure:  (string-copy string)
      ; procedure:  (string-fill! string char)



      ;; 6.8  Vectors

      ; Vectors are heterogeneous structures whose elements are
      ; indexed by integers. A vector typically occupies less space
      ; than a list of the same length, and the average time needed
      ; to access a randomly chosen element is typically less for the
      ; vector than for the list.
      ;
      ; Vectors are written using the notation #(obj ...). For
      ; example, a vector of length 3 containing the number zero
      ; in element 0, the list (2 2 2 2) in element 1, and the
      ; string "Anna" in element 2 can be written as follows:
      ;
      ;      #(0 (2 2 2 2) "Anna")
      ;
      ; * ol specific note: Additionally vectors can be written
      ;   using the notation [obj ...]
      ;   Отличие нотаций: [] не квотирует элементы. Кроме того
      ;   в ol к элементам ветора можно обращаться с помощью ref
      ;   и тогда элементы ветора индексируются с 1, не с 0.
      ;
      ; Vector constants are self-evaluating, so they do not need
      ; to be quoted in programs.

      ; procedure:  (vector? obj)
      ; Returns #t if obj is a vector; otherwise returns #f.

      (define (vector? o)
         (eq? (type o) type-vector))

      ; procedure:  (make-vector k)
      ; procedure:  (make-vector k fill)
      ; procedure:  (make-vector list)     * ol extension
      ;
      ; Returns a newly allocated vector of k elements. If a second
      ; argument is given, then each element is initialized to fill.
      ; Otherwise the initial contents of each element is #false
      ; (unspecified in Scheme).
      (define make-vector make-vector)

      ; procedure:  (vector obj ...)
      ;
      ; Returns a newly allocated vector whose elements contain
      ; the given arguments. It is analogous to list.

      ; todo: make `vector` function
      ;; (define (vector . args) ; the fastest vector allocator
      ;;    (vm:make type-vector args))
      (define-syntax vector
         (syntax-rules ()
            ((vector . staff)
               (vm:new type-vector . staff))))

      (assert (vector 'a 'b 'c)                ===>  #(a b c))

      ; procedure:  (vector-length vector)                * (scheme vector)
      ; procedure:  (vector-ref vector k)                 * (scheme vector)
      ; procedure:  (vector-set! vector k obj)            * (scheme vector)
      ; procedure:  (vector->list vector)                 * (scheme vector)
      ; procedure:  (vector->list vector start)           * (scheme vector)
      ; procedure:  (vector->list vector start end)       * (scheme vector)
      ; procedure:  (list->vector list)                   * (scheme vector)
      ; procedure:  (vector->string vector)               * (scheme vector)
      ; procedure:  (vector->string vector start)         * (scheme vector)
      ; procedure:  (vector->string vector start end)     * (scheme vector)
      ; procedure:  (string->vector string)               * (scheme vector)
      ; procedure:  (string->vector string start)         * (scheme vector)
      ; procedure:  (string->vector string start end)     * (scheme vector)
      ; procedure:  (vector-copy vector)                  * (scheme vector)
      ; procedure:  (vector-copy vector start)            * (scheme vector)
      ; procedure:  (vector-copy vector start end)        * (scheme vector)
      ; procedure:  (vector-copy! to at from)             * (scheme vector)
      ; procedure:  (vector-copy! to at from start)       * (scheme vector)
      ; procedure:  (vector-copy! to at from start end)   * (scheme vector)
      ; procedure:  (vector-append vector ...)            * (scheme vector)
      ; procedure:  (vector-fill! vector fill)            * (scheme vector)
      ; procedure:  (vector-fill! vector fill start)      * (scheme vector)
      ; procedure:  (vector-fill! vector fill start end)  * (scheme vector)


      ;; 6.9  Bytevectors
      ;
      ; Bytevectors represent blocks of binary data. They are
      ; fixed-length sequences of bytes, where a byte is an exact
      ; integer in the range from 0 to 255 inclusive. A bytevector
      ; is typically more space-efficient than a vector containing
      ; the same values.      
      ;
      ; Bytevectors are written using the notation #u8(byte...).
      ; For example, a bytevector of length 3 containing the byte
      ; 0 in element 0, the byte 10 in element 1, and the byte 5 in
      ; element 2 can be written as follows:
      ;
      ;      #u8(0 10 5)
      ;
      ; Bytevector constants are self-evaluating, so they do not
      ; need to be quoted in programs.

      ; procedure: (bytevector? obj)
      (define (bytevector? obj)
         (eq? (type obj) type-bytevector))

      ; procedure: (make-bytevector k)
      ; procedure: (make-bytevector k byte)
      (define make-bytevector
         (case-lambda
            ((k)
               (vm:alloc type-bytevector k))
            ((k byte)
               (vm:alloc type-bytevector k byte))))

      ; procedure: (bytevector byte ...)
      (define (bytevector . bytes)
         (vm:alloc type-bytevector bytes))

      ; * extra bytevector staff implemented in (scheme bytevector)
      ; procedure: (bytevector-length bytevector)           * (scheme bytevector)
      ; procedure: (bytevector-u8-ref bytevector k)         * (scheme bytevector)
      ; procedure: (bytevector-u8-set! bytevector k byte)   * (scheme bytevector)
      ; procedure: (bytevector-copy bytevector)             * (scheme bytevector)
      ; procedure: (bytevector-copy bytevector start)       * (scheme bytevector)
      ; procedure: (bytevector-copy bytevector start end)   * (scheme bytevector)
      ; procedure: (bytevector-copy! to at from)            * (scheme bytevector)
      ; procedure: (bytevector-copy! to at from start)      * (scheme bytevector)
      ; procedure: (bytevector-copy! to at from start end)  * (scheme bytevector)
      ; procedure: (bytevector-append bytevector ...)       * (scheme bytevector)
      ; procedure: (utf8->string bytevector)                * (scheme bytevector)
      ; procedure: (utf8->string bytevector start)          * (scheme bytevector)
      ; procedure: (utf8->string bytevector start end)      * (scheme bytevector)
      ; procedure: (string->utf8 string)                    * (scheme bytevector)
      ; procedure: (string->utf8 string start)              * (scheme bytevector)
      ; procedure: (string->utf8 string start end)          * (scheme bytevector)

      ; 6.10  Control features
      ;
      ; This chapter describes various primitive procedures which
      ; control the flow of program execution in special ways.
      ; ...

      ; procedure:  (procedure? obj)
      (define (procedure? o)
         (case (type o)
            (type-procedure #true)
            (type-closure #true)
            (type-bytecode #true)))

      (assert (procedure? car)                  ===> #true)
      (assert (procedure? 'car)                 ===> #false)
      (assert (procedure? (lambda (x) (cons x x)))
                                                ===> #true)
      (assert (call-with-current-continuation procedure?)
                                                ===> #true)

      ; *ol* extension
      (define (ff? o)
         (or (eq? (vm:and (type o) #b1111100) 24)
             (eq? o #empty)))

      ; *ol* extension
      (define object? ff?)

      ; *ol* extension
      (define (bytecode? o)
         (eq? (type o) type-bytecode))

      ; *ol* extension
      ; TODO: exchange procedure? and function?
      (define (function? o)
         (case (type o)
            (type-procedure #true)
            (type-closure #true)
            (type-bytecode #true)))

      ; procedure:  (apply proc arg1 ...) * builtin

      ; procedure:  (map proc list1 ...)  * (scheme list)
      ; procedure:  (string-map proc string1 ...)  * (scheme string)
      ; procedure:  (vector-map proc vector1 ...)  * (scheme vector)

      ; procedure:  (for-each proc list1 ...)  * (scheme list)
      ; procedure:  (string-for-each proc string1 ...)  * (scheme string)
      ; procedure:  (vector-for-each proc vector1 ...)  * (scheme vector)

      ; procedure:  (call-with-current-continuation proc) * (src vm)

      (define call/cc call-with-current-continuation)

      (assert (call/cc procedure?)             ===> #true)

      ; procedure:  (values obj ...)          * builtin /special

      ; procedure:  (call-with-values producer consumer)
      (define-syntax call-with-values
         (syntax-rules ()
            ((call-with-values (lambda () exp) (lambda (arg ...) body))
               (values-apply exp (lambda (arg ...) body)))
            ((call-with-values thunk (lambda (arg ...) body))
               (values-apply (thunk) (lambda (arg ...) body)))))

      ; procedure:  (dynamic-wind before thunk after) ; todo
      ;

      ; 6.11  Exceptions

      ; procedure:  (with-exception-handler handler thunk)  * (scheme exceptions)
      ; procedure:  (raise obj) * (scheme exceptions)
      ; procedure:  (raise-continuable obj)  * (scheme exceptions)
      ; procedure:  (error message obj ...)  * (scheme exceptions)
      ; procedure:  (error-object? obj)
      ; macro:      (error-object-message error-object)
      ; procedure:  (error-object-irritants error-object) * no
      ; procedure:  (read-error? obj) * no
      ; procedure:  (file-error? obj) * no

      ; -- i'm here ----------------------



      ;; 6.5  Eval
      ; ...
      ; procedure:  (eval expression environment-specifier)  * (lang eval)
      ; procedure:  (scheme-report-environment version)      * (lang eval)
      ; procedure:  (null-environment version)               * (lang eval)
      ; optional procedure:  (interaction-environment)       * (lang eval)

      ;; 6.6
      ; ...

      ;; 6.13.2. Input
      ; procedure:  (read)
      ; procedure:  (read port)
      ; procedure:  (read-char)
      ; procedure:  (read-char port)
      ; procedure:  (peek-char)
      ; procedure:  (peek-char port)
      ; procedure:  (read-line)
      ; procedure:  (read-line port)
      ; procedure:  (eof-object? obj)  * (scheme base)
      ; procedure:  (eof-object)       * (scheme base)
      ; procedure:  (char-ready?)
      ; procedure:  (char-ready? port)
      ; procedure:  (read-string k)
      ; procedure:  (read-string k port)
      ; procedure:  (read-u8)
      ; procedure:  (read-u8 port)
      ; procedure:  (peek-u8)
      ; procedure:  (peek-u8 port)
      ; procedure:  (u8-ready?)
      ; procedure:  (u8-ready? port)
      ; procedure:  (read-bytevector k)
      ; procedure:  (read-bytevector k port)
      ; procedure:  (read-bytevector! bytevector)
      ; procedure:  (read-bytevector! bytevector port)
      ; procedure:  (read-bytevector! bytevector port start)
      ; procedure:  (read-bytevector! bytevector port start end)

      (define (eof? o) (eq? o #eof))


      ;; 6.14. System interface
      ; ...

      ; procedure: (features)
      (define-syntax features
         (syntax-rules (*features*)
            ((features)
               *features*)))

      ; 4.1.1  Variable references


      ; i hate special characters, especially in such common operations.
      ; let* (let sequence) is way prettier and a bit more descriptive


      ;; now a function

      ; 4.2.6  Quasiquotation

      (define-syntax cons* ; * MIT/GNU Scheme, srfi 1
         (syntax-rules ()
            ((cons* a) a)
            ((cons* a . b)
               (cons a (cons* . b)))))


      (define-syntax define-library
         (syntax-rules (export import begin _define-library define-library
                        version license keywords description)
            ; remove version, license, keywords, author and description as comments
            ((define-library x ... (version . ?) . tl)
               (define-library x ... . tl))
            ((define-library x ... (license . ?) . tl)
               (define-library x ... . tl))
            ((define-library x ... (keywords . ?) . tl)
               (define-library x ... . tl))
            ((define-library x ... (description . ?) . tl)
               (define-library x ... . tl))


            ;; push export to the end (should syntax-error on multiple exports before this) ?
            ((define-library x ... (export . e) term . tl)
             (define-library x ... term (export . e) . tl))

            ;; lift all imports above begins
            ;((define-library x ... (begin . b) (import-old . i) . tl)
            ; (define-library x ... (import-old . i) (begin . b) . tl))

            ;; convert to special form understood by the repl
            ;((define-library name (import-old . i) ... (begin . b) ... (export . e))
            ; (_define-library 'name '(import-old . i) ... '(begin . b) ... '(export . e)))

            ;; accept otherwise in whatever order
            ((define-library thing ...)
             (_define-library (quote thing) ...))

            ;; fail otherwise
            ((_ . wtf)
               (syntax-error "Weird library contents: " (quote . (define-library . wtf))))))

      ;; toplevel library operations expand to quoted values to be handled by the repl
      ;(define-syntax import  (syntax-rules (_import)  ((import  thing ...) (_import  (quote thing) ...))))
      ;(define-syntax include (syntax-rules (_include) ((include thing ...) (_include (quote thing) ...))))

      (define (value? obj) (eq? (size obj) #false))
      (define (reference? obj) (not (value? obj)))

      (assert (value? 0)                           ===> #true)
      (assert (value? 1)                           ===> #true)
      (assert (value? -7)                          ===> #true)
      (assert (value? "0")                         ===> #false)
      (assert (value? 1.1)                         ===> #false)
      (assert (value? #true)                       ===> #true)

      (assert (reference? 0)                       ===> #false)
      (assert (reference? "0")                     ===> #true)
      (assert (reference? 1.1)                     ===> #true)
      (assert (reference? #true)                   ===> #false)


      ; 3.2. Disjointness of types
      ; No object satisfies more than one of the following predicates:
      ;
      ; boolean?          pair?
      ; symbol?           number?
      ; char?             string?
      ; vector?           port?
      ; procedure?
      ;
      ; These predicates define the types boolean, pair, symbol, number, char (or character), string, vector, port, and procedure. The empty list is a special object of its own type; it satisfies none of the above predicates.
      ;
      ; Although there is a separate boolean type, any Scheme value can be used as a boolean value for the purpose of a conditional test. As explained in section 6.3.1, all values count as true in such a test except for #false.

      (define (port? o)
         (eq? (type o) type-port))


      ; non standard, owl extension
      (define-syntax let*/cc
         (syntax-rules (call/cc)
            ((let*/cc (var) . tail)
               (syntax-error "let*/cc: continuation name cannot be empty"))
            ((let*/cc var . body)
               (call/cc (λ (var) (let* . body))))))

      ;; stop the vm *immediately* without flushing input or anything else with return value n
      (define (halt n)               (vm:exit n))

      ;; owl backward compatibility
      (define null #null)
)
   ; ---------------------------
   (export
      syntax-error runtime-error
      assert error

      ;apply (builtin) ;apply/cc
      call-with-current-continuation
      call/cc let*/cc

      idf greater?

      ; not required to be exported:
      ;  quote values lambda setq
      ;  let-eval ifeq brae values-apply
      ;  cons car cdr ref type size set-ref set-ref! eq? less?
      ;  clock, syscall
      ;  ff:red ff:black ff:toggle ff:red? ff:right?
      ;  ff-apply vector-apply

      ; 4.1.4  Procedures
      λ ; same as 'lambda'
      ; 4.1.5, 4.2.2  Conditionals
      if and or
      ; 4.1.6  Assignments
      set! box unbox
      ; 4.2.1  Binding constructs and Sequencing
      letrec let begin let*
      ; 5.3  Variable definitions
      define ;define*
      define-values
      ; 6.1  Equivalence predicates
      eqv? equal?  ;eq?  * builtin
      ; 4.2.2  Conditionals
      cond case
      when unless
      ; 4.2.5  Delayed evaluation
      delay force
      ; 4.2.8  Quasiquotation
      quasiquote
      ; 4.2.9  Case-lambda
      case-lambda ;  * (srfi 16)
      ; 6.2.1  Numerical types
      type-enum+ type-enum- type-int+ type-int-
      type-rational type-complex type-inexact
      ; 6.2.6  Numerical operations
      integer? rational? real? complex? number?
      exact? inexact? exact-integer?
      inexact exact
      ; 6.2.8  Other data types
      type-bytecode
      type-procedure type-closure
      type-constructor
      type-pair
      type-blob-dispatch type-blob-leaf
      type-bytevector
      ;type-ff-black-leaf
      type-vector
      type-symbol
      type-const
      type-rlist-spine type-rlist-node
      type-port
      type-string type-string-wide type-superstring
      type-thread-state
      type-vptr
      ;type-ff type-ff-r type-ff-red type-ff-red-r
      ; 6.3  Booleans
      not boolean?
      ; 6.4. Pairs and lists
      pair? ;cons car cdr  * builtin
      set-car! set-cdr!
      caar cadr cdar cddr
      null? list?
      make-list list length
      append reverse list-tail list-ref list-set!
      memq memv member
      ; 6.5. Symbols
      symbol? symbol=? ;symbol->string string->symbol  * (owl string)
      ; 6.8  Vectors
      vector?
      make-vector
      vector
      ; 6.9  Bytevectors   * (scheme bytevector)
      bytevector?
      make-bytevector
      bytevector
      ; 6.10  Control features
      ff? bytecode? function?
      procedure? apply
      object?
      call-with-current-continuation call/cc
      call-with-values
      
      ; 6.11  Exceptions                         * (scheme exceptions)

      ; ----------------------------
      list length append reverse
      cons*
      define-library

      port?  eof?
      value? reference?

      ; 6.14
      features

      ; ol extension:
      halt

      ; owl extension:
      null

      -- ++ ; * ol internal staff
))
