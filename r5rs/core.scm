; http://www.schemers.org/Documents/Standards/R5RS/HTML/
(define-library (r5rs core)
   (begin

      ; basic Otus Lisp elements:

      ;; special forms:
      ; quote lambda setq
      ; values apply-values
      ; ifeq ifary ol:let

      ;; virtual machine primitives:
      ; raw cons car cdr ref type size cast raw? set set! eq? less?
      ; vm:add vm:sub vm:mul vm:div vm:shr vm:shl vm:and vm:or vm:xor
      ; clock syscall vm:version fxmax fxmbits vm:wordsize
      ; tuple-apply mkt listuple
      ; ff-apply ff:red ff:black ff:toggle ff:red? ff:right?




      ; ========================================================================================================
      ; Scheme
      ;
      ; Revised(5) Report on the Algorithmic Language Scheme
      ;                  Dedicated to the Memory of ALGOL 60
      ;
      ; ========================================================================================================

      ;;; Chapter 1
      ;;; Overview of Scheme
      ; 1.1  Semantics
      ; 1.2  Syntax


      ; 1.3  Notation and terminology
      ; 1.3.1  Primitive, library, and optional features
      ; 1.3.2  Error situations and unspecified behavior
      (define-syntax syntax-error
         (syntax-rules (runtime-error)
            ((syntax-error . stuff)
               (runtime-error "Syntax error: " (quote stuff)))))

      ; 1.3.3  Entry format
      ; 1.3.4  Evaluation examples
      ; 1.3.5  Naming conventions


      ;;; Chapter 2
      ;;; Lexical conventions
      ;
      ; This section gives an informal account of some of the lexical conventions used in writing Scheme
      ; programs. For a formal syntax of Scheme, see section 7.1.
      ; Upper and lower case forms of a letter are never distinguished except within character and string
      ; constants. For example, Foo is the same identifier as FOO, and #x1AB is the same number as #X1ab.
      ;
      ; 2.1  Identifiers
      ;
      ; lambda        q
      ; list->vector  soup
      ; +             V17a
      ; <=?           a34kTMNs
      ; the-word-recursion-has-many-meanings
      ;
      ; Extended alphabetic characters may be used within identifiers as if they were letters. The
      ; following are extended alphabetic characters:
      ;
      ; ! $ % & * + - . / : < = > ? @ ^ _ ~
      ;

      ; 2.2  Whitespace and comments
      ; 2.3  Other notations


      ;;; Chapter 3
      ;;; Basic concepts
      ; 3.1  Variables, syntactic keywords, and regions
      ; 3.2  Disjointness of types
      ; 3.3  External representations
      ; 3.4  Storage model
      ; 3.5  Proper tail recursion


      ;;; Chapter 4
      ;;; Expressions
      ; 4.1  Primitive expression types

      ; 4.1.1  Variable references
      ; syntax:  <variable>

      ; 4.1.2  Literal expressions
      ; syntax:  quote <datum>                * builtin
      ; syntax:  '<datum>                     * builtin
      ; syntax:  <constant>                   * builtin

      ; 4.1.3  Procedure calls
      ; syntax:  (<operator> <operand1> ...)  * builtin

      ; 4.1.4  Procedures
      ; syntax:  (lambda <formals> <body>)    * builtin
      (define-syntax λ
         (syntax-rules ()
            ((λ . x) (lambda . x))))

      ; http://srfi.schemers.org/srfi-16/srfi-16.html
      ; srfi syntex: (case-lambda ...
      (define-syntax case-lambda
         (syntax-rules ()
            ((case-lambda)
               (lambda () (raw type-bytecode '(27)))) ; arity-error
            ; ^ should use syntax-error instead, but not yet sure if this will be used before error is defined
            ((case-lambda (formals . body))
               ;; downgrade to a run-of-the-mill lambda
               (lambda formals . body))
            ((case-lambda (formals . body) . rest)
               ;; make a list of options to be compiled to a chain of code bodies w/ jumps
               ;; note, could also merge to a jump table + sequence of codes, but it doesn't really matter
               ;; because speed-sensitive stuff will be compiled to C where this won't matter
               (ifary (lambda formals . body)
                  (case-lambda . rest)))))

      ; -------------------
      ; 4.1.5  Conditionals
      ; syntax:  if <test> <consequent> <alternate>
      ; syntax:  if <test> <consequent>
      (define-syntax if
         (syntax-rules (not eq? null? empty? zero?)
            ((if val       then)      (if val then #false))
            ((if (not val) then else) (if val else then))
            ((if (eq? a b) then else) (ifeq a b then else))
            ((if (null? test)  then else) (if (eq? test #null) then else))  ; boot image size and compilation speed optimization
            ((if (a . b)   then else) ((lambda (x) (if x then else)) (a . b)))
            ((if #true     then else)  then)
            ((if #false    then else)  else)
            ((if t         then else) (ifeq t #false else then))))

      ; ------------------
      ; 4.1.6  Assignments
      ; syntax: set! <variable> <expression>


      ;; 4.2  Derived expression types
      ; The constructs in this section are hygienic, as discussed in section 4.3. For reference purposes,
      ; section 7.3 gives macro definitions that will convert most of the constructs described in this
      ; section into the primitive constructs described in the previous section.

      ; -------------------
      ; 4.2.1  Conditionals
      ; library syntax:  (cond <clause1> <clause2> ...)
      (define-syntax cond
         (syntax-rules (else =>)
            ((cond) #false)
            ((cond (else exp . rest))
               ((lambda () exp . rest)))        ; (begin ...)
            ((cond (clause => exp) . rest)
               ((lambda (fresh)
                  (if fresh
                     (exp fresh)
                     (cond . rest)))  clause))
            ((cond (clause exp . rest-exps) . rest)
               (if clause
                  ((lambda () exp . rest-exps)) ; (begin ...)
                  (cond . rest)))))

      ; library syntax:  (case <key> <clause1> <clause2> ...)
      (define-syntax case
         (syntax-rules (else eqv? memv =>)
            ((case (op . args) . clauses)
               ((lambda (fresh) ; let ((fresh (op.args)))
                  (case fresh . clauses)) (op . args)))
            ((case thing) #false)
            ((case thing ((a) => exp) . clauses)
               (if (eqv? thing (quote a))
                  (exp thing)
                  (case thing . clauses)))
            ((case thing ((a ...) => exp) . clauses)
               (if (memv thing (quote (a ...)))
                  (exp thing)
                  (case thing . clauses)))
            ((case thing ((a) . body) . clauses)
               (if (eqv? thing (quote a))
                  ((lambda () . body)) ; means (begin . body)
                  (case thing . clauses)))
            ((case thing (else => func))
               (func thing))
            ((case thing (else . body))
               ((lambda () . body)))   ; means (begin . body)
            ((case thing ((a . b) . body) . clauses)
               (if (memv thing (quote (a . b)))
                  ((lambda () . body)) ; means (begin . body)
                  (case thing . clauses)))
            ((case thing (atom . then) . clauses) ;; added for (case (type foo) (type-foo thenfoo) (type-bar thenbar) ...)
               (if (eq? thing atom)
                  ((lambda () . then)) ; means (begin . body)
                  (case thing . clauses)))))

      ; library syntax:  (and <test1> ...)
      (define-syntax and
         (syntax-rules ()
            ((and) #true)
            ((and a) a)
            ((and a . b)
               (if a (and . b) #false))))

      ; library syntax:  (or <test1> ...)
      (define-syntax or
         (syntax-rules ()
            ((or) #false)
            ((or (a . b) . c)
               ((lambda (x) (or x . c))  (a . b)))
            ((or a . b)
               (if a a (or . b)))))


      ; -------------------------
      ; 4.2.2  Binding constructs

      ; The three binding constructs let, let*, and letrec give Scheme a block structure, like Algol 60.
      ; The syntax of the three constructs is identical, but they differ in the regions they establish
      ; for their variable bindings. In a let expression, the initial values are computed before any of
      ; the variables become bound; in a let* expression, the bindings and evaluations are performed
      ; sequentially; while in a letrec expression, all the bindings are in effect while their initial
      ; values are being computed, thus allowing mutually recursive definitions.

      ; library syntax:  (letrec <bindings> <body>)
      (define-syntax letrec
         (syntax-rules ()
            ((letrec ((?var ?val) ...) ?body) (ol:let (?var ...) (?val ...) ?body))
            ((letrec vars body ...) (letrec vars (begin body ...)))))

      ; library syntax:  (letrec* ...) - extension
      (define-syntax letrec*
         (syntax-rules ()
            ((letrec () . body)
               (begin . body))
            ((letrec* ((var val) . rest) . body)
               (letrec ((var val))
                  (letrec* rest . body)))))

      ; library syntax:  (let <bindings> <body>)
      ;                  (let keyword <bindings> <body>) named let, from 4.2.4 Iteration
      (define-syntax let
         (syntax-rules ()
            ((let ((var val) ...) exp . rest)
               ((lambda (var ...) exp . rest) val ...))
               ;why not (ol:let (var ...) (val ...) exp . rest)) ???
            ((let keyword ((var init) ...) exp . rest)
               (letrec ((keyword (lambda (var ...) exp . rest))) (keyword init ...)))))

      ; library syntax:  (let* <bindings> <body>)
      (define-syntax let*
         (syntax-rules (<=)
            ((let* (((var ...) gen) . rest) . body)
               (apply-values gen (lambda (var ...) (let* rest . body))))
            ((let* ((var val) . rest-bindings) exp . rest-exps)
               ((lambda (var) (let* rest-bindings exp . rest-exps)) val))
            ; http://srfi.schemers.org/srfi-71/srfi-71.html
            ((let* ((var ... (op . args)) . rest-bindings) exp . rest-exps)
               (apply-values (op . args)
                  (lambda (var ...)
                     (let* rest-bindings exp . rest-exps))))
            ((let* ((var ... node) . rest-bindings) exp . rest-exps)
               (tuple-apply node
                  (lambda (var ...)
                     (let* rest-bindings exp . rest-exps))))
            ((let* (((name ...) <= value) . rest) . code)
               (tuple-apply value
                  (lambda (name ...)
                     (let* rest . code))))
            ((let* ()) exp)
            ((let* () exp . rest)
               ((lambda () exp . rest)))))

      ; lets === let*, TEMP!
      (define-syntax lets
         (syntax-rules ()
            ((lets . stuff) (let* . stuff))))


      ; -----------------
      ; 4.2.3  Sequencing
      ; library syntax:  (begin <expression1> <expression2> ...)
      (define-syntax begin
         (syntax-rules (define letrec define-values let*-values letrec) ; todo: rename define-values to define*
            ((begin exp) exp)
            ((begin (define . a) (define . b) ... . rest)
               (begin 42 () (define . a) (define . b) ... . rest))
            ((begin (define-values (val ...) . body) . rest)
               (let*-values (((val ...) (begin . body))) . rest))
            ((begin 42 done (define ((op . args1) . args2) . body) . rest)
               (begin 42 done (define (op . args1) (lambda args2 . body)) . rest))
            ((begin 42 done (define (var . args) . body) . rest)
               (begin 42 done (define var (lambda args . body)) . rest))
            ((begin 42 done (define var exp1 exp2 . expn) . rest)
               (begin 42 done (define var (begin exp1 exp2 . expn)) . rest))
            ((begin 42 done (define var val) . rest)
               (begin 42 ((var val) . done) . rest))
            ((begin 42 done . exps)
               (begin 43 done () exps))
            ((begin 43 (a . b) c exps)
               (begin 43 b (a . c) exps))
            ((begin 43 () bindings exps)
               (letrec bindings (begin . exps)))
            ((begin first . rest)
               ((lambda (free) (begin . rest))  first))))


      ; ----------------
      ; 4.2.4  Iteration
      ; library syntax: do ((<variable1> <init1> <step1>) ...) (<test> <expression> ...) <command> ...
      (define-syntax do ; ?
         (syntax-rules ()
            ((do ((var init step) ...) (test expr ...) command ...)
               (let loop ((var init) ...)
                  (if test
                     (begin expr ...)
                     (loop step ...))))))


      ; -------------------------
      ; 4.2.5  Delayed evaluation
      ; library syntax:  delay <expression>
      (define-syntax delay
         (syntax-rules ()
            ((delay (op . args))
               (lambda () (op . args)))
            ((delay value) value)))


      ; ---------------------
      ; 4.2.6  Quasiquotation
      ; `(a ,(+ 1 2) ,(map abs '(4 -5 6)) b) ===> (a 3 (4 5 6) b)
      ; `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) ===> (a 3 4 5 6 b)
      (define-syntax quasiquote
         (syntax-rules (unquote quote unquote-splicing append _work _sharp_vector list->vector)
                                                   ;          ^         ^
                                                   ;          '-- mine  '-- added by the parser for #(... (a . b) ...) -> (_sharp_vector ... )
            ((quasiquote _work () (unquote exp)) exp)
            ((quasiquote _work (a . b) (unquote exp))
               (list 'unquote (quasiquote _work b exp)))
            ((quasiquote _work d (quasiquote . e))
               (list 'quasiquote
                  (quasiquote _work (() . d) . e)))
            ((quasiquote _work () ((unquote-splicing exp) . tl))
               (append exp
                  (quasiquote _work () tl)))
            ((quasiquote _work () (_sharp_vector . es))
               (list->vector
                  (quasiquote _work () es)))
            ((quasiquote _work d (a . b))
               (cons (quasiquote _work d a)
                     (quasiquote _work d b)))
            ((quasiquote _work d atom)
               (quote atom))
            ((quasiquote . stuff)
               (quasiquote _work () . stuff))))


      ;; 4.3  Macros
      ; 4.3.1  Binding constructs for syntactic keywords
      ; 4.3.2  Pattern language


      ;;; Chapter 5
      ;;; Program structure
      ;; 5.1  Programs
      ;; 5.2  Definitions
      (define-syntax define
         (syntax-rules (lambda) ;λ
            ((define op a b . c)
               (define op (begin a b . c)))
            ((define ((op . args) . more) . body)
               (define (op . args) (lambda more . body)))
            ((define (op . args) body)
               (define op
                  (letrec ((op (lambda args body))) op)))
            ((define name (lambda (var ...) . body))
               (setq name (ol:let (name) ((lambda (var ...) . body)) name)))
            ((define op val)
               (setq op val))))

;      ;; not defining directly because ol:let doesn't yet do variable arity
;      ;(define list ((lambda (x) x) (lambda x x)))
;
;      ;; fixme, should use a print-limited variant for debugging
;
       ; EXTENSION, unused!
;      (define-syntax define*
;         (syntax-rules (print list)
;            ((define* (op . args) . body)
;               (define (op . args)
;                  (print " * " (list (quote op) . args))
;                  .  body))
;            ((define* name (lambda (arg ...) . body))
;               (define* (name arg ...) . body))))

      ; EXTENSION, maybe unused!
      ; the internal one is handled by begin. this is just for toplevel.
      (define-syntax define-values
         (syntax-rules (list)
            ((define-values (val ...) . body)
               (setq (val ...)
                  (let* ((val ... (begin . body)))
                     (list val ...))))))

      ; EXTENSION, maybe r7rs!
      (define-syntax let*-values
         (syntax-rules ()
            ((let*-values (((var ...) gen) . rest) . body)
               (apply-values gen
                  (lambda (var ...) (let*-values rest . body))))
            ((let*-values () . rest)
               (begin . rest))))



      ; 5.2.1  Top level definitions
      ; 5.2.2  Internal definitions
      ; 5.3  Syntax definitions


      ;;; Chapter 6
      ;;; Standard procedures
      ;
      ; This chapter describes Scheme's built-in procedures. The initial (or ``top level'') Scheme
      ; environment starts out with a number of variables bound to locations containing useful values,
      ; most of which are primitive procedures that manipulate data. For example, the variable abs is
      ; bound to (a location initially containing) a procedure of one argument that computes the
      ; absolute value of a number, and the variable + is bound to a procedure that computes sums.
      ; Built-in procedures that can easily be written in terms of other built-in procedures are
      ; identified as ``library procedures''.
      ;

      ; this is temporary simlified 'assert' that use 'eq?', please be careful!
      (define-syntax assert
         (syntax-rules (===>)
            ((assert expression)
               (assert expression ===> #true))
            ((assert expression ===> result)
               (ifeq expression (quote result) #true
                  ((raw 16 '(27 4 5 6 7 8 24 8)) ; (sys a b c d)
                     #null 5 "assertion error: " (cons (quote expression) (cons "must be" (cons (quote result) #null))))))))
;            ((assert result expression . stuff)
;               (if (eq? expression result) #t
;                  ((raw type-bytecode '(27 4 5 6 7 8  24 8))
;                     '() 5 "assertion error: " (cons (quote expression) (cons "must be" (cons result '()))))))))
;                 (call/cc (λ (resume) (sys resume 5 "Assertion error: " (list (quote expression) (quote stuff)))))

      ;; ---------------------------
      ;; 6.1  Equivalence predicates

      ; procedure:  (eqv? obj1 obj2)
      (define (eqv? a b)
         (cond
            ((eq? a b)
               #true)
;            ((symbol? a) #false) ; would have been eq?, because they are interned
;            ((pair? a)
;               (if (pair? b)
;                  (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
;                  #false))
;            (else
;               (let ((sa (size a)))
;                  (cond
;                     ; a is immediate -> would have been eq?
;                     ((not sa)   #false)
;                     ; same size
;                     ((eq? sa (size b))
;                        (let ((ta (type a)))
;                           ; check equal types
;                           (if (eq? ta (type b))
;                              (if (raw? a)
;                                 ; equal raw objects, check bytes
;                                 (lets
;                                    ((ea (size a)) ; raw objects may have padding bytes, so recheck the sizes
;                                     (eb (size b)))
;                                    (if (eq? ea eb)
;                                       (if (eq? ea 0)
;                                          #true
;                                          (eq-bytes a b (- ea 1)))
;                                       #false))
;                                 ; equal ntuples, check fields
;                                 (eq-fields a b equal? sa))
;                              #false)))
;                     (else #false))))))
            (else #false)))
      ; tbd.


      ; --------------------------
      ; procedure: (eq? obj1 obj2)    * builtin
      ;
      ; Rationale: It will usually be possible to implement `eq?' much more efficiently than `eqv?',
      ; for example, as a simple pointer comparison instead of as some more complicated operation.
      ; One reason is that it may not be possible to compute `eqv?' of two numbers in constant time,
      ; whereas `eq?' implemented as pointer comparison will always finish in constant time. `Eq?'
      ; may be used like `eqv?' in applications using procedures to implement objects with state
      ; since it obeys the same constraints as `eqv?'.

      ; library procedure: (equal? obj1 obj2)
      ;

;      (define-syntax (eqv? a b)
;         (cond
;            ((eq? a b)
;               #true)
;;            ((symbol? a) #false) ; would have been eq?, because they are interned
;            ((pair? a)
;               (if (pair? b)
;                  (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
;                  #false))
;            (else
;               (let ((sa (size a)))
;                  (cond
;                     ; a is immediate -> would have been eq?
;                     ((not sa)   #false)
;                     ; same size
;                     ((eq? sa (size b))
;                        (let ((ta (type a)))
;                           ; check equal types
;                           (if (eq? ta (type b))
;                              (if (raw? a)
;                                 ; equal raw objects, check bytes
;                                 (lets
;                                    ((ea (size a)) ; raw objects may have padding bytes, so recheck the sizes
;                                     (eb (size b)))
;                                    (if (eq? ea eb)
;                                       (if (eq? ea 0)
;                                          #true
;                                          (eq-bytes a b (- ea 1)))
;                                       #false))
;                                 ; equal ntuples, check fields
;                                 (eq-fields a b equal? sa))
;                              #false)))
;                     (else #false))))))
;            (else #false)))

      ;; 6.2  Numbers
      ; -------------------------------
      ; This data types related to olvm
      ;     - not a part of r5rs -
      (define type-fix+              0) ; value
      (define type-fix-             32) ; value
      (define type-int+             40) ; reference
      (define type-int-             41) ; reference
      (define type-rational         42) ; reference
      (define type-complex          43) ; reference

      ; 6.2.1  Numerical types
      ; 6.2.2  Exactness
      ; 6.2.3  Implementation restrictions
      ; 6.2.4  Syntax of numerical constants

      ; ---------------------------
      ; 6.2.5  Numerical operations
      ;
      ; procedure:  (number? obj)
      (define (number? o)
         (case (type o)
            (type-fix+ #true)
            (type-fix- #true)
            (type-int+ #true)
            (type-int- #true)
            (type-rational #true)
            (type-complex #true)
            (else #false)))

      ; procedure:  (complex? obj)
      ; procedure:  (real? obj)
      ; procedure:  (rational? obj)
      ; procedure:  (integer? obj)
      ; procedure:  (exact? z)
      ; procedure:  (inexact? z)

      ; *** declared in (r5rs math), (r5rs math-extra)
      ; procedure:  (= z1 z2 z3 ...) <- (r5rs math)
      ; procedure:  (< x1 x2 x3 ...)
      ; procedure:  (> x1 x2 x3 ...)
      ; procedure:  (<= x1 x2 x3 ...)
      ; procedure:  (>= x1 x2 x3 ...)
      ; library procedure:  (zero? z)
      ; library procedure:  (positive? x)
      ; library procedure:  (negative? x)
      ; library procedure:  (odd? n)
      ; library procedure:  (even? n)
      ; library procedure:  (max x1 x2 ...)
      ; library procedure:  (min x1 x2 ...)
      ; procedure:  (+ z1 ...)
      ; procedure:  (* z1 ...)
      ; procedure:  (- z1 z2)
      ; procedure:  (- z)
      ; optional procedure:  (- z1 z2 ...)
      ; procedure:  (/ z1 z2)
      ; procedure:  (/ z)
      ; optional procedure:  (/ z1 z2 ...)
      ; library procedure: (abs x)
      ; procedure: quotient (n1 n2)
      ; procedure: remainder n1 n2
      ; procedure: modulo n1 n2
      ; library procedure: gcd n1 ...
      ; library procedure: lcm n1 ...
      ; procedure: numerator q
      ; procedure: denominator q
      ; procedure: floor x
      ; procedure: ceiling x
      ; procedure: truncate x
      ; procedure: round x
      ; library procedure: rationalize x y
      ; procedure: exp z
      ; procedure: log z
      ; procedure: sin z
      ; procedure: cos z
      ; procedure: tan z
      ; procedure: asin z
      ; procedure: acos z
      ; procedure: atan z
      ; procedure: atan y x
      ; procedure: sqrt z
      ; procedure: expt z1 z2
      ; procedure: make-rectangular x1 x2
      ; procedure: make-polar x3 x4
      ; procedure: real-part z
      ; procedure: imag-part z
      ; procedure: magnitude z
      ; procedure: angle z
      ; procedure: exact->inexact z
      ; procedure: inexact->exact z

      ; ---------------------------------
      ; 6.2.6  Numerical input and output
      ; procedure:  (number->string z) <- (r5rs strings?)
      ; procedure:  (number->string z radix) <- (r5rs strings?)
;      (define (number->string n base)
;         (list->string (render-number n null base)))

      ; procedure:  (string->number string) <- (lang s-exp?)
      ; procedure:  (string->number string radix) <- (lang s-exp?)
;      (define string->number (case-lambda
;         ((str base) (list->number (string->list str) base))
;         (str        (list->number (string->list str) 10))))


      ;; *********************
      ;; 6.3  Other data types
      ;
      ;  This section describes operations on some of Scheme's non-numeric data types: booleans, pairs,
      ;  lists, symbols, characters, strings and vectors.
      ;
      ; -------------------------------
      ; This data types related to olvm
      ;     - not a part of r5rs -
;      (define type-fix-             32) ; value
;      (define type-int+             40) ; reference
;      (define type-int-             41) ; reference
;      (define type-rational         42) ; reference
;      (define type-complex          43) ; reference

      ;define type-fix+              0) ; value
      (define type-pair              1) ; reference
      (define type-tuple             2) ; reference
      (define type-string            3) ; reference, raw -> 35 (#b100000 + 3)?
      (define type-symbol            4) ; reference
      ; 5   TODO(?): (define type-string-wide      5) ; reference, raw
      ; 6
      ; 7
      (define type-ff-black-leaf     8) ; reference ; TODO: move to 28
      ; 9

      (define type-rlist-spine      10) ; reference
      (define type-vector-leaf      11) ; reference

      (define type-port             12) ; value
      (define type-const            13) ; value

      (define type-rlist-node       14) ; reference
      (define type-vector-dispatch  15) ; reference

      (define type-bytecode         16) ; reference, raw     ; declared functions (?)
      (define type-proc             17) ; reference          ; from otus lisp bin (?)
      (define type-clos             18) ; reference          ; from (import smth) (?)

      (define type-vector-raw       19) ; reference, raw     ; see also TBVEC in c/ovm.c

      ; 20
      (define type-string-dispatch  21) ; reference
      (define type-string-wide      22) ; reference, raw
      ; 23

      ;; transitional trees or future ffs
      (define type-ff               24) ; reference
      (define type-ff-r             25) ; reference
      (define type-ff-red           26) ; reference
      (define type-ff-red-r         27) ; reference
      ; + type-ff-red, type-ff-right

      ;28
      ;29
      ;30

      (define type-thread-state     31) ; reference
      (define type-vptr             49) ; reference,  raw

      ; ---------------
      ; 6.3.1  Booleans
      ;
      ; Of all the standard Scheme values, only #f counts as false in conditional expressions.
      ; Except for #f, all standard Scheme values, including #t, pairs, the empty list, symbols,
      ; numbers, strings, vectors, and procedures, count as true.
      ;      Note: Programmers accustomed to other dialects of Lisp should be aware that Scheme
      ;            distinguishes both #f and the empty list from the symbol nil.
      ; Boolean constants evaluate to themselves, so they do not need to be quoted in programs.
      (assert #t                                     ===>  #t)
      (assert #f                                     ===>  #f)
      (assert (quote #f)                             ===>  #f)


      ; library procedure:  (not obj)
      (define (not x)
         (if x #false #true))

      (assert (not #t)                               ===>  #f)
      (assert (not 3)                                ===>  #f)
      (assert (not '(3 . 0))                         ===>  #f)
      (assert (not #f)                               ===>  #t)
      (assert (not '())                              ===>  #f)
      (assert (not cons)                             ===>  #f)
      (assert (not 'nil)                             ===>  #f)


      ; library procedure:  (boolean? obj)
      (define (boolean? o)
         (cond
            ((eq? o #true) #true)
            ((eq? o #false) #true)
            (else #false)))

      (assert (boolean? #f)                          ===>  #t)
      (assert (boolean? 0)                           ===>  #f)
      (assert (boolean? '())                         ===>  #f)


      ; Оставить здесь только самые абзово необходимые вещи, все остальное переместить в:
      ;   6.3.2 -> (r5rs lists)
      ;   6.3.3 -> (r5rs symbols)
      ;   6.3.4 -> (r5rs characters)
      ;   6.3.5 -> (r5rs strings)
      ;   6.3.6 -> (r5rs vectors)
      ;
      ;

      ; ----------------------
      ; 6.3.2. Pairs and lists

      ; procedure:  (pair? obj)
      (define (pair? o)
         (eq? (type o) type-pair))

      (assert (pair? '(a . b))                       ===>  #t)
      (assert (pair? '(a b c))                       ===>  #t)
      (assert (pair? '())                            ===>  #f)
      (assert (pair? '#(a b))                        ===>  #f)

      ; procedure:  (cons obj1 obj2)    * builtin
      ; procedure:  (car pair)          * builtin
      ; procedure:  (cdr pair)          * builtin
      ; procedure:  (set-car! pair obj)
      (define (set-car! o v)
         (set-ref! o 1 v))

      ; procedure:  (set-cdr! pair obj)
      (define (set-cdr! o v)
         (set-ref! o 2 v))

      ; library procedure:  (caar pair) <- (r5rs lists)
      ; library procedure:  (cadr pair) <- (r5rs lists)
      ; ...
      ; library procedure:  (cdddar pair) <- (r5rs lists)
      ; library procedure:  (cddddr pair) <- (r5rs lists)

      ; library procedure:  (null? obj)
      (define (null? x)
         (eq? x #null))

      ; library procedure:  (list? obj)
      (define (list? l)
         (cond
            ((null? l) #true)
            ((pair? l) (list? (cdr l)))
            (else #false)))

      ; library procedure:  (list obj ...)
      (define-syntax list
         (syntax-rules ()
            ((list) '())
            ((list a . b)
               (cons a (list . b)))))

      ; library procedure:  (length list)
      (define (length l)
         (let loop ((n 0) (l l))
            (if (null? l)
               n
               (apply-values (vm:add n 1)
                  (lambda (n carry)
;                     (if carry (runtime-error ...))
                     (loop n (cdr l)))))))

      (assert (length '(a b c))                      ===>  3)
      (assert (length '(a (b) (c d e)))              ===>  3)
      (assert (length '())                           ===>  0)



      ; library procedure:  (append list ...)
;      (define (app a b app)
;         (if (null? a)
;            b
;            (cons (car a) (app (cdr a) b app))))
;
;      (define (appl l appl)
;         (if (null? (cdr l))
;            (car l)
;            (app (car l) (appl (cdr l) appl) app)))

      (define append
         (let*((app (lambda (a b app)
                  (if (null? a)
                     b
                     (cons (car a) (app (cdr a) b app)))))
               (appl (lambda (l appl)
                  (if (null? (cdr l))
                     (car l)
                     (app (car l) (appl (cdr l) appl) app)))))
         (case-lambda
            ((a b) (app a b app))
            ((a b . cs) (app a (app b (appl cs appl) app) app))
            ((a) a)
            (() '()))))

      ; library procedure:  (reverse list)
      (define (reverse l)
         (let rev-loop ((a l) (b '()))
         (if (null? a)
            b
            (rev-loop (cdr a) (cons (car a) b)))))

      ; library procedure:  (list-tail list k)
      ; library procedure:  (list-ref list k)

      ; library procedure:  (memq obj list)
      ; library procedure:  (memv obj list)
      ; library procedure:  (member obj list)

      ; library procedure:  (assq obj alist)
      ; library procedure:  (assv obj alist)
      ; library procedure:  (assoc obj alist)


      ; --------------
      ; 6.3.3. Symbols

      ; procedure:  (symbol? obj)
      (define (symbol? o)
         (eq? (type o) type-symbol))

      ; procedure:  (symbol->string symbol) *tbd
      ; procedure:  (string->symbol string) *tbd


      ; 6.3.4. Characters
      ; (char? obj) procedure
      (define (char? o) (number? o))


      ; 6.3.5. Strings
      ;; (string? obj) procedure
      (define (string? o)
         (case (type o)
            (type-string #true)
            (type-string-wide #true)
            (type-string-dispatch #true)
            (else #false)))


      ; 6.3.6. Vectors
      ;; (vector? obj) procedure
      (define (vector? o) ; == raw or a variant of major type 11?
         (case (type o)
            (type-vector-raw #true)
            (type-vector-leaf #true)
            (type-vector-dispatch #true)
            (else #false)))


      ;; *********************
      ;; 6.4  Control features

      ; *ol* extension
      (define (ff? o)        ; OL extension
         (or (eq? o #empty)
             (eq? 24 (vm:and (type o) #b1111100))))

      ; *ol* extension
      (define (bytecode? o)  ; OL extension
         (eq? (type o) type-bytecode))

      ; *ol* extension
      (define (function? o)  ; OL extension
         (case (type o)
            (type-proc #true)
            (type-clos #true)
            (type-bytecode #true)
            (else #false)))

      ; procedure:  (procedure? obj)
      (define (procedure? o)
         (or (function? o) (ff? o)))


      ; procedure:  (apply proc arg1 ... args)  *builtin

      ; library procedure:  (map proc list1 list2 ...)
;      (define (map fn lst)
;         (if (null? lst)
;            '()
;            (let*
;               ((head tail lst)
;                (head (fn head))) ;; compute head first
;               (cons head (map fn tail)))))

      ; experimental syntax for map for variable count of arguments
      ;  can be changed to map used (apply f (map car .)) and (map cdr .))
      ; todo: test and change map to this version
      (define map (case-lambda
         ((f a b c) (let loop ((a a)(b b)(c c))
                        (if (null? a)
                           '()
                           (cons (f (car a) (car b) (car c)) (loop (cdr a) (cdr b) (cdr c))))))
         ((f a b)   (let loop ((a a)(b b))
                        (if (null? a)
                           '()
                           (cons (f (car a) (car b)) (loop (cdr a) (cdr b))))))
         ((f a)     (let loop ((a a))
                        (if (null? a)
                           '()
                           (cons (f (car a)) (loop (cdr a))))))
         (() #f)))

       ; library procedure:  (for-each proc list1 list2 ...)
       ; library procedure:  (force promise)

       ; procedure:  (call-with-current-continuation proc)
       ; Continuation - http://en.wikipedia.org/wiki/Continuation
       (define apply-cont (raw type-bytecode '(#x54)))  ;; не экспортим, внутренняя

       (define call-with-current-continuation
          ('_sans_cps
             (λ (k f)
                (f k (case-lambda
                        ((c a) (k a))
                        ((c a b) (k a b))
                        ((c . x) (apply-cont k x))))))) ; (apply-cont k x)

       (define call/cc call-with-current-continuation)


       ; procedure:  (values obj ...)
       ; procedure:  (call-with-values producer consumer)
       ; procedure:  (dynamic-wind before thunk after)
       ;

      ;; 6.5  Eval
      ; ...

      ;; 6.6
      ; ...

      ;; 6.6.2 Input
      ; library procedure:  (read)
      ; library procedure:  (read port)
      ; procedure:  (read-char)
      ; procedure:  (read-char port)
      ; procedure:  (peek-char)
      ; procedure:  (peek-char port)
      ; procedure:  (eof-object? obj)
      ; procedure:  (char-ready?)
      ; procedure:  (char-ready? port)

      (define (eof? o) (eq? o #eof))


       ;; **********
       ;; *OL Tuples

      (define-syntax tuple
         (syntax-rules ()
            ((tuple a . bs) ;; there are no such things as 0-tuples
               (mkt 2 a . bs))))

      ; replace this with typed destructuring compare later on

      (define-syntax tuple-case
         (syntax-rules (else _ is eq? bind div)
            ((tuple-case (op . args) . rest)
               (let ((foo (op . args)))
                  (tuple-case foo . rest)))
            ;;; bind if the first value (literal) matches first of pattern
            ((tuple-case 42 tuple type ((this . vars) . body) . others)
               (if (eq? type (quote this))
                  (tuple-apply tuple
                     (lambda (ignore . vars) . body))
                  (tuple-case 42 tuple type . others)))
            ;;; bind to anything
            ((tuple-case 42 tuple type ((_ . vars) . body) . rest)
               (tuple-apply tuple
                  (lambda (ignore . vars) . body)))
            ;;; an else case needing the tuple
            ((tuple-case 42 tuple type (else is name . body))
               (let ((name tuple))
                  (begin . body)))
            ;;; a normal else clause
            ((tuple-case 42 tuple type (else . body))
               (begin . body))
            ;;; throw an error if nothing matches
            ((tuple-case 42 tuple type)
               (syntax-error "weird tuple-case"))
            ;;; get type and start walking
            ((tuple-case tuple case ...)
               (let ((type (ref tuple 1)))
                  (tuple-case 42 tuple type case ...)))))




      ;; note, no let-values yet, so using let*-values in define-values
; .......................
; .......................
; .......................
; .......................













      ; 4.1.1  Variable references


      ; i hate special characters, especially in such common operations.
      ; let* (let sequence) is way prettier and a bit more descriptive


      ;; now a function

      ; 4.2.6  Quasiquotation


      (define-syntax ilist
         (syntax-rules ()
            ((ilist a) a)
            ((ilist a . b)
               (cons a (ilist . b)))))


      (define-syntax call-with-values
         (syntax-rules ()
            ((call-with-values (lambda () exp) (lambda (arg ...) body))
               (apply-values exp (lambda (arg ...) body)))
            ((call-with-values thunk (lambda (arg ...) body))
               (apply-values (thunk) (lambda (arg ...) body)))))






      (define-syntax define-library
         (syntax-rules (export import begin _define-library define-library)
            ;; push export to the end (should syntax-error on multiple exports before this)
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


;      (define o (λ (f g) (λ (x) (f (g x)))))
;
;      (define i (λ (x) x))
;
;      (define self i)
;
;      ; (define call/cc  ('_sans_cps (λ (k f) (f k (λ (r a) (k a))))))
;
;      (define (i x) x)
;      (define (k x y) x)


      ;; these are core data structure type tags which are fixed and some also relied on by the vm

      ;; ALLOCATED


      ;;           allocated/pointers     allocated/rawdata    immediate
      ;; (size  x)         n                       n               #false

      (define (value? obj) (eq? #false (size obj)))
      (define reference? size)
      ;(define (raw? o) (syscall 1001 o #f #f))
;         (case (type o)
;            (type-string #t)
;            (type-string-wide #t)
;            (type-bytecode #t)
;            (type-port #t)
;            (type-vector-raw #t)
;
;         ))






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
      ; Although there is a separate boolean type, any Scheme value can be used as a boolean value for the purpose of a conditional test. As explained in section 6.3.1, all values count as true in such a test except for #f. This report uses the word ``true'' to refer to any Scheme value except #f, and the word ``false'' to refer to #f.
      (define (port? o)
         (eq? (type o) type-port))




      ; 4. Expressions
      ; 4.1 Primitive expression types
      ; 4.1.1 Variable references

      ; 4.1.1  Variable references
;      (define-syntax define
;         (syntax-rules (lambda λ)
;            ((define op a b . c)
;               (define op (begin a b . c)))
;            ((define ((op . args) . more) . body)
;               (define (op . args) (lambda more . body)))
;            ((define (op . args) body)
;               (define op
;                  (letrec ((op (lambda args body))) op)))
;            ((define name (lambda (var ...) . body))
;               (setq name (ol:let (name) ((lambda (var ...) . body)) name)))
;            ((define name (λ (var ...) . body)) ; fasten for (λ) process
;               (setq name (ol:let (name) ((lambda (var ...) . body)) name)))
;            ((define op val)
;               (setq op val))))

      ; 4.1.2 Literal expressions
      ; ...




      ; 6. Standard procedures


      ;; (complex? obj ) procedure
      ;; (real? obj ) procedure
      ;; (rational? obj ) procedure
      ;; (integer? obj ) procedure


      ; 6.3. Other data types
      ; 6.3.1. Booleans
      ;; (not obj) library procedure


      ;
      ;

      ; 6.4 Control features

      ;; force is effectively unnecessary in Ol, so might as well signal a
      ;; warning if this is used, because the code probably assumes
      ;; mutable state.

      (define (force thunk) (thunk))


      ;(assert #t (procedure? car))
      ;(assert #f (procedure? 'car))
      ;(assert #t (procedure? (lambda (x) x)))


      ;; essential procedure: apply proc args
      ;; procedure: apply proc arg1 ... args
      (define apply      (raw type-bytecode '(#x14)))  ;; <- no arity, just call 20

      ;; ...

      ;; procedure: call-with-current-continuation proc

      ; non standard, owl extension
      (define-syntax lets/cc
         (syntax-rules (call/cc)
            ((lets/cc (om . nom) . fail)
               (syntax-error "let/cc: continuation name cannot be " (quote (om . nom))))
            ((lets/cc var . body)
               (call/cc (λ (var) (lets . body))))))

      ; internal, todo: to be created and renamed
      (define sys (raw 16 '(27 4 5 6 7 8  24 8)))

      ; differs from previous by using (equal?) instead of (eq?)
      (define-syntax assert
         (syntax-rules (===>)
            ((assert expression)
               (assert expression ===> #true))
            ((assert expression ===> result)
               (if (equal? expression (quote result)) #true
                  ((raw 16 '(27 4 5 6 7 8 24 8))
                     #null 5 "assertion error: " (cons (quote expression) (cons "must be" (cons (quote result) #null))))))))

      (define (runtime-error reason info) ; todo: move to (owl mcp)?
         (call/cc (λ (resume) (sys resume 5 reason info))))
      (define error runtime-error)
)
; ---------------------------
   (export
      λ syntax-error assert error runtime-error

      if cond case and or not
      letrec letrec* let let* let*-values lets
      begin do
      delay force

      quasiquote
      define ;define*
      list length append reverse
      ilist tuple tuple-case
      call-with-values define-library
      case-lambda
      define-values

      ; список типов
      type-complex
      type-rational
      type-int+
      type-int-

      value? raw? reference?

      type-bytecode
      type-proc
      type-clos
      type-fix+
      type-fix-
      type-pair
      type-vector-dispatch
      type-vector-leaf
      type-vector-raw
      type-ff-black-leaf
      type-tuple
      type-symbol
      type-const
      type-rlist-spine
      type-rlist-node
      type-port
      type-string
      type-string-wide
      type-string-dispatch
      type-thread-state
      type-vptr

      ;; sketching types
      type-ff               ;; k v, k v l, k v l r, black node with children in order
      type-ff-r             ;; k v r, black node, only right black child
      type-ff-red           ;; k v, k v l, k v l r, red node with (black) children in order
      type-ff-red-r         ;; k v r, red node, only right (black) child

      ;; k v, l k v r       -- type-ff
      ;; k v r, k v l r+    -- type-ff-right
      ;; k v l, k v l+ r    -- type-ff-leftc


      apply
      call-with-current-continuation call/cc lets/cc

      ; 3.2.
      boolean? pair? symbol? number? char? string? vector? port? procedure? null? eof?
      ; ol extension:
      bytecode? function? ff?

      map list?

      set-car! set-cdr!
))
