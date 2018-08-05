; minimal set of Scheme (with Ol subset)
(define-library (scheme core)
   (import
      (src vm) ; virtual machine codes and primitives:
               ; vm:new vm:make vm:new-raw-object
               ; cons car cdr ref type size vm:cast vm:raw? set set! eq? less?
               ; vm:add vm:sub vm:mul vm:div vm:shr vm:shl vm:and vm:or vm:xor
               ; clock syscall vm:version vm:maxvalue vm:valuewidth
               ; tuple-apply ff-apply
               ; ff:red ff:black ff:toggle ff:red? ff:right?
               ;
               ; apply apply/cc arity-error
               ; call-with-current-continuation
      
      (scheme case-lambda)  ; case-lambda
      (r5rs srfi-87))       ; <= in cases
   (begin
      ; basic Otus Lisp elements:

      ;; special forms: (declared in lang/env.scm)
      ;
      ; quote values lambda setq
      ; letq ifeq either values-apply
      ;
      ; =================================================================
      ; Scheme
      ;
      ; Revised(7) Report on the Algorithmic Language Scheme
      ;                  Dedicated to the Memory of ALGOL 60
      ;
      ; =================================================================

      ; Programming languages should be designed not by piling feature on
      ; top of feature, but by removing the weaknesses and restrictions
      ; that make additional features appear necessary. Scheme demonstrates
      ; that a very small number of rules for forming expressions, with no
      ; restrictions on how they are composed, suffice to form a practical
      ; and efficient programming language that is flexible enough to support
      ; most of the major programming paradigms in use today.


      ;                      DESCRIPTION OF THE LANGUAGE

      ;;; ---------------------------------------------------------------
      ;;; Chapter 1
      ;;; Overview of Scheme

      ; 1.1  Semantics
      ;
      ; Scheme is a statically scoped programming language. Each
      ; use of a variable is associated with a lexically apparent
      ; binding of that variable.
      ;
      ; Scheme is a dynamically typed language. Types are asso-
      ; ciated with values (also called objects) rather than with
      ; variables. Statically typed languages, by contrast, asso-
      ; ciate types with variables and expressions as well as with
      ; values.
      ;
      ; Scheme has latent ..................
      ; ......

      ; 1.2  Syntax
      ;
      ; Scheme, like most dialects of Lisp, employs .....
      ; ......

      ; 1.3  Notation and terminology
      ;
      ; 1.3.1  Primitive, library, and optional features
      ;
      ; It is required that every implementation of Scheme support
      ; all features that are not marked as being optional. ......
      ; ...

      ; 1.3.2  Error situations and unspecified behavior
      ;
      ; When speaking of an error situation, this report .......
      ; ....

      ; * ol specific
      (setq runtime-error (lambda (reason info)
         (call-with-current-continuation (lambda (resume) (vm:sys resume 5 reason info)))))

      ; 4.3.3. Signaling errors in macro transformers
      ;
      (define-syntax syntax-error
         (syntax-rules (runtime-error)
            ((syntax-error . stuff)
               (runtime-error "syntax error: " (quote stuff)))))

      ; * internal testing staff
      ; note: this is simplified 'assert' that uses 'eq?'. please be careful!
      ; todo: make assert check values recursively!!!
      (define-syntax assert
         (syntax-rules (===>)
            ((assert expression ===> expectation)
               (ifeq ((lambda (x) x) expression) (quote expectation)
                  #true
                  (runtime-error "assertion error:" (cons (quote expression) (cons "must be" (cons (quote expectation) #null))))))))
      
      ; * ol specific
      (setq error runtime-error) ; [yc] is it required?

      ; 1.3.3  Entry format
      ;
      ; Chapters 4 and 6 are organized into entries. Each ....
      ; ........

      ; 1.3.4  Evaluation examples
      ; 
      ; The symbol "==>" used in program examples should be
      ; read "evaluates to." ...
      ; ....

      ; 1.3.5  Naming conventions
      ;
      ; By convention, the names of procedures that always return
      ; a boolean value usually end in “?”. Such procedures are
      ; called predicates.
      ;
      ; By convention, the names of procedures that store values
      ; into previously allocated locations (see section 3.4) usually
      ; end in "!". Such procedures are called mutation procedures.
      ; By convention, the value returned by a mutation
      ; procedure is unspecified.
      ;
      ; By convention, "->"" appears within the names of procedures
      ; that take an object of one type and return an analogous
      ; object of another type. For example, list->vector
      ; takes a list and returns a vector whose elements are the
      ; same as those of the list.


      ;;; ---------------------------------------------------------------
      ;;; Chapter 2
      ;;; Lexical conventions
      ;
      ; This section gives an informal account of some of the lexical
      ; conventions used in writing Scheme programs. For a formal
      ; syntax of Scheme, see section 7.1.
      ;
      ; 2.1  Identifiers
      ;
      ; Most identifiers allowed by other .......
      ; ....
      ;
      ; Here are some examples of identifiers:
      ;
      ; lambda        q
      ; list->vector  soup
      ; +             V17a
      ; <=?           a34kTMNs
      ; the-word-recursion-has-many-meanings
      ;
      ; All implementations of Scheme must support the following
      ; extended identifier characters:
      ;
      ; ! $ % & * + - . / : < = > ? @ ^ _ ~
      ;
      ; Alternatively, an identifier can be represented by a se-
      ; quence of zero or more characters enclosed within vertical
      ; lines (|), analogous to string literals.
      ;
      ; ...................................

      ; 2.2  Whitespace and comments
      ;
      ; Whitespace characters are spaces and newlines. .........
      ; ..........

      ; 2.3  Other notations
      ;
      ; For a description of ........
      ; ..........


      ;;; ---------------------------------------------------------------
      ;;; Chapter 3
      ;;; Basic concepts

      ; 3.1  Variables, syntactic keywords, and regions
      ;
      ; An identifier may name a type of syntax, or ...
      ; ..

      ; 3.2  Disjointness of types
      ;
      ; No object satisfies more than one ...
      ; ..

      ; 3.3  External representations
      ;
      ; An important concept in Scheme (and Lisp) is that ...
      ; ..

      ; 3.4  Storage model
      ; Variables and objects such as pairs, vectors, and ...
      ; .........

      ; 3.5  Proper tail recursion
      ; Implementations of Scheme are required to be ...
      ; ...............


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
      ; syntax:  <variable>
      ;
      ; An expression consisting of a variable (section 3.1) is a
      ; variable reference. The value of the variable reference is
      ; the value stored in the location to which the variable is
      ; bound. It is an error to reference an unbound variable.

      ; 4.1.2  Literal expressions
      ;
      ; syntax:  quote <datum>                * builtin
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

      ;(assert ((lambda x x) 3 4 5 6)                  ===>  (3 4 5 6))
      ;(assert ((lambda (x y . z) z) 3 4 5 6)          ===>  (5 6))

      ; 4.1.5  Conditionals
      ;
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

      (assert (if (less? 2 3) 'yes 'no)               ===>  yes)
      (assert (if (less? 3 2) 'yes 'no)               ===>  no)

      ; syntax:  unless <test> <consequent> <alternate> * ol specific
      ; syntax:  unless <test> <consequent> * ol specific
      (define-syntax unless
         (syntax-rules ()
            ((unless val then)      (if val #false then))
            ((unless val then else) (if val else then))))

      (assert (unless (less? 2 3) 'yes 'no)               ===>  no)
      (assert (unless (less? 3 2) 'yes 'no)               ===>  yes)

      ; 4.1.6  Assignments
      ;
      ; syntax: set! <variable> <expression>  * not supported


      ; 4.2  Derived expression types
      ;
      ; The constructs in this section are hygienic, as discussed
      ; in section 4.3. For reference purposes, section 7.3 gives
      ; macro definitions that will convert most of the constructs
      ; described in this section into the primitive constructs de-
      ; scribed in the previous section.

      ; 4.2.1  Conditionals
      ;
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

      (assert (cond ((less? 2 3) 'greater)
                    ((less? 3 2) 'less))                  ===>  greater)
      (assert (cond ((less? 3 3) 'greater)
                    ((less? 3 3) 'less)
                    (else 'equal))                        ===>  equal)
      (assert (cond ((car (cdr '((a 1) (b 2)))) => car)
                    (else #f))                            ===>  b)

      ; library syntax:  (case <key> <clause1> <clause2> ...)
      (define-syntax case
         (syntax-rules (else eqv? memv =>)
            ((case (op . args) . clauses)
               ((lambda (fresh) ; let ((fresh (op.args)))
                  (case fresh . clauses)) (op . args)))
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
                  ((lambda () . body)) ; means (begin . body)
                  (case thing . clauses)))
            ; http://srfi.schemers.org/srfi-87/srfi-87.html
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

      ;no memv yet to check this asserts
      ;(assert (case 6
      ;          ((2 3 5 7) 'prime)
      ;          ((1 4 6 8 9) 'composite))                 ===> composite)
      ;(assert (case (car '(c d))
      ;          ((a e i o u) 'vowel)
      ;          ((w y) 'semivowel)
      ;          (else 'consonant))                        ===>  consonant)

      ; library syntax:  (and <test1> ...)
      (define-syntax and
         (syntax-rules ()
            ((and) #true)
;            ((and (a . b) . c)
;               ((lambda (x) (and x . c))  (a . b)))
            ((and a) a)
            ((and a . b)
               (if a (and . b) #false))))

      (assert (and (eq? 2 2) (less? 1 2))                 ===> #true)
      (assert (and (eq? 2 2) (less? 2 1))                 ===> #false)
      (assert (and 1 2 '(f g) 'c)                         ===> c)
      (assert (and)                                       ===> #true)

      ; library syntax:  (or <test1> ...)
      (define-syntax or
         (syntax-rules ()
            ((or) #false)
            ((or (a . b) . c)
               ((lambda (x) (or x . c))  (a . b)))
            ((or a) a)
            ((or a . b)
               (if a a (or . b)))))

      (assert (or (eq? 2 2) (less? 1 2))                  ===> #true)
      (assert (or (eq? 2 2) (less? 2 1))                  ===> #true)
      (assert (or #f #f #f)                               ===> #false)
      (assert (or #f 'c #f)                               ===> c)

      ; 4.2.2  Binding constructs
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

      ; library syntax:  (letrec <bindings> <body>)
      (define-syntax letrec
         (syntax-rules ()
            ((letrec ((?var ?val) ...) ?body) (letq (?var ...) (?val ...) ?body))
            ((letrec vars body ...) (letrec vars (begin body ...)))))

      ; library syntax:  (letrec* ...) - r7rs
      ;(define-syntax letrec*
      ;   (syntax-rules ()
      ;      ((letrec () . body)
      ;         (begin . body))
      ;      ((letrec* ((var val) . rest) . body)
      ;         (letrec ((var val))
      ;            (letrec* rest . body)))))

      ; library syntax:  (let <bindings> <body>)
      ;                  (let keyword <bindings> <body>) named let, from 4.2.4 Iteration
      (define-syntax let
         (syntax-rules ()
            ((let ((var val) ...) exp . rest)
               ((lambda (var ...) exp . rest) val ...))
               ;why not (letq (var ...) (val ...) exp . rest)) ???
            ((let keyword ((var init) ...) exp . rest)
               (letrec ((keyword (lambda (var ...) exp . rest))) (keyword init ...)))))

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

      ; 4.2.3  Sequencing
      ;
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

      ; 4.2.4  Iteration       * moved to (scheme r5rs iteration)

      ; 4.2.5  Delayed evaluation
      ;
      ; library syntax:  delay <expression>
      (define-syntax delay
         (syntax-rules ()
            ((delay (op . args))
               (lambda () (op . args)))
            ((delay value) value)))

      ; 4.2.6  Quasiquotation
      ;
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


      ; 4.3  Macros
      ;
      ; Scheme programs can define and use .......
      ; ......

      ; 4.3.1  Binding constructs for syntactic keywords
      ;
      ; syntax: (let-syntax ...
      ; syntax: (letrec-syntax ...

      ; 4.3.2  Pattern language
      ;
      ; (syntax-rules


      ;;; ---------------------------------------------------------------
      ;;; Chapter 5
      ;;; Program structure

      ; 5.1  Programs
      ; A Scheme program consists of a sequence ........
      ; ........

      ; 5.2  Definitions
      ;
      ; Definitions are valid in some, but not all, contexts .......
      ; ...........

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
               (setq name (letq (name) ((lambda (var ...) . body)) name)))
            ((define op val)
               (setq op val))))

;      ;; not defining directly because letq doesn't yet do variable arity
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
               (values-apply gen
                  (lambda (var ...) (let*-values rest . body))))
            ((let*-values () . rest)
               (begin . rest))))

      ; 5.2.1  Top level definitions
      ;
      ; At the top level of a program ........
      ; ......

      ; 5.2.2  Internal definitions
      ; 
      ; Definitions may occur at the beginning .......
      ; ...........
      

      ; 5.3  Syntax definitions
      ;
      ; Syntax definitions are valid only at the top .......
      ; ......
      ;
      ; (define-syntax <keyword> <transformer spec>)


      ;;; ---------------------------------------------------------------
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

      ; 6.1  Equivalence predicates
      ;
      ; A predicate is a procedure that always returns .......
      ; ...........

      ; procedure:  (eqv? obj1 obj2)  * in (owl equal) (todo: update)

      ; procedure:  (eq? obj1 obj2)   * builtin
      
      ; library procedure:  (equal? obj1 obj2)  * in (owl equal) (todo: update)

      
      ; 6.2  Numbers
      ; 
      ; Numerical computation has traditionally been .......
      ; .........

      ; This data types related to olvm
      ;     - not a part of r5rs -
      ; todo: move to (src vm) ?
      (define type-fix+             TFIX+)
      (define type-fix-             TFIX-)
      (define type-int+             TINT+)
      (define type-int-             TINT-)
      (define type-rational         TRATIONAL)
      (define type-complex          TCOMPLEX)
      (define type-inexact          TINEXACT)

      ; 6.2.1  Numerical types
      ;
      ; Mathematically, numbers may be arranged ........
      ; ..............

      ; 6.2.2  Exactness
      ;
      ; Scheme numbers are either exact or inexact. .......
      ; ......
      ;
      ; note: implementation of inexact numbers in Otus Lisp are not finished
      ;       and in progress

      ; 6.2.3  Implementation restrictions
      ;
      ; Implementations of Scheme are not required to .........
      ; ...............

      ; 6.2.4  Syntax of numerical constants
      ;
      ; The syntax of the written representations ........
      ; ... 

      ; 6.2.5  Numerical operations
      ;
      ; The reader is referred to section .......
      ; ...
      ; todo: move fully to (scheme r5rs numerical)

      ; procedure:  (integer? obj)
      (define (integer? a)
         (case (type a)
            (type-fix+ #true)
            (type-fix- #true)
            (type-int+ #true)
            (type-int- #true)))

      ; procedure:  (rational? obj)
      (define (rational? a)
      (or
         (integer? a)
         (eq? (type a) type-rational)))

      ; procedure:  (inexact? z)
      (define (inexact? a)
         (eq? (type a) type-inexact))

      ; procedure:  (real? obj)
      (define (real? a)
      (or
         (rational? a)
         (inexact? a)))

      ; procedure:  (complex? obj)
      (define (complex? a)
      (or
         (real? a)
         (eq? (type a) type-complex)))

      ; procedure:  (exact? z)
      (define (exact? a)
         (real? a))

      ; procedure:  (number? obj)
      (define (number? a)
         (complex? a))


      (assert (complex? 3+4i)                        ===>  #t)
      (assert (complex? 3)                           ===>  #t)
      (assert (real? 3)                              ===>  #t)
      (assert (real? -2.5+0.0i)                      ===>  #t)
      (assert (real? 1e10)                           ===>  #t)
      (assert (rational? 6/10)                       ===>  #t)
      (assert (rational? 6/3)                        ===>  #t)
      (assert (integer? 3+0i)                        ===>  #t)
      (assert (integer? 3.0)                         ===>  #t)
      (assert (integer? 8/4)                         ===>  #t)

      ; *** declared in (r5rs math), (r5rs math-extra)
      ; library procedure:  (zero? z)
      (define (zero? x) (eq? x 0))
      ; library procedure:  (positive? x)
      ; library procedure:  (negative? x)
      ; library procedure:  (odd? n)
      ; library procedure:  (even? n)
      ; library procedure:  (max x1 x2 ...)
      ; library procedure:  (min x1 x2 ...)
      ; procedure:  (= z1 z2 z3 ...)
      ; procedure:  (< x1 x2 x3 ...)
      ; procedure:  (> x1 x2 x3 ...)
      ; procedure:  (<= x1 x2 x3 ...)
      ; procedure:  (>= x1 x2 x3 ...)
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
      (define (exact->inexact n) (vm:cast n type-inexact))

      ; procedure: inexact->exact z
      (define (inexact->exact n) (vm:cast n type-rational))

      ; 6.2.6  Numerical input and output
      ;
      ; procedure:  (number->string z) <- (r5rs strings?)
      ; procedure:  (number->string z radix) <- (r5rs strings?)
;      (define (number->string n base)
;         (list->string (render-number n null base)))

      ; procedure:  (string->number string) <- (lang s-exp?)
      ; procedure:  (string->number string radix) <- (lang s-exp?)
;      (define string->number (case-lambda
;         ((str base) (list->number (string->list str) base))
;         (str        (list->number (string->list str) 10))))


      ; 6.3  Other data types
      ;
      ; This section describes operations on some of Scheme's non-
      ; numeric data types: booleans, pairs, lists, symbols, char-
      ; acters, strings and vectors.
      ;
      ; -------------------------------
      ; This data types related to olvm
      ;     - not a part of r5rs -
      (define type-pair              TPAIR)   ; reference
      (define type-tuple             TTUPLE)  ; reference
      (define type-string            TSTRING) ; reference, raw -> 35 (#b100000 + 3)?
      (define type-symbol            TSYMBOL) ; reference
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

      (define type-bytecode         TBYTECODE)  ; reference, raw bytecode
      (define type-proc             TPROCEDURE) ; reference, pure function
      (define type-clos             TCLOSURE)   ; reference, function with closure(s)

      (define type-vector-raw       19) ; reference, raw     ; see also TBVEC in c/ovm.c
      (define type-bytevector       19) ; reference, raw     ; same as type-vector-raw

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

      ; 6.3.1  Booleans
      ;
      ; Of all the standard Scheme values, only #f counts as false in conditional expressions.
      ; Except for #f, all standard Scheme values, including #t, pairs, the empty list, symbols,
      ; numbers, strings, vectors, and procedures, count as true.
      ;      Note: Programmers accustomed to other dialects of Lisp should be aware that Scheme
      ;            distinguishes both #f and the empty list from the symbol nil.
      ; Boolean constants evaluate to themselves, so they do not need to be quoted in programs.
      (assert #t                                ===>  #t)
      (assert #f                                ===>  #f)
      (assert (quote #f)                        ===>  #f)


      ; library procedure:  (not obj)
      (define (not x)
         (if x #false #true))

      (assert (not #t)                          ===>  #f)
      (assert (not 3)                           ===>  #f)
      (assert (not '(3 . 0))                    ===>  #f)
      (assert (not #f)                          ===>  #t)
      (assert (not '())                         ===>  #f)
      (assert (not cons)                        ===>  #f)
      (assert (not 'nil)                        ===>  #f)


      ; library procedure:  (boolean? obj)
      (define (boolean? o)
         (cond
            ((eq? o #true) #true)
            ((eq? o #false) #true)
            (else #false)))

      (assert (boolean? #f)                          ===>  #t)
      (assert (boolean? 0)                           ===>  #f)
      (assert (boolean? '())                         ===>  #f)

      ; todo: Оставить здесь только самые базово необходимые вещи, все остальное переместить в:
      ;   6.3.2 -> (scheme r5rs lists)
      ;   6.3.3 -> (scheme r5rs symbols)
      ;   6.3.4 -> (scheme r5rs characters)
      ;   6.3.5 -> (scheme r5rs strings)
      ;   6.3.6 -> (scheme r5rs vectors)

      ; 6.3.2  Pairs and lists
      ; A pair (sometimes called a dotted pair) is a record ......
      ; ......

      ; procedure:  (pair? obj)
      (define (pair? o)
         (eq? (type o) type-pair))

      (assert (pair? '(a . b))                       ===>  #t)
      (assert (pair? '(a b c))                       ===>  #t)
      (assert (pair? '())                            ===>  #f)
      (assert (pair? '#(a b))                        ===>  #f)

      ; procedure:  (cons obj1 obj2)    * builtin
      (define cons cons)

      ; procedure:  (car pair)          * builtin
      (define car car)

      ; procedure:  (cdr pair)          * builtin
      (define cdr cdr)

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
      ;  olvm notes: always returning fixnum, so can be checked by eq?, not =
      (define (length l)
         (let loop ((n 0) (l l))
            (if (null? l)
               n
               (values-apply (vm:add n 1) ; use internal vm math, not math library
                  (lambda (n carry) ; theoretically impossible case: (if carry (runtime-error "Too long list to fit in fixnum"))
                     (loop n (cdr l)))))))

      (assert (length '(a b c))                      ===>  3)
      (assert (length '(a (b) (c d e)))              ===>  3)
      (assert (length '())                           ===>  0)

      ; library procedure:  (append list ...)
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

      ; library procedure:  (list-tail list k)   * todo

      ; library procedure:  (list-ref list k)
      (define (list-ref lst pos)
         (cond
            ((null? lst) #false) ; temporary instead of (syntax-error "lref: out of list" pos))
            ((eq? pos 0) (car lst))   ; use internal vm math, not math library
            (else (list-ref (cdr lst) (values-apply (vm:sub pos 1) (lambda (n carry) n))))))

      ; library procedure:  (memq obj list)   * todo
      ; library procedure:  (memv obj list)
      ; library procedure:  (member obj list)

      ; library procedure:  (assq obj alist)
      ; library procedure:  (assv obj alist)
      ; library procedure:  (assoc obj alist)

      ; 6.3.3  Symbols
      ;
      ; Symbols are objects whose usefulness rests on the fact .......
      ; .......

      ; procedure:  (symbol? obj)
      (define (symbol? o)
         (eq? (type o) type-symbol))

      ; procedure:  (symbol->string symbol) *tbd
      ; procedure:  (string->symbol string) *tbd

      ; 6.3.4  Characters   * moved to (r5rs characters)
      ; Characters are objects that represent printed characters such as letters and digits.
      ; Characters are written using the notation #\<character> or #\<character name>.
      ;
      ; procedure:  (char? obj)
      ; procedure:  (char=? char1 char2)
      ; procedure:  (char<? char1 char2)
      ; procedure:  (char>? char1 char2)
      ; procedure:  (char<=? char1 char2)
      ; procedure:  (char>=? char1 char2)
      ; library procedure:  (char-ci=? char1 char2)
      ; library procedure:  (char-ci<? char1 char2)
      ; library procedure:  (char-ci>? char1 char2)
      ; library procedure:  (char-ci<=? char1 char2)
      ; library procedure:  (char-ci>=? char1 char2)
      ; library procedure:  (char-alphabetic? char)
      ; library procedure:  (char-numeric? char)
      ; library procedure:  (char-whitespace? char)
      ; library procedure:  (char-upper-case? letter)
      ; library procedure:  (char-lower-case? letter)
      ; procedure:  (char->integer char)
      ; procedure:  (integer->char n)
      ; library procedure:  (char-upcase char)
      ; library procedure:  (char-downcase char)


      ; 6.3.5. Strings
      ; Strings are sequences of characters. Strings are written as sequences of characters enclosed
      ; within doublequotes (`"'). A doublequote can be written inside a string only by escaping it
      ; with a backslash (\), as in "The word \"recursion\" has many meanings."
      ;
      ; procedure:  (string? obj)

      ; procedure:  (make-string k)
      ; procedure:  (make-string k char)
;      (define make-string
;         (case-lambda
;            ((k) (vm:make-blob type-string k))
;            ((k char) (app a (app b (appl cs appl) app) app))
;            ((a) a)
;            (() '()))))
;         ()
;         (list->string (repeat char n)))

      ; library procedure:  (string char ...)
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
      ; library procedure:  (string=? string1 string2)
      ; library procedure:  (string-ci=? string1 string2)
      ; library procedure:  (string<? string1 string2)
      ; library procedure:  (string>? string1 string2)
      ; library procedure:  (string<=? string1 string2)
      ; library procedure:  (string>=? string1 string2)
      ; library procedure:  (string-ci<? string1 string2)
      ; library procedure:  (string-ci>? string1 string2)
      ; library procedure:  (string-ci<=? string1 string2)
      ; library procedure:  (string-ci>=? string1 string2)
      ; library procedure:  (substring string start end)
      ; library procedure:  (string-append string ...)
      ; library procedure:  (string->list string)
      ; library procedure:  (list->string list)
      ; library procedure:  (string-copy string)
      ; library procedure:  (string-fill! string char)



      ; (r7rs) 6.8  Vectors
      ; moved to (scheme vector)

      ;; *********************
      ;; 6.4  Control features
      ;
      ; This chapter describes various primitive procedures which control the flow of program
      ; execution in special ways. The procedure? predicate is also described here.

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

      (assert (procedure? car)                    ===> #t)
      (assert (procedure? 'car)                   ===> #f)

      ; procedure:  (apply proc arg1 ... args)  * builtin
      (define apply apply)

      ; library procedure:  (map proc list1 list2 ...)
      ; The dynamic order in which proc is applied to the elements of the lists is unspecified.
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
      (define map
         (let ((map1 (lambda (f a)
                        (let loop ((a a))
                           (if (null? a)
                              #null
                              (cons (f (car a)) (loop (cdr a))))))))

      (case-lambda
         ((f a)      (map1 f a))
         ((f a b)    (let loop ((a a)(b b)) ; map2
                        (if (null? a)
                           #null
                           (cons (f (car a) (car b)) (loop (cdr a) (cdr b))))))
         ((f a b . c) ; mapN
                     (let loop ((args (cons a (cons b c))))
                        (if (null? (car args)) ; закончились
                           #null
                           (cons (apply f (map1 car args)) (loop (map1 cdr args))))))

;         ((f a b c) (let loop ((a a)(b b)(c c))
;                        (if (null? a)
;                           #null
;                           (cons (f (car a) (car b) (car c)) (loop (cdr a) (cdr b) (cdr c))))))
         (() #f))))

       ; library procedure:  (for-each proc list1 list2 ...)
       ; library procedure:  (force promise)

      ; procedure:  (call-with-current-continuation proc)
      (define call-with-current-continuation
         ('_sans_cps (lambda (k f)
                        ;(f k (lambda (c . x) (apply/cc k x))))))
                        ; speeduped version:
                        (f k (case-lambda
                           ((c a) (k a))
                           ((c a b) (k a b))
                           ((c . x) (apply/cc k x)))))))

      (define call/cc call-with-current-continuation)

      ; procedure:  (values obj ...)          * builtin /special

      ; procedure:  (call-with-values producer consumer)
      (define-syntax call-with-values
         (syntax-rules ()
            ((call-with-values (lambda () exp) (lambda (arg ...) body))
               (values-apply exp (lambda (arg ...) body)))
            ((call-with-values thunk (lambda (arg ...) body))
               (values-apply (thunk) (lambda (arg ...) body)))))

      ; procedure:  (dynamic-wind before thunk after)
      ;

      ;; 6.5  Eval
      ; ...
      ; procedure:  (eval expression environment-specifier)  * (lang eval)
      ; procedure:  (scheme-report-environment version)      * (lang eval)
      ; procedure:  (null-environment version)               * (lang eval)
      ; optional procedure:  (interaction-environment)       * (lang eval)

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
               (vm:new type-tuple a . bs))))

      ; replace this with typed destructuring compare later on

      (define-syntax tuple-case
         (syntax-rules (else _ is eq? div)
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
;      (define (i x) x)
;      (define (k x y) x)


      ;; these are core data structure type tags which are fixed and some also relied on by the vm

      ;; ALLOCATED


      ;;           allocated/pointers     allocated/rawdata    immediate
      ;; (size  x)         n                       n               #false

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
;               (setq name (letq (name) ((lambda (var ...) . body)) name)))
;            ((define name (λ (var ...) . body)) ; fasten for (λ) process
;               (setq name (letq (name) ((lambda (var ...) . body)) name)))
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

      ;; ...

      ;; procedure: call-with-current-continuation proc

      ; non standard, owl extension
      (define-syntax lets/cc
         (syntax-rules (call/cc)
            ((lets/cc (om . nom) . fail)
               (syntax-error "let/cc: continuation name cannot be " (quote (om . nom))))
            ((lets/cc var . body)
               (call/cc (λ (var) (lets . body))))))

      ; real assert. differs from temporary by using equal? instead of eq?
      (define-syntax assert
         (syntax-rules (===>)
            ((assert expression ===> result)
               (if (not (equal? expression (quote result)))
                  (vm:sys #false 5 "assertion error: " (list (quote expression) "must be" (quote result)))))))

      ;; used syscalls
      (define (exec function . args) (syscall 59 function args #f))
      (define (yield)                (syscall 24 #t #false #false))

      (define (halt n)               (syscall 60 n n n))

      ;; stop the vm *immediately* without flushing input or anything else with return value n
      ;; make thread sleep for a few thread scheduler rounds
      (define (set-ticker-value n) (syscall 1022 n #false #false))
      (define (wait n) ; is it required?
         (if (eq? n 0)
            0
            (let* ((n _ (vm:sub n 1)))
               (set-ticker-value 0)
               (wait n))))


      ;; special things exposed by the vm
      (define (set-memory-limit n) (syscall 12 n #f #f))
      (define (get-word-size)      (syscall 1008 #false #false #false))
      (define (get-memory-limit)   (syscall 12 #f #f #f))
)
; ---------------------------
   (export
      λ ; ol extension
      syntax-error assert runtime-error error

      ; 
      if unless cond case and or
      letrec let let*            lets ; lets - ol specific, let*-values - r7rs
      begin                      ;do ; todo: move do to (scheme r5rs iteration)
      delay force

      quasiquote
      define ;define*
      list length append reverse
      ilist tuple tuple-case
      call-with-values define-library
      (exports (scheme case-lambda)) ;case-lambda
      (exports (r5rs srfi-87)) ;=> in cases
      define-values ; ol specific

      ; 6.2 (numbers)
      type-fix+
      type-fix-
      type-int+
      type-int-
      type-rational
      type-complex
      type-inexact

      integer? rational? complex? real? number? exact? inexact?
      exact->inexact inexact->exact

      ; 6.3 (other data types)
      type-bytecode
      type-proc
      type-clos
      type-pair
      type-vector-dispatch
      type-vector-leaf
      type-vector-raw
      type-bytevector
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

      ; 6.3
      not boolean? pair? symbol? port? procedure? null? eof?

      value? reference?
      zero?

      ; 6.3.2 (pairs and lists)
      list-ref

      ; ol extension:
      bytecode? function? ff?

      map list?

      exec yield
      halt wait
      set-ticker-value
      set-memory-limit get-word-size get-memory-limit

      set-car! set-cdr!
))
