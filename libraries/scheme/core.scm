; minimal set of Scheme (with Ol subset)
(define-library (scheme core)
   (version 2.0)
   (license MIT/LGPL3)
   (keywords (scheme core ol))
   (description "
      Core Otus-Lisp Scheme library.")

   (import
      (src vm) ;; Codes and primitives of the Otus Lisp Virtual Machine that
               ;;  do not need to be exported by libraries.
               ;
               ; object creation/modification:
               ;   vm:new vm:make vm:makeb vm:cast
               ;   cons car cdr ref type size set-ref set-ref! eq? less?
               ; basic math primitives:
               ;   integer:
               ;     vm:add vm:sub vm:mul vm:div vm:shr vm:shl
               ;   floating-point (if built with OLVM_INEXACTS)
               ;     vm:fp1 (xFE: fsin, xFF: fcos),
               ;     vm:fp2 (xD9: <, xC1: +, xE9: -, xC9: *, xF9: /)
               ;   binary:
               ;     vm:and vm:or vm:xor
               ; special:
               ;   vm:pin, vm:unpin, vm:deref
               ;   clock, syscall
               ; vm info:
               ;   vm:version vm:maxvalue vm:valuewidth vm:features
               ; associative arrays support:
               ;   ff:red ff:black ff:toggle ff:red? ff:right?
               ; execution flow:
               ;   ff-apply tuple-apply
               ;
               ; exported functions:
               ;   apply apply/cc arity-error
               ;   call-with-current-continuation
               ;
               ;; special forms: (declared in lang/env.scm)
               ;
               ; quote values lambda setq
               ; letq ifeq either values-apply
               ;

      (scheme srfi-16)   ; case-lambda
      (scheme srfi-87)   ; <= in cases
      (scheme srfi-71))  ; (let* ((a b (values..

   ; -----------------------------------------------------------------
   ; internal staff
   (begin
      ; internal: (-1 obj), (+1 obj)
      ; note: todo: add theoretically impossible case:
      ;       (if carry (runtime-error "Too long list to fit in fixnum"))

      (setq |-1| (lambda (n) ; * internal
         (values-apply (vm:sub n 1) (lambda (n carry) n))))
      (setq |+1| (lambda (n) ; * internal
         (values-apply (vm:add n 1) (lambda (n carry) n))))
      (setq |0.0| (vm:fp2 #xC9 42 0)) ; * internal

      ; * ol specific: (runtime-error reason info)
      (setq runtime-error (lambda (reason info)
         (call-with-current-continuation (lambda (resume) (vm:sys resume 5 reason info)))))

      ; * temporary, allows basic assert implementation
      (setq equal? (lambda (a b)
         (ifeq a b #true #false)))

      ; * internal automation testing staff
      ; note: please be careful!
      ;       this is simplified 'assert' that uses 'eq?' before real 'equal?' be implemented in chapter 6
      (define-syntax assert
         (syntax-rules (===>)
            ((assert expression ===> expectation)
               (ifeq (equal? ((lambda (x) x) expression) (quote expectation)) #true
                  #true
                  (runtime-error "assertion error:" (cons (quote expression) (cons "must be" (cons (quote expectation) #null))))))))
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
      ;
      ;
      ;                      DESCRIPTION OF THE LANGUAGE
      ;
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

      ; 4.3.3. Signaling errors in macro transformers
      ;
      (define-syntax syntax-error
         (syntax-rules (runtime-error)
            ((syntax-error . staff)
               (runtime-error "syntax error: " (quote staff)))))

      ; * ol specific
      (setq error runtime-error) ; [yc] is it required?

      ; * ol some warnings
      (setq error:please-import-langintern (lambda ()
         (runtime-error "Please, import (lang intern) to get the function.")))

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
            ((if (null? t) then else) (if (eq? t #null) then else))  ; boot image size and compilation speed optimization
            ((if (a . b)   then else) (letq (x) ((a . b)) (if x then else)))
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
      (setq set! (lambda (variable expression)
         (runtime-error "No set! is allowed." "(sometimes you can use set-ref!, check the docs.)")))

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

      ; library syntax:  (and <test1> ...)
      (define-syntax and
         (syntax-rules ()
            ((and) #true)
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
            ((or (a . b) . c) ; additional optimization
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

      ; library syntax:  delay-force <expression>
      ; todo.

      ; library syntax:  force <expression>
      (setq force (lambda (thunk) (thunk)))

      ; (promise? obj )
      ; todo.

      ; (make-promise obj )
      ; todo.


      ; 4.2.6. Dynamic bindings
      ; * declared in (scheme dynamic-bindings)

      ; 4.2.7. Exception handling
      ; todo: * declared in (scheme exceptions)
      ; todo: guard, raise

      ; 4.2.8. Quasiquotation
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

      ; 4.2.9. Case-lambda
      ; * declared in (scheme srfi-16)

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
            ((define ((name . args) . more) . body)
               (define (name . args) (lambda more . body)))
            ((define (name . args) . body)
               (setq name (letq (name) ((lambda args . body)) name)))
            ((define name (lambda (var ...) . body))
               (setq name (letq (name) ((lambda (var ...) . body)) name)))
            ((define name val)
               (setq name val))
            ((define name a b . c)
               (define name (begin a b . c)))))

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

      ; * ol specific
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
      ; A predicate is a procedure that always returns a boolean
      ; value (#t or #f). An equivalence predicate is the compu-
      ; tational analogue of a mathematical equivalence relation;
      ; it is symmetric, reflexive, and transitive. Of the equiva-
      ; lence predicates described in this section, eq? is the finest
      ; or most discriminating, equal? is the coarsest, and eqv?
      ; is slightly less discriminating than eq?.

      ; (r7rs) procedure:  (eq? obj1 obj2)   * builtin
      (define eq? eq?)

      ; (r7rs) procedure:  (eqv? obj1 obj2)
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
         (if (or (eq? typea TINT+)
                 (eq? typea TINT-)
                 (eq? typea TRATIONAL)
                 (eq? typea TCOMPLEX))
            (and
               (eq? (type a) (type b))
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
               (eq? (type a) (type b))
               (let loop ((n (|-1| (size a))))
                  (if (eq? (ref a n) (ref b n))
                     (if (eq? n 0)
                        #true
                        (loop (|-1| n)))))))))))

      (assert (eqv? 'a 'a)                  ===> #true) ; symbols
      (assert (eqv? '() '())                ===> #true) ; empty lists
      (assert (eqv? 2 2)                    ===> #true) ; atomic integer numbers
      (assert (eqv? 72057594037927936
                    72057594037927936)      ===> #true) ; long (not atomic) integer numbers
      (assert (eqv? 0.33 0.33)              ===> #true) ; rational numbers
      (assert (eqv? 7/3 14/6)               ===> #true) ; rational numbers
      (assert (eqv? 2+3i 2+3i)              ===> #true) ; complex numbers
      (assert (eqv? (vm:cast 2 TINEXACT)
                    (vm:cast 2 TINEXACT))   ===> #true) ; inexact numbers
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
      (assert (eqv? 2 (vm:cast 2 TINEXACT)) ===> #false)

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
      (assert (eqv? (vm:cast 2 TINEXACT)
                    (vm:cast 3 TINEXACT))   ===> #false)
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

      (assert (eqv? #() #())              ===> #true)  ; * ol specific, (in r7rs unspecified)
      (assert (eqv? (lambda (x) x)
                    (lambda (x) x))         ===> #true)  ; * ol specific, (in r7rs unspecified), depends on (lang assemble)
      ;assert (eqv? (lambda (x) x)
      ;             (lambda (y) y))         ===> #true)  ; * ol specific, (in r7rs unspecified), depends on (lang assemble)
      ;assert (eqv? 1.0e0 1.0f0)            ===> unspecified
      (assert (eqv? +nan.0 +nan.0)          ===> #true)  ; * ol specific, (in r7rs unspecified)

      ;(assert (eqv? '(a) '(a))              ===> #false) ; * ol specific, (in r7rs unspecified)
      ;assert (eqv? "a" "a")                ===> unspecified
      ;assert (eqv? '(b) (cdr '(a b)))      ===> unspecified
      ;assert (letrec ((f (lambda () (if (eqv? f g) ’both ’f)))
      ;                (g (lambda () (if (eqv? f g) ’both ’g))))
      ;          (eqv? f g))                ===> unspecified


      ; (r7rs) procedure:  (equal? obj1 obj2)
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
               (if (eq? 0 sa)
                  #true
               ; (> sa 0)
               (if (ref a 0) ; ==(blob? a), 0 in ref works only for blobs
                  ; comparing blobs
                  (let loop ((n (|-1| sa)))
                     (if (eq? (ref a n) (ref b n))
                        (if (eq? n 0)
                           #true
                           (loop (|-1| n)))))
                  ; recursive comparing objects
                  (let loop ((n sa))
                     (if (eq? n 0)
                        #true
                        (if (equal? (ref a n) (ref b n))
                           (loop (|-1| n)))))))))))


      ; 6.2  Numbers
      ;
      ; Numerical computation has traditionally been .......
      ; .........

      ; This data types related to olvm
      ;     - not a part of r7rs -
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

      ; (r7rs) 6.2.6  Numerical operations
      ;
      ; These numerical type predicates can be applied to any kind
      ; of argument, including non-numbers. They return #t if the
      ; object is of the named type, and otherwise they return #f.
      ; In general, if a type predicate is true of a number then
      ; all higher type predicates are also true of that number.
      ; Consequently, if a type predicate is false of a number, then
      ; all lower type predicates are also false of that number.

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

      ; procedure:  (real? obj)
      (define (real? a)
      (or
         (rational? a)
         (eq? (type a) type-inexact)
      ;  (and (eq? (type a) type-complex) (zero? (cdr a)) ; not required, cause math lib casts such complex numbers to real
         (equal? a +inf.0)
         (equal? a -inf.0)
         (equal? a +nan.0)))

      ; procedure:  (complex? obj)
      (define (complex? a)
      (or
         (real? a)
         (eq? (type a) type-complex)))

      ; Note: In many implementations the complex? procedure will
      ; be the same as number?, but unusual implementations may rep-
      ; resent some irrational numbers exactly or may extend the num-
      ; ber system to support some kind of non-complex numbers.
      ;
      ; procedure:  (number? obj)
      (define (number? a)
         (complex? a))


      ; procedure:  (exact? z)
      (define (exact? a)
         (rational? a))

      ; procedure:  (inexact? z)
      (define (inexact? a)
         (and (complex? a)
            (eq? (rational? a) #f))) ; not rational

      ; procedure:  (exact-integer? z)
      (define (exact-integer? z)
         (integer? z))



      (assert (complex? 3+4i)                        ===>  #t)
      (assert (complex? 3)                           ===>  #t)
      (assert (complex? 3.4)                         ===>  #t)
      (assert (real? 3)                              ===>  #t)
      (assert (real? 3.4)                            ===>  #t)
      (assert (real? -2.5+0.0i)                      ===>  #t)
      (assert (real? 1e10)                           ===>  #t)
      (assert (rational? 6/10)                       ===>  #t)
      (assert (rational? 6/3)                        ===>  #t)
      (assert (rational? 3+4i)                       ===>  #f)
      (assert (integer? 3+0i)                        ===>  #t)
      (assert (integer? 3+4i)                        ===>  #f)
      (assert (integer? 3.0)                         ===>  #t)
      (assert (integer? 3.4)                         ===>  #f)
      (assert (integer? 8/4)                         ===>  #t)
      (assert (integer? 8/5)                         ===>  #f)

      ; *** declared in (r5rs math), (r5rs math-extra)
      ; library procedure:  (zero? z)
      (define (zero? x)
      (or
         (eq? x 0)
         (equal? x |0.0|))) ; supports inexact numbers

      (assert (zero? 0)                              ===>  #t)
      (assert (zero? 4)                              ===>  #f)
      (assert (zero? (vm:fp2 #xE9 7 7))              ===>  #t)
      (assert (zero? (vm:fp2 #xC1 7 7))              ===>  #f)

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
      (define (inexact n) (vm:cast n type-inexact))
      (define exact->inexact inexact) ; r5rs

      ; procedure: inexact->exact z
      (define (exact n) (vm:cast n type-rational))
      (define inexact->exact exact)   ; r5rs

      ; 6.2.7  Numerical input and output
      ; * included in (scheme numerical-io)


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
      (define type-string            TSTRING) ; reference, blob -> 35 (#b100000 + 3)?
      (define type-symbol            TSYMBOL) ; reference
      ; 5   TODO(?): (define type-string-wide      5) ; reference, blob
      ; 6
      ; 7
      ;(define type-ff-black-leaf     8) ; reference ; TODO: move to 28
      ; 9

      (define type-rlist-spine      10) ; reference
      (define type-vector-leaf      11) ; reference

      (define type-port             12) ; value
      (define type-const            13) ; value

      (define type-rlist-node       14) ; reference
      (define type-vector-dispatch  15) ; reference

      (define type-bytecode         TBYTECODE)  ; reference, blob bytecode
      (define type-proc             TPROCEDURE) ; reference, pure function
      (define type-clos             TCLOSURE)   ; reference, function with closure(s)
      (define type-bytevector       19) ; reference, blob

      ; 20
      (define type-string-dispatch  21) ; reference
      (define type-string-wide      22) ; reference
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
      (define type-vptr             49) ; reference,  blob

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

      ; (r7rs) 6.4  Pairs and lists
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

      ; cxr library procedure:  (caaaar pair)  <- (scheme cxr)
      ; cxr library procedure:  (caaadr pair)  <- (scheme cxr)
      ; ...
      ; cxr library procedure:  (cdddar pair) <- (scheme cxr)
      ; cxr library procedure:  (cddddr pair) <- (scheme cxr)

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
         (let ((make (lambda (n fill)
                        (let loop ((n n) (out '()))
                           (if (eq? n 0)
                              out
                              (loop (|-1| n) (cons fill out)))))))
         (case-lambda
            ((k)
               (make k #false))
            ((k fill)
               (make k fill)))))


      ; procedure:  (list obj ...)
      (define-syntax list
         (syntax-rules ()
            ((list) '())
            ((list a . b)
               (cons a (list . b)))))

      ; very interesting question:
      ;  why macro, not a function?
      ; answer:
      ;  the function unwraps to LD/LD/LD/LD/LD/MOV2/MOV2/MOV2/GOTO/JAFX sequnce,
      ;  but macro unwraps to LD/CONS/LD/CONS/LD/CONS. So, we have a goot speedup in case of
      ;  list as macro.

      ; procedure:  (length list)
      ;  olvm notes: always returning fixnum, so can be checked by eq?, not only =
      (define (length l)
         (let loop ((n 0) (l l))
            (if (null? l)
               n
               (loop (|+1| n) (cdr l)))))

      (assert (length '(a b c))                      ===>  3)
      (assert (length '(a (b) (c d e)))              ===>  3)
      (assert (length '())                           ===>  0)

      ; procedure:  (append list ...)
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
      ; todo: asserts!
      ;; (append ’(x) ’(y))
      ;; (append ’(a) ’(b c d))
      ;; (append ’(a (b)) ’((c))) =⇒
      ;; =⇒
      ;; =⇒ (x y)
      ;; (a b c d)
      ;; (a (b) (c))
      ;; (append ’(a b) ’(c . d))
      ;; (append ’() ’a) =⇒
      ;; =⇒ (a b c . d)
      ;; a

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
            (list-tail (cdr list) (|-1| k))))

      ; procedure:  (list-ref list k)
      ;
      ; The list argument can be circular, but it is an error if list has
      ; k or fewer elements.
      (define (list-ref list k)
         (cond
            ((null? list) #false) ; temporary instead of (syntax-error "lref: out of list" pos))
            ((eq? k 0) (car list))   ; use internal vm math, not math library
            (else (list-ref (cdr list) (|-1| k)))))

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
                              (unless (comparer obj (car list))
                                 (f obj (cdr list))
                                 list)))))) ; found
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
                 ((1 4 6 8 9) 'composite))     ===> composite)
      (assert (case (car '(c d))
                 ((a e i o u) 'vowel)
                 ((w y) 'semivowel)
                 (else 'consonant))            ===> consonant)

      (assert (memq 'a '(a b c))               ===> (a b c))
      (assert (memq 'b '(a b c))               ===> (b c))
      (assert (memq 'd '(a b c))               ===> #false)
      (assert (memq '(a) '(b (a) c))           ===> #false)
      (assert (member '(a) '(b (a) c))         ===> ((a) c))
      (assert (member "a" '(3 "b" "c") less?)  ===> ("b" "c"))
      (assert (memq 101 '(100 101 102))        ===> (101 102)) ; * ol specific, (in r7rs unspecified)
      (assert (memv 101 '(100 101 102))        ===> (101 102))

      ; procedure:  (assq obj alist)           * moved to (scheme base)
      ; procedure:  (assv obj alist)           * moved to (scheme base)
      ; procedure:  (assoc obj alist)          * moved to (scheme base)
      ; procedure:  (assoc obj alist compare)  * moved to (scheme base)

      ; procedure:  (list-copy obj)
      ;
      ; Returns a newly allocated copy of the given obj if it is a
      ; list. Only the pairs themselves are copied; the cars of the
      ; result are the same (in the sense of eqv?) as the cars of list.
      ; If obj is an improper list, so is the result, and the final cdrs
      ; are the same in the sense of eqv?. An obj which is not a
      ; list is returned unchanged. It is an error if obj is a circular
      ; list.
      (define (list-copy obj) ; BUG: does not fit to documentation
         (reverse (reverse obj)))

      ; 6.5  Symbols
      ;
      ; Symbols are objects whose usefulness rests on the fact ...
      ; .......

      ; procedure:  (symbol? obj)
      (define (symbol? o)
         (eq? (type o) type-symbol))

      ; procedure:  (symbol=? symbol1 symbol2 symbol3 ...)
      (define (symbol=? . os)
         (unless (null? os)
            (let loop ((o (cdr os)))
               (if (null? o)
                  #true
                  (and (equal? (car os) (car o)) (loop (cdr o)))))))

      ; procedure:  (symbol->string symbol)
      (define (symbol->string . args)
         (error:please-import-langintern)) ; * (lang eval)

      ; procedure:  (string->symbol string)
      (define (string->symbol . args)
         (error:please-import-langintern)) ; * (lang eval)

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
;            ((k) (vm:vm:makeb type-string k))
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

      ; (r7rs) 6.9  Bytevectors
      ;
      ; procedure: (bytevector? obj)
      (define (bytevector? obj)
         (eq? (type obj) type-bytevector))

      ; procedure: (make-bytevector k)
      ; procedure: (make-bytevector k byte)
      (define make-bytevector
         (case-lambda
            ((k)
               (vm:makeb type-bytevector k))
            ((k byte)
               (vm:makeb type-bytevector k byte))))

      ; procedure: (bytevector byte ...)
      (define (bytevector . bytes)
         (vm:makeb type-bytevector bytes))

      ; tbd.

      ; Bytevectors represent blocks of binary data. They are
      ; fixed-length sequences of bytes, where a byte is an exact
      ; integer in the range from 0 to 255 inclusive. A bytevector
      ; is typically more space-efficient than a vector containing
      ; the same values.

      ;; *********************
      ;; 6.10  Control features
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

      ; procedure:  (map proc list1 list2 ...)
      (define map (lambda (f a)
         (let loop ((a a))
            (if (null? a)
               #null
               (cons (f (car a)) (loop (cdr a)))))))
      (define map (case-lambda
         ((f a)      (map f a))
         ((f a b)    (let loop ((a a)(b b)) ; map2
                        (if (null? a)
                           #null
                           (cons (f (car a) (car b)) (loop (cdr a) (cdr b))))))
         ; possible speedup:
         ;((f a b c) (let loop ((a a)(b b)(c c))
         ;              (if (null? a)
         ;                 #null
         ;                 (cons (f (car a) (car b) (car c)) (loop (cdr a) (cdr b) (cdr c))))))
         ((f a b . c) ; mapN
                     (let loop ((args (cons a (cons b c))))
                        (if (null? (car args)) ; закончились
                           #null
                           (cons (apply f (map car args)) (loop (map cdr args))))))
         ((f ) #null)))

         (assert (map cadr '((a b) (d e) (g h)))  ===> (b e h))

      ; library procedure:  (for-each proc list1 list2 ...)
      (define for-each (lambda (f a)
         (let loop ((a a))
            (unless (null? a)
               (begin
                  (f (car a))
                  (loop (cdr a)))))))
      (define for-each (case-lambda
         ((f a)      (for-each f a))
         ((f a b)    (let loop ((a a)(b b)) ; map2
                        (unless (null? a)
                           (begin
                              (f a b)
                              (loop (cdr a) (cdr b))))))
         ((f a b . c) ; mapN
                     (let loop ((a (cons a (cons b c))))
                        (unless (null? (car a)) ; закончились
                           (begin
                              (apply f (map car a))
                              (loop (map cdr a))))))

;         ((f a b c) (let loop ((a a)(b b)(c c))
;                        (if (null? a)
;                           #null
;                           (cons (f (car a) (car b) (car c)) (loop (cdr a) (cdr b) (cdr c))))))
         ((f) #f)))


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


      ;; 6.14. System interface
      ; ...

      ; procedure: (features)
      (define-syntax features
         (syntax-rules (*features*)
            ((features)
               *features*)))



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


      ;(assert #t (procedure? car))
      ;(assert #f (procedure? 'car))
      ;(assert #t (procedure? (lambda (x) x)))


      ;; essential procedure: apply proc args

      ;; ...

      ;; procedure: call-with-current-continuation proc

      ; non standard, owl extension
      (define-syntax let*/cc
         (syntax-rules (call/cc)
            ((let*/cc (var) . tail)
               (syntax-error "let/cc: continuation name cannot be empty"))
            ((let*/cc var . body)
               (call/cc (λ (var) (let* . body))))))

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
      syntax-error runtime-error
      assert error

      apply ;apply/cc
      call-with-current-continuation
      call/cc let*/cc

      ; not required to be exported:
      ;  quote values lambda setq
      ;  letq ifeq either values-apply
      ;  cons car cdr ref type size set-ref set-ref! eq? less?
      ;  clock, syscall
      ;  ff:red ff:black ff:toggle ff:red? ff:right?
      ;  ff-apply tuple-apply

      ; 4.1.5  Conditionals
      if unless cond case and or set!
      ; 4.2.2  Binding constructs
      letrec let let*            lets ; lets - ol specific, let*-values - r7rs
      ; 4.2.3  Sequencing
      begin ; do
      ; 4.2.5  Delayed evaluation
      delay force
      ; 4.2.8. Quasiquotation
      quasiquote
      ; 4.2.9. Case-lambda
      (exports (scheme srfi-16))

      ; ---------------------------------------------------------
      define ;define*
      list length append reverse
      ilist tuple tuple-case
      call-with-values define-library
      define-values ; ol specific

      ; 4.1

      ; 6.2 (numbers)
      type-fix+
      type-fix-
      type-int+
      type-int-
      type-rational
      type-complex
      type-inexact

      integer? rational? complex? real? number? exact? inexact?
      inexact exact exact->inexact inexact->exact

      ; 6.3 (other data types)
      type-bytecode
      type-proc
      type-clos
      type-pair
      type-vector-dispatch
      type-vector-leaf
      type-bytevector
      ;type-ff-black-leaf
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


      ; (r7rs) 6.1  Equivalence predicates
      eq? eqv? equal?

      ; 6.3
      not boolean? symbol? port? procedure? eof?

      ; (r7rs) 6.4. Pairs and lists
      pair? cons car cdr
      set-car! set-cdr!
      caar cadr cdar cddr
      null? list?
      make-list list length
      append reverse list-tail list-ref list-set!
      memq memv member
      list-copy

      ; (r7rs) 6.5. Symbols
      symbol? symbol=? symbol->string string->symbol

      value? reference?
      zero?

      list-ref

      ; (r7rs) 6.9  Bytevectors
      bytevector?
      make-bytevector
      bytevector

      ; 6.14
      features

      ; ol extension:
      bytecode? function? ff?

      map list? for-each

      exec yield
      halt wait
      set-ticker-value
      set-memory-limit get-word-size get-memory-limit

      set-car! set-cdr!
))
