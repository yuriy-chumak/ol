; http://www.schemers.org/Documents/Standards/R5RS/HTML/
(define-library (r5rs base)
   (begin
   
      ; ========================================================================================================
      ; Scheme
      ;
      ; Revised(3) Report on the Algorithmic Language Scheme
      ;                  Dedicated to the Memory of ALGOL 60
      ;
      ; ========================================================================================================
   
      ;;; Chapter 1
      ;;; Overview of Scheme
      ;; 1.1  Semantics
      ;; 1.2  Syntax
      ;; 1.3  Notation and terminology
      ; 1.3.1  Primitive, library, and optional features
      ; 1.3.2  Error situations and unspecified behavior
      (define-syntax syntax-error
         (syntax-rules (error)
            ((syntax-error . stuff)
               (error "Syntax error: " (quote stuff)))))
               
      ; 1.3.3  Entry format
      ; 1.3.4  Evaluation examples
      ; 1.3.5  Naming conventions
      
      
      ;;; Chapter 2
      ;;; Lexical conventions
      ;; 2.1  Identifiers
      ;lambda        q
      ;list->vector  soup
      ;+             V17a
      ;<=?           a34kTMNs
      ;the-word-recursion-has-many-meanings
      ;
      ; ! $ % & * + - . / : < = > ? @ ^ _ ~
      
      ;; 2.2  Whitespace and comments
      ;; 2.3  Other notations
      
      
      ;;; Chapter 3
      ;;; Basic concepts
      ;; 3.1  Variables, syntactic keywords, and regions
      ;; 3.2  Disjointness of types
      ;; 3.3  External representations
      ;; 3.4  Storage model
      ;; 3.5  Proper tail recursion
      
      
      ;;; Chapter 4
      ;;; Expressions
      ;; 4.1  Primitive expression types
      ; 4.1.1  Variable references
      ; syntax:  <variable>
      
      
      ; 4.1.2  Literal expressions
      ; 4.1.3  Procedure calls
      ; 4.1.4  Procedures
      ; syntax:  (lambda <formals> <body>) 
      (define-syntax λ
         (syntax-rules () 
            ((λ . x) (lambda . x))))

      ; 4.1.5  Conditionals
      ; Temporary hack: if inlines some predicates.
      (define-syntax if
         (syntax-rules 
            (not eq? and null? pair? empty? type =)
            ((if test exp) (if test exp #false))
            ((if (not test) then else) (if test else then))
            ((if (null? test) then else) (if (eq? test '()) then else))
            ((if (empty? test) then else) (if (eq? test #empty) then else)) ;; FIXME - handle with partial eval later
            ((if (eq? a b) then else) (_branch 0 a b then else))            
            ((if (a . b) then else) ((lambda (x) (if x then else)) (a . b))) ; was: (let ((x (a . b))) (if x then else))
            ((if #false then else) else)
            ((if #true then else) then)
            ((if test then else) (_branch 0 test #false else then))))
      
      ; 4.1.6  Assignments
      ;; 4.2  Derived expression types
      ; 4.2.1  Conditionals
      ; (cond)
      ; (case)
      ; (and)
      ; (or)
      ; 4.2.2  Binding constructs
      ; (let)
      ; (let*)
      ; ...
      
      ; 4.2.3  Sequencing
      (define-syntax begin
         (syntax-rules (define define-syntax letrec define-values let*-values) ; ===>
            ;((begin
            ;   (define-syntax key1 rules1)
            ;   (define-syntax key2 rules2) ... . rest)
            ;   (letrec-syntax ((key1 rules1) (key2 rules2) ...)
            ;      (begin . rest)))
            ((begin exp) exp)
            ;((begin expression ===> wanted . rest)  ;; inlined assertions
            ;   (begin
            ;   (let ((val expression))
            ;      (if (eq? val (quote wanted)) #t
            ;         (sys '() 5 "assertion error: " (cons (quote expression) (cons "must be" (cons wanted '()))))))
            ;   (begin . rest)))
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
               ((lambda (free)
                  (begin . rest))
                  first))))
      ; 4.2.4  Iteration
      ; 4.2.5  Delayed evaluation
      ; 4.2.6  Quasiquotation
      ; 4.3  Macros
      ; 4.3.1  Binding constructs for syntactic keywords
      ; 4.3.2  Pattern language
      
      
      ;;; Chapter 5
      ;;; Program structure
      ;; 5.1  Programs
      ;; 5.2  Definitions
      ; 5.2.1  Top level definitions
      ; 5.2.2  Internal definitions
      ; 5.3  Syntax definitions
      
      
      ;;; Chapter 6
      ;;; Standard procedures
      ;; ....................
      
      

;      (define-syntax λ 
;         (syntax-rules () 
;            ((λ . x) (lambda . x))))

;      (define-syntax assert
;         (syntax-rules (if sys eq?)
;            ((assert result expression . stuff)
;               (if (eq? expression result) #t
;                  (sys '() 5 "assertion error: " (cons (quote expression) (cons "must be" (cons result '()))))))))
;;                 (call/cc (λ (resume) (sys resume 5 "Assertion error: " (list (quote expression) (quote stuff)))))


      ;; note, no let-values yet, so using let*-values in define-values

      (define-syntax letrec
         (syntax-rules (rlambda)
            ((letrec ((?var ?val) ...) ?body) (rlambda (?var ...) (?val ...) ?body))
            ((letrec vars body ...) (letrec vars (begin body ...)))))

      (define-syntax letrec*
         (syntax-rules ()
            ((letrec () . body)
               (begin . body))
            ((letrec* ((var val) . rest) . body)
               (letrec ((var val))
                  (letrec* rest . body)))))

      (define-syntax let
            (syntax-rules ()
               ((let ((var val) ...) exp . rest) 
                  ((lambda (var ...) exp . rest) val ...))
               ((let keyword ((var init) ...) exp . rest) 
                  (letrec ((keyword (lambda (var ...) exp . rest))) (keyword init ...)))))





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

      (define-syntax case
         (syntax-rules (else eqv? memv =>)
            ((case (op . args) . clauses)
               (let ((fresh (op . args)))
                  (case fresh . clauses)))
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
                  (begin . body)
                  (case thing . clauses)))
            ((case thing (else => func))
               (func thing))
            ((case thing (else . body))
               (begin . body))
            ((case thing ((a . b) . body) . clauses)
               (if (memv thing (quote (a . b)))
                  (begin . body)
                  (case thing . clauses)))
            ((case thing (atom . then) . clauses) ;; added for (case (type foo) (type-foo thenfoo) (type-bar thenbar) ...)
               (if (eq? thing atom)
                  (begin . then)
                  (case thing . clauses)))))



      ;; expand case-lambda syntax to to (_case-lambda <lambda> (_case-lambda ... (_case-lambda <lambda> <lambda)))
      (define-syntax case-lambda
         (syntax-rules (lambda _case-lambda)
            ((case-lambda) #false) 
            ; ^ should use syntax-error instead, but not yet sure if this will be used before error is defined
            ((case-lambda (formals . body))
               ;; downgrade to a run-of-the-mill lambda
               (lambda formals . body))
            ((case-lambda (formals . body) . rest)
               ;; make a list of options to be compiled to a chain of code bodies w/ jumps
               ;; note, could also merge to a jump table + sequence of codes, but it doesn't really matter
               ;; because speed-sensitive stuff will be compiled to C where this won't matter
               (_case-lambda (lambda formals . body)
                  (case-lambda . rest)))))




      ; 4.1.1  Variable references

      ;; not defining directly because rlambda doesn't yet do variable arity
      ;(define list ((lambda (x) x) (lambda x x)))

      ;; fixme, should use a print-limited variant for debugging

      (define-syntax define*
         (syntax-rules (print list)
            ((define* (op . args) . body)
               (define (op . args) 
                  (print " * " (list (quote op) . args))
                  .  body))
            ((define* name (lambda (arg ...) . body))
               (define* (name arg ...) . body))))

      (define-syntax let*
         (syntax-rules (<=)
            ((let* (((var ...) gen) . rest) . body)
               (receive gen (lambda (var ...) (let* rest . body))))
            ((let* ((var val) . rest-bindings) exp . rest-exps)
               ((lambda (var) (let* rest-bindings exp . rest-exps)) val))
            ((let* ((var ... (op . args)) . rest-bindings) exp . rest-exps)
               (receive (op . args)
                  (lambda (var ...) 
                     (let* rest-bindings exp . rest-exps))))
            ((let* ((var ... node) . rest-bindings) exp . rest-exps)
               (bind node
                  (lambda (var ...) 
                     (let* rest-bindings exp . rest-exps))))
            ((let* (((name ...) <= value) . rest) . code)
               (bind value
                  (lambda (name ...)
                     (let* rest . code))))
            ((let* ()) exp)
            ((let* () exp . rest) (begin exp . rest))))

      ;; the internal one is handled by begin. this is just for toplevel.
      (define-syntax define-values
         (syntax-rules (list)
            ((define-values (val ...) . body)
               (_define (val ...)
                  (let* ((val ... (begin . body)))
                     (list val ...))))))

      (define-syntax let*-values
         (syntax-rules ()
            ((let*-values (((var ...) gen) . rest) . body)
               (receive gen
                  (lambda (var ...) (let*-values rest . body))))
            ((let*-values () . rest)
               (begin . rest))))
               
      ; i hate special characters, especially in such common operations.
      ; let* (let sequence) is way prettier and a bit more descriptive 
      (define-syntax lets
         (syntax-rules ()
            ((lets . stuff) (let* . stuff))))

      (define-syntax or
         (syntax-rules ()
            ((or) #false)
            ((or (a . b) . c)
               (let ((x (a . b)))
                  (or x . c)))
            ((or a . b)
               (if a a (or . b)))))

      (define-syntax and
         (syntax-rules ()
            ((and) #true)
            ((and a) a)
            ((and a . b)
               (if a (and . b) #false))))

      ;; now a function
      (define-syntax list
         (syntax-rules ()
            ((list) '())
            ((list a . b)
               (cons a (list . b)))))

      ; 4.2.6  Quasiquotation
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

      (define-syntax ilist
         (syntax-rules ()
            ((ilist a) a)
            ((ilist a . b)
               (cons a (ilist . b)))))

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
                  (bind tuple
                     (lambda (ignore . vars) . body))
                  (tuple-case 42 tuple type . others)))
            ;;; bind to anything
            ((tuple-case 42 tuple type ((_ . vars) . body) . rest)
               (bind tuple
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

      (define-syntax call-with-values
         (syntax-rules ()
            ((call-with-values (lambda () exp) (lambda (arg ...) body))
               (receive exp (lambda (arg ...) body)))
            ((call-with-values thunk (lambda (arg ...) body))
               (receive (thunk) (lambda (arg ...) body)))))

      (define-syntax do
        (syntax-rules ()
          ((do 
            ((var init step) ...)
            (test expr ...)
            command ...)
           (let loop ((var init) ...)
            (if test 
               (begin expr ...)
               (loop step ...))))))


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
               (_define name (rlambda (name) ((lambda (var ...) . body)) name)))
;            ((define name (λ (var ...) . body))
;               (_define name (rlambda (name) ((lambda (var ...) . body)) name)))
            ((define op val)
               (_define op val))))



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

      (define (not x)
         (if x #false #true))

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
      (define type-pair              1)
      
      (define type-bytecode         16)
      (define type-proc             17)
      (define type-clos             18)
      (define type-vector-dispatch  15)
      (define type-vector-leaf      11)
      (define type-vector-raw       19) ;; see also TBVEC in c/ovm.c
      (define type-ff-black-leaf     8)
      (define type-symbol            4)
      (define type-tuple             2)
      (define type-symbol            4)
      (define type-rlist-node       14)
      (define type-rlist-spine      10)
      (define type-string            3)
      (define type-string-wide      22)
      (define type-string-dispatch  21)
      (define type-thread-state     31)
      (define type-record            5)

      ;; transitional trees or future ffs
      (define type-ff               24)
      (define type-ff-r             25)
      (define type-ff-red           26)
      (define type-ff-red-r         27)

      ; + type-ff-red, type-ff-right

      ; 8 - black ff leaf
      ;; IMMEDIATE
      (define type-fix+              0)
      (define type-fix-             32)
      (define type-int+             40)
      (define type-int-             41)
      (define type-rational         42)
      (define type-complex          43) ;; 3 free below
      (define type-eof              20) ;; moved from 4, clashing with symbols
;      (define type-const            13) ;; old type-null, moved from 1, clashing with pairs
      (define type-port             12)
      (define type-socket           44)
      (define type-tcp-client       62)


      ;;           allocated/pointers     allocated/rawdata    immediate
      ;; (size  x)         n                       n               #false
      ;; (sizeb x)       #false                    n               #false

      (define (immediate? obj) (eq? #false (size obj)))
      (define allocated? size)
      (define raw?       sizeb)
      (define (record? x) (eq? type-record (type x)))

      (define-syntax _record-values 
         (syntax-rules (emit find)
            ((_record-values emit tag mk pred () fields tail)
               (values tag mk pred . tail))
            ((_record-values emit tag mk pred (x ... (field accessor)) fields tail)
               ;; next must cons accessor of field to tail, so need to lookup its position
               (_record-values find tag mk pred (x ...) fields tail field fields (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
            ((_record-values find tag mk pred left fields tail key (key . rest) (pos . poss))
               (_record-values emit tag mk pred left fields ((lambda (x) (ref x pos)) . tail))) 
            ((_record-values find tag mk pred left fields tail key (x . rest) (pos . poss))
               (_record-values find tag mk pred left fields tail key rest poss))
            ((_record-values find tag mk pred left fields tail key () (pos . poss))
               (syntax-error "Not found in record: " key)) 
            ((_record-values find tag mk pred left fields tail key (x . rest) ())
               (syntax-error "Implementation restriction: add more offsets to define-record-type macro" tag)))) 

      (define-syntax define-record-type
         (syntax-rules (emit)
            ((define-record-type name (constructor fieldname ...) pred (field accessor) ...)
               (define-values
                  (name constructor pred accessor ...)
                  (let ((tag (quote name))) ; ← note, not unique after redefinition, but atm seems useful to get pattern matching
                     (_record-values emit 
                        tag     
                        (lambda (fieldname ...) (mkt type-record tag fieldname ...))
                        (lambda (ob) (eq? tag (ref ob 1))) 
                        ((field accessor) ...) (fieldname ...) ()))))))






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

      (define (bytecode? o)  ; OL extension
         (eq? (type o) type-bytecode))

      (define (function? o)  ; OL extension
         (case (type o)
            (type-proc #true)
            (type-clos #true)
            (type-bytecode #true)
            (else #false)))

      (define (ff? o)        ; OL extension
         (or (eq? o #empty)
             (eq? 24 (fxband (type o) #b1111100))))


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
;               (_define name (rlambda (name) ((lambda (var ...) . body)) name)))
;;            ((define name (λ (var ...) . body)) ; fasten for (λ) process
;;               (_define name (rlambda (name) ((lambda (var ...) . body)) name)))
;            ((define op val)
;               (_define op val))))

      ; 4.1.2 Literal expressions
      ; ...




      ; 6. Standard procedures

      ; 6.2.5. Numerical operations
      ;; (number? obj) procedure
      (define (number? o)
         (case (type o)
            (type-fix+ #true)
            (type-fix- #true)
            (type-int+ #true)
            (type-int- #true)
            (type-rational #true)
            (type-complex #true)
            (else #false)))

      ;; (complex? obj ) procedure
      ;; (real? obj ) procedure
      ;; (rational? obj ) procedure
      ;; (integer? obj ) procedure


      ; 6.3. Other data types
      ; 6.3.1. Booleans
      ;; (not obj) library procedure
      ;; (boolean? obj) library procedure
      (define (boolean? o)
         (cond
            ((eq? o #true) #true)
            ((eq? o #false) #true)
            (else #false)))

      ; 6.3.2. Pairs and lists
      ;; (pair? obj) procedure
      (define (pair? o)
         (eq? (type o) type-pair))

      ; 6.3.3. Symbols
      ;; (symbol? obj) procedure
      (define (symbol? o)
         (eq? (type o) type-symbol))

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

      ; 
      ; 

      ; 6.4 Control features

      ;; (procedure? obj) procedure
      (define (procedure? o)
         (or (function? o) (ff? o)))

      ;(assert #t (procedure? car))
      ;(assert #f (procedure? 'car))
      ;(assert #t (procedure? (lambda (x) x)))
      

      ;; essential procedure: apply proc args
      ;; procedure: apply proc arg1 ... args
      (define apply      (raw type-bytecode '(#x14)))  ;; <- no arity, just call 20

      ;; ...

      ;; procedure: call-with-current-continuation proc
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

      ; non standard, owl extension
      (define-syntax lets/cc
         (syntax-rules (call/cc)
            ((lets/cc (om . nom) . fail) 
               (syntax-error "let/cc: continuation name cannot be " (quote (om . nom)))) 
            ((lets/cc var . body) 
               (call/cc (λ (var) (lets . body))))))

)
; ---------------------------
   (export
      λ syntax-error ;assert

      begin 
      quasiquote letrec let if 
      letrec* let*-values
      cond case define define*
      lets let* or and list
      ilist tuple tuple-case 
      call-with-values do define-library
      case-lambda
      define-values
      define-record-type
      _record-values
      not
      
      ; список типов
      type-complex
      type-rational
      type-int+
      type-int-
      type-record

      immediate? allocated? raw? record?

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
      type-eof
      type-tuple
      type-symbol
;      type-const
      type-rlist-spine
      type-rlist-node
      type-port 
      type-socket     ; todo: remove and use (cons 'socket port)
      type-tcp-client ; todo: remove and use (cons 'tcp-client port)
      type-string
      type-string-wide
      type-string-dispatch
      type-thread-state

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
      boolean? pair? symbol? number? char? string? vector? port? procedure?
      ; ol extension:
      bytecode? function? ff?
   )

)