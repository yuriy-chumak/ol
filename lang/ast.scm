;;;
;;; Converting S-exps to a more compact and checked AST
;;;

(define-library (lang ast)

   (export call? var? value-of sexp->ast mkcall mklambda mkvarlambda mkvar mkval)

   (import
      (scheme base)
      (scheme srfi-1)

      (owl list-extra)
      (owl math)
      (owl list)
      (owl symbol)
      (owl io) ; for display
      (lang env))

   (begin
      (define (ok exp env) ['ok exp env])
      (define (fail reason) ['fail reason])

      (define (call? thing) (eq? (ref thing 1) 'call))
      (define (var? thing) (eq? (ref thing 1) 'var))
      (define (value-of node) (ref node 2))

      (define (mkval val)
         ['value val])

      (define (mklambda formals body)
         ['lambda formals body])

      ;; formals is a list as usual, but last one will be bound to an arg list
      ;; having an extra var? field because the fixed one could be merged to this later
      (define (mkvarlambda formals body)
         ['lambda-var #false formals body])

      (define (mkcall rator rands)
         ['call rator rands])

      ;;;; cps adds a cont + system
      (define (mkprim op args)
         ['prim op args])

      (define (mkvar sym)
         ['var sym])

      ;; formals-sexp → (sym ..)|#false fixed-arity?
      (define (check-formals lst)
         (let loop ((lst lst) (out null))
            (cond
               ((null? lst)
                  (values (reverse out) #true))
               ((symbol? lst) ;; variable arity
                  (if (has? out lst) ;; reappearence
                     (values #f #f)
                     (values (reverse (cons lst out)) #false)))
               ((symbol? (car lst))
                  (if (has? out (car lst))
                     (values #f #f)
                     (loop (cdr lst) (cons (car lst) out))))
               (else
                  (values #f #f)))))

      (define (fixed-formals-ok? sexp)
         (lets ((formals fixed? (check-formals sexp)))
            (and formals fixed?)))

      (define (translate-direct-call exp env fail translate)
         (case (lookup env (car exp))
            (['special thing]
               (case thing
                  ((quote)
                     (if (eq? (length exp) 2)
                        (mkval (cadr exp))
                        (list "Strange quote: " exp)))
                  ((lambda)
                     (let ((len (length exp)))
                        (cond
                           ((eq? len 3)
                              (lets
                                 ((formals (cadr exp))
                                  (body (caddr exp))
                                  (formals fixed?
                                    (check-formals formals)))
                                 (cond
                                    ((not formals) ;; non-symbols, duplicate variables, etc
                                       (fail (list "Bad lambda: " exp)))
                                    (fixed?
                                       (mklambda formals
                                          (translate body (env-bind env formals) fail)))
                                    (else
                                       (mkvarlambda formals
                                          (translate body (env-bind env formals) fail))))))
                           ((> len 3)
                              ;; recurse via translate
                              (let
                                 ((formals (cadr exp))
                                  (body (cddr exp)))
                                 (translate
                                    (list 'lambda formals
                                       (cons 'begin body)) env fail)))
                           (else
                              (fail (list "Bad lambda: " exp))))))
                  ((letq) ;;; (letq formals definitions body)
                     (if (eq? (length exp) 4)
                        (let
                           ((formals (lref exp 1))
                            (values (lref exp 2))
                            (body (lref exp 3)))
                           (if
                              (and
                                 (list? values)
                                 (fixed-formals-ok? formals)
                                 (eq? (length formals) (length values)))
                              (let ((env (env-bind env formals)))
                                 ['letq formals
                                    (map
                                       (lambda (x) (translate x env fail))
                                       values)
                                    (translate body env fail)])
                              (fail (list "Bad letq: " exp))))
                        (fail (list "Bad letq: " exp))))
                  ((ifeq) ;;; (ifeq a b then else)
                     (if (eq? (length exp) 5)
                        (let ((a (second exp))
                              (b (third exp))
                              (then (fourth exp))
                              (else (fifth exp)))
                           ['ifeq
                              (translate a env fail)
                              (translate b env fail)
                              (translate then env fail)
                              (translate else env fail)])
                        (fail (list "Bad ifeq " exp))))
                  ((either) ; (either (lambda-ok) (lambda-else))
                     (if (eq? (length exp) 3)
                        ['either
                           (translate (second exp) env fail)
                           (translate (third exp) env fail)]
                        (fail (list "Bad either node: " exp))))

                  ((values)
                     ['values
                        (map (lambda (arg) (translate arg env fail)) (cdr exp))])
                  ((values-apply)
                     ['values-apply
                        (translate (lref exp 1) env fail)
                        (translate (lref exp 2) env fail)])
                  ;; FIXME pattern
                  (else
                     (fail
                        (list
                           "Unknown special operator in ast conversion: "
                           exp)))))
            (['bound]
               (mkcall (mkvar (car exp))
                  (map
                     (lambda (x) (translate x env fail))
                     (cdr exp))))
            ;; both now handled by apply-env
            ;((undefined)
            ;  (fail (list "i do not know this function" exp)))
            ; left here to handle primops temporarily
            (['defined value]
               (mkcall value
                  (map (lambda (x) (translate x env fail)) (cdr exp))))
            (else
               ; could be useful for (eval (list + 1 2) env)
               ; so just warn for now
               (fail
                  (list
                     "Unknown value type in ast conversion: "
                     (list 'name (car exp) 'value  (lookup env (car exp)))))
               ;(mkval exp)
               )))

      (define (translate exp env fail)
         (cond
            ((null? exp) (mkval exp))
            ((list? exp)
               (if (symbol? (car exp))
                  (translate-direct-call exp env fail translate)
                  (mkcall
                     (translate (car exp) env fail)
                     (map
                        (lambda (x)
                           (translate x env fail))
                        (cdr exp)))))
            ((symbol? exp)
               (case (lookup env exp)
                  (['bound]
                     (mkvar exp))
                  ;; should be already handled in apply-env
                  (['defined value]
                     value)
                  (['special thing]
                     (fail
                        (list "a special thing being used as an argument: " exp)))
                  (['undefined]
                     (fail (list "what are '" exp "'?")))
                  (else
                     (fail
                        (list "Strange value in ast conversion: "
                           (lookup env exp))))))
            (else (mkval exp))))

      ; -> #(ok exp' env) | #(fail reason)

      (define (sexp->ast exp env)
;         (if (env-get env '*debug-ast* #false) (begin
;            (display "sexp->ast: ")
;            (print exp)))
;         (call/cc
;            (lambda (drop)
;               (let ((translated
;                           (translate exp env
;                              (lambda (reason) (drop (fail reason))))))
;                  (if (env-get env '*interactive* #false) (begin
;                     (display "sexp->ast result: ")
;                     (print translated)))
;                  ['ok
;                     translated
;                     env]))))
         (call/cc
            (lambda (drop)
               ['ok
                  (translate exp env (lambda (reason) (drop (fail reason))))
                  env])))


))