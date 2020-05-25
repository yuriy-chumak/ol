;
(define-library (lang env)

(export
      lookup env-bind
      empty-env
      apply-env env-fold
      env-set-macro env-del
      env-get     ;; env key default → val | default
      env-del     ;; env key → env'
      env-set     ;; env-set env key val → env'
      env-keep    ;; env (name → name' | #false) → env'
      env-get-raw ;; env key → value      -- temporary
      env-put-raw ;; env key value → env' -- temporary
      env-keys)   ;; env → (key ...)

   (import
      (scheme base)
      (lang primop)
      (owl ff)
      (owl list)
      (owl string)
      (owl render)
      (owl list-extra)
      (owl math)
      (owl io)
      (src vm)
      (scheme misc))

   (begin
      ;;; these cannot be in primop since they use lists and ffs

      (define empty-env #empty) ;; will change with ff impl

      (define env-del del)

      (define lookup ;; <- to be replaced with env-get
         (let ((undefined ['undefined]))
            (λ (env key)
               (get env key undefined))))

      ;; get a value from env, or return def if not there or not a value
      (define (env-get env key def)
         (case (lookup env key)
            (['defined val]
               (case val
                  (['value v] v)
                  (else def)))
            (else def)))

      (define env-get-raw get) ;; will use different ff
      (define env-put-raw put) ;; will use different ff

      (define (env-set env key val)
         (put env key
            ['defined ['value val]]))

      (define (env-set-macro env key transformer)
         (put env key
            ['macro transformer]))

      (define-syntax invoke
         (syntax-rules ()
            ((invoke module name arg ...)
               ((env-get module (quote name)
                  (lambda (arg ...)
                     (runtime-error "invoke: failed to invoke "
                        (cons (quote name)
                           (list arg ...)))))
                  arg ...))))

      ;; mark an argument list (possibly improper list of symbols) as bound
      (define env-bind
         (let ((bound ['bound]))
            (λ (env keys)
               (let loop ((env env) (keys keys))
                  (cond
                     ((null? keys) env)
                     ((pair? keys)
                        (loop (put env (car keys) bound) (cdr keys)))
                     (else ;; improper argument list
                        (put env keys bound)))))))

      ;;;
      ;;; apply-env
      ;;;

      ; this compiler pass maps sexps to sexps where each free
      ; occurence of a variable is replaced by it's value

      ; this is functionally equivalent to making a big
      ; (((lambda (name ..) exp) value)), but the compiler currently
      ; handles values occurring in the sexp itself a bit more efficiently

      (define (ok env exp) ['ok exp env])
      (define (fail reason) ['fail reason])

      (define (value-exp val)
         ; represent the literal value val safely as an s-exp
         (if (or (pair? val) (symbol? val))
            (list 'quote val)
            val))

      (define (handle-symbol exp env fail)
         ; (print (list 'handle-symbol exp 'being (lookup env exp)))
         (case (lookup env exp)
            (['bound] exp)
            (['defined defn]
               (case defn
                  (['value val]
                     (value-exp val))
                  (else is funny
                     (fail (list "funny defined value: " funny)))))
            (['undefined]
               (fail
               (let ((error (bytes->string (foldr render '() (list "'" exp "'?")))))
                  (if (has? '(q quit exit stop ret) exp)
                     (list "What is " error " \n/if you want to quit just write ,quit (with comma!)/")
                     (list "What is " error)))))
            (else is bad
               (fail (list "The symbol" exp "has a funny value: '" bad "'")))))

      (define (formals-cool? call)
         (let ((formals (cadr call)))
            (let loop ((formals formals))
               (cond
                  ((and (pair? formals) (symbol? (car formals)))
                     (loop (cdr formals)))
                  ((symbol? formals) #true)
                  ((null? formals) #true)
                  (else #false)))))

      (define (walker env fail)
         (define (walk exp)
            ; (print (list 'walk exp))
            (cond
               ((null? exp)
                  ; allow null as a self-evaluating form
                  (list 'quote exp))
               ((list? exp)
                  (case (car exp)
                     ((quote) exp)
                     ((lambda)
                        (if (and (eq? (length exp) 3) (formals-cool? exp))
                           (list 'lambda (cadr exp)
                              ((walker (env-bind env (cadr exp)) fail)
                                 (caddr exp)))
                           (fail (list "funny lambda " exp))))
                     ((let-eval)
                        (if (and (eq? (length exp) 4) (formals-cool? exp))
                           (let ((walk (walker (env-bind env (cadr exp)) fail)))
                              (list 'let-eval
                                 (cadr exp)
                                 (map walk (caddr exp))
                                 (walk (car (cdddr exp)))))
                           (fail (list "funny let-eval " (list exp 'len (length exp) 'forms (formals-cool? exp))))))
                     ((values values-apply ifeq brae)
                        (cons (car exp) (map walk (cdr exp))))
                     (else
                        (map walk exp))))
               ((symbol? exp)
                  (handle-symbol exp env fail))
               ((pair? exp)
                  (fail (list "improper code: " exp)))
               ((number? exp)
                  exp)
               (else
                  (list 'quote exp))))
         walk)

      ; drop definitions from env to unbound occurrences in exp
      ; after macros have been expanded

      (define (apply-env exp env)
         (call/cc
            (lambda (ret)
               (ok env
                  ((walker env
                     (lambda (reason)
                        (ret (fail reason))))
                     exp)))))

      (define env-fold ff-fold)

      (define env-del del)

      ;; take a subset of env
      ;; fixme - misleading name
      (define (env-keep env namer)
         (env-fold
            (λ (out name value)
               (let ((name (namer name)))
                  (if name (put out name value) out)))
            empty env))

      (define (env-keys env)
         (ff-fold (λ (words key value) (cons key words)) null env))

))
