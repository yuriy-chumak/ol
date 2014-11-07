;; vm rimops

;; todo: convert tuple to variable arity
;; todo: convert arity checks 17 -> 25

(define-library (owl primop)
   (export 
      primops
      primop-name ;; primop → symbol | primop
      multiple-return-variable-primops
      variable-input-arity?
      special-bind-primop?
      ;; primop wrapper functions
      run 
      set-ticker
      clock 
      sys-prim
      bind
      ff-bind
		mkt
      halt
      wait
      ;; extra ops
      set-memory-limit get-word-size get-memory-limit start-seccomp

      apply apply-cont ;; apply post- and pre-cps
      call/cc call-with-current-continuation 
      lets/cc
      )

   (import
      (owl defmac))

   (begin
      ; список команд виртуальной машины
      (define JF2 25)
      (define ARITYERROR 17)
      (define RET 24)

      ; пара служебных функций:
      (define (append a b) ; append
         (if (eq? a '())
            b
            (cons (car a) (append (cdr a) b))))

      (define (- a b)
         (let* ((n _ (fx- a b)))
            n))
      (define (+ a b)
         (let* ((n _ (fx+ a b)))
            n))
      (define (nth o n)
         (car (ref o n)))
      
      (define (length lst) ; length of lst
         (let loop ((lst lst) (n 0))
            (if (eq? lst '())
               n
;               (loop (cdr lst) (1+ n)))))
               (let* ((p (+ n 1)))
                 (loop (cdr lst) (+ n 1))))))

      (define (func lst)
         (let*
            ((arity (car lst))
             (lst (cdr lst))
             (len (length lst)))
            (raw
               (cons JF2 (cons arity (cons 0 (cons len ; 25 is JF2
                  (append lst '(17)))))) ;; fail if arity mismatch
               type-bytecode #false)))

      (define (desc name bytecode function)
         (tuple name   (nth bytecode 2)
                    (- (car bytecode) 1) 1 function)) 

      ;; changing any of the below 3 primops is tricky. they have to be recognized by the primop-of of 
      ;; the repl which builds the one in which the new ones will be used, so any change usually takes 
      ;; 2 rebuilds.

      ; these 2 primops require special handling, mainly in cps
      ; похоже, они нигде не используются?
      (define ff-bind ;; turn to badinst soon, possibly return later 
         ; (func '(2 49))
         '__ff-bind__

         )
      (define bind
         ; (func '(2 32 4 5 24 5))
         '__bind__
         )
      ; this primop is the only one with variable input arity
      (define mkt 
         '__mkt__
         ;(func '(4 23 3 4 5 6 7 24 7))
         )

      ;; these rest are easy
      ; 24 = RET
      ;                  '(arity command arguments . command arguments)
      ; арность + непосредственный байткод примитивов
      ; дело в том, что эти команды оформляются как JF2 арность байткод, JF2 проверяет (и выравнивает, если надо) арность
      (define CONS       '(3 51 4 5 6      24 6)) ; 51 = CONS
      (define CAR        '(2 52 4 5        24 5)) ; 52 = CAR
      (define CDR        '(2 53 4 5        24 5)) ; 53 = CDR
      (define SET-CAR!   '(3 11 4 5 6      24 6))
      (define SET-CDR!   '(3 12 4 5 6      24 6))


      (define cons        (func CONS))
      (define car         (func CAR))
      (define cdr         (func CDR))
      (define run         (func '(3 50 4 5 6      24 6)))
      (define set-ticker  (func '(2 62 4 5        24 5)))
      (define sys-prim    (func '(5 63 4 5 6 7 8  24 8)))
      (define clock       (func '(1  9 3 5        61 3 4 2 5 2))) ; 9 = MOVE, 61 = CLOCK
      (define sys         (func '(4 27 4 5 6 7    24 7)))
      (define sizeb       (func '(2 28 4 5        24 5)))
      (define raw         (func '(4 60 4 5 6 7    24 7)))
      (define _connect    (func '(3 34 4 5 6      24 6))) ;; <- remove and add to sys
      (define _sleep      (func '(2 37 4 5        24 5)))   ;; <- move to sys
      (define fxband      (func '(3 55 4 5 6      24 6)))
      (define fxbor       (func '(3 56 4 5 6      24 6)))
      (define fxbxor      (func '(3 57 4 5 6      24 6)))
      (define type-byte   (func '(2 15 4 5        24 5))) ;; fetch just type information. old type will be removed later.
      (define type        type-byte)
      (define size        (func '(2 36 4 5        24 5)))
      (define cast        (func '(3 22 4 5 6      24 6)))
      (define set!        (func '(4 10 4 5 6 7    24 7)))
      (define ref         (func '(3 47 4 5 6      24 6))) ; op47 = ref t o r = prim_ref(A0, A1)
      (define refb        (func '(3 48 4 5 6      24 6)))
      (define ff-toggle   (func '(2 46 4 5        24 5)))
      (define set-car!    (func SET-CAR!))
      (define set-cdr!    (func SET-CDR!))


      ;; make thread sleep for a few thread scheduler rounds
      (define (wait n)
         (if (eq? n 0)
            n
            (let* ((n _ (fx- n 1)))
               (set-ticker 0)
               (wait n))))

      ;; from cps
      (define (special-bind-primop? op)
         (or (eq? op 32) (eq? op 49)))

      ;; fixme: handle multiple return value primops sanely (now a list)
      (define multiple-return-variable-primops
         (list 49 26 38 39 40 58 59 37 61))

      (define (variable-input-arity? op) (eq? op 23)) ;; mkt

      (define primops
         (list
            ;;; input arity includes a continuation
            (tuple 'sys          27 4 1 sys) ; тут что-то особенное (в смысле, что количество аргументов тут 4, а не 3 написано)
            
            (tuple 'sizeb        28 1 1 sizeb)   ;; raw-obj -> numbe of bytes (fixnum)
            (tuple 'raw          60 3 1 raw)   ;; make raw object, and *add padding byte count to type variant*
            (tuple '_connect     34 2 1 _connect)   ;; (connect host port) -> #false | socket-fd
            (tuple '_sleep       37 1 1 _sleep)   ;; (_sleep nms) -> #true
            (desc 'cons     CONS     cons)
            (desc 'car      CAR      car)
            (desc 'cdr      CDR      cdr)
            (desc 'set-car! SET-CAR! set-car!)
            (desc 'set-cdr! SET-CDR! set-cdr!)
            (tuple 'eq?          54 2 1 eq?)
            (tuple 'fxband       55 2 1 fxband)
            (tuple 'fxbor        56 2 1 fxbor)
            (tuple 'fxbxor       57 2 1 fxbxor)
            (tuple 'type-byte    15 1 1 type-byte) ;; get just the type bits (new)
            (tuple 'type         15 1 1 type)
            (tuple 'size         36 1 1 size)  ;;  get object size (- 1)
            (tuple 'cast         22 2 1 cast)  ;; cast object type (works for immediates and allocated)
            (tuple 'set!         10 3 1 set!)  ;; set!
            (tuple 'ref          47 2 1 ref)   ;;
            (tuple 'refb         48 2 1 refb)      ;;
            (tuple 'mkt          23 'any 1 mkt)   ;; mkt type v0 .. vn t
            (tuple 'ff-toggle    46 1 1 ff-toggle)))  ;; (fftoggle node) -> node', toggle redness


      (define set      (func '(4 45 4 5 6 7 24 7)))
      (define lesser?  (func '(3 44 4 5 6 24 6)))
      (define listuple (func '(4 35 4 5 6 7 24 7)))
      (define mkblack  (func '(5 42 4 5 6 7 8 24 8)))
      (define mkred    (func '(5 43 4 5 6 7 8 24 8)))
      (define red?     (func '(2 41 4 5 24 5)))
      (define fxqr     (func '(4 26))) ;; <- placeholder 
      (define fx+      (func '(4 38 4 5 6 7 24 7)))
      (define fx-      (func '(4 40 4 5 6 7 24 7)))
      (define fx>>     (func '(4 58 4 5 6 7 24 7)))
      (define fx<<     (func '(4 59 4 5 6 7 24 7)))
      
      (define apply      (raw '(20)                type-bytecode #false)) ;; <- no arity, just call 20
      (define apply-cont (raw (list (fxbor 20 64)) type-bytecode #false))

      (define primops (append primops
         (list
            (tuple 'bind         32 1 #false bind)  ;; (bind thing (lambda (name ...) body)), fn is at CONT so arity is really 1
            (tuple 'set          45 3 1 set)   ;; (set tuple pos val) -> tuple'
            (tuple 'lesser?      44 2 1 lesser?)  ;; (lesser? a b)
            (tuple 'listuple     35 3 1 listuple)  ;; (listuple type size lst)
            (tuple 'mkblack      42 4 1 mkblack)   ; (mkblack l k v r)
            (tuple 'mkred        43 4 1 mkred)   ; ditto
            (tuple 'ff-bind      49 1 #false ff-bind)  ;; SPECIAL ** (ffbind thing (lambda (name ...) body)) 
            (tuple 'red?         41 1 #false red?)  ;; (red? node) -> bool
            (tuple 'fxqr         26 3 3 'fxqr)   ;; (fxdiv ah al b) -> qh ql r
            (tuple 'fx+          38 2 2 fx+)   ;; (fx+ a b)      ;; 2 out 
            (tuple 'fx*          39 2 2 fx*)   ;; (fx* a b)      ;; 2 out
            (tuple 'ncons        29 2 1 ncons)   ;;
            (tuple 'ncar         30 1 1 ncar)   ;;
            (tuple 'ncdr         31 1 1 ncdr)   ;;
            (tuple 'fx-          40 2 2 fx-)   ;; (fx- a b)       ;; 2 out
            (tuple 'fx>>         58 2 2 fx>>)   ;; (fx>> a b) -> hi lo, lo are the lost bits
            (tuple 'fx<<         59 2 2 fx<<)   ;; (fx<< a b) -> hi lo, hi is the overflow
            (tuple 'clock        61 0 2 clock) ; (clock) → posix-time x ms
            (tuple 'set-ticker   62 1 1 set-ticker)
            (tuple 'sys-prim     63 4 1 sys-prim))))

      ;; special things exposed by the vm
      (define (set-memory-limit n) (sys-prim 7 n n n))
      (define (get-word-size)      (sys-prim 8 #false #false #false))
      (define (get-memory-limit)   (sys-prim 9 #false #false #false))
      (define (start-seccomp)      (sys-prim 10 #false #false #false)) ; not enabled by defa

      ;; stop the vm *immediately* without flushing input or anything else with return value n
      (define (halt n)             (sys-prim 6 n n n))


      (define call/cc
         ('_sans_cps
            (λ (k f)
               (f k
                  (case-lambda
                     ((c a) (k a))
                     ((c a b) (k a b))
                     ((c . x) (apply-cont k x)))))))

      (define call-with-current-continuation call/cc)

      (define-syntax lets/cc 
         (syntax-rules (call/cc)
            ((lets/cc (om . nom) . fail) 
               (syntax-error "let/cc: continuation name cannot be " (quote (om . nom)))) 
            ((lets/cc var . body) 
               (call/cc (λ (var) (lets . body))))))

      ;; non-primop instructions that can report errors
      (define (instruction-name op)
         (cond
            ((eq? op 17) 'arity-error)
            ((eq? op 32) 'bind)
            ((eq? op 50) 'run)
            (else #false)))
         
      (define (primop-name pop)
         (let ((pop (fxband pop 63))) ; ignore top bits which sometimes have further data
            (or (instruction-name pop)
               (let loop ((primops primops))
                  (cond
                     ((null? primops) pop)
                     ((eq? pop (ref (car primops) 2))
                        (ref (car primops) 1))
                     (else
                        (loop (cdr primops))))))))

; проверку типов вынесем на уровень компилятора!
; можно и в отдельный файл
      ; from syscall.scm
      (define (syscall op a b)
         (call/cc (λ (resume) (sys resume op a b))))
      (define (error reason info)
         (syscall 5 reason info))
      (define (pair? x) (eq? type-pair (type x))) ; list.scm

      (define (set-car! object value)
         (if (pair? object)
            (set-car! object value)
            (error "set-car! first argument is not a pair")))

))
