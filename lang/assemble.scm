;;;
;;; Bytecode assembly
;;;

; user only in compile.scm
(define-library (lang assemble)
   (export
      assemble-code)

   (import
      (r5rs core)
      (owl ff)
      (owl list)
      (owl math)
      (owl list-extra)
      (only (owl interop) interact)
      (lang register)
      (owl primop))

   (begin

      ;; primops = (#(name opcode in-args|#f out-args|#f wrapper-fn) ...)

      ;; ff of opcode → (in|#f out|#f), #f if variable
      (define primop-arities
         (fold
            (λ (ff node)
               (lets ((name op in out wrapper node))
                  (put ff op (cons in out))))
            empty primops))

      (define (opcode-arity-ok? op in out)
         (let ((node (getf primop-arities op)))
            (if node
               (and
                  (or (eq? in  (car node)) (not (car node)))
                  (or (eq? out (cdr node)) (not (cdr node))))
               #true)))


      ; vm-instructions
      (define MOVE  9) ; move a, t:      Ra -> Rt
      (define REFI  1) ; refi a, p, t:   Ra[p] -> Rt, p unsigned
      (define MOVE2 5) ; two moves, 4 args

      ; load
      (define LD   14) ; ld a, t:        Rt = a, signed byte
      (define LDN  (+ 13 (<< 1 6))) ; 77
      (define LDT  (+ 13 (<< 2 6))) ; 141  ldt t:          Rt = true
      (define LDF  (+ 13 (<< 3 6))) ; 205  ldf t:          Rt = false

      ;
      (define CLOS0 3) ; clos lp, o, nenv, e0 ... en, t:
      (define CLOC0 4) ; cloc lp, o, nenv, e0 ... en, t:
      (define CLOS1 6)
      (define CLOC1 7)

      ; conditional jumps
      (define JEQ   8) ; jeq a b o1 o2
      (define JZ   (+ 16 (<< 0 6))) ; jump-imm[0] if zero
      (define JN   (+ 16 (<< 1 6))) ; jump-imm[0] if null
      (define JE   (+ 16 (<< 2 6))) ; jump-imm[0] if empty
      (define JF   (+ 16 (<< 3 6))) ; jump-imm[0] if false
      (define JF2  25) ; jump if arity failed
      (define JF2-ex (+ JF2 (<< 1 6))) ; JF2 with extra flag

      ; executions
      (define GOTO 2) ; jmp a, nargs    call Ra with nargs args
      ;(define GOTO-CODE 18) ; not used for now, check (fn-type)
      ;(define GOTO-PROC 19) ; not used for now, check (fn-type)
      ;(define GOTO-CLOS 21) ; not used for now, check (fn-type)
      (define RET 24) ; ret a:          call R3 (usually cont) with Ra

      (define ARITY-ERROR 17)

;              (igoto . 26)   ; indirect goto
;              (mk   . 9)      ; mk n, a0, ..., an, t, size up to 256
;              (mki  . 11)     ; mki size, type, v1, ..., vn, to
;              (ref  . 12)     ; ref a, p, t     Rt = Ra[p] + checks, unsigned

;              (set-ref . 25)     ; set-ref a, p, b     Ra[Rp] = Rb
;              (jbf . 26)     ; jump-binding tuple n f offset ... r1 ... rn

              ;; ldi = 13                                                                                    ;+
;              (movh . 13)       ;                                                                            ;+

      (define (reg a)
         (if (eq? (type a) type-fix+)
            (if (< a n-registers)
               a
               (runtime-error "register too high: " a))
            (runtime-error "bad register: " a)))


      ;;;
      ;;; Bytecode assembly
      ;;;

      (define (output-code op lst)
         (if (eq? op (vm:and op #xff))
            (cons op lst)
            (output-code
               (>> op 8)
               (cons (band op #xff) lst))))

      ; rtl -> list of bytes
      ;; ast fail-cont → code' | (fail-cont <reason>)
      (define (assemble code fail)
         (tuple-case code
            ((ret a)
               (list RET (reg a)))
            ((move a b more)
               (let ((tl (assemble more fail)))
                  (if (eq? (car tl) MOVE) ;; [move a b] + [move c d] = [move2 a b c d] to remove a common dispatch
                     (ilist MOVE2 (reg a) (reg b) (cdr tl))
                     (ilist MOVE (reg a) (reg b) tl))))
            ((prim op args to more)
               (cond
                  ;; fixme: handle mk differently, this was supposed to be a temp hack
                  ((> op #xff)
                     (output-code op
                        (cons (reg (length (cdr args))) ; tuple size
                           (cons (reg (car args)) ; type
                              (append (map reg (cdr args))
                                 (cons (reg to)
                                    (assemble more fail)))))))
                  ((variable-input-arity? op)
                     ;; fixme: no output arity check
                     (cons op
                        (cons (length args)
                           (append (map reg args)
                              (cons (reg to)
                                 (assemble more fail))))))
                  ((fixnum? to)
                     (if (opcode-arity-ok? op (length args) 1)
                        (cons op
                           (append (map reg args)
                              (cons to
                                 (assemble more fail))))
                        (fail (list "bad opcode arity for" (or (primop-name op) op) (length args) 1))))
                  ((list? to)
                     (if (opcode-arity-ok? op (length args) (length to))
                        (if (has? multiple-return-variable-primops op)
                           (cons op
                              (append (map reg args)
                                 ; <- nargs implicit, FIXME check nargs opcode too
                                 (append (map reg to)
                                    (assemble more fail))))
                           (cons op
                              (append (map reg args)
                                 (cons (length to)          ; <- prefix with output arity
                                    (append (map reg to)
                                       (assemble more fail))))))
                        (fail (list "Bad opcode arity for " (list (or (primop-name op) op) (length args) (length to))))))
                  (else
                     (fail (list "bad case of primop in assemble: " (or (primop-name op) op))))))
            ;; fixme: closures should have just one RTL node instead of separate ones for clos-proc and clos-code
            ((clos-proc lpos offset env to more)
               ;; make a 2-level closure
               (if (= lpos 1)
                  (cons CLOS1
                     (cons (+ 2 (length env))
                        ;; size of object (hdr code e0 ... en)
                        (cons offset
                           (append (map reg env)
                              (cons (reg to)
                                 (assemble more fail))))))
                  (cons CLOS0
                     (cons (+ 2 (length env))
                        ;; size of object (hdr code e0 ... en)
                        (cons (reg lpos)
                           (cons offset
                              (append (map reg env)
                                 (cons (reg to)
                                    (assemble more fail)))))))))
            ((clos-code lpos offset env to more)      ;; make a 1-level closure
               (if (= lpos 1)
                  (cons CLOC1
                     (cons (+ 2 (length env))
                        ;; size of object (hdr code e0 ... en)
                        (cons offset
                           (append (map reg env)
                              (cons (reg to)
                                 (assemble more fail))))))
                  (cons CLOC0
                     (cons (+ 2 (length env))
                        ;; size of object (hdr code e0 ... en)
                        (cons (reg lpos)
                           (cons offset
                              (append (map reg env)
                                 (cons (reg to)
                                    (assemble more fail)))))))))
            ((ld val to cont)
               (cond
                  ;; todo: add implicit load values to free bits of the instruction
                  ((eq? val null)
                     (ilist LDN (reg to)
                        (assemble cont fail)))
                  ((fixnum? val)
                     (let ((code (assemble cont fail)))
                        (if (or (> val 126) (< val -126)) ; would be a bug
                           (fail (list "ld: big value: " val)))
                        (ilist LD
                           (if (< val 0) (+ 256 val) val)
                           (reg to) code)))
                  ((eq? val #false)
                     (ilist LDF (reg to)
                        (assemble cont fail)))
                  ((eq? val #true)
                     (ilist LDT (reg to)
                        (assemble cont fail)))
                  (else
                     (fail (list "cannot assemble a load for " val)))))
            ((refi from offset to more)
               (ilist
                  REFI (reg from) offset (reg to)
                  (assemble more fail)))
            ((goto op nargs)
               (list GOTO (reg op) nargs))
            ;((goto-code op n)
            ;   (list GOTO-CODE (reg op) n)) ;; <- arity needed for dispatch
            ;((goto-proc op n)
            ;   (list GOTO-PROC (reg op) n))
            ;((goto-clos op n)
            ;   (list GOTO-CLOS (reg op) n))
            ;; todo: all jumps could have parameterized lengths (0 = 1-byte, n>0 = 2-byte, being the max code length)
            ((jeq a b then else)
               (lets
                  ((then (assemble then fail))
                   (else (assemble else fail))
                   (len (length else)))
                  (cond
                     ((< len #xffff) (ilist JEQ (reg a) (reg b) (band len #xff) (>> len 8) (append else then)))
                     (else (fail (list "need a bigger jump instruction: length is " len))))))
            ((jz a then else) ; todo: merge next four cases into one
               (lets
                  ((then (assemble then fail))
                   (else (assemble else fail))
                   (len (length else)))
                  (cond
                     ((< len #xffff) (ilist JZ (reg a) (band len #xff) (>> len 8) (append else then)))
                     (else (fail (list "need a bigger jump instruction: length is " len))))))
            ((jn a then else)
               (lets
                  ((then (assemble then fail))
                   (else (assemble else fail))
                   (len (length else)))
                  (cond
                     ((< len #xffff) (ilist JN (reg a) (band len #xff) (>> len 8) (append else then)))
                     (else (fail (list "need a bigger jump instruction: length is " len))))))
            ((je a then else)
               (lets
                  ((then (assemble then fail))
                   (else (assemble else fail))
                   (len (length else)))
                  (cond
                     ((< len #xffff) (ilist JE (reg a) (band len #xff) (>> len 8) (append else then)))
                     (else (fail (list "need a bigger jump instruction: length is " len))))))
            ((jf a then else)
               (lets
                  ((then (assemble then fail))
                   (else (assemble else fail))
                   (len (length else)))
                  (cond
                     ((< len #xffff) (ilist JF (reg a) (band len #xff) (>> len 8) (append else then)))
                     (else (fail (list "need a bigger jump instruction: length is " len))))))
            (else
               ;(print "assemble: what is " code)
               (fail (list "Unknown opcode " code)))))

      ;; make bytecode and intern it (to improve sharing, not mandatory)
      (define (bytes->bytecode bytes)
         (interact 'intern (raw type-bytecode bytes)))

      ; code rtl object -> executable code
      ;; todo: exit via fail cont
      ;; todo: pass tail here or have case-lambda nodes be handled internally with a foldr
      (define (assemble-code obj tail)
         (tuple-case obj
            ((code arity insts)
               (assemble-code (tuple 'code-var #true arity insts) tail))
            ((code-var fixed? arity insts)
               (let* ((insts (allocate-registers insts)))
                  (if (not insts)
                     (runtime-error "failed to allocate registers" "")
                     (lets/cc ret
                        ((fail (λ (why) (runtime-error "Error in bytecode assembly: " why) #false))
                         (bytes (assemble insts fail))
                         (len (length bytes)))
                        (if (> len #xffff)
                           (runtime-error "too much bytecode: " len))
                        (bytes->bytecode
                           (if fixed?
                              ; без проверки на арность проваливается тест "case-lambda"
                              ; todo: оставить проверку для lambda, забрать для всего остального
                              (ilist JF2 arity
                                 (band 255 (>> len 8))    ;; hi jump
                                 (band 255 len)           ;; low jump
                                 (append bytes
                                    (if (null? tail)
                                       (list ARITY-ERROR)
                                       tail)))
                              (ilist JF2-ex (if fixed? arity (- arity 1))
                                 (band 255 (>> len 8))    ;; hi jump
                                 (band 255 len)           ;; low jump
                                 (append bytes
                                    (if (null? tail)
                                       (list ARITY-ERROR) ;; force error
                                       tail)))))))))
            (else
               (runtime-error "assemble-code: unknown AST node " obj))))

))
