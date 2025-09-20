;;;
;;; Bytecode assembly
;;;

; user only in compile.scm
(define-library (lang assemble)
   (export
      assemble-code
      fork-bytecode-interner)

   (import
      (scheme base)
      (owl ff)
      (owl math)
      (otus async)
      (owl list-extra)
      (src vm)
      (lang env)
      (lang primop)
      (lang register))

   (begin
      (define is-less #false)
      (define is-equal #true)
      (define is-greater '())

      (define (compare-bytes a b pos end)
         (if (eq? pos end)
            is-equal
            (let ((ab (ref a pos))
                  (bb (ref b pos)))
               (cond
                  ((eq? ab bb)
                     (compare-bytes a b (+ pos 1) end))
                  ((less? ab bb)
                     is-less)
                  (else
                     is-greater)))))

      ;; shorter is less, otherwase lexical comparison from start
      (define (compare-code a b)
         (let*((as (size a))
               (bs (size b)))
            (cond
               ((eq? as bs)
                  (compare-bytes a b 0 as))
               ((less? as bs)
                  is-less)
               (else
                  is-greater))))

      ;; fixme: should occasionally balance the tree

      ;; codes bcode value → codes'
      (define (insert-code codes bytecode value)
         (if codes
            (let*((l k v r codes)
                  (res (compare-code k bytecode)))
               (cond
                  ((eq? res is-equal)
                     [l bytecode value r])
                  ((eq? res is-less)
                     [(insert-code l bytecode value) k v r])
                  (else
                     [l k v (insert-code r bytecode value)])))
            [#false bytecode value #false]))

      (define (lookup-code codes bytecode)
         (if codes
            (let*((l k v r codes)
                  (res (compare-code k bytecode)))
               (cond
                  ((eq? res is-equal)
                     k)
                  ((eq? res is-less)
                     (lookup-code l bytecode))
                  (else
                     (lookup-code r bytecode))))))

      ;; codes bcode → codes(') bcode(')
      (define (intern-code codes bytecode)
         ; todo: if more than N lookups for same code,
         ;       jit it.
         (let ((res (lookup-code codes bytecode)))
            (if res
               (values codes res)
               (values (insert-code codes bytecode #false) bytecode)))) ; value removed for feature use

      ; start internal assembly interner
      (define bytecode-server ['bytecode-server]) ; make server name unique

      (define (fork-bytecode-interner bytecodes)
         (let ((codes (fold
                        (λ (codes pair)
                           (insert-code codes (car pair) (cdr pair)))
                        #false bytecodes)))
            (actor bytecode-server (lambda ()
               (let loop ((codes codes))
                  (let*((envelope (wait-mail))
                        (sender msg envelope)
                        ; assert (string? msg)
                        (codes code (intern-code codes msg)))
                     (mail sender code)
                     (loop codes)))))))

      ;; make bytecode and intern it
      (define (bytes->bytecode bytes)
         (await (mail bytecode-server
            (vm:alloc type-bytecode bytes))))


      (define (reg a)
         (if (eq? (type a) type-value+)
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
         (define need-a-bigger-jump-instruction "need a bigger jump instruction: length is ")
         (define (jx OP a then else)
            (let*((then (assemble then fail))
                  (else (assemble else fail))
                  (len (length else)))
               (cond
                  ((< len #xffff) (cons* OP (reg a) (band len #xff) (>> len 8) (append else then)))
                  (else (fail (list need-a-bigger-jump-instruction len))))))

         (case code
            (['ret a]
               (list RET (reg a)))
            (['move a b more]
               (let ((tl (assemble more fail)))
                  (if (eq? (car tl) MOVE) ;; [move a b] + [move c d] = [move2 a b c d] to remove a common dispatch
                     (cons* MOV2 (reg a) (reg b) (cdr tl))
                     (cons* MOVE (reg a) (reg b) tl))))
            (['prim op args to more]
               (cond
                  ;; fixme: handle mk differently, this was supposed to be a temp hack
                  ((> op #xff) ; dead leaf - we do not have opcodes larger than 255 (for now)
                     (output-code op
                        (cons (reg (length (cdr args))) ; vector size
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
                  ((eq? (type to) type-value+)
                     (if (opcode-arity-ok? op (length args) 1)
                        (cons op
                           (append (map reg args)
                              (cons to
                                 (assemble more fail))))
                        (fail (list "Bad opcode arity for" (primop-name op) (length args) 1))))
                  ((list? to)
                     (if (opcode-arity-ok? op (length args) (length to))
                        (if (multiple-return-variable-primop? op)
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
                        (fail (list "Bad opcode arity for " (primop-name op) (length args) (length to)))))
                  (else
                     (fail (list "Bad case of primop in assemble: " (primop-name op))))))

            (['cons-close closure? lpos offset env to more]
               ; make a 2-level closure
               (cons* CLOS
                  (if closure? type-closure type-procedure) ;; type of object
                  (+ 2 (length env)) ;; size of object (hdr code e0 ... en)
                  (reg lpos) offset  ;; env (reg, index)
                  (append (map reg env) ;; e0 ... en
                     (cons (reg to)
                           (assemble more fail)))))

            (['ld val to cont]
               (cond
                  ;; todo: add implicit load values to free bits of the instruction
                  ((eq? val #null)
                     (cons* LDN (reg to)
                        (assemble cont fail)))
                  ((eq? val #false)
                     (cons* LDF (reg to)
                        (assemble cont fail)))
                  ((eq? val #true)
                     (cons* LDT (reg to)
                        (assemble cont fail)))
                  ((eq? val #empty)
                     (cons* LDE (reg to)
                        (assemble cont fail)))
                  ((eq? (type val) type-value+)
                     (let ((code (assemble cont fail)))
                        (if (> val 126) ;(or (> val 126) (< val -126)) ; would be a bug
                           (fail (list "ld: big value: " val)))
                        (cons* LD
                           (if (< val 0) (+ 256 val) val)
                           (reg to) code)))
                  (else
                     (fail (list "cannot assemble a load for " val)))))
            (['refi from offset to more]
               (cons*
                  REFI (reg from) offset (reg to)
                  (assemble more fail)))
            (['goto op nargs]
               (list GOTO (reg op) nargs))
            ;((goto-code op n)
            ;   (list GOTO-CODE (reg op) n)) ;; <- arity needed for dispatch
            ;((goto-proc op n)
            ;   (list GOTO-PROC (reg op) n))
            ;((goto-clos op n)
            ;   (list GOTO-CLOS (reg op) n))
            ;; todo: all jumps could have parameterized lengths (0 = 1-byte, n>0 = 2-byte, being the max code length)
            (['jeq a b then else]
               (let*((then (assemble then fail))
                     (else (assemble else fail))
                     (len (length else)))
                  (cond
                     ((< len #xffff) (cons* JEQ (reg a) (reg b) (band len #xff) (>> len 8) (append else then)))
                     (else (fail (list need-a-bigger-jump-instruction len))))))
            (['jz a then else] ; todo: merge next four cases into one
               (jx JZ a then else))
            (['jn a then else]
               (jx JN a then else))
            (['je a then else]
               (jx JE a then else))
            (['jf a then else]
               (jx JF a then else))
            (else
               ;(print "assemble: what is " code)
               (fail (list "Unknown opcode " code)))))

      ; code rtl object -> executable code
      ;; todo: exit via fail cont
      ;; todo: pass tail here or have case-lambda nodes be handled internally with a foldr
      (define (assemble-code obj tail)
         (case obj
            (['code arity insts]
               (assemble-code ['code-var #true arity insts] tail))
            (['code-var fixed? arity insts]
               (let* ((insts (allocate-registers insts)))
                  (if (not insts)
                     (runtime-error "failed to allocate registers")
                     (let*/cc ret  ((fail (λ (why) (runtime-error "Error in bytecode assembly: " why) #false))
                                    (bytes (assemble insts fail))
                                    (len (length bytes)))
                        (if (> len #xffff) ; TODO: can be removed?
                           (runtime-error "too much bytecode: " len))
                        (bytes->bytecode
                           (if fixed?
                              ; без проверки на арность проваливается тест "case-lambda"
                              ; todo: оставить проверку для lambda, забрать для всего остального
                              (cons* JAF arity
                                 (band 255 (>> len 8))    ;; hi jump
                                 (band 255 len)           ;; low jump
                                 (append bytes
                                    (if (null? tail)
                                       (list ARITY-ERROR)
                                       tail)))
                              (cons* JAX (if fixed? arity (- arity 1))
                                 (band 255 (>> len 8))    ;; hi jump
                                 (band 255 len)           ;; low jump
                                 (append bytes
                                    (if (null? tail)
                                       (list ARITY-ERROR) ;; force error
                                       tail)))))))))
            (else
               (runtime-error "assemble-code: unknown AST node " obj))))

))
