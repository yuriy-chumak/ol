;;;
;;; Bytecode assembly
;;;

; user only in compile.scm
(define-library (lang assemble)
   (export
      assemble-code
      fork-bytecode-interner)

   (import
      (scheme core)
      (owl ff)
      (owl list)
      (owl math)
      (owl interop)
      (owl list-extra)
      (only (owl interop) interact)
      (src vm)
      (lang env)
      (lang primop)
      (lang register))

   (begin
      (define bytecode-server 'bytecode-server)

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
                     (tuple l bytecode value r))
                  ((eq? res is-less)
                     (tuple (insert-code l bytecode value) k v r))
                  (else
                     (tuple l k v (insert-code r bytecode value)))))
            (tuple #false bytecode value #false)))

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
         (let ((res (lookup-code codes bytecode)))
            (if res
               (values codes res)
               (values (insert-code codes bytecode #false) bytecode)))) ; value removed for feature use

      ; start internal assembly interner
      (define (fork-bytecode-interner bytecodes)
         (let ((codes (fold (λ (codes pair) (insert-code codes (car pair) (cdr pair))) #false bytecodes)))
            (fork-server bytecode-server (lambda ()
               (let loop ((codes codes))
                  (let*((envelope (wait-mail))
                        (sender msg envelope))
                     (cond
                        ((bytecode? msg)
                           ;(debug "interner: interning bytecode")
                           (let*((codes code (intern-code codes msg)))
                              (mail sender code)
                              (loop codes)))
                        ; todo: simplify this
                        (else
                           (mail sender #false)
                           (loop codes)))))))))

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
                  ((fix+? to)
                     (if (opcode-arity-ok? op (length args) 1)
                        (cons op
                           (append (map reg args)
                              (cons to
                                 (assemble more fail))))
                        (fail (list "Bad opcode arity for" (or (primop-name op) op) (length args) 1))))
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
                        (fail (list "Bad opcode arity for " (or (primop-name op) op) (length args) (length to)))))
                  (else
                     (fail (list "Bad case of primop in assemble: " (or (primop-name op) op))))))
            ;; fixme: closures should have just one RTL node instead of separate ones for clos-proc and clos-code
            ((clos-proc lpos offset env to more)
               ;; make a 2-level closure
               (if (eq? lpos 1)
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
               (if (eq? lpos 1)
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
                  ((eq? val #false)
                     (ilist LDF (reg to)
                        (assemble cont fail)))
                  ((eq? val #true)
                     (ilist LDT (reg to)
                        (assemble cont fail)))
                  ((eq? val #empty)
                     (ilist LDE (reg to)
                        (assemble cont fail)))
                  ((fix+? val)
                     (let ((code (assemble cont fail)))
                        (if (> val 126) ;(or (> val 126) (< val -126)) ; would be a bug
                           (fail (list "ld: big value: " val)))
                        (ilist LD
                           (if (< val 0) (+ 256 val) val)
                           (reg to) code)))
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
         ; fastest:
         ; (vm:makeb type-bytecode bytes)) ; more memory, less cpu
         ; middle case: intern only bytecode larger than x
         ; slowest but memory best case:
         (interact bytecode-server      ; more cpu, less memory
            (vm:makeb type-bytecode bytes))) ; make blob

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
                              (ilist JAF arity
                                 (band 255 (>> len 8))    ;; hi jump
                                 (band 255 len)           ;; low jump
                                 (append bytes
                                    (if (null? tail)
                                       (list ARITY-ERROR)
                                       tail)))
                              (ilist JAFX (if fixed? arity (- arity 1))
                                 (band 255 (>> len 8))    ;; hi jump
                                 (band 255 len)           ;; low jump
                                 (append bytes
                                    (if (null? tail)
                                       (list ARITY-ERROR) ;; force error
                                       tail)))))))))
            (else
               (runtime-error "assemble-code: unknown AST node " obj))))

))
