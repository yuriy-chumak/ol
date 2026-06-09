;;;
;;; Bytecode assembly
;;;

; user only in compile.scm
(define-library (lang assemble)
   (export
      assemble-code
      start-bytecode-interner)

   (import
      (scheme base)
      (owl ff)
      (owl math)
      (otus async)
      (owl list-extra)
      (src vm)
      (owl io)
      (only (owl list) all some)
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
      (define bytecode-server '|'Bytecodes|)

      (define (start-bytecode-interner bytecodes)
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
      ;; (define (bytes->bytecode bytes)
      ;;    (await (mail bytecode-server
      ;;       (vm:alloc type-bytecode bytes))))

      ;; make bytecode
      (define (bytes->bytecode bytes)
         (vm:alloc type-bytecode bytes))


      (define (reg8 a) (less? a 256))


      (define (LO x) (band x #xff)); TODO: change to macro
      (define (HI x) (>> x 8))     ; TODO: change to macro
      (define-macro reg (lambda (a) a))


      ;;;
      ;;; Bytecode assembly
      ;;;

      (define MOVE/16 (+ MOVE 64))
      (define REFI/16 (+ REFI 64))
      (define GOTO/16 (+ GOTO 64))

      (define RET/16 (+ RET 64))
      (define LD/16 (+ LD/ 64))
      (define LD8/16 (+ LD8 64))

      ; rtl -> list of bytes
      ;; ast fail-cont → code' | (fail-cont <reason>)
      (define (assemble code fail)
         (define need-a-bigger-jump-instruction "need a bigger jump instruction: length is ")
         (define (b OP a then else)
            (let*((then (assemble then fail))
                  (else (assemble else fail))
                  (len (length else)))
               (cond
                  ((< len #xffff) (cons* B/ OP (reg a) (band len #xff) (>> len 8) (append else then))) ; #o60 = B
                  (else (fail (list need-a-bigger-jump-instruction len))))))

         (case code
            (['ret a]
               (if (reg8 a)
                  (list RET (reg a))
                  (list RET/16 (LO a) (HI a))))
            (['move a b more]
               (let ((tl (assemble more fail)))
                  (if (and (reg8 a) (reg8 b)) ; 8bit regs
                     (if (eq? (car tl) MOVE) ;; [move a b] + [move c d] = [move2 a b c d] (very handy speedup)
                        (cons* MOV2 (reg a) (reg b) (cdr tl))
                        (cons* MOVE (reg a) (reg b) tl))
                     (cons* MOVE/16 ; TODO: add MOVE2 16-bit version (maybe)
                        (LO a) (HI a)
                        (LO b) (HI b) tl))))
            (['prim op args to more]
               (cond
                  ; (vm:new) includes type as part of opcode (TODO: change to something more convenient)
                  ((less? #xFF op)
                     ; assert (HI op) == NEW
                     (define len (length args))
                     (if (all reg8 (cons* len to args))
                        (cons* (HI op) (LO op) ; code, type
                           len ; vector size
                           (append args ; (map reg args)
                              (cons (reg to)
                                    (assemble more fail))))
                        ; vm:new/64, assert (= (HI op) NEW)
                        (cons* (+ (HI op) 64) (LO op) ; code, type
                           (LO len) (HI len)
                           (append (foldr (lambda (x tl)
                                             (cons* (LO x) (HI x) tl))
                                       #n args)
                              (cons*
                                 (LO to) (HI to)
                                 (assemble more fail))))))
                  ; vm:new, vm:make, vm:alloc, syscall, vm:set!
                  ((variable-input-arity? op)
                     (unless (all reg8 args) (runtime-error "PRIM16" args))
                     (unless (all reg8 (if (list? to) to (list to))) (runtime-error "PRIM16" to))
                     ;; fixme: no output arity check
                     (cons op
                        (cons (length args)
                           (append args
                              (cons to
                                 (assemble more fail))))))
                  ; returning one argument
                  ((eq? (type to) type-value+)
                     (unless (memq op '(51)) ; TEMP, not a CONS
                        (unless (all reg8 args) (runtime-error "PRIM16" args))
                        (unless (reg8 to) (runtime-error "PRIM16" to)))

                     (if (opcode-arity-ok? op (length args) 1)
                        (if (all reg8 (cons to args))
                           (cons op
                              (append args ; (map reg args)
                                 (cons (reg to)
                                    (assemble more fail))))
                           (cons (+ op 64) ;; wide opcode
                              (append (foldr (lambda (x tl)
                                                (cons* (LO x) (HI x) tl))
                                          #n args)
                                 (cons* (LO to) (HI to)
                                    (assemble more fail)))))
                        (fail (list "Bad opcode arity for" (primop-name op) (length args) 1))))
                  ; returning multiple arguments
                  ((list? to)
                     (unless (all reg8 args) (runtime-error "PRIM16" args))
                     (unless (all reg8 to) (runtime-error "PRIM16" to))

                     (if (opcode-arity-ok? op (length args) (length to))
                        (if (multiple-return-variable-primop? op)
                           (cons op
                              (append args ; (map reg args)
                                 ; <- nargs implicit, FIXME check nargs opcode too
                                 (append to ; (map reg to)
                                    (assemble more fail))))
                           (cons op
                              (append args ; (map reg args)
                                 (cons (length to)          ; <- prefix with output arity
                                    (append to ; (map reg to)
                                       (assemble more fail))))))
                        (fail (list "Bad opcode arity for " (primop-name op) (length args) (length to)))))
                  (else
                     (fail (list "Bad case of primop in assemble: " (primop-name op))))))

            (['cons-close closure? lpos offset regs to more]
               (define len (+ 2 (length regs)))

               ; make a 2-level closure
               (if (all reg8 (cons* lpos offset to regs))
                  (cons* CLOS
                     (if closure? type-closure type-procedure) ;; type of object
                     len ;; size of object (hdr code e0 ... en)
                     (reg lpos) offset  ;; env (reg, index)
                     (append regs ; (map reg env) ;; e0 ... en
                        (cons (reg to)
                              (assemble more fail))))
                  (cons* CLOS/16
                     (if closure? type-closure type-procedure) ;; type of object
                     (LO len) (HI len) ;; size of object (hdr code e0 ... en)
                     (LO lpos) (HI lpos) (LO offset) (HI offset) ;; env (reg, index)
                     (append (foldr (lambda (x tl) ; todo: change #n to (cons* (LO ...))
                                       (cons* (LO x) (HI x) tl))
                                 #n regs)
                        (cons* (LO to) (HI to)
                              (assemble more fail))))))

            (['ld val to cont]
               (cond
                  ;; todo: add implicit load values to free bits of the instruction
                  ((eq? val #null) ;; LDN
                     (if (reg8 to)
                        (cons* LD/ LDN (reg to)
                           (assemble cont fail))
                        (cons* LD/16 LDN (LO to) (HI to)
                           (assemble cont fail))))
                  ((eq? val #false)
                     (unless (reg8 to) (runtime-error "LDF16" to))
                     (cons* LD/ LDF (reg to) ; TODO: LDF, TODO: (if (reg8 to) ... else ...16-bit...
                        (assemble cont fail)))
                  ((eq? val #true)
                     (if (reg8 to)
                        (cons* LD/ LDT (reg to)
                           (assemble cont fail))
                        (cons* LD/16 LDT (LO to) (HI to)
                           (assemble cont fail))))
                  ((eq? val #empty)
                     (unless (reg8 to) (runtime-error "LDE16" to))
                     (cons* LD/ LDE (reg to)
                        (assemble cont fail)))
                  ((eq? (type val) type-value+)
                     (let ((code (assemble cont fail)))
                        (if (> val 126) ;(or (> val 126) (< val -126)) ; would be a bug
                           (fail (list "ld: big value: " val)))
                        (if (reg8 to)
                           (cons* LD8
                              (if (< val 0) (+ 256 val) val)
                              (reg to) code)
                           (cons* LD8/16
                              (if (< val 0) (+ 256 val) val)
                              (LO to) (HI to) code))))
                  (else
                     (fail (list "cannot assemble a load for " val)))))
            (['refi a offset b more]
               (if (and (reg8 a) (reg8 b) (reg8 offset)) ; 8bit regs
                  (cons* REFI
                     a offset b
                     (assemble more fail))
               else
                  (cons* REFI/16
                     (LO a) (HI a)
                     (LO offset) (HI offset)
                     (LO b) (HI b)
                     (assemble more fail))))
            (['goto op nargs] ; +
               (if (and (reg8 op) (reg8 nargs))
                  (list GOTO op nargs)
               else
                  (list GOTO/16 (LO op) (HI op) (LO nargs) (HI nargs))))
            ;((goto-code op n)
            ;   (list GOTO-CODE (reg op) n)) ;; <- arity needed for dispatch
            ;((goto-proc op n)
            ;   (list GOTO-PROC (reg op) n))
            ;((goto-clos op n)
            ;   (list GOTO-CLOS (reg op) n))
            ;; todo: all jumps could have parameterized lengths (0 = 1-byte, n>0 = 2-byte, being the max code length)
            (['beq a b then else]
               (unless (all reg8 (list a b)) (runtime-error "BEQ16" (list a b)))
               (let*((then (assemble then fail))
                     (else (assemble else fail))
                     (len (length else)))
                  (cond
                     ((< len #xffff) (cons* BEQ (reg a) (reg b) (band len #xff) (>> len 8) (append else then)))
                     (else (fail (list need-a-bigger-jump-instruction len))))))

            (['bz a then else]
               (unless (reg8 a) (runtime-error "BZ16" a))
               (b BZ a then else))
            (['bt a then else]
               (unless (reg8 a) (runtime-error "BT16" a))
               (b BT a then else))
            (['bn a then else]
               (unless (reg8 a) (runtime-error "BN16" a))
               (b BN a then else))
            (['be a then else]
               (unless (reg8 a) (runtime-error "BE16" a))
               (b BE a then else))
            (['bf a then else]
               (unless (reg8 a) (runtime-error "BF16" a))
               (b BF a then else))

            (else
               ;(print "assemble: what is " code)
               (fail (list "Unknown opcode " code)))))

      ; code rtl object -> executable code
      ;; todo: exit via fail cont
      ;; todo: pass tail here or have case-lambda nodes be handled internally with a foldr
      (define (assemble-code obj tail)
         ;; (print "=======================")
         ;; (print "assemble-code: " obj ", " tail)
         (case obj
            (['code arity insts]
               (assemble-code ['code-var #true arity insts] tail))
            (['code-var fixed? arity insts]
               (let* ((insts (allocate-registers insts)))
                  (if (not insts)
                     (runtime-error "failed to allocate registers")
                  else
                     (let*/cc ret  ((fail (λ (why) (runtime-error "Error in bytecode assembly: " why) #false))
                                    (bytes (assemble insts fail))
                                    (len (length bytes)))
                        (if (> len #xffff) ; TODO: can be removed?
                           (runtime-error "too much bytecode: " len))
                        (define end (append bytes
                                       (if (null? tail)
                                          (list ARITY-ERROR)
                                          tail)))
                        (bytes->bytecode
                           (if fixed?
                              ; без проверки на арность проваливается тест "case-lambda"
                              ; todo: оставить проверку для lambda, забрать для всего остального
                              (cons* BNA arity
                                 (band 255 (>> len 8))    ;; hi jump
                                 (band 255 len)           ;; low jump
                                 end)
                              (cons* BNAV (if fixed? arity (- arity 1))
                                 (band 255 (>> len 8))    ;; hi jump
                                 (band 255 len)           ;; low jump
                                 end)))))))
            (else
               (runtime-error "assemble-code: unknown AST node " obj))))

))
