;todo: move primops from src/vm to lang/primop
(define-library (lang primop)

(export
      primop-of primitive?
      primop-name ;; primop → symbol | primop
      *special-forms*
      special-bind-primop? variable-input-arity?
      multiple-return-variable-primop? opcode-arity-ok? opcode-arity-ok-2?)

   (import
      (scheme core) (src vm)
      (scheme list)
      (owl list) (owl ff) (owl math))

   (begin
      ;; ff of opcode → wrapper
      (define prim-opcodes ;; ff of wrapper-fn → opcode
         (fold (λ (ff node)
                  (put ff (ref node 5) (ref node 2)))
            empty *primops*))

      ;; later check type, get first opcode and compare to primop wrapper
      (define (primop-of val)
         (get prim-opcodes val #false))


      (define primitive? primop-of)

      ; используется в выводе сообщений "инструкция такая-то сфейлила"
      (define (primop-name pop)
         (let ((pop (vm:and pop #x3F))) ; ignore top bits which sometimes have further data
            (if (eq? pop 17)
               'arity-error ;; non-primop instruction that can report errors
               (let loop ((primops *primops*))
                  (cond
                     ((null? primops) pop)
                     ((eq? pop (ref (car primops) 2))
                        (ref (car primops) 1))
                     (else
                        (loop (cdr primops))))))))

      ;; from cps
      (define (special-bind-primop? op) ; vector-apply and ff-apply
         (has? special-bind-primops op))

      (define (variable-input-arity? op)
         (has? variable-input-arity-primops op))

      (define (multiple-return-variable-primop? op)
         (has? multiple-return-variable-primops op))

      ;; primops = (#(name opcode in-args|#f out-args|#f wrapper-fn|#f) ...)

      ;; ff of opcode → (in|#f out|#f), #f if variable
      (define primop-arities ; internal
         (fold
            (λ (ff node)
               (let* ((name op in out wrapper node))
                  (put ff op (cons in out))))
            empty *primops*))

      (define (opcode-arity-ok? op in out)
         (let ((node (getf primop-arities op)))
            (if node
               (and
                  (or (eq? in  (car node)) (not (car node)))
                  (or (eq? out (cdr node)) (not (cdr node))))
               #true)))

      ;; fixme: ??? O(n) search for opcode->primop. what the...
      (define (opcode->primop op)
         (let
            ((node
               (some
                  (λ (x) (if (eq? (ref x 2) op) x #false))
                  *primops*)))
            (if node node (runtime-error "Unknown primop: " op))))

      (define (opcode-arity-ok-2? op n)
         (vector-apply (opcode->primop op)
            (λ (name op in out fn)
               (or (eq? in n) (eq? in 'any)))))

      ;; only special forms supported by the compiler, no primops etc
      (define *special-forms* {
         'quote  ['special 'quote]
         'values ['special 'values]
         'lambda ['special 'lambda]

         'setq   ['special 'setq]
         'let-eval ['special 'let-eval]

         'ifeq   ['special 'ifeq]
         'brae   ['special 'brae]

         'values-apply ['special 'values-apply]
      })

))
