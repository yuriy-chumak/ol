;;;
;;; Value interning and conversions
;;;

; have mappings:
;  - strings->symbol
;  - bytes->bytecode (where returned bytecode may use spacial primops)
; (- intern-char     -> x → x')

; this module makes unique symbols table, so same symbols must have same address.
; e.g. (eq? 'sym1 'sym1) => #true
(define-library (lang intern)
   (export
      string->symbol
      symbol->string
      ;initialize-interner
      string->uninterned-symbol
      ;string->interned-symbol       ;; tree string → tree' symbol
      ;put-symbol                    ;; tree sym → tree'
      ;empty-symbol-tree
      ;defined?

      fork-intern-interner)

   (import
      (scheme core)
      (owl string)
      (owl interop)
      (owl list)
      (owl math)
      (owl io)
      (owl ff)
      (owl symbol))

   (begin
      ; hack warning, could use normal = and < here, but
      ; using primitives speeds up parsing a bit

      ; TODO: подумать о табличке символов как ff с хеш-ключами

      (define empty-symbol-tree #false)
      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      ; #false = s1 is less, 0 = equal, 1 = s1 is more
      (define (walk s1 s2)
         (cond
            ((null? s1)
               (cond
                  ((pair? s2) #false)
                  ((null? s2) 0)
                  (else (walk s1 (s2)))))
            ((pair? s1)
               (cond
                  ((pair? s2)
                     (lets
                        ((a as s1)
                         (b bs s2))
                        (cond
                           ((eq? a b) (walk as bs))
                           ((less? a b) #false)
                           (else #true))))
                  ((null? s2) 1)
                  (else (walk s1 (s2)))))
            (else (walk (s1) s2))))

      ; сравнить две строки
      (define (compare s1 s2)
         (walk (str-iter s1) (str-iter s2)))

      ; FIXME, add a typed ref instruction

      (define (string->uninterned-symbol str)
         (vm:new type-symbol str))

      (define (symbol->string ob)
         (ref ob 1))

      (define (string->symbol str)
         (interact 'intern str))

      ; lookup node str sym -> node' sym'
      (define (maybe-lookup-symbol node str)
         (if node
            (lets
               ((this (symbol->string (ref node 2)))
                (res (compare str this)))
               (cond
                  ((eq? res 0) ; match
                     (ref node 2))
                  (res
                     (maybe-lookup-symbol (ref node 1) str))
                  (else
                     (maybe-lookup-symbol (ref node 3) str))))
            #false))

      ;; node is an unbalanced trie of symbols (F | (Tuple L sym R))
      (define (put-symbol node sym)
         (if node
            (lets
               ((this (ref node 2))
                (res (compare (symbol->string sym) (symbol->string this))))
               (cond
                  ((eq? res 0)
                     (set-ref node 2 sym))
                  (res
                     (set-ref node 1
                        (put-symbol (ref node 1) sym)))
                  (else
                     (set-ref node 3
                        (put-symbol (ref node 3) sym)))))
            [#false sym #false]))

      ;; note, only leaf strings for now
      (define (string->interned-symbol root str)
         (let ((old (maybe-lookup-symbol root str)))
            (if old
               (values root old)
               (let ((new (string->uninterned-symbol str)))
                  (values (put-symbol root new) new)))))

      ;; obj → bytecode | #false
      (define (bytecode-of func)
         (cond
            ((bytecode? func) func)
            ((function? func) (bytecode-of (ref func 1)))
            (else #false)))

      (define (debug . args)
         (apply print-to (cons stderr args))
         42)

      ; call before
      (define (fork-intern-interner symbols)
         (let ((codes (fold put-symbol empty-symbol-tree symbols)))
            (fork-server 'intern (lambda ()
               (let loop ((codes codes))
                  (let*((envelope (wait-mail))
                        (sender msg envelope))
                     (cond
                        ((string? msg)
                           (let*((codes symbol (string->interned-symbol codes msg)))
                              (mail sender symbol)
                              (loop codes)))
                        (else
                           (mail sender #false)
                           (loop codes)))))))))

      ; fixme: invalid
      ;(define-syntax defined?
      ;   (syntax-rules (*toplevel*)
      ;      ((defined? symbol)
      ;         (get *toplevel* symbol #false))))
))
