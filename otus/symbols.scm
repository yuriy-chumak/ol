(define-library (otus symbols)
   (export
      string->symbol
      string->uninterned-symbol
      symbol->string

      format-symbol ; TODO?: rename to symbol->utf8stream
      fork-symbol-interner
   )

   (import
      (scheme core)
      (owl string) (owl regex)
      (scheme list)
      (otus async))

(begin
   (define (symbol->string str)
      (if (symbol? str)
         (ref str 1)))

   (define (string->symbol str) ; todo: move to (otus symbols)?
      (if (string? str)
         (await (mail 'symbols str)))) ; doesn't work without valid 'symbols coroutine

   (define (format-symbol obj tl)
      (format-string
         (let ((str (symbol->string obj)))
            (if str
               (cond
                  ((eq? (string-length str) 0) "||") ; empty symbol
                  ;; fixme: doesn't quotes internal |:s yet
                  ((m/[ .]/ str)
                     (string-append (string-append "|" str) "|"))
                  (else str))
               (runtime-error "Not a symbol: " obj)))
         tl))

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

   ; call before
   (define (fork-symbol-interner symbols)
      (actor 'symbols (lambda ()
         (define codes (fold put-symbol empty-symbol-tree symbols))
         (let loop ((codes codes))
            (let*((envelope (wait-mail))
                  (sender msg envelope)  ; assert (string? msg)
                  (codes symbol (string->interned-symbol codes msg)))
               (mail sender symbol)
               (loop codes))))))

))
