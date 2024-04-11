(define-library (owl symbol)
   (export format-symbol)

   (import
      (scheme core)
      (scheme list)
      (owl string))

   (begin
      (define (format-symbol obj tl)
         (format-string
            (if (eq? (type obj) type-symbol)
               (let ((str (symbol->string obj)))
                  (cond
                     ((eq? (string-length str) 0) "||") ; empty symbols
                     ;; fixme: doesn't quote internal |:s yet
                     ((has? (string->list str) #\space) ; (m/ / str)
                        (string-append (string-append "|" str) "|"))
                     (else str)))
               (runtime-error "Not a symbol: " obj))
            tl))
))