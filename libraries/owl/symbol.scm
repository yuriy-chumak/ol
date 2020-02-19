(define-library (owl symbol)
   (export render-symbol)

   (import
      (scheme core)
      (owl string))

   (begin
      (define (render-symbol obj tl)
         (render-string
            (if (eq? (type obj) type-symbol)
               (let ((str (symbol->string obj)))
                  (cond
                     ((string=? str "")
                        "||")    ;; make empty symbols less invisible
                     ((m/ / str) ;; fixme: doesn't quote internal |:s yet
                        (string-append (string-append "|" str) "|"))
                     (else str)))
               (runtime-error "Not a symbol: " obj))
            tl))
))