(let loop ((line (read-line)))
   (when line
      (cond
         ((m/^[0-9A-Z]+$/ line)
            (print "#x" line))
         ((g/([0-9A-Z]+)/g line) => (lambda (re)
            (define a (string->number (lref re 0) 16))
            (define b (string->number (lref re 1) 16))
            (for-each (lambda (i)
                  (display "#x")
                  (display (number->string i 16))
                  (display " "))
               (iota (- b a -1) a)))) )
      (loop (read-line))))
