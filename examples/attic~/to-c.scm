(display "unsigned char _binary_repl_start[] = \"")
(for-each (lambda (x)
             (display "\\x")
             (display (string (ref "0123456789abcdef" (div x 16))))
             (display (string (ref "0123456789abcdef" (mod x 16)))))
   (file->list "repl"))
(display "\";")

