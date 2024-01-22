Tips and Tricks
===============

1. How to assemble a string from parts in the nicest way?
   * Note: this is not a functional way! For a functional way, use conventional `string-append`.
   ```scheme
   (begin
      (define buffer (open-output-string))
      (for-each (lambda (i)
            (display-to buffer "0x")
            (display-to buffer (number->string i 16))
            (display-to buffer ", "))
         (append
            (list 1 2 3 4 5)
            (string->list "Hello")
            (bytevector->list (bytevector 55 77 99))))
      (display-to buffer "0")

      (get-output-string buffer))
   ==> "0x1, 0x2, 0x3, 0x4, 0x5, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x37, 0x4d, 0x63, 0"
   ```

