;; check file io, assuming run via makefile
(define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

(lets
   ((port (open-output-file "tmp/test")))
   (write-bytes port (format-any "Hello, world!" null))
   (close-port port))

(lets
   ((vec (file->bytevector "tmp/test")))
      (print (list->string (bytevector->list vec))))

