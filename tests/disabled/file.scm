;; check file io, assuming run via makefile

(lets
   ((port (open-output-file "tmp/test")))
   (write-bytes port (render "Hello, world!" null))
   (close-port port))

(lets
   ((vec (file->bytevector "tmp/test")))
      (print (list->string (bytevector->list vec))))

