(fold (lambda (n x)
      (print x "/" n)
      (* n 10))
   1
   (iota 100))
