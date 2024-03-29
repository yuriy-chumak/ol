; http://rosettacode.org/wiki/Loops/Continue#C

(let loop ((i 1))
   (when (less? i 11)
      (call/cc (lambda (continue)
         (display i)
         (when (zero? (mod i 5))
            (print)
            (continue #f))
         (display ", ")))
      (loop (+ i 1))))
