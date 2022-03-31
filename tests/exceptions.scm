; no exception
(call-with-current-continuation
   (lambda (k)
      (with-exception-handler
         (lambda (x)
            (display "condition: ")
            (write x)
            (newline)
            (k 'exception))
         (lambda ()
            (print (+ 1 17))))))

; exception
(call-with-current-continuation
   (lambda (k)
      (with-exception-handler
         (lambda (x)
            (display "condition: ")
            (write x)
            (newline)
            (k 'exception))
         (lambda ()
            (print (+ 1 (raise 'an-error)))))))

; exception without call/cc
(with-exception-handler
   (lambda (x)
      (display "something went wrong\n"))
   (lambda ()
      (print (+ 1 (raise 'an-error)))))

; exception from nested function
(define (test a b)
   (if (= a b)
      (raise "equal")
   else
      (+ a b)))

(with-exception-handler
   (lambda (x)
      (print "exception: " x))
   (lambda ()
      (print (test 1 2))
      (print (test 7 7))))

; exception from exception
(with-exception-handler
   (lambda (x)
      (print "exception 1: " x))
   (lambda ()
      (with-exception-handler
         (lambda (y)
            (print "exception 2: " y)
            (raise y))
         (lambda ()
            (print (test 17 12))
            (print (test 77 77))))))

; vm exception
(with-exception-handler
   (lambda (x)
      (print "exception: " x))
   (lambda ()
      (42)))

(print "done.")
