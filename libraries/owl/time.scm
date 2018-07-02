;; todo: date handling

(define-library (owl time)

   (export 
      elapsed-real-time
      timed
      time
      time-ms)

   (import
      (scheme core)
      (owl io)
      (owl interop)
      (owl math))

   (begin
      ;(define (clock) (syscall 96 #f #f #f)) (syscall 96) in usec, (clock) in ms

      (define (elapsed-real-time thunk)
         (display "timing: ")
         (flush-port 1)
         (lets
            ((ss sms (clock))
             (res (thunk))
             (es ems (clock))
             (elapsed
               (- (+ ems (* es 1000))
                  (+ sms (* ss 1000)))))
            (print elapsed "ms")
            res))

      (define-syntax timed
         (syntax-rules ()
            ((timed exp)
               (timed exp (quote exp)))
            ((timed exp comment)
               (lets
                  ((ss sms (clock))
                   (res exp)
                   (es ems (clock))
                   (elapsed
                     (- (+ ems (* es 1000))
                        (+ sms (* ss 1000)))))
                  (print*-to stderr (list comment ": " elapsed "ms"))
                  res))))

      ;; note: just passing unix time without adding the extra seconds
      (define (time)
         (lets ((ss ms (clock))) ss))

      (define (time-ms) 
         (lets ((ss ms (clock))) 
            (+ (* ss 1000) ms)))))

