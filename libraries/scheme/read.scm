(define-library (scheme read)
(export
   read)

(import
   (scheme core)
   (owl async) (owl io)
   (scheme vector)
   (only (lang sexp) sexp))

(begin
; --------------------------

   ; math + simplification (hope we not use larger than max number of bytes files?)
   (define (+ n x)
      (values-apply (vm:add n x) (lambda (n carry) n)))
   (define (<< n x)
      (values-apply (vm:shl n x) (lambda (overflow n) n)))

   (define (read-impl port)
      (define server ['read])
      (fork-server server (lambda ()
         (let this ((cache (make-bytevector 14)) (pos 0))
            (let*((envelope (wait-mail))
                  (sender msg envelope))
               (if msg ; #false to stop the thread, else - number of character
                  (let loop ((cache cache) (pos pos))
                     (cond
                        ((eq? pos (size cache)) ; надо ли увеличить размер кеша?
                           (let ((cache (vm:makeb type-bytevector (vector->list cache) (<< (size cache) 1))))
                              (loop cache pos)))
                        ((less? msg pos)
                           (mail sender (ref cache msg)))
                        (else
                           ; let's read new byte..
                           (define char (syscall 0 port 1))
                           (if (memq char '(#f #t #eof))
                              (mail sender char)
                              (begin
                                 (set-ref! cache pos (ref char 0))
                                 (loop cache (+ pos 1))))))
                     (this cache pos)))))))

      (define (non-buffered-input-stream-n n)
         (lambda ()
            (define in (await (mail server n)))
            (case in
               (#f #null) ; port error
               (#t ; input not ready
                  (await (mail 'io 5))
                  (non-buffered-input-stream-n n))
               (#eof ; end-of-file
                  (close-port port)
                  #null)
               (else
                  (cons in (non-buffered-input-stream-n (+ n 1)))))))

      ((sexp)
         #null
         (non-buffered-input-stream-n 0)
         0
         (λ (left data-tail pos val) ; ok
            (mail server #false)
            val)))

   ; public function
   (define read (case-lambda
      ((port)
         (read-impl port))
      (()
         (read-impl stdin))))

))