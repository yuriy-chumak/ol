(define-library (scheme read)
(export
   read)

(import
   (scheme core)
   (otus async) (owl io)
   (owl math)
   (scheme vector)
   (only (lang sexp) sexp))

(begin

   ; * internal function
   (define (read-impl port)
      (define coname ['read])
      (actor coname (lambda ()
         (let this ((cache (make-bytevector 14)) (pos 0))
            (let*((envelope (wait-mail))
                  (sender msg envelope))
               (if msg ; #false to stop the thread, else - number of characters
                  (let loop ((cache cache) (pos pos))
                     (cond
                        ((eq? pos (size cache)) ; do we need to increase cache size?
                           (let ((new-cache (vm:alloc type-bytevector (<< (size cache) 1))))
                              (vm:set! new-cache 0 cache 0 (size cache))
                              (loop new-cache pos)))
                        ((less? msg pos)
                           (mail sender (ref cache msg)))
                        (else
                           ; let's read new byte..
                           (define char (syscall 0 port 1))
                           (if (memq char '(#f #t #eof))
                              (mail sender char)
                           else
                              (set-ref! cache pos (ref char 0))
                              (loop cache (+ pos 1)))))
                     (this cache pos)))))))

      (define (non-buffered-input-stream-n n)
         (lambda ()
            (define in (await (mail coname n)))
            (case in
               (#f #null) ; port error
               (#t   ; input not ready
                  (sleep 5)
                  (non-buffered-input-stream-n n))
               (#eof     ; end-of-file
                  (close-port port)
                  #null)
               (else
                  (cons in (non-buffered-input-stream-n (+ n 1)))))))

      (let* ((l r p val ((sexp)
                           #null ; no left part of stream
                           (non-buffered-input-stream-n 0)
                           0 ; start position in stream
                           (λ (l r p v) ; ok
                              (mail coname #false)
                              (values l r p v)))))
         val))

   ; public function
   (define read (case-lambda
      ((port)
         (read-impl port))
      (()
         (read-impl stdin))))

))
