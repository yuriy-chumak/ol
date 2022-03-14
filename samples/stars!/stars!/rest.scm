(define-library (stars! rest)
   (export
      GET POST
      register
   )
   (import
      (scheme core)
      (otus ffi) (owl string)
      (owl parse) (lib http)
      (owl io)
      (scheme dynamic-bindings)
      (file json))

(begin
   (define session (make-parameter "*"))
   (define (register s)
      (session s))

   (define (REQUEST TYPE URL body)
      (let*((options (map c-string (list
               "curl" "-s" "-i" URL
               "-X" TYPE
               "--header" (string-append "X-Joatmon-SID: " (session)))))
            (options (append options (map c-string (if body
               (list
                  "--header" "Content-Type: application/json"
                  "--data-binary" "@-")
               #null)))))
      
         (define In (syscall 22))
         (define Out (syscall 22))
         (define Err (syscall 22))
         ; fork "curl" with options
         (define Pid
            (syscall 59 (c-string "/usr/bin/curl") options
               (list (car In) (cdr Out) (cdr Err))))
         (for-each close-port (list
            (car In) (cdr Err) (cdr Out)))
         (write-json body (cdr In))
         (close-port (cdr In))

         (print "stderr: " (force (port->bytestream (car Err))))
         (close-port (car Err))

         (define response (try-parse http-parser (port->bytestream (car Out)) #t))
         (if response
         then
            (case (ref (ref (car response) 1) 2)
               (200 ; OK
                  (define json (read-json (cdr response)))
                  (close-port (car Out))
                  (print "Server answered 200")
                  json)
               (404 ; OK
                  (define json (read-json (cdr response)))
                  (close-port (car Out))
                  (print "Server answered 404")
                  json)
               (else is code
                  (print "Server answered " code)
                  (close-port (car Out))
                  code)))))

   (define (GET URL)
      (REQUEST "GET" URL #false))
   (define (POST URL body)
      (REQUEST "POST" URL body))

))
