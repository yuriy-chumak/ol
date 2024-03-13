(define-library (stars! rest)
   (export
      GET POST PUT
      DELETE PATCH

      server
      session
   )
   (import
      (scheme base)
      (otus ffi) (owl string)
      (owl parse) (lib http)
      (owl io)
      (scheme dynamic-bindings)
      (file json))

(begin
   (define server (make-parameter "http://127.0.0.1:4002"))
   (define session (make-parameter "*"))

   (define (REQUEST TYPE URL body)
      (let*((options (map c-string (list
               "curl" "-sS" "-i" (string-append (server) URL)
               "--request" TYPE
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

         (print "stderr: " (list->string (force (port->bytestream (car Err)))))
         (close-port (car Err))

         (define response (try-parse http-parser (port->bytestream (car Out)) #t))
         (if response
            (case (ref (ref (car response) 1) 2)
               (200 ; OK
                  (define json (read-json (cdr response)))
                  (close-port (car Out))
                  (print "Server answered 200")
                  json)
               (else is code
                  (print "Server answered " code)
                  (close-port (car Out))
                  code)))))

   (define (GET url)
      (REQUEST "GET" url #false))
   (define (POST url body)
      (REQUEST "POST" url body))
   (define (PUT url body)
      (REQUEST "PUT" url body))

   (define (DELETE url body)
      (REQUEST "DELETE" url body))
   (define (PATCH url body)
      (REQUEST "PATCH" url body))

))
