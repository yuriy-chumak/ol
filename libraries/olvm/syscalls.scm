(define-library (olvm syscalls)
   (export
      read write
      open close
      lseek stat
      strftime
      gettimeofday

      ; socket api:
      socket
      bind listen select accept
      connect
      sendfile getpeername
      )

   (import
      (scheme core)
      (owl string)
      (srfi 16))

(begin
   (define read (case-lambda
      ((port)
            (syscall 0 port))
      ((port count)
            (syscall 0 port count))))

   (define write (case-lambda
      ((port buffer)
            (syscall 1 port buffer))
      ((port buffer count)
            (syscall 1 port buffer count))))

   (define open (case-lambda
      ((name mode)
            (syscall 2 name mode))
      ((name mode blocking?)
            (syscall 2 name mode blocking?))
      ((name mode blocking? flags)
            (syscall 2 name mode blocking? flags))))

   (define (close port)
      (syscall 3 port))

   (define (lseek port offset whence)
      (syscall 8 port offset whence))

   (define (stat port/file)
      (syscall 4 (if (string? port/file) (c-string port/file) port/file)))

   (define strftime (case-lambda
      ((fmt) (syscall 201 (c-string fmt)))
      ((fmt time) (syscall 201 (c-string fmt) time))))

   (define (gettimeofday) (syscall 96))

   ; sockets API
   (define (socket) (syscall 41))

   (define (bind socket port) (syscall 49 socket port))
   (define (listen socket) (syscall 50 socket))
   (define (select socket timeout)
      (syscall 23 socket timeout))
   (define (accept socket) (syscall 43 socket))
   (define (connect socket addr port)
      (syscall 42 socket addr port))
   (define (sendfile out in offset count)
      (syscall 40 out in offset count))
   
   (define (getpeername socket) (syscall 51 socket))

))
