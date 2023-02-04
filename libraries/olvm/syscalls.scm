(define-library (olvm syscalls)
   (export
      read write
      open close
      lseek stat
      sendfile
      )

   (import
      (scheme core)
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
      (syscall 4 port/file))

   (define (sendfile out in offset count)
      (syscall 40 out in offset count))

))
