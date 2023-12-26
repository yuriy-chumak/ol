(define-library (lib system)
   (export
      pipe
      close-pipe
      execvp waitpid
      system)
   (import
      (scheme core)
      (scheme list)
      (owl io) (owl string))
(begin
   ; car to read, cdr to write
   (define pipe (case-lambda
      (() (syscall 22))
      ((flags) (syscall 22 flags))))

   (define pipe? pair?)

   (define (close-pipe pp)
      (if (car pp) (close-port (car pp)))
      (if (cdr pp) (close-port (cdr pp))))

   (define execvp
      (define (execvp commands in out err ports)
         (define args (map c-string commands))
         (define Pid (syscall 59 (car args) args ports))

         (when (pipe? in) ; pipe?
            (close-port (car in))
            (set-car! in #false))
         (when (pipe? out)
            (close-port (cdr out))
            (set-cdr! out #false))
         (when (pipe? err)
            (close-port (cdr err))
            (set-cdr! err #false))
         ; return pid if ok
         Pid)
      (define (->car in)
         (cond
            ((pipe? in) (car in))
            ((port? in) in) ))
      (define (->cdr in)
         (cond
            ((pipe? in) (cdr in))
            ((port? in) in) ))

      ; main
      (case-lambda
         ((commands)
            (execvp commands #f #f #f #null))
         ((commands in)
            (execvp commands in #f #f (list (->car in))))
         ((commands in out)
            (execvp commands in out #f (list (->car in) (->cdr out))))
         ((commands in out err)
            (execvp commands in out err (list (->car in) (->cdr out) (->cdr err))))))

   (define (waitpid pid)
      (syscall 61 pid))

   (define (system command)
      (syscall 1017 (c-string command)))
))