(define-library (lib system)
	(export
      pipe
      close-pipe
      system)
   (import
      (scheme core)
      (scheme list)
      (owl io) (owl string))
(begin
   (define pipe (case-lambda
      (() (syscall 22))
      ((bytes) (syscall 22 bytes))))

   (define (close-pipe pp)
      (when (car pp) (close-port (car pp)))
      (when (cdr pp) (close-port (cdr pp))))

   (define system
      (define (system commands in out err ports)
         (define args (map c-string commands))
         (define Pid (syscall 59 (car args) args ports))

         (when in
            (close-port (car in))
            (set-car! in #false))
         (when out
            (close-port (cdr out))
            (set-cdr! out #false))
         (when err
            (close-port (cdr err))
            (set-cdr! err #false))
         ; return pid if ok
         Pid)
      ; main
      (case-lambda
         ((commands)
            (system commands #f #f #f #null))
         ((commands in)
            (system commands in #f #f (list (car in))))
         ((commands in out)
            (system commands in out #f (list (car in) (cdr out))))
         ((commands in out err)
            (system commands in out err (list (car in) (cdr out) (cdr err))))))

))