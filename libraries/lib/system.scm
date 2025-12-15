(define-library (lib system)
   (export
      pipe
      close-pipe
      execvp waitpid
      system
      popen)
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
      (define (execvp commands in out err)
         (define args (map c-string
               (if (pair? commands) commands (list commands))))
         (define Pid (syscall 59 (car args) args (list in out err)))

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

      ; main
      (case-lambda
         ((commands)
            (execvp commands #f #f #f))
         ((commands in)
            (execvp commands in #f #f))
         ((commands in out)
            (execvp commands in out #f))
         ((commands in out err)
            (execvp commands in out err))
         ))

   (define (waitpid pid)
      (syscall 61 pid))

   (define (system command)
      (syscall 1017 (c-string command)))

   ; mode chars:
   ;  "w" - write to stdin (ref result 1) manually,
   ;  "r" - read stdout (ref result 2) and skip stderr (typically printed to terminal),
   ;  "R" - read stdout AND stderr together,
   ;  "E" - read stderr (ref result 3) separately
   (define popen
      (define (popen command mode)
         (define In (if (m/w/ mode) (pipe #b01) #f))
         (define Out (if (m/r|R/ mode) (pipe #b10) #f)) ; "R" means read both stderr and stdout
         (define Err (if (m/E/ mode) (pipe #b10) #f))

         ; TODO: add Windows and MacOS support
         (define command-line (list "/bin/sh" "-c"
            (string-append command (if (m/R/ mode) " 2>&1" ""))))
         (define Pid (execvp command-line In Out Err))
         (when Pid [
               (if In (cdr In))
               (if Out (car Out))
               (if Err (car Err))
               Pid
            ]))
      (case-lambda
         ((command)
            (popen command "r"))
         ((command mode)
            (popen command mode)) ))

))
