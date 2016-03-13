;;;
;;; Extra IO etc exposed via the syscall
;;;

;; Adding some extra system primops to see how much could be added while 
;; still keeping the generated .c code portable, win32 being the main 
;; reason for worry.

(define-library (owl sys)
   (export
      dir-fold
      dir->list
      exec
      wait
      chdir
      kill
      uname
      getenv
      sighup
      signint
      sigquit
      sigill
      sigabrt
      sigfpe
      sigkill
      sigsegv
      sigpipe
      sigalrm
      sigterm)

   (import
      (r5rs core)

      (owl string)
      (owl math)
      (owl equal)
      (owl io)
      (owl interop)
      (owl list)
      (owl vector))

   (begin

      ;;;
      ;;; Unsafe operations not to be exported
      ;;;

      ;; string → #false | unsafe-dirptr
      (define (open-dir path)
         (let ((cs (c-string path)))
            (if (and cs (<= (string-length cs) #xffff))
               (syscall 1011 cs #false #false)
               #false)))

      ;; unsafe-dirfd → #false | eof | bvec
      (define (read-dir obj)
         (syscall 78 obj #false #false))

      ;; _ → #true
      (define (close-dir obj)
         (syscall 1013 obj #false #false))

      ;;;
      ;;; Safe derived operations
      ;;;

      ;; dir elements are #false or fake strings, which have the type of small raw ASCII
      ;; strings, but may in fact contain anything the OS happens to allow in a file name.

      (define (dir-fold op st path)
         (let ((dfd (open-dir path)))
            (if dfd
               (let loop ((st st))
                  (let ((val (read-dir dfd)))
                     (cond
                        ((eof? val) st)
                        ((equal? val ".") (loop st))
                        ((equal? val "..") (loop st))
                        (else (loop (op st val))))))
               #false)))

      (define (dir->list path)
         (dir-fold (λ (seen this) (cons this seen)) null path))

      (define (chdir path)
         (let ((path (c-string path)))
            (and path
               (syscall 1020 path #false #false))))

      ;;;
      ;;; Processes
      ;;;

      ;; path (arg0 ...), arg0 customarily being path
      ;; returns only if exec fails

      (define (exec path args)
         (let*
            ((path (c-string path))
             (args (map c-string args)))
            (if (and path (all (λ (x) x) args))
               (syscall 1017 path args #false)
               (cons path args))))

      (define (wait pid)
         (let ((res (syscall 1019 pid (cons #false #false) #false)))
            (cond
               ((not res) res)
               ((eq? res #true)
                  (interact sleeper-id 6) ;; sleep using the associated IO thread
                  (wait pid))
               (else
                  ;; pair of (<exittype> . <result>)
                  res))))

      (define sighup   1)      ; hangup from controlling terminal or proces
      (define signint  2)      ; interrupt (keyboard)
      (define sigquit  3)      ; quit (keyboard)
      (define sigill   4)      ; illegal instruction
      (define sigabrt  6)      ; abort(3)
      (define sigfpe   8)      ; floating point exception
      (define sigkill  9)      ; kill signal
      (define sigsegv 11)      ; bad memory access
      (define sigpipe 13)      ; broken pipe
      (define sigalrm 14)      ; alarm(2)
      (define sigterm 15)      ; termination signal

      ;; pid signal → success?
      (define (kill pid signal)
         (syscall 1021 pid signal #false))


      (define (uname)
         (syscall 63 #f #f #f))
      ;;;
      ;;; Environment variables
      ;;;

      ;; str → bvec | F
      (define (getenv str)
         (let ((str (c-string str)))
            (if str
               (let ((bvec (syscall 1016 str #false #false)))
                  (if bvec
                     (bytes->string (vec->list bvec))
                     #false))
               #false)))))
