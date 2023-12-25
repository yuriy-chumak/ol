;;;
;;; Extra IO etc exposed via the syscall
;;;

;; Adding some extra system primops to see how much could be added while 
;; still keeping the generated .c code portable, win32 being the main 
;; reason for worry.

(define-library (owl sys)
   (export
      kill
      uname
      getenv
      ;; system

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
      (scheme core)

      (owl string)
      (owl math)
      (owl io)
      (otus async)
      (owl list))

   (begin
      (print-to stderr "(owl sys) is deprecated and will be removed soon! Use (lib c) instead.")

      ;;;
      ;;; Unsafe operations not to be exported
      ;;;

      ;; ;; string → #false | unsafe-dirptr
      ;; (define (open-dir path)
      ;;    (let ((cs (c-string path)))
      ;;       (if (and cs (<= (string-length cs) #xffff))
      ;;          (syscall 1011 cs #false #false)
      ;;          #false)))

      ;; ;; unsafe-dirfd → #false | eof | bvec
      ;; (define (read-dir obj)
      ;;    (syscall 78 obj #false #false))

      ;; ;; _ → #true
      ;; (define (close-dir obj)
      ;;    (syscall 1013 obj #false #false))

      ;;;
      ;;; Safe derived operations
      ;;;

      ;; dir elements are #false or fake strings, which have the type of small raw ASCII
      ;; strings, but may in fact contain anything the OS happens to allow in a file name.

      ;; (define (dir-fold op st path)
      ;;    (let ((dfd (open-dir path)))
      ;;       (if dfd
      ;;          (let loop ((st st))
      ;;             (let ((val (read-dir dfd)))
      ;;                (cond
      ;;                   ((eof? val) st)
      ;;                   ((equal? val ".") (loop st))
      ;;                   ((equal? val "..") (loop st))
      ;;                   (else (loop (op st val))))))
      ;;          #false)))

      ;; (define (dir->list path)
      ;;    (dir-fold (λ (seen this) (cons this seen)) null path))

      ;;;
      ;;; Processes
      ;;;

      ;; path (arg0 ...), arg0 customarily being path
      ;; returns only if exec fails

      ;; (define (system command . args)
      ;;    (syscall 1017 (c-string
      ;;       (fold (lambda (f s) (string-append f (string-append " " s))) "" (cons command args)))))

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
         (syscall 62 pid signal))


      (define (uname)
         (syscall 63))
      ;;;
      ;;; Environment variables
      ;;;

      ;; str → bvec | F
      (define (getenv str)
         (let ((str (c-string str)))
            (if str
               (syscall 1016 str))))
))
