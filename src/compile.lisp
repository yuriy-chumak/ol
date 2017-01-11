(import (owl parse))
(import (lang thread))

(define syntax-error-mark (list 'syntax-error))

(define (syntax-fail pos info lst)
   (print "error")
   #false)
   ;(list syntax-error-mark info
   ;   (list ">>> " "x" " <<<")))

(define parser
   (let-parses (
         (line (get-greedy+ (get-rune-if (lambda (x) (not (eq? x #\newline))))))
         (skip (get-imm #\newline)))
      (runes->string line)))

; ...
(define (sleep1) (syscall 1200 #f #f #f))

(define main (lambda (args)
         (start-thread-controller
            (list ;1 thread
               (tuple 'init
                  (λ ()
                     (fork-server 'main (lambda ()
                        ;; get basic io running
                        (start-base-threads)

                        ;; repl needs symbol etc interning, which is handled by this thread
                        ;(fork-server 'intern interner-thunk)

                        ;; set a signal handler which stop evaluation instead of owl
                        ;; if a repl eval thread is running
                        ;(set-signal-action repl-signal-handler)

                        ;; repl
   (print "<pre>utf8 test: хелло\n")

   (let loop ((request (fd->exp-stream stdin "" parser syntax-fail #f)))
      (print "request: " request)
      (if request
         (begin
         (print (car request)) (sleep1)))
      (loop (force (cdr request))))

   (print "</pre>")

                              )))))
            null) ; no threads state

))


(display "unsigned char *language = (unsigned char*) \"")

(for-each (lambda (x)
             (display "\\x")
             (display (string (ref "0123456789abcdef" (div x 16))))
             (display (string (ref "0123456789abcdef" (mod x 16)))))
          (fasl-encode main))
(display "\";")
