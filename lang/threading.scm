;;;
;;; Thread controller
;;;

;; thread controller is like the kernel of owl lisp. it handles
;; activation and suspension of threads, and has a vector of
;; functions which are like the system calls, via which a thread
;; send requests via the thread scheduler to other threads or
;; the underlying system.

;; todo: make it a bug to send mail to a thread having no inbox.

(define-library (lang threading)

   (export
      start-thread-controller
      *debug-threading*)

   (import
      (scheme base)
      (only (src vm) vm:run)
      (owl queue)
      (otus async)
      (owl ff)
      (owl list)
      (owl math)
      (owl string)
      (otus format)
      (lang env)
      (only (lang eval) print-repl-error)
      (lang error)
      (owl io))

   (begin
      ; debug messages
      ; question: maybe change to *threading-log-level* and use (less? LEVEL *debug-threading*) instead of naked if
      (define *debug-threading* '(#f))

      (define (sys:write fd buffer)
         (syscall 1 fd buffer #false))

      (define stderr (vm:cast 2 type-port))
      (define (print . args)
         (sys:write stderr (make-bytevector (foldr format-any '(#\newline) args))))

      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      (define link-tag "mcp/links")
      (define signal-tag "mcp/break")

      (define (signal-halt threads state controller)
         (print-to stderr "stopping on signal")
         (halt 42)) ;; exit owl with a specific return value
      (define thread-quantum 10000)

      (define (bad-interop id a b c todo done state)
         (system-println "mcp: got bad interop")
         (values todo done state))

      ; -> state ok? x #false|waked-thread
      (define (deliver-mail state to envelope)
         (let ((st (get state to #false)))
            (cond
               ((pair? st) ;; currently working, leave a mail to inbox queue
                  (values #true
                     (ff-update state to (qsnoc envelope st))
                     #false))
               ((not st) ;; no such thread, or just no inbox
                  (when (car *debug-threading*)
                     (system-stderr "ol: dropping envelope to missing thread: ")
                     (system-stderr (bytes->string (format-any to '(#\newline))))
                     (system-stderr "    envelope: ")
                     (system-stderr (bytes->string (format-any envelope '(#\newline)))))
                  (values #false state #false))
               (else ;; activate the state function
                  (values #true
                     (ff-update state to qnull) ;; leave an inbox
                     [to (λ () (st envelope))]))))) ;; activate it

      (define (deliver-messages todo done state subs msg tc)
         (if (null? subs)
            (tc todo done state)
            (lets ((ok? state waked (deliver-mail state (car subs) msg)))
               (if waked
                  (deliver-messages (cons waked todo) done state (cdr subs) msg tc)
                  (deliver-messages todo done state (cdr subs) msg tc)))))

      ;; (define eval-break-message ['repl-eval ['breaked]])

      (define (subscribers-of state id)
         (get (get state link-tag empty) id null))

      ; remove the thread and report to any interested parties about the event
      (define (drop-delivering todo done state id msg tc)
         (let ((subs (subscribers-of state id)))
            (if (null? subs)
            then
               ;; no threads were waiting for something that is being removed, so tell stderr about it
               (case (ref (ref msg 2) 1)
                  ; runtime error or vm error
                  ((error crash)
                     (vector-apply (ref msg 2)
                        (lambda (state code reason clarification)
                           (print-repl-error
                              (verbose-ol-error #e code reason clarification))))))
                     
               (tc todo done (del state id))
            else
               (deliver-messages todo done
                  (del (ff-update state link-tag (del (get state link-tag empty) id)) id)
                  subs msg tc))))

      ;; thread dropping, O(n)
      (define (drop-from-list lst tid) ; -> lst'
         (cond
            ((null? lst) lst)
            ((eq? (ref (car lst) 1) tid) (cdr lst))
            (else
               (cons (car lst)
                  (drop-from-list (cdr lst) tid)))))

      ; drop a possibly running thread and notify linked
      (define (drop-thread id todo done state msg tc) ; -> todo' x done' x state'
         (drop-delivering
            (drop-from-list todo id)
            (drop-from-list done id)
            state id msg tc))

      ; l id → #false|thread l', O(n) running threads
      (define (catch-thread l id)
         (if (null? l)
            (values #false l)
            (let ((this (car l)))
               (if (eq? id (ref this 1))
                  (values this (cdr l))
                  (lets ((caught l (catch-thread (cdr l) id)))
                     (values caught (cons this l)))))))

      (define return-value-tag "rval") ; a unique key if thread state ff

      (define mcp-syscalls
         [
            ; id: name of the thread
            ; a: 
            ; b: 
            ; c:
            ; todo:
            ; done:
            ; state:
            ; tc:

            ; 1, runnig and time slice exhausted (the usual suspect, handled directly in scheduler)
            (λ (id a b c todo done state tc)
               ; (system-println "interop 1 - switch thread")
               (tc todo (cons [id a] done) state))

            ; 2, thread finished, drop
            (λ (id a b c todo done state tc)
               ; (system-println "mcp: interop 2 -- thread finished")
               (drop-delivering todo done state id
                  [id ['finished a b c]] tc))

            ; 3, vm thrown internal error (on assert)
            (λ (id a b c todo done state tc)
               ;(system-println "mcp: interop 3 -- vm error")
               ;; set crashed exit value proposal
               (let ((state (put state return-value-tag 127)))
                  (drop-delivering todo done state id
                     [id ['crash a b c]] tc)))

            ; 4, coroutine
            (λ (id cont opts thunk todo done state tc)
               ; (system-println "mcp: interop 4 -- coroutine")
               (lets((new-id (car opts))
                     (todo (cons* [new-id thunk] [id (λ () (cont new-id))] todo))
                     (state
                        (fold (λ (state req)
                                 (cond
                                    ((eq? req 'link)
                                       ;; forker wants to receive any issues the thread runs into
                                       (let ((links (get state link-tag empty)))
                                          (put state link-tag
                                             (put links new-id (list id)))))
                                    ((eq? req 'mailbox)
                                       ;; the thread should have a mailbox for communication in state
                                       (put state new-id qnull))
                                    (else
                                       (system-println "fork: bad parameter")
                                       state)))
                           state (cdr opts))))
                  (tc todo done state)))

            ; 5, user thrown error. (runtime-error ...)
            (λ (id a b c todo done state tc)
               ; (system-println "mcp: interop 5 -- (runtime-error ...)")
               (drop-delivering todo done state id
                  [id ['error a b c]] tc))

            ;; return mails to my own inbox (in reverse order, newest on top)

            ; 6, (return-mails rl)
            (λ (id cont rmails foo todo done state tc)
               ; (system-println "interop 6 - return-mails")
               (let ((queue (get state id qnull)))
                  (tc (cons [id (λ () (cont 'done))] todo)
                     done (put state id (foldr qsnoc queue rmails)))))

            ; 7, am i the only thread?
            (λ (id cont b c todo done state tc)
               ; (system-println "interop 7 - am i the only thread?")
               (tc
                  (cons [id (λ () (cont (and (null? todo) (null? done))))] todo)
                  done state))

            ; 8, get running thread ids (sans self)
            (λ (id cont b c todo done state tc)
               ; (system-println "interop 8 - get running thread ids")
               (let
                  ((ids
                     (append
                        (map (λ (x) (ref x 1)) todo)
                        (map (λ (x) (ref x 1)) done))))
                  (tc
                     (cons
                        [id (λ () (cont ids))]
                        todo)
                     done state)))

            ; 9, send mail
            ; return id of thread if delievered or not
            (λ (id cont to msg todo done state tc)
               ; (system-stderr "interop 9 - mail")

               ; send a normal mail
               ; return addressee if ok, #false otherwise
               (let*((ok? state waked (deliver-mail state to [id msg]))
                     (todo (if ok?
                              (cons [id (λ () (cont to))] todo)
                              (cons [id (λ () (cont #f))] todo))))
                  (if waked
                     (tc (cons* (car todo) waked (cdr todo)) done state)
                     (tc todo done state))))

            ; 10, breaked - call signal handler (unused)
            (λ (id a b c todo done state thread-controller)
               ; (system-println "interop 10 - break")
               (let ((all-threads (cons [id a] (append todo done))))
                  ;; tailcall signal handler and pass controller to allow resuming operation
                  ((get state signal-tag signal-halt) ; default to standard mcp
                     all-threads state thread-controller)))

            ; 11, reset mcp state (usually means exit from mcp repl)
            (λ (id cont threads state xtodo xdone xstate tc)
               ; (system-println "interop 11 - swapping mcp state")
               (tc threads null state))

            ; 12, set break action (unused)
            (λ (id cont choice x todo done state tc)
               ; (system-println "interop 12 - set break action")
               (tc
                  (cons [id (λ () (cont #true))] todo)
                  done (put state signal-tag choice)))

            ; 13, look for mail in my inbox at state
            (λ (id cont foo nonblock? todo done state tc)
               ; (system-println "interop 13 - look for mail in my inbox at state")
               (lets ((valp queue (quncons (get state id qnull) #false)))
                  (cond
                     (valp      ;; envelope popped from inbox
                        (tc (cons [id (λ () (cont valp))] todo) done
                           (ff-update state id queue)))
                     (nonblock? ;; just tell there is no mail with #false
                        (tc (cons [id (λ () (cont #false))] todo) done state))
                     (else      ;; leave thread continuation waiting
                        (tc todo done (put state id cont))))))

            ; 14, memory limit was exceeded
            (λ (id a b c todo done state tc)
               (system-println "interop 14 - memlimit exceeded, dropping a thread")
               ; for now, kill the currently active thread (a bit dangerous)
               (drop-delivering todo done state id
                  [id ['crash 'memory-limit b c]] tc))

            ; 15, drop local thread
            (λ (id cont target c todo done state tc)
               ; (system-println "interop 15 - drop local thread")
               (drop-thread target
                  (cons [id (λ () (cont ['killing target]))] todo)
                  done state [target ['killed-by id]] tc))

            ; 16, wrap the whole world to a thunk
            (λ (id cont path c todo done state tc)
               ; (system-println "interop 16 - wrap the whole world to a thunk")
               (let
                  ((resume
                     (vm:new type-constructor (λ (args)
                        (tc (cons [id (λ () (cont 'resumed))] todo)
                           done state)))))
                  (tc (cons [id (λ () (cont resume))] todo) done state)))

            ; 17, catch or release a running thread (not touching mailbox etc)
            (λ (id cont catch? info todo done state tc)
               ;; (system-println "interop 17 - catch or release a running thread (not touching mailbox etc)")
               (if catch?
                  (lets
                     ((all (append todo done))
                      (val all (catch-thread all info)))
                     (tc
                        (cons [id (λ () (cont val))] all)
                        null state))
                  (tc
                     (cons* [id (λ () (cont 'released))] info todo)
                     done state)))

            ; 18, get a list of currently running thread ids
            (λ (id cont b c todo done state tc)
               ; (system-println "interop 18 - get a list of currently running thread ids")
               (lets
                  ((grab (λ (l n) (cons (ref n 1) l)))
                   (ids (fold grab (fold grab null todo) done)))
                  (tc (cons [id (λ () (cont (cons id ids)))] todo) done state)))

            ; 19, set return value proposal
            (λ (id cont b c todo done state tc)
               ; (system-println "interop 19 - set return value proposal")
               (tc (cons [id (λ () (cont b))] todo) done (put state return-value-tag b)))

            ; 20, start profiling, removed
            #false
            ; 21, end profiling, removed
            #false
            ; 22, nestable parallel computation (deprecated, removed)
            #false
      
            ; 23, link thread (if you forgot "-linked")
            (λ (id cont target c todo done state tc)
               (lets
                  ((links (get state link-tag empty))
                   (followers (get links target #n))
                   (links
                     (if (memq id followers)
                        links
                        (put links target (cons id followers)))))
                  (tc ;tc
                     (cons [id (λ () (cont target))] todo)
                     done
                     (put state link-tag links))))

      ])

      ;; todo: add deadlock detection here (and other bad terminal waits)
      (define (thread-return-value state)
         (get state return-value-tag 0))

      (define (bytecode-of thing default)
         (cond
            ((bytecode? thing) thing)
            ((function? thing) (bytecode-of (ref thing 1) default))
            (else default)))

      ;; store profiling info about this call
      ;; the exec is either a thunk to be run in a thread as a result of
      ;; forking or a interop being answered, or a vm-generated vector which
      ;; has arguments for the next function call and the function at the
      ;; *last* slot of the vector.

      (define (update-state state exec)
         (if (eq? (type exec) type-vector) ;; vm thread suspensions are vectors
            (lets
               ((bcode (bytecode-of (ref exec (size exec)) 'not-a-function)) ;; identify place based in bytecode which is inert
                (prof (get state 'prof #false))
                (count (get prof bcode 0))
                (prof (put prof bcode (+ count 1)))
                (state (ff-update state 'prof prof)))
               state)
            ;; don't record anything for now for the rare thread starts and resumes with interop results
            state))


      (define (thread-controller todo done state)
         ;; (print-to stderr "(thread-controller " todo " - " done " + " state)
         (if (null? todo)
            (if (null? done)
               (thread-return-value state)  ;; nothing left to run, TODO: use last one code if no "shutdown" code
               (thread-controller done #null state))    ;; new scheduler round
         else
            (let*((this todo todo)
                  (id st this))
               (lets ((op a b c (vm:run st thread-quantum)))
                  (if (eq? op 1)
                     (thread-controller todo (cons [id a] done) state)
                     ((ref mcp-syscalls op) id a b c todo done state thread-controller))))))

      (define (start-thread-controller threads)
         (thread-controller threads #null {}))

      ;; ;; signal handler which kills the 'repl-eval thread if there, or repl
      ;; ;; if not, meaning we are just at toplevel minding our own business.
      ;; (define (repl-signal-handler threads state controller)
      ;;    (if (first (λ (x) (eq? (ref x 1) 'repl-eval)) threads #false)
      ;;       ;; there is a thread evaling user input, linkely gone awry somehow
      ;;       (drop-thread 'repl-eval threads null state eval-break-message controller)
      ;;       ;; nothing evaling atm, exit owl
      ;;       (signal-halt threads state controller)))
))
