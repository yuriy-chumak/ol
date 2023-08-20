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
      start-thread-controller)

   (import
      (scheme base)
      (only (src vm) vm:run)
      (owl queue)
      (otus async)
      (owl ff)
      (owl list)
      (owl math)
      (owl string)
      (owl render)
      (lang env)
      (only (lang eval) print-repl-error)
      (lang error)
      (owl io))

   (begin
      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      (define poll-tag "mcp/polls")
      (define buffer-tag "mcp/buffs")
      (define link-tag "mcp/links")
      (define signal-tag "mcp/break")

      (define (signal-halt threads state controller)
         (print-to stderr "stopping on signal")
         (halt 42)) ;; exit owl with a specific return value
      (define thread-quantum 10000)

      (define (bad-interop id a b c todo done state)
         (system-println "mcp: got bad interop")
         (values todo done state))

      ; -> state x #false|waked-thread
      (define (deliver-mail state to envelope)
         (let ((st (get state to #false)))
            (cond
               ((pair? st) ;; currently working, leave a mail to inbox queue
                  (values
                     (ff-update state to (qsnoc envelope st))
                     #false))
               ((not st) ;; no such thread, or just no inbox
                  (system-stderr "ol: dropping envelope to missing thread: ")
                  (system-stderr (bytes->string (render to '(#\newline))))
                  (system-stderr "    envelope: ")
                  (system-stderr (bytes->string (render envelope '(#\newline))))
                  (values state #false))
               (else ;; activate the state function
                  (values
                     (ff-update state to qnull) ;; leave an inbox
                     [to (λ () (st envelope))]))))) ;; activate it

      (define (deliver-messages todo done state subs msg tc)
         (if (null? subs)
            (tc todo done state)
            (lets ((state waked (deliver-mail state (car subs) msg)))
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
                  ((error crashed)
                     (vector-apply (ref msg 2)
                        (lambda (state code reason clarification)
                           (print-repl-error
                              (verbose-ol-error code reason clarification))))))
                     
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
            ; id: name of thread
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
                     [id ['crashed a b c]] tc)))

            ; 4, fork
            (λ (id cont opts thunk todo done state tc)
               ; (system-println "mcp: interop 4 -- fork")
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
               (let ((todo (cons [id (λ () (cont to))] todo)))
                  ; send a normal mail
                  (lets ((state waked (deliver-mail state to [id msg])))
                     (if waked
                        (tc (cons* (car todo) waked (cdr todo)) done state)
                        (tc todo done state)))))

            ; 10, breaked - call signal handler
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

            ; 12, set break action
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

            ;; todo: switch memory limit to a hard one in ovm.c
            ; 14, memory limit was exceeded
            (λ (id a b c todo done state tc)
               (system-println "interop 14 - memlimit exceeded, dropping a thread")
               ; for now, kill the currently active thread (a bit dangerous)
               (drop-delivering todo done state id
                  [id ['crashed 'memory-limit b c]] tc))

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
               (system-println "interop 17 - catch or release a running thread (not touching mailbox etc)")
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

            ;;; 20 & 21 change during profiling

            ; 20, start profiling, no-op during profiling returning 'already-profiling
            #null
            ;(λ (id cont b c todo done state tc)
            ;   ; (system-println "interop 20 - start profiling")
            ;   (tc (cons [id (λ () (cont 'already-profiling))] todo) done state))
            ;
            ; 21, end profiling, resume old ones, pass profiling info
            #null
            ;(λ (id cont b c todo done state tc)
            ;   ; (system-println "interop 21 - end profiling, resume old ones")
            ;   (lets
            ;      ((prof (get state 'prof #false)) ;; ff storing profiling info
            ;       (tc (get prof 'tc #false))      ;; normal thread scheduler
            ;       (prof (del prof 'tc)))         ;; give just the collected data to thread
            ;      (tc (cons [id (λ () (cont prof))] todo) done
            ;         (del state 'prof))))

            ; 22, nestable parallel computation
            (λ (id cont opts c todo done state tc)
               ; (system-println "interop 22 - nestable parallel computation")
               (lets
                  ((por-state [cont opts c]))
                  (tc (cons [id por-state] todo) done state)))
      ])

      ;; todo: add deadlock detection here (and other bad terminal waits)
      (define (halt-thread-controller state)
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

;      (define mcp-syscalls
;         (lets
;            ((syscalls mcp-syscalls-during-profiling)
;             (syscalls
;               (set-ref syscalls 20
;                  (λ (id cont b c todo done state tc)
;                     ;; make a new thread scheduler using the other interop set
;                     (define (scheduler self todo done state)
;                        (if (eq? todo null)
;                           (if (null? done)
;                              (halt-thread-controller state)
;                              (self self done null state))
;                           (lets
;                              ((this todo todo)
;                               (id st this)
;                               (state (update-state state st))
;                               (op a b c (run st 0))) ; looks like never called?
;                              ;(print "run returns: ")
;                              (if (eq? op 1)
;                                 ; out of time, usual suspect, short path here
;                                 (self self todo (cons [id a] done) state)
;                                 ((ref mcp-syscalls-during-profiling op) id a b c todo done state self))))) ; <- difference here
;
;                     (scheduler scheduler (cons [id (λ () (cont 'started-profiling))] todo) done
;                        (put state 'prof           ;; profiling data is stored under key 'prof
;                           (put empty 'tc tc)))))) ;; store normal scheduler there for resuming on interop 21
;             (syscalls
;               (set-ref syscalls 21 ;; end-profiling interop doesn't do anything when not profiling
;                  (λ (id cont b c todo done state tc)
;                     (tc tc (cons [id (λ () (cont 'not-profiling-you-fool))] todo) done state)))))
;            syscalls))

;      (define (enter-mcp controller threads state)
;         ; could break here also when threads is just repl-input
;         (controller controller
;            (list
;               ['mcp
;                  (λ ()
;                     ((get state signal-tag signal-halt) ; exit by default
;                        threads state controller))])
;            null empty))


      ; (enter-mcp thread-controller done state) -- no way to go here without the poll, rethink that

      ;; nested thread steps cause
      ;;    - exit and false -> forget todo & done
      ;;    - crash -> forget

      ;; st=#(cont todo done) → finished? st'
      ;; finished? = #f -> new state but no result yet, #t = result found and state is thunk, else error
      ;; TODO: downgrade to single state and prune useless such nodes from tree whenever # of options is reduced down to 1
      (define (step-parallel st)
         (lets ((cont todo done st))
            (if (null? todo)
               (if (null? done)
                  ;; no options left
                  (values #true (λ () (cont null)))
                  ;; rewind the track, spin the record and take it back
                  (step-parallel [cont done null]))
               (lets ((state todo todo))
                  (if (eq? (type state) type-vector)
                     (lets ((fini state (step-parallel state)))
                        (if (eq? fini null)
                           ;; crashed, propagate
                           (values null state)
                           ;; either par->single or single->value state change,
                           ;; but consumed a quantum already so handle it in next round
                           (values #false [cont todo (cons state done)])))
                     (lets ((op a b c (vm:run state thread-quantum)))
                        ;(print (list "run returns: " op a b c))
                        (cond
                           ((eq? op 1) ;; out of time, a is new state
                              (values #false
                                 [cont todo (cons a done)]))
                           ((eq? op 2) ;; finished, return value and thunk to continue computation
                              (values #true
                                 (λ () (cont (cons a (λ () (start-nested-parallel-computation todo done)))))))
                           ((eq? op 22) ;; start nested parallel computation
                              (lets ((contp a) (todop b) (donep c)
                                     (por-state [contp todop donep]))
                                 (values #false
                                    [cont todo (cons por-state done)])))
                           (else
                              ;; treat all other reasons and syscalls as errors
                              (print "bad interop op within par: " op)
                              (values null [op a b c])))))))))

      (define (thread-controller todo done state)
         (if (eq? todo null)
            (if (null? done)
            then
               ;; (print-to stderr "nothing left to run: " state)
               (halt-thread-controller state)  ;; nothing left to run, TODO: use last one code if no "shutdown" code
            else
               (thread-controller done null state))    ;; new scheduler round
         else
            (lets
               ((this todo todo)
                (id st this))
               (if (eq? (type st) type-vector)
                  ;; parallel or node, hunt next slice to run and proceed
                  (lets ((fini stp (step-parallel st)))
                     (cond
                        ((not fini)
                           ;; no result found yet but options remaining - keep on truckin
                           (thread-controller todo (cons [id stp] done) state))
                        ((eq? fini #true)
                           ;; some result was found and stp is a thunk to proceed computation
                           (thread-controller todo (cons [id stp] done) state))
                        (else
                           ;; TODO: something failed and stp is an error code. time to crash.
                           (print "GONDOR!")
                           "GONDOR!")))
                  (lets ((op a b c (vm:run st thread-quantum)))
                     (if (eq? op 1)
                        (thread-controller todo (cons [id a] done) state)
                        ((ref mcp-syscalls op) id a b c todo done state thread-controller)))))))

      (define (start-thread-controller threads)
         (thread-controller threads #null #empty))

      ;; ;; signal handler which kills the 'repl-eval thread if there, or repl
      ;; ;; if not, meaning we are just at toplevel minding our own business.
      ;; (define (repl-signal-handler threads state controller)
      ;;    (if (first (λ (x) (eq? (ref x 1) 'repl-eval)) threads #false)
      ;;       ;; there is a thread evaling user input, linkely gone awry somehow
      ;;       (drop-thread 'repl-eval threads null state eval-break-message controller)
      ;;       ;; nothing evaling atm, exit owl
      ;;       (signal-halt threads state controller)))
))
