(define-library (otus async)
   (export
      coroutine actor
      async await sleep
      link

      mail wait-mail check-mail
      wait-mail-from

      ; 'linked' means "it mails to owner a message with a final status of the thread"
      coroutine-linked
      actor-linked
      async-linked await-linked
      
      ; other threading functions
      running-threads single-thread? kill
      exit-thread shutdown

      ; * internal functions
      start-nested-parallel-computation wrap-the-whole-world-to-a-thunk)

   (import
      (src vm)
      (scheme core)
      (lang error))

   (begin
      ; list of interop codes can be found in lang/threading as mcp-syscalls
      (define (mcp op a b)
         (call/cc (λ (resume) (vm:mcp resume op a b))))

      ; 2 = exit from coroutine
      (define (exit-thread value) ; todo: rename to exit-coroutine
         (mcp 2 value value))

      ; 4 = create a thread
      (define (coroutine name thunk)
         (mcp 4 (list name) thunk))

      (define (coroutine-linked name thunk)
         (mcp 4 (list name 'link) thunk))

      ; the thread should have a mailbox for communication in state
      (define (actor name handler)
         (mcp 4 (list name 'mailbox) handler))
      
      (define (actor-linked name handler)
         (mcp 4 (list name 'mailbox 'link) handler))

      ; (return-mails), * internal
      (define (return-mails rmails)
         (mcp 6 rmails rmails))

      ; am i the only thread?
      (define (single-thread?)
         (mcp 7 #true #true))

      ; get running thread ids
      (define (running-threads)
         (mcp 8 #false #false))

      ; send a mail
      (define (mail to msg)
         (mcp 9 to msg))

      ; drop a thread
      (define (kill id) ; todo: rename
         (mcp 15 id #false))

      ;; (define (catch-thread id)
      ;;    (mcp 17 #true id))

      ;; (define (release-thread thread)
      ;;    (mcp 17 #false thread))

      ; exit current thread and make a proposal to the exit code
      ; todo: Stop all threads and exit program with proposed value
      (define (shutdown value)
         (mcp 19 value value) ;; set exit value proposal in thread scheduler
         (exit-thread value) value) ;; stop self and leave the rest (io etc) running to completion

      (define (wrap-the-whole-world-to-a-thunk a b)
         (mcp 16 a b))
      (define (start-nested-parallel-computation a b)
         (mcp 22 a b))

      (define (link id)
         (mcp 23 id id))

      (define (wait-mail)  (mcp 13 #false #false))
      (define (check-mail) (mcp 13 #false #true))

      (define (accept-mail pred)
         (let loop ((this (wait-mail)) (rev-spam '()))
            (cond
               ((pred this)
                  (return-mails rev-spam) ; return the other mails to mailbox as such
                  this)
               (else
                  (loop (wait-mail) (cons this rev-spam))))))

      (define (wait-mail-from name)
         (accept-mail (λ (e) (eq? (ref e 1) name))))



      (define async (case-lambda
         ((thunk) (coroutine [] thunk))
         ((name thunk)
                  (coroutine name thunk))))

      ; forker wants to receive any issues the thread runs into
      (define async-linked
         (case-lambda
            ((name thunk)
               (coroutine-linked name thunk))
            ((thunk)
               (coroutine-linked [] thunk))))

      ; Message passing (aka mailing) is asynchronous, and at least
      ; in a one-core environment order-preserving. interact is like
      ; mail, but it blocks the thread until the desired response
      ; arrives. Messages are of the form #(<sender-id> <message>).

      ; example: (await (mail 'who ['a 'message]))
      (define (await name)
         (if name
            (ref (wait-mail-from name) 2)))

      (define (await-linked name)
         (define answer (await name))
         (case answer
            ;; evaluated, typical behavior (ok, fail)
            (['finished result]
               result)

            ; (VM::FAIL ...), vm pushed an error
            (['crash opcode a b]
               (runtime-error "vm error" (verbose-ol-error #e opcode a b)))

            ; (runtime-error ...)
            ; note, these could easily be made resumable if continuation
            (['error code reason clarification]
               (runtime-error "ol error" (verbose-ol-error #e code reason clarification)))
            (else is foo
               (runtime-error "unknown error" foo))))


      ;; (define coroutine actor)
      ;; (define coroutine-linked actor-linked)

      ; sleep

      ; number of microseconds to sleep for real at a time when no threads are running but
      ; they want to sleep, typically waiting for input or output
      (define us-per-round 10000) ; 10 ms
      (define (set-ticker-value n) (syscall 1022 n))

      (define sleep (case-lambda
         (() (set-ticker-value 0))
         ((rounds)
            (let loop ((rounds rounds))
               (set-ticker-value 0)
               (if (eq? rounds 0)
                  rounds
               else
                  (if (single-thread?)
                     (syscall 35 us-per-round)) ; syscall 'sleep', TODO: remove this, use "wait n" instead
                  (let* ((rounds _ (vm:sub rounds 1)))
                     (loop rounds))))) ))

))
