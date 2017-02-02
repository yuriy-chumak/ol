(define-library (owl interop)

   (export
      interact fork accept-mail wait-mail check-mail
      exit-owl release-thread catch-thread set-signal-action
      single-thread? kill mail fork-linked-server fork-server
      return-mails fork-server fork-linked fork-named exit-thread exit-owl
      poll-mail-from running-threads par*
      start-nested-parallel-computation wrap-the-whole-world-to-a-thunk ; ??
      par por* por interop)

   (import
      (r5rs core))

   (begin
      ;; спецоперация для общения с менеджером внутренних потоков (todo: переименовать)
      (define sys (vm:raw type-bytecode '(27 4 5 6 7 8  24 8)))


      (define (interop op a b)
         (call/cc (λ (resume) (sys resume op a b))))

      (define (exit-thread value)
         (interop 2 value value))

      ;; 3 = vm thrown error
      (define (fork-named name thunk)
         (interop 4 (list name) thunk))

      (define (fork-linked name thunk)
         (interop 4 (list name 'link) thunk))

      (define (fork-server name handler)
         (interop 4 (list name 'mailbox) handler))

      (define (return-mails rmails)
         (interop 6 rmails rmails))

      (define (fork-linked-server name handler)
         (interop 4 (list name 'mailbox 'link) handler))

      (define (running-threads)
         (interop 8 #false #false))

      (define (mail id msg)
         (interop 9 id msg))

      (define (kill id)
         (interop 15 id #false))

      (define (single-thread?)
         (interop 7 #true #true))

      (define (set-signal-action choice)
         (interop 12 choice #false))

      (define (catch-thread id)
         (interop 17 #true id))

      (define (release-thread thread)
         (interop 17 #false thread))

      (define (exit-owl value)
         (interop 19 value value) ;; set exit value proposal in thread scheduler
         (exit-thread value))     ;; stop self and leave the rest (io etc) running to completion

      ;; (executable ...) → (first-value . rest-ll) | (), or crash if something crashes in them
      (define (par* ts)
         (interop 22 ts '()))


      (define (wrap-the-whole-world-to-a-thunk a b)
         (interop 16 a b))
      (define (start-nested-parallel-computation a b)
         (interop 22 a b))
      ;; macro for calling from code directly
      (define-syntax par
         (syntax-rules ()
            ((par exp ...)
               (par* (list (λ () exp) ...)))))

      (define (por* ts)
         (let loop ((rss (par* ts)))
            (cond
               ((null? rss) #false)
               ((car rss) => (λ (result) result))
               (else (loop ((cdr rss)))))))

      (define-syntax por
         (syntax-rules ()
            ((por exp ...)
               (por* (list (λ () exp) ...)))))

      (define (wait-mail)           (interop 13 #false #false))
      (define (check-mail)          (interop 13 #false #true))

      (define (accept-mail pred)
         (let loop ((this (wait-mail)) (rev-spam '()))
            (cond
               ((pred this)
                  (return-mails rev-spam) ; return the other mails to mailbox as such
                  this)
               (else
                  (loop (wait-mail) (cons this rev-spam))))))

      ;; wait mail from given thread for a while, giving other threads time (or sleeping) while waiting
      ;; todo: could interact with the sleeper thread to allow vm go to sleep between rounds

      (define (return-from-wait value spam)
         (if (null? spam)
            value
            (begin
               (return-mails spam)
               value)))

      (define (poll-mail-from id rounds default)
         (let loop ((envp (check-mail)) (spam '()) (rounds rounds))
            (cond
               ((not envp)
                  (if (eq? rounds 0)
                     (return-from-wait default spam)
                     ;; no mail, request a thread switch and recurse, at which point all other threads have moved
                     (begin
                        ;(set-ticker 0) ;; FIXME restore this when librarized
                        ;; no bignum math yet at this point
                        (lets ((rounds _ (vm:sub rounds 1)))
                           (loop (check-mail) spam rounds)))))
               ((eq? (ref envp 1) id)
                  ;; got it
                  (return-from-wait (ref envp 2) spam))
               (else
                  ;; got spam, keep waiting
                  (loop (check-mail) (cons envp spam) rounds)))))


      (define (fork thunk)
         ; the tuple is fresh and therefore a proper although rather
         ; nondescriptive thread name
         (fork-named (tuple 'anonimas) thunk))


      ; Message passing (aka mailing) is asynchronous, and at least
      ; in a one-core environment order-preserving. interact is like
      ; mail, but it blocks the thread until the desired response
      ; arrives. Messages are of the form #(<sender-id> <message>).

      (define (interact whom message)
         (mail whom message)
         (ref (accept-mail (λ (env) (eq? (ref env 1) whom))) 2))

))
