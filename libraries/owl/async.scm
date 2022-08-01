(define-library (owl async)

   (export
      fork fork-named
      fork-linked fork-server
      fork-linked-server

      interact
      par par* por por*
      (exports (otus async)))

   (import
      (src vm)
      (scheme core)
      (otus async))

   (begin
      ; list of interop codes can be found in lang/threading as mcp-syscalls
      (define (mcp op a b)
         (call/cc (λ (resume) (vm:mcp resume op a b))))

      (define fork async)
      (define fork-named async-named)
      (define fork-linked async-linked)
      (define fork-server actor)
      (define fork-linked-server actor-linked)

      ;; (executable ...) → (first-value . rest-ll) | (), or crash if something crashes in them
      (define (par* ts)
         (mcp 22 ts '()))

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

      ; Message passing (aka mailing) is asynchronous, and at least
      ; in a one-core environment order-preserving. interact is like
      ; mail, but it blocks the thread until the desired response
      ; arrives. Messages are of the form #(<sender-id> <message>).

      (define (interact whom message) ; deprecated
         (await (mail whom message)))

))
