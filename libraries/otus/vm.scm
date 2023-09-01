(define-library (otus vm)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol vm))
   (description "
      Otus-Lisp Virtual Machine management")

   (export 
      set-ticker-value
      set-memory-limit
      get-memory-limit)

   (import
      (scheme core))

   (begin
      ;; make thread sleep for a few thread scheduler rounds
      (define (set-ticker-value n) (syscall 1022 n))

      ;; special things exposed by the vm
      (define (set-memory-limit n) (syscall 1007 n))
      (define (get-memory-limit)   (syscall 1009))
))