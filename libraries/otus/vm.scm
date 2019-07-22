(define-library (otus vm)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol vm))
   (description "
      Otus-Lisp Virtual Machine management")

   (export 
      set-ticker-value
      wait
      set-memory-limit
      get-memory-limit)

   (import
      (scheme core))

   (begin
      ;; make thread sleep for a few thread scheduler rounds
      (define (set-ticker-value n) (syscall 1022 n))
      (define (wait n) ; is it required?
         (if (eq? n 0)
            0
            (let* ((n _ (vm:sub n 1)))
               (set-ticker-value 0)
               (wait n))))

      ;; special things exposed by the vm
      (define (set-memory-limit n) (syscall 1007 n))
      (define (get-memory-limit)   (syscall 1009))
))