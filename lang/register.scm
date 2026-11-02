;;;
;;; Register allocation
;;;

; Earlier compilation steps produce RTL that has an unbounded set
; of registers. Register allocation handles limiting them to the
; number available in VM, while also trying to retarget operations
; to more sensible registers.

(define-library (lang register)

   (export
      allocate-registers)

   (import
      (scheme base)
      (owl ff)
      (owl math)
      (owl list-extra)
      (owl io)
      (owl list)
      (scheme misc))

   (begin
      (define (allocate-registers rtl) rtl)

   ))
