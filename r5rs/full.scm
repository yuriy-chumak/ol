(define-library (r5rs full)
(import
   (scheme core)
   (r5rs srfi-1)  ; List Library
   (r5rs characters)
;      (r5rs strings)
)
(export
   (exports (scheme core))
   (exports (r5rs srfi-1))

   (exports (r5rs characters))
)
      
(begin #true))