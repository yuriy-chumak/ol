(define-library (r5rs full)
(import
   (r5rs core)
   (r5rs srfi-1)  ; List Library
   (r5rs srfi-87) ; => in case clauses
   (r5rs characters)
;      (r5rs strings)
)
(export
   (exports (r5rs core))
   (exports (r5rs srfi-1))
   (exports (r5rs srfi-87))

   (exports (r5rs characters))
)
      
(begin #true))