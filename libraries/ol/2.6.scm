(define-library (ol 2.6)
   (version 2.6)
   (license MIT/LGPL3)
   (comment "compat 2.6 layer")
(import
   (scheme core))

(export
   ;; internal type names
   type-string-dispatch
   type-enum+ type-enum-
   type-int+ type-int-

   ;; ...
)

(begin
   (define type-string-dispatch type-superstring)

   (define type-enum+ type-value+)
   (define type-enum- type-value-)
   (define type-int+ type-integer+)
   (define type-int- type-integer-)

))
