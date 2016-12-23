(define type-record            5) ; reference, ?


      (define (record? x) (eq? type-record (type x)))

      (define-syntax _record-values
         (syntax-rules (emit find)
            ((_record-values emit tag mk pred () fields tail)
               (values tag mk pred . tail))
            ((_record-values emit tag mk pred (x ... (field accessor)) fields tail)
               ;; next must cons accessor of field to tail, so need to lookup its position
               (_record-values find tag mk pred (x ...) fields tail field fields (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
            ((_record-values find tag mk pred left fields tail key (key . rest) (pos . poss))
               (_record-values emit tag mk pred left fields ((lambda (x) (ref x pos)) . tail)))
            ((_record-values find tag mk pred left fields tail key (x . rest) (pos . poss))
               (_record-values find tag mk pred left fields tail key rest poss))
            ((_record-values find tag mk pred left fields tail key () (pos . poss))
               (syntax-error "Not found in record: " key))
            ((_record-values find tag mk pred left fields tail key (x . rest) ())
               (syntax-error "Implementation restriction: add more offsets to define-record-type macro" tag))))

      (define-syntax define-record-type
         (syntax-rules (emit)
            ((define-record-type name (constructor fieldname ...) pred (field accessor) ...)
               (define-values
                  (name constructor pred accessor ...)
                  (let ((tag (quote name))) ; ← note, not unique after redefinition, but atm seems useful to get pattern matching
                     (_record-values emit
                        tag
                        (lambda (fieldname ...) (vm:new type-record tag fieldname ...))
                        (lambda (ob) (eq? tag (ref ob 1)))
                        ((field accessor) ...) (fieldname ...) ()))))))

(define-record-type pare (kons x y)
   pare?
   (x kar)
   (y kdr))

(define x (kons 'foo 'bar))

(if (record? x)
   (print x))

(print
   (let ((x (kons 'lemon 'curry)))
      (if (pare? x)
         (kdr x)
         'chili)))

(define-record-type trire (tri x y z)
  tri?
  (y tri2)
  (z tri3)
  (x tri1))

(let ((x (tri 11 22 33)))
   (print
      (list
         (tri1 x)
         (tri2 x)
         (tri3 x))))
