; 4.2.6 Dynamic bindings
(define-library (scheme dynamic-bindings)
(export
   make-parameter
   parameterize)
(import
   (scheme core)
   (owl ff) (otus async))
(begin
   (setq coname '|4.2.6 Dynamic bindings|)
   (coroutine coname (lambda ()
      (let loop ((bindings #empty))
         (let*((envelope (wait-mail))
               (sender msg envelope))
            ; only vectors allowed
            (let ((index (ref msg 1)))
               (if (not index)   ; #false means "add new parameter"
                  (let*((index _ (vm:add (bindings 0 0) 1))
                        (bindings (put bindings 0 index)))
                     (mail sender index)
                     (loop (put bindings index (cons (ref msg 2) (ref msg 3)))))
               else
                  ; otherwise set parameter value and return old
                  (let*((value (bindings index #false))
                        (converter (cdr value)))
                     (mail sender (car value))
                     (if (eq? (size msg) 1) ; only return value
                        (loop bindings)
                     else
                        (loop (put bindings index
                           (if converter
                              (cons (converter (ref msg 2)) converter)
                              (cons (ref msg 2) #false))))))))))))

   (define make-parameter (begin
      (define (return index)
         (case-lambda
            ((); just return value
               (await (mail coname [index])))
            ((new)
               (await (mail coname [index new])))))
      (case-lambda
         ((init)
            (return (await (mail coname [#false init]))))
         ((init converter)
            (return (await (mail coname [#false (converter init) converter])))))))

   (define-syntax parameterize
      (syntax-rules ()
         ((parameterize ((var val) . rest-bindings) exp . rest-exps)
            (let ((old (var val)) ; save var
                  (value (parameterize rest-bindings exp . rest-exps)))
               (var old) ; restore var value
               value))
         ((parameterize ()) exp)
         ((parameterize () exp . rest)
            ((lambda () exp . rest)))))
))