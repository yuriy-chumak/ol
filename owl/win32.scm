(define-library (owl win32)
   (export
      MessageBox
        MB_OK MB_OKCANCEL
   )

   (import (owl defmac)
           (owl string)) ;; reload default macros needed for defining libraries etc

   (begin
      ; dlopen == load-library
      ; dlsym == get-proc-address
      (define (dlopen name flag) (sys-prim 30 (c-string name) flag #false))
      (define (dlsym  type dll name)
         (let ((function (cons type (sys-prim 31 dll (c-string name) #false))))
            (lambda args
               (sys-prim 32 (cdr function) (car function) args))))


      ; user32
      (define user32_dll (dlopen "user32" 0))

      (define MessageBox (dlsym type-fix+ user32_dll "MessageBoxA"))
        (define MB_OK 0)
        (define MB_OKCANCEL 1)

   ))