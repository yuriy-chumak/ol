; todo: оформить как отдельный модуль и переименовать
; need to check name as string and flag as integer
; 
;not working yet - (import (owl win32))

(define (dlopen name flag) (sys-prim 1030 (c-string name) flag #false))
(define (dlsym  type dll name) ; todo: переименовать в get-proc-address ?
   (let ((function (cons type (sys-prim 1031 dll (c-string name) #false))))
      (lambda args
         (sys-prim 1032 (cdr function) (car function) args))))

(define user32_dll (dlopen "user32" 0))
  (define IDOK 1)
  (define IDCANCEL 2)

(define MessageBox (dlsym type-fix+ user32_dll "MessageBoxA"))
  (define MB_OK 0)
  (define MB_OKCANCEL 1)
  (define MB_ICONASTERISK 64)

(if (=
  (MessageBox 0 (c-string "Please, press OK for test pass!") (c-string "dlopen test")
    (bor MB_OKCANCEL MB_ICONASTERISK))
  IDOK)
    (print "OK")
    (print "CANCEL"))
; todo: вроде бы все строки и так заканчиваются на '\0' - проверить
;(define echo "echo server")



; в момент импорта сделать все нужные привязки
; export (MessageBox)  и т.д.
