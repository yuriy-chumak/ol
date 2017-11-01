; todo: оформить как отдельный модуль и переименовать
; need to check name as string and flag as integer
; 
;not working yet - (import (owl win32))

(define (dlopen name) (sys-prim 174 (c-string name) 1 #false))
(define (dlsym  type dll name) ; todo: переименовать в get-proc-address ?
   (let ((rtty (cons type prototype))
         (function (syscall 177 dll (c-string name) #false)))
      (if function
      (lambda args
         (exec ffi function rtty args)))))

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
