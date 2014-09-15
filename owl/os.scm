(define-library (owl os)
   (export
      *OS*
   )

   (import
      (owl defmac) (owl io)
      (owl pinvoke))
   (begin

; временные константы, пока не придумаю как лучше закодировать
; 1 - windows
; 2 - linux
; 3 - macos
(define *OS* 1) ; windows
;  (if (null? (dlopen "kernel32" 0)) 0 1))

))
