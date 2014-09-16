;; todo: date handling

(define-library (owl pinvoke)

   (export 
      dlopen
      dlsym ;dlsym-c
      
;      INTEGER FLOAT
      type-handle type-float type-void
   )

   (import
      (owl defmac)
      (owl io)
      (owl math)
      (owl string))

   (begin

; принимаются типы:
; int (type-int+)
; float (type-rational)
; char* (type-string)
; void** (type-tuple)
; handle (новый тип type-handle)
;(define INTEGER type-int+)     ; todo: rename to the TINTEGER or similar
;(define FLOAT   type-rational) ; todo: same


; функция dlopen ищет динамическую библиотеку *name* (если она не загружена - загружает)
;  и возвращает ее уникальный handle
(define (dlopen name flag) (sys-prim 30 (c-string name) flag #false))
; функция dlsym связывает название функции с самой функцией и позволяет ее вызывать 
(define (dlsym  dll type name . prototype)
;  (print "dlsym: " name)
   ; todo: add arguments to the call of function and use as types
   ; должно быть так: если будет явное преобразование типа в аргументе функции, то пользовать его
   ; иначе использовать указанное в arguments; обязательно выводить предупреждение, если количество аргументов не
   ; совпадает (возможно еще во время компиляции)
   (let ((rtty (cons type prototype))
         (function (sys-prim 31 dll (c-string name) #false))) ; todo: избавиться от (c-string)
      (lambda args
;        (print "pinvoke: " name)
         (sys-prim 32 function args rtty))))
;; dlsym-c - аналог dlsym, то с правилом вызова __cdecl         
;;(define (dlsym-c type dll name . prototype)
;;; todo: отправлять тип функции третим параметром (sys-prim 31) и в виртуальной машине
;;;   возвращать структуру с (byte-vector адрес-функции адрес-вызыватора-с-соответвующей-конвенцией) ? 
;;   (let ((function (cons '((bor type 64) . prototype) (sys-prim 31 dll (c-string name) #false)))) ; todo: избавиться от (c-string)
;;;;;(let ((function (cons (bor type 64) (sys-prim 31 dll (c-string name) #false)))) ; todo: переделать 64 во что-то поприятнее
;;      (lambda args ;  function       type          ;arguments
;;         (sys-prim 32 (cdr function) (car function) args))))

; а тут система типов функций, я так думаю, что проверку аргументов надо забабахать сюда?
;(define (INTEGER arg) (cons 45 arg))
;(define (FLOAT arg)   (cons 46 arg))
;(define (DOUBLE arg)  '(47 arg))

; для результата, что превышает x00FFFFFF надо использовать type-handle
(define type-handle 45)
(define type-float  type-rational)
;(define type-double 47) ; пока нету, но возможно будет
(define type-void   48)

))
