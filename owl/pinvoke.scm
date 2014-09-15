;; todo: date handling

(define-library (owl pinvoke)

   (export 
      dlopen
      dlsym dlsym-c
      
      INTEGER FLOAT
      type-handle type-float type-void
   )

   (import
      (owl defmac)
      (owl io)
      (owl math)
      (owl string))

   (begin

; функция dlopen ищет динамическую библиотеку *name* (если она не загружена - загружает)
;  и возвращает ее уникальный handle
(define (dlopen name flag) (sys-prim 30 (c-string name) flag #false))
; функция dlsym (todo: rename to dlsym) связывает название функции с самой функцией и позволяет ее вызывать 
(define (dlsym  type dll name . arguments)
   ; todo: add arguments to the call of function and use as types
   ; должно быть так: если будет явное преобразование типа в аргументе функции, то пользовать его
   ; иначе использовать указанное в arguments; обязательно выводить предупреждение, если количество аргументов не
   ; совпадает (возможно еще во время компиляции)
   (let ((function (cons type (sys-prim 31 dll (c-string name) #false)))) ; todo: избавиться от (c-string)
      (lambda args
         (sys-prim 32 (cdr function) (car function) args))))
; dlsym-c - аналог dlsym, то с правилом вызова __cdecl         
(define (dlsym-c type dll name)
; todo: отправлять тип функции третим параметром (sys-prim 31) и в виртуальной машине
;   возвращать структуру с (byte-vector адрес-функции адрес-вызыватора-с-соответвующей-конвенцией) ? 
   (let ((function (cons (bor type 64) (sys-prim 31 dll (c-string name) #false)))) ; todo: переделать 64 во что-то поприятнее
      (lambda args ;  function       type          ;arguments
         (sys-prim 32 (cdr function) (car function) args))))

; а тут система типов функций, я так думаю, что проверку аргументов надо забабахать сюда?
(define (INTEGER arg) (cons 45 arg))
(define (FLOAT arg)   (cons 46 arg))
;(define (DOUBLE arg)  '(47 arg))

; для результата, что превышает x00FFFFFF надо использовать type-handle
(define type-handle 45)
(define type-float  46)
;(define type-double 47) ; пока нету, но возможно будет
(define type-void   48)

))
