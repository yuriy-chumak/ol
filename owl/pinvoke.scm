; Platform Invoke library

;;; Copyright (c) 2014, Yuriy Chumak
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. Use in source and binary forms are not permitted in projects under
;;;    GNU General Public Licenses and its derivatives.
;;;
;;; THIS SOFTWARE IS PROVIDED BY ART OBREZAN ''AS IS'' AND ANY
;;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL ART OBREZAN BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;
; using example:
;(import (owl pinvoke))
;(define user32 (dlopen "user32" 0))
;  (define IDOK 1)
;  (define IDCANCEL 2)
;
;  (define MessageBox (dlsym user32 type-fix+ "MessageBoxA" type-int+ type-string type-string type-int+))
;    (define MB_OK 0)
;    (define MB_OKCANCEL 1)
;    (define MB_ICONASTERISK 64)
;
;(if (=
;  (MessageBox 0 "Please, press OK for test pass!" (c-string "PInvoke sample use")
;    (bor MB_OKCANCEL MB_ICONASTERISK))
;  IDOK)
;    (print "OK")
;    (print "CANCEL")))


;; todo: date handling

(define-library (owl pinvoke)
   (export 
      dlopen
      dlclose
      dlsym dlsym+
      pinvoke exec
      uname

      RTLD_LAZY
      RTLD_NOW
      RTLD_BINDING_MASK
      RTLD_NOLOAD
      RTLD_DEEPBIND
      RTLD_GLOBAL
      RTLD_LOCAL
      RTLD_NODELETE

      ; todo: rename type-float to type-float32 and type-double to type-float64
      type-float type-double type-void type-void* type-void** ; type-vptr, type-vptr* ?
      load-dynamic-library

      ; по-поводу calling convention:
      ; под Windows дефолтный конвеншен - __stdcall, под линукс - __cdecl
      ;  пока что пусть остается так.
      __stdcall __cdecl __fastcall

;      (define type-memp             62) ; value
;      (define type-userdata         62)
;      (define type-void             48) ; no return pinvoke value
;      (define type-void*            49)  ; 

   )

   (import
      (r5rs core)
      (owl io)
      (owl math)
      (owl primop) ; exec
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


; The MODE argument to `dlopen' contains one of the following:
(define RTLD_LAZY	      #x00001); Lazy function call binding.
(define RTLD_NOW        #x00002); Immediate function call binding.
(define RTLD_BINDING_MASK   #x3); Mask of binding time value.
(define RTLD_NOLOAD     #x00004); Do not load the object.
(define RTLD_DEEPBIND   #x00008); Use deep binding.

; If the following bit is set in the MODE argument to `dlopen',
; the symbols of the loaded object and its dependencies are made
; visible as if the object were linked directly into the program.
(define RTLD_GLOBAL     #x00100)

; Unix98 demands the following flag which is the inverse to RTLD_GLOBAL.
; The implementation does this by default and so we can define the
; value to zero.
(define RTLD_LOCAL      0)

; Do not delete object when closed.
(define RTLD_NODELETE	#x01000)

; функция dlopen ищет динамическую библиотеку *name* (если она не загружена - загружает)
;  и возвращает ее уникальный handle (type-port)
(define dlopen (case-lambda
   ((name flag) (syscall 174 (if (string? name) (c-string name) name) flag      #false))
   ((name)      (syscall 174 (if (string? name) (c-string name) name) RTLD_LAZY #false))
   (()          (syscall 174 '()                                      RTLD_LAZY #false))))
(define (dlclose module) (syscall 176 module #f #f))

(define pinvoke (syscall 177 (dlopen) "pinvoke" #f))

; функция dlsym связывает название функции с самой функцией и позволяет ее вызывать
(define (dlsym+ dll name)
   (let ((function (syscall 177 dll (c-string name) #false)))
      (if function
      (lambda args
         (exec function args #false)))))

(define (dlsym  dll type name . prototype)
;  (print "dlsym: " name)
   ; todo: add arguments to the call of function and use as types
   ; должно быть так: если будет явное преобразование типа в аргументе функции, то пользовать его
   ; иначе использовать указанное в arguments; обязательно выводить предупреждение, если количество аргументов не
   ; совпадает (возможно еще во время компиляции)
   (let ((rtty (cons type prototype))
         (function (syscall 177 dll (c-string name) #false)))
      (if function
      (lambda args
         (exec pinvoke  function rtty args)))))

(define (load-dynamic-library name)
   (let ((dll (dlopen name)))
      (if dll
         (lambda (type name . prototype)
            (let ((rtty (cons type prototype))
                  (function (syscall 177 dll (c-string name) #f))) ; todo: избавиться от (c-string)
               (if function
                  (lambda args
                     (exec pinvoke  function rtty args))))))))


;(define (dlsym+ dll type name . prototype) (dlsym dll type name 44 prototype))
;; dlsym-c - аналог dlsym, то с правилом вызова __cdecl
;;(define (dlsym-c type dll name . prototype)
;;; todo: отправлять тип функции третим параметром (syscall 177) и в виртуальной машине
;;;   возвращать структуру с (byte-vector адрес-функции адрес-вызыватора-с-соответвующей-конвенцией) ? 
;;   (let ((function (cons '((bor type 64) . prototype) (syscall 171 dll (c-string name) #false)))) ; todo: избавиться от (c-string)
;;;;;(let ((function (cons (bor type 64) (syscall 177 dll (c-string name) #false)))) ; todo: переделать 64 во что-то поприятнее
;;      (lambda args ;  function       type          ;arguments
;;         (syscall 59 (cdr function) (car function) args))))

; Calling Conventions
(define (__stdcall  arg) (+ arg   0)) ; __stdcall is default for Windows
(define (__cdecl    arg) (+ arg  64))
(define (__fastcall arg) (+ arg 128))

; а тут система типов функций, я так думаю, что проверку аргументов надо забабахать сюда?
;(define (INTEGER arg) (cons 45 arg))
;(define (FLOAT arg)   (cons 46 arg))
;(define (DOUBLE arg)  '(47 arg))

; для результата, что превышает x00FFFFFF надо использовать type-handle
; 44 - is socket but will be free
;(define type-handle 45)
(define type-float  46) ;was: type-rational)
(define type-double 47) ; пока нету, но возможно будет
(define type-void   48)
(define type-void*  49)
(define type-void** 113) ; #x40 + 49

;; OS detection
(define (uname) (syscall 63 #f #f #f))

; see also: http://www.boost.org/doc/libs/1_55_0/libs/predef/doc/html/predef/reference/boost_os_operating_system_macros.html
))