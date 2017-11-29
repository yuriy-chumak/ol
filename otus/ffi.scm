; * Copyright(c) 2014 - 2017 Yuriy Chumak
; *
; * -------------------------------------
; * This program is free software;  you can redistribute it and/or
; * modify it under the terms of the GNU General Public License as
; * published by the Free Software Foundation; either version 3 of
; * the License, or (at your option) any later version.
; *
; * This program is distributed in the hope that it will be useful,
; * but WITHOUT ANY WARRANTY; without even the implied warranty of
; * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

; TODO: rename type-vptr to ffi-vptr, and all other types
; TODO: all type-* constants should be used by vm, all fft-* only by ffi
(define-library (otus ffi)
   (export
      dlopen
      dlclose
      dlsym dlsym+
      ffi uname

      RTLD_LAZY
      RTLD_NOW
      RTLD_BINDING_MASK
      RTLD_NOLOAD
      RTLD_DEEPBIND
      RTLD_GLOBAL
      RTLD_LOCAL
      RTLD_NODELETE


      type-short ; 16-bit integer
      type-int   ; 32-bit integer

      ; special "variable length on different platforms" type
      ; windows, ia32: 4 bytes
      ; windows, ia64: 4 bytes
      ; linux, ia32:   4 bytes
      ; linux, ia64:   8 bytes
      ; macosx, ia32:  4 bytes
      ; macosx, ia64:  8 bytes

      type-int16
      type-int32
      type-int64 ; 64-bit integer

      type-integer
      fft-float
      fft-double

      fft-void fft-void* fft-void**

      fft-unknown
      type-callable
      fft-any


      load-dynamic-library
      ; по-поводу calling convention:
      ; под Windows дефолтный конвеншен - __stdcall, под линукс - __cdecl
      ;  пока что пусть остается так.
      __stdcall __cdecl __fastcall

      int32->ol


      ; platforem independent types
      fft-int16 fft-int16* fft-int16& ; signed 16-bit value
      fft-int32 fft-int32* fft-int32& ; signed 32-bit value
      fft-int64 fft-int64* fft-int64& ; signed 64-bit value

      fft-uint16 fft-uint16* fft-uint16& ; unsigned 16-bit value
      fft-uint32 fft-uint32* fft-uint32& ; unsigned 32-bit value
      fft-uint64 fft-uint64* fft-uint64& ; unsigned 64-bit value

      ; c-like defaults
      ;fft-char  fft-signed-char  fft-unsigned-char
      fft-short fft-signed-short fft-unsigned-short
      fft-int   fft-signed-int   fft-unsigned-int
      fft-long

      ; units
      make-32bit-array
      make-64bit-array
   )

   (import
      (otus lisp))

(begin

;; OS detection
(define (uname) (syscall 63 #f #f #f))

; принимаются типы:
; int (type-int+)
; float (type-rational)
; char* (type-string)
; void** (type-tuple)
; handle (новый тип type-handle)
;(define INTEGER type-int+)     ; todo: rename to the TINTEGER or similar
;(define FLOAT   type-rational) ; todo: same


; The MODE argument to `dlopen' contains one of the following:
(define RTLD_LAZY       #x00001); Lazy function call binding.
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
(define RTLD_NODELETE   #x01000)

; функция dlopen ищет динамическую библиотеку *name* (если она не загружена - загружает)
;  и возвращает ее уникальный handle (type-port)
(define dlopen (case-lambda
   ((name flag) (syscall 174 (if (string? name) (c-string name) name) flag      #false))
   ((name)      (syscall 174 (if (string? name) (c-string name) name) RTLD_LAZY #false))
   (()          (syscall 174 '()                                      RTLD_LAZY #false))))
(define (dlclose module) (syscall 176 module #f #f))

(define ffi (syscall 177 (dlopen) "ffi" #f))

; функция dlsym связывает название функции с самой функцией и позволяет ее вызывать
(define (dlsym+ dll name)
   (let ((function (syscall 177 dll (c-string name) #false)))
      (if function
      (lambda args
         (exec function args #false)))))

(define (dlsym  dll type name . prototype)
   ; todo: add arguments to the call of function and use as types
   ; должно быть так: если будет явное преобразование типа в аргументе функции, то пользовать его
   ; иначе использовать указанное в arguments; обязательно выводить предупреждение, если количество аргументов не
   ; совпадает (возможно еще во время компиляции)
   (let ((rtty (cons type prototype))
         (function (syscall 177 dll (c-string name) #false)))
      (if function
      (lambda args
         (exec ffi  function rtty args)))))

(define (load-dynamic-library name)
   (let ((dll (dlopen name)))
      (if dll
         (lambda (type name . prototype)
            (let ((rtty (cons type prototype))
                  (function (syscall 177 dll (c-string name) #f))) ; todo: избавиться от (c-string)
               (if function
                  (lambda args
                     (exec ffi  function rtty args))))))))


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
; default call is __stdcall for windows and __cdecl for linux (for x32)
; you can directly provide required calling convention:
(define (__cdecl    arg) (+ arg #b01000000))
(define (__stdcall  arg) (+ arg #b10000000))
(define (__fastcall arg) (+ arg #b11000000))

; а тут система типов функций, я так думаю, что проверку аргументов надо забабахать сюда?
;(define (INTEGER arg) (cons 45 arg))
;(define (FLOAT arg)   (cons 46 arg))
;(define (DOUBLE arg)  '(47 arg))

; для результата, что превышает x00FFFFFF надо использовать type-handle
; 44 - is socket but will be free
;(define type-handle 45)
; todo: (vm:cast type-constant) and start from number 1?
(define type-integer type-int+) ; deprecated
(define fft-float   46)
(define fft-double  47)
(define fft-void    48)
(define fft-void*   49)  ; same as type-vptr
(define fft-void**  (bor fft-void* #x40))

(define type-int16  51)  (define type-short type-int16) ; deprecated
(define type-int32  52)  (define type-int   type-int32) ; deprecated
(define type-int64  53)
;define type-int128 54)
;define type-int256 55)
;define type-int512 56)

(define fft-unknown 62)
(define type-callable 61)
(define fft-any 63)

; new ffi types:
(define fft-int16 51) (define fft-int16* (bor fft-int16 #x40)) (define fft-int16& (bor fft-int16 #x80))
(define fft-int32 52) (define fft-int32* (bor fft-int32 #x40)) (define fft-int32& (bor fft-int32 #x80))
(define fft-int64 53) (define fft-int64* (bor fft-int64 #x40)) (define fft-int64& (bor fft-int64 #x80))

(define fft-uint16 56) (define fft-uint16* (bor fft-uint16 #x40)) (define fft-uint16& (bor fft-uint16 #x80))
(define fft-uint32 57) (define fft-uint32* (bor fft-uint32 #x40)) (define fft-uint32& (bor fft-uint32 #x80))
(define fft-uint64 58) (define fft-uint64* (bor fft-uint64 #x40)) (define fft-uint64& (bor fft-uint64 #x80))

; platform dependent defaults
(define fft-short fft-int16)
(define fft-signed-short fft-short)
(define fft-unsigned-short fft-uint16)

(define fft-int fft-int32)
(define fft-signed-int fft-int)
(define fft-unsigned-int fft-uint16)

(define fft-long
   (cond
      ((eq? (vm:wordsize) 4)                   ; 32-bit platforms
         fft-int32)
      ((string-ci=? (ref (uname) 1) "Windows") ; 64-bit windows
         fft-int32)
      (else                          ; all other 64-bit platforms
         fft-int64)))

(define fft-ulong (+ fft-long 4)) ; hack

; -- utils ----------------------------

(define (make-32bit-array len)
   (map (lambda (_) 16777216) (repeat #f len)))

(define (make-64bit-array len)
   (map (lambda (_) 72057594037927936) (repeat #f len)))

; -- convertors -----------------------
(define int32->ol (case (vm:endianness)
   (1 (lambda (vector offset)
         (+     (ref vector offset)
            (<< (ref vector (+ offset 1)) 8)
            (<< (ref vector (+ offset 2)) 16)
            (<< (ref vector (+ offset 3)) 24))))
   (else
      (print "Unknown endianness")
      #false)))

; see also: http://www.boost.org/doc/libs/1_55_0/libs/predef/doc/html/predef/reference/boost_os_operating_system_macros.html
))