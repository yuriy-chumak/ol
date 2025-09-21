;
; Copyright(c) 2014 - 2021 Yuriy Chumak
; --------------------------------------------------------------
; This program is free software;  you can redistribute it and/or
; modify it under the terms of the GNU General Public License as
; published by the Free Software Foundation; either version 3 of
; the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
; --------------------------------------------------------------

; TODO: rename type-vptr to ffi-vptr, and all other types
; TODO: all type-* constants should be used by vm, all fft-* only by ffi
(define-library (otus ffi)
   (export
      ; low level primitives (someone may be interested to use)
      dlopen dlclose dlerror dlsym
      ; high level primitive
      load-dynamic-library


      ffi uname *uname*

      make-vptr
      make-callback

      RTLD_LAZY
      RTLD_NOW
      RTLD_BINDING_MASK
      RTLD_NOLOAD
      RTLD_DEEPBIND
      RTLD_GLOBAL
      RTLD_LOCAL
      RTLD_NODELETE

      NULL nullptr

      ; olvm callable type
      type-callable

      fft-void fft-void* fft-void**

      fft-bool
      fft-unknown
      fft-any



      ; по-поводу calling convention:
      ; под Windows дефолтный конвеншен - __stdcall, под линукс - __cdecl
      ;  пока что пусть остается так.
      __stdcall __cdecl __fastcall

       ; structures declaration
      struct

      ; type and value convertors
      fft* ; make c-like pointer from type
      fft& ; make c-like reference from type

      ; platforem independent types
      fft-int8  fft-int8*  fft-int8&
      fft-int16 fft-int16* fft-int16& ; signed 16-bit value
      fft-int32 fft-int32* fft-int32& ; signed 32-bit value
      fft-int64 fft-int64* fft-int64& ; signed 64-bit value

      fft-uint8  fft-uint8*  fft-uint8&
      fft-uint16 fft-uint16* fft-uint16& ; unsigned 16-bit value
      fft-uint32 fft-uint32* fft-uint32& ; unsigned 32-bit value
      fft-uint64 fft-uint64* fft-uint64& ; unsigned 64-bit value

      ; c-like defaults
      fft-char  fft-signed-char  fft-unsigned-char
      fft-short fft-signed-short fft-unsigned-short
      fft-int   fft-signed-int   fft-unsigned-int

      fft-int*  fft-int&

      fft-long  fft-signed-long  fft-unsigned-long
      fft-long-long fft-signed-long-long fft-unsigned-long-long
      fft-size-t ; deprecated
      fft-size_t

      fft-enum

      fft-float
      fft-double

      ; fft data constructors
      make-32bit-array
      make-64bit-array
      make-vptr-array

      ; utility functions
      sizeof

      ; fft data manipulation helpers
      vptr->string
      vptr->bool
      vptr->value

      vptr->bytevector
      bytevector->void*

      bytevector->int16
      bytevector->uint16
      bytevector->int32
      bytevector->uint32
      bytevector->int64
      bytevector->uint64

      bytevector->float
      bytevector->double
   )

   (import
      (src vm)
      (otus lisp))

(begin

;; OS detection
(define (uname) (syscall 63))
(define *uname* (syscall 63))

;; (define (execve function . args) (apply function args)) ; syscall disabled

; The MODE argument to `dlopen' contains one of the following:
(define RTLD_LAZY         #x1) ; Lazy function call binding.
(define RTLD_NOW          #x2) ; Immediate function call binding.
(define RTLD_BINDING_MASK #x3) ; Mask of binding time value.
(define RTLD_NOLOAD       #x4) ; Do not load the object.
(define RTLD_DEEPBIND     #x8) ; Use deep binding.

; If the following bit is set in the MODE argument to `dlopen',
; the symbols of the loaded object and its dependencies are made
; visible as if the object were linked directly into the program.
(define RTLD_GLOBAL     #x100)

; Unix98 demands the following flag which is the inverse to RTLD_GLOBAL.
; The implementation does this by default and so we can define the
; value to zero.
(define RTLD_LOCAL 0)

; Do not delete object when closed.
(define RTLD_NODELETE  #x1000)

; функция dlopen ищет динамическую библиотеку *name* (если она не загружена - загружает)
;  и возвращает ее уникальный handle (type-port)
(define dlopen (case-lambda
   ((name flag) (syscall 174 (if (string? name) (c-string name) name) flag))
   ((name)      (syscall 174 (if (string? name) (c-string name) name) RTLD_LAZY))
   (()          (syscall 174 #false                                   RTLD_LAZY))))
(define (dlclose module) (syscall 176 module))
(define (dlerror)        (syscall 178))

; функция dlsym связывает название функции с самой функцией и позволяет ее вызывать
; внимание! приведение типов к С-like НЕ ПРОИЗВОДИТСЯ!
; функция низкоуровневая и предназначена в первую очередь для расширения языка
(define (dlsym dll name)
   (syscall 177 dll (c-string name)))

; olvm ffi exported functions
(define ffi (dlsym (dlopen) "OLVM_ffi"))
(unless ffi
   (runtime-error "assertion error: ol built with no ffi support, please check a HAVE_DLOPEN and OLVM_FFI build variables."))

; smart "dlopen/dlsym"
(define (load-dynamic-library name)
   (let ((dll (dlopen name)))
      (if dll
         (lambda (type name . prototype)
            ;;; todo: отправлять тип функции третим параметром (syscall 177) и в виртуальной машине
            ;;;   возвращать структуру с (byte-vector адрес-функции адрес-вызыватора-с-соответвующей-конвенцией) ?
            ;;   (let ((function (cons '((bor type 64) . prototype) (syscall 171 dll (c-string name) #false))))
            ;;;;;(let ((function (cons (bor type 64) (dlsym dll (c-string name))))) ; todo: переделать 64 во что-то поприятнее
            ;;      (lambda args ;  function       type          ;arguments
            ;;         (syscall 59 (cdr function) (car function) args))))

            ; todo: add arguments to the call of function and use as types
            ; должно быть так: если будет явное преобразование типа в аргументе функции, то пользовать его
            ; иначе использовать указанное в arguments; обязательно выводить предупреждение, если количество аргументов не
            ; совпадает (возможно еще во время компиляции)
            (let ((rtti (cons type prototype))
                  (function (dlsym dll name)))
               (if function
                  (lambda args
                     (ffi function rtti args))))))))

(define (make-vptr) (vm:cast 0 type-vptr))

(define ffi:mkcb (dlsym (dlopen) "OLVM_mkcb"))
(define (make-callback pinned-object)
   (ffi:mkcb pinned-object))


; Calling Conventions
; default call is __stdcall for windows and __cdecl for linux (for x32)
; you can directly provide required calling convention:
(define (__cdecl    arg) (vm:ior arg #x1000))
(define (__stdcall  arg) (vm:ior arg #x2000))
(define (__fastcall arg) (vm:ior arg #x3000))

(define-syntax struct
   (syntax-rules ()
      ((struct . stuff) (list . stuff))))

; type convertors
(define (fft* type)
   (cons 1 type))
(define (fft& type)
   (cons 2 type))

(define type-callable 61)

(define fft-float   46)
(define fft-double  47)
(define fft-void    48)
(define fft-void*   49)  ; same as type-vptr
(define fft-void** (fft* fft-void*))

(define fft-bool 60)
(define fft-unknown 62)
(define fft-any 63)

; basic fft constants
(define NULL (vm:cast 0 fft-void*))
(define nullptr NULL)

; new ffi types:
(define fft-int8  50)  (define fft-int8*  (fft* fft-int8))    (define fft-int8&  (fft& fft-int8))
(define fft-int16 51)  (define fft-int16* (fft* fft-int16))   (define fft-int16& (fft& fft-int16))
(define fft-int32 52)  (define fft-int32* (fft* fft-int32))   (define fft-int32& (fft& fft-int32))
(define fft-int64 53)  (define fft-int64* (fft* fft-int64))   (define fft-int64& (fft& fft-int64))

(define fft-uint8  55) (define fft-uint8*  (fft* fft-uint8))  (define fft-uint8&  (fft& fft-uint8))
(define fft-uint16 56) (define fft-uint16* (fft* fft-uint16)) (define fft-uint16& (fft& fft-uint16))
(define fft-uint32 57) (define fft-uint32* (fft* fft-uint32)) (define fft-uint32& (fft& fft-uint32))
(define fft-uint64 58) (define fft-uint64* (fft* fft-uint64)) (define fft-uint64& (fft& fft-uint64))

; --=( platform dependent defaults )=----
(define ffi:sizeof (dlsym (dlopen) "OLVM_sizeof"))
   (setq |char| 1)
   (setq |short| 2)
   (setq |int| 3)
   (setq |long| 4)
   (setq |long long| 5)
   (setq |size_t| 6)
   (setq |float| 10)
   (setq |double| 11)
   (setq |void*| 20)

(define fft-signed-char fft-int8)
(define fft-unsigned-char fft-uint8)
(define fft-char fft-int8)                  (assert (ffi:sizeof |char|) ===> 1)

(define fft-signed-short fft-int16)
(define fft-unsigned-short fft-uint16)
(define fft-short fft-signed-short)         (assert (ffi:sizeof |short|) ===> 2)

(define fft-signed-int fft-int32)
(define fft-unsigned-int fft-uint32)
(define fft-int fft-signed-int)             (assert (ffi:sizeof |int|) ===> 4)

; size of long depends on OS and machine word size:
; ia32/amd64: https://software.intel.com/en-us/articles/size-of-long-integer-type-on-different-architecture-and-os
; arm32/64: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.den0024a/ch08s02.html
; all, ia32: 4 bytes
; windows, ia64: 4 bytes
; linux, ia64:   8 bytes
; macosx, ia64:  8 bytes

(define fft-signed-long (case (ffi:sizeof |long|)
   (4 fft-int32)
   (8 fft-int64)
   (else (runtime-error "assertion error: unsupported native 'long' type size"))))
(define fft-unsigned-long (+ fft-signed-long 5))
(define fft-long fft-signed-long)

(define fft-signed-long-long fft-int64)
(define fft-unsigned-long-long fft-uint64)
(define fft-long-long fft-signed-long-long) (assert (ffi:sizeof |long long|) ===> 8)

(define fft-enum fft-int)

(define fft-size_t (case (ffi:sizeof |size_t|)
   (4 fft-int32)
   (8 fft-int64)
   (else (runtime-error "assertion error: unsupported native 'size_t' type size"))))

(define fft-size-t fft-size_t)

; ...
(define fft-int* (fft* fft-int))
(define fft-int& (fft& fft-int))

; -- sizeof ---------------------------
(define sizeof ffi:sizeof)

; -- utils ----------------------------

; makers
(define (make-32bit-array len)
   (map (lambda (_) 16777216) (repeat #f len)))

(define (make-64bit-array len)
   (map (lambda (_) 72057594037927936) (repeat #f len)))

(define (make-vptr-array len)
   (map (lambda (_) (make-vptr)) (repeat #f len)))

);begin

(begin

   (define ffi:idf (make-vptr))
   (define (cast x t) ; cast vptr to the type t
      (ffi ffi:idf (list t type-vptr) (list x)))

   (define (vptr->string vptr)
      (cast vptr type-string))

   (define (vptr->bool vptr)
      (cast vptr fft-bool))

   (define (vptr->value vptr type)
      (cast vptr type))

   (define (bytevector->int16 bvec offset)
      (cast (cons bvec offset) (fft& fft-int16)))
   (define (bytevector->uint16 bvec offset)
      (cast (cons bvec offset) (fft& fft-uint16)))
   (define (bytevector->int32 bvec offset)
      (cast (cons bvec offset) (fft& fft-int32)))
   (define (bytevector->uint32 bvec offset)
      (cast (cons bvec offset) (fft& fft-uint32)))
   (define (bytevector->int64 bvec offset)
      (cast (cons bvec offset) (fft& fft-int64)))
   (define (bytevector->uint64 bvec offset)
      (cast (cons bvec offset) (fft& fft-uint64)))

   (define (bytevector->float bvec offset)
      (cast (cons bvec offset) (fft& fft-float)))
   (define (bytevector->double bvec offset)
      (cast (cons bvec offset) (fft& fft-double)))

   (define (bytevector->void* bvec offset)
      (let ((void* (make-vptr)))
         (vm:set! void* 0 bvec offset (+ offset (size void*)))))

   (define (vptr->bytevector vptr sizeof)
      (syscall 9 vptr sizeof))

)

; notification for ",save":
(begin
   (define notification-text (vm:new type-constructor (lambda (args)
      (print "You restored session with ffi enabled.")
      (print "All ffi handles became invalid. Be careful!"))))
   (define (notification) notification-text)
))
