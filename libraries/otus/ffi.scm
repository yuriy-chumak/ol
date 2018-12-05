;
; Copyright(c) 2014 - 2018 Yuriy Chumak
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


      ffi sizeof uname *uname*

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

      fft-float
      fft-double

      fft-void fft-void* fft-void**

      fft-unknown
      fft-any



      ; по-поводу calling convention:
      ; под Windows дефолтный конвеншен - __stdcall, под линукс - __cdecl
      ;  пока что пусть остается так.
      __stdcall __cdecl __fastcall

      ; type and value convertors
      fft* ; make c-like pointer from type
      fft& ; make c-like reference from type

      int32->ol   ; fft-void* -> int32 number ; temp

      ; platforem independent types
      fft-int8  fft-int8*  fft-int8&
      fft-int16 fft-int16* fft-int16& ; signed 16-bit value
      fft-int32 fft-int32* fft-int32& ; signed 32-bit value
      fft-int64 fft-int64* fft-int64& ; signed 64-bit value

      fft-uint16 fft-uint16* fft-uint16& ; unsigned 16-bit value
      fft-uint32 fft-uint32* fft-uint32& ; unsigned 32-bit value
      fft-uint64 fft-uint64* fft-uint64& ; unsigned 64-bit value

      ; c-like defaults
      fft-char  fft-signed-char  fft-unsigned-char
      fft-short fft-signed-short fft-unsigned-short
      fft-int   fft-signed-int   fft-unsigned-int

      fft-int*  fft-int&

      ; special "variable length on different platforms" type 'long'
      ; windows, ia32: 4 bytes
      ; windows, ia64: 4 bytes
      ; linux, ia32:   4 bytes
      ; linux, ia64:   8 bytes
      ; macosx, ia32:  4 bytes
      ; macosx, ia64:  8 bytes
      fft-long  fft-signed-long  fft-unsigned-long
      fft-long-long fft-signed-long-long fft-unsigned-long-long

      fft-enum

      ; fft data constructors
      make-32bit-array
      make-64bit-array
      make-vptr-array

      ; function for boxing and unboxing single values
      box unbox

      ; fft data manipulation helpers
      vptr->vector vptr->string
      extract-void*
      extract-number
   )

   (import
      (src vm)
      (otus lisp))

(begin

;; OS detection
(define (uname) (syscall 63 #f #f #f))
(define *uname* (syscall 63 #f #f #f))

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
   ((name flag) (syscall 174 (if (string? name) (c-string name) name) flag      #f))
   ((name)      (syscall 174 (if (string? name) (c-string name) name) RTLD_LAZY #f))
   (()          (syscall 174 #false                                   RTLD_LAZY #f))))
(define (dlclose module) (syscall 176 module #f #f))
(define (dlerror)        (syscall 178 #false #f #f))

(define ffi (syscall 177 (dlopen) "OL_ffi" #f))

; функция dlsym связывает название функции с самой функцией и позволяет ее вызывать
; внимание! приведение типов к С-like НЕ ПРОИЗВОДИТСЯ!
; функция низкоуровневая и предназначена в первую очередь для расширения языка
(define (dlsym dll name)
   (let ((function (syscall 177 dll (c-string name) #false)))
      (if function
      (lambda args
         (exec function args #false)))))


(define (load-dynamic-library name)
   (let ((dll (dlopen name)))
      (if dll
         (lambda (type name . prototype)
            ;;; todo: отправлять тип функции третим параметром (syscall 177) и в виртуальной машине
            ;;;   возвращать структуру с (byte-vector адрес-функции адрес-вызыватора-с-соответвующей-конвенцией) ?
            ;;   (let ((function (cons '((bor type 64) . prototype) (syscall 171 dll (c-string name) #false)))) ; todo: избавиться от (c-string)
            ;;;;;(let ((function (cons (bor type 64) (syscall 177 dll (c-string name) #false)))) ; todo: переделать 64 во что-то поприятнее
            ;;      (lambda args ;  function       type          ;arguments
            ;;         (syscall 59 (cdr function) (car function) args))))

            ; todo: add arguments to the call of function and use as types
            ; должно быть так: если будет явное преобразование типа в аргументе функции, то пользовать его
            ; иначе использовать указанное в arguments; обязательно выводить предупреждение, если количество аргументов не
            ; совпадает (возможно еще во время компиляции)
            (let ((rtti (cons type prototype))
                  (function (syscall 177 dll (c-string name) #f))) ; todo: избавиться от (c-string)
               (if function
                  (lambda args
                     (exec ffi function rtti args))))))))

(define (make-vptr) (vm:cast 0 type-vptr))

(define mkcb (syscall 177 (dlopen) "OL_mkcb" #f))
(define (make-callback pinned-object)
   (exec mkcb pinned-object))


; Calling Conventions
; default call is __stdcall for windows and __cdecl for linux (for x32)
; you can directly provide required calling convention:
(define (__cdecl    arg) (vm:or arg #x1000))
(define (__stdcall  arg) (vm:or arg #x2000))
(define (__fastcall arg) (vm:or arg #x3000))

; type convertors
(define (fft* type)
   (vm:or type #x10000))
(define (fft& type)
   (vm:or type #x20000))

; а тут система типов функций, я так думаю, что проверку аргументов надо забабахать сюда?
;(define (INTEGER arg) (cons 45 arg))
;(define (FLOAT arg)   (cons 46 arg))
;(define (DOUBLE arg)  '(47 arg))

; для результата, что превышает x00FFFFFF надо использовать type-handle
; 44 - is socket but will be free
;(define type-handle 45)
; todo: (vm:cast type-constant) and start from number 1?
(define type-callable 61)

(define fft-float   46)
(define fft-double  47)
(define fft-void    48)
(define fft-void*   49)  ; same as type-vptr
(define fft-void** (fft* fft-void*))

(define fft-unknown 62)
(define fft-any 63)

; basic fft constants
(define NULL (vm:cast 0 fft-void*))
(define nullptr NULL)

; new ffi types:
(define fft-int8  50)  (define fft-int8*  type-string)       (define fft-int8&  type-string)
(define fft-int16 51)  (define fft-int16* (fft* fft-int16))  (define fft-int16& (fft& fft-int16))
(define fft-int32 52)  (define fft-int32* (fft* fft-int32))  (define fft-int32& (fft& fft-int32))
(define fft-int64 53)  (define fft-int64* (fft* fft-int64))  (define fft-int64& (fft& fft-int64))

(define fft-uint8  55) (define fft-uint8*  type-string)       (define fft-uint8&  type-string)
(define fft-uint16 56) (define fft-uint16* (fft* fft-uint16)) (define fft-uint16& (fft& fft-uint16))
(define fft-uint32 57) (define fft-uint32* (fft* fft-uint32)) (define fft-uint32& (fft& fft-uint32))
(define fft-uint64 58) (define fft-uint64* (fft* fft-uint64)) (define fft-uint64& (fft& fft-uint64))

; platform dependent defaults
(define fft-char fft-int8)
(define fft-signed-char fft-int8)
(define fft-unsigned-char fft-int8)

(define fft-short fft-int16)
(define fft-signed-short fft-int16)
(define fft-unsigned-short fft-uint16)

(define fft-int fft-int32)
(define fft-signed-int fft-int32)
(define fft-unsigned-int fft-uint32)

(define fft-int* (fft* fft-int))
(define fft-int& (fft& fft-int))

; long:
(setq wordsize (size nullptr))
(define fft-long
   (cond
      ((eq? wordsize 4)              ; 32-bit platforms
         fft-int32)
      ((string-ci=? (ref (uname) 1) "Windows") ; 64-bit windows
         fft-int32)
      (else                          ; all other 64-bit platforms
         fft-int64)))
(define fft-signed-long fft-long)
(define fft-unsigned-long (+ fft-long 5))

(define fft-long-long fft-int64)
(define fft-signed-long-long fft-int64)
(define fft-unsigned-long-long fft-uint64)

(define fft-enum fft-int)

; -- sizeof ---------------------------
(define (sizeof type)
   (case type
      (fft-int8  1)
      (fft-uint8  1)
      (fft-int16 2)
      (fft-uint16 2)
      (fft-int32 4)
      (fft-uint32 4)
      (fft-int64 8)
      (fft-uint64 8)
      (fft-float 4)
      (fft-double 8)
      (fft-void* (size nullptr))
      (else (if (list? type)
               (fold (lambda (f el) (+ f (sizeof el))) 0 type)))))

; -- utils ----------------------------

(define (make-32bit-array len)
   (map (lambda (_) 16777216) (repeat #f len)))

(define (make-64bit-array len)
   (map (lambda (_) 72057594037927936) (repeat #f len)))

(define (make-vptr-array len)
   (map (lambda (_) (vm:cast 0 fft-void*)) (repeat #f len)))

; -- convertors -----------------------
(setq endianness
   (let ((x (vm:cast 1 type-vptr)))
      (cond
         ((eq? (ref x 0) 1)
            'little-endian)
         (else
            2)))) ; todo: fix THIS


(define int32->ol (case endianness
   ('little-endian (lambda (vector offset)
         (+     (ref vector    offset   )
            (<< (ref vector (+ offset 1))  8)
            (<< (ref vector (+ offset 2)) 16)
            (<< (ref vector (+ offset 3)) 24))))
   (else (lambda (vector offset)
         (+     (ref vector (+ offset 3))
            (<< (ref vector (+ offset 2))  8)
            (<< (ref vector (+ offset 1)) 16)
            (<< (ref vector    offset   ) 24))))))

(define int64->ol (case endianness
   ('little-endian (lambda (vector offset)
         (+     (ref vector    offset   )
            (<< (ref vector (+ offset 1))  8)
            (<< (ref vector (+ offset 2)) 16)
            (<< (ref vector (+ offset 3)) 24)
            (<< (ref vector (+ offset 4)) 32)
            (<< (ref vector (+ offset 5)) 40)
            (<< (ref vector (+ offset 6)) 48)
            (<< (ref vector (+ offset 7)) 56))))))

; boxing/unboxing
(define (box value)
   (list value))
(define (unbox list)
   (car list))

;(vptr->vector vptr sizeof-in-bytes)
(define vptr->vector (cond
   ; linux:
   ((or
      (string-ci=? (ref *uname* 1) "Linux")
      (string-ci=? (ref *uname* 1) "Android"))
      (let ((memcpy ((load-dynamic-library #false) fft-void "memcpy" fft-void* fft-void* fft-unsigned-int)))
         (lambda (vptr sizeof)
            (let ((vector (make-bytevector sizeof)))
               (memcpy vector vptr sizeof)
               vector))))
   ; win:
   ((string-ci=? (ref *uname* 1) "Windows")
      (lambda (vptr sizeof)
         (let ((vector (make-bytevector sizeof)))
            ; ...
            vector)))
   ; asm.js:
   ((string-ci=? (ref *uname* 1) "emscripten")
      (let ((memcpy ((load-dynamic-library #false) fft-void "memcpy" fft-void* fft-void* fft-unsigned-int)))
         (lambda (vptr sizeof)
            (let ((vector (make-bytevector sizeof)))
               (memcpy vector vptr sizeof)
               vector))))
   (else
      (print "Unknown OS"))))

(define (extract-void* vector offset)
   ;; TODO:
   ;; (let ((void* (make-vptr)))
   ;;    (map (lambda (i j) ; for-each
   ;;          (print (ref vector j)))
   ;;          ;(set-ref! void* i (ref vector j)))
   ;;       (iota (size void*) 0)
   ;;       (iota (size void*) offset))
   ;;    void*))
   (vm:cast
      (fold (lambda (val offs)
               (+ (<< val 8) (ref vector offs)))
         0 (reverse (iota (size nullptr) offset)))
      type-vptr))

(define (extract-number vector offset length)
   (let ((number
            (fold (lambda (val offs)
                     (+ (<< val 8) (ref vector offs)))
               0 (reverse (iota length offset))))
         (max (<< 1 (* 8 length))))
      (if (<= number (>> max 1))
         number
         (- number max))))

(define (vptr->string vptr)
   (fold string-append "#x"
      (map (lambda (i)
            (let ((hex "0123456789abcdef"))
               (list->string (list
                  (ref hex (>> (ref vptr i) 4))
                  (ref hex (band (ref vptr i) 15))))))
         (reverse (iota wordsize))))) ; todo: use (vm:endiannes)



; see also: http://www.boost.org/doc/libs/1_55_0/libs/predef/doc/html/predef/reference/boost_os_operating_system_macros.html
))
