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
      dlsym

      RTLD_LAZY
      RTLD_NOW
      RTLD_BINDING_MASK
      RTLD_NOLOAD
      RTLD_DEEPBIND
      RTLD_GLOBAL
      RTLD_LOCAL
      RTLD_NODELETE
      
      type-float type-double type-void
      
      ; по-поводу calling convention:
      ; под Windows дефолтный конвеншен - __stdcall, под линукс - __cdecl
      ;  пока что пусть остается так.
      __stdcall __cdecl __fastcall
      
      ;*PLATFORM*
;     *OS_WINDOWS* *OS_LINUX* *OS_ANDROID* *OS_MACOS*
      *OS* ; todo: решить, что лучше - одна переменная или много
           ; с одной можно сделать (case *OS* (...))
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
(define (dlopen name flag) (sys-prim 1030 name flag #false))

; The MODE argument to `dlopen' contains one of the following:
(define RTLD_LAZY	#x00001)	; Lazy function call binding.
(define RTLD_NOW	#x00002)	; Immediate function call binding.
(define	RTLD_BINDING_MASK   #x3)	; Mask of binding time value.
(define RTLD_NOLOAD	#x00004)	; Do not load the object.
(define RTLD_DEEPBIND	#x00008)	; Use deep binding.

; If the following bit is set in the MODE argument to `dlopen',
; the symbols of the loaded object and its dependencies are made
; visible as if the object were linked directly into the program.
(define RTLD_GLOBAL	#x00100)

; Unix98 demands the following flag which is the inverse to RTLD_GLOBAL.
; The implementation does this by default and so we can define the
; value to zero.
(define RTLD_LOCAL	0)

; Do not delete object when closed.
(define RTLD_NODELETE	#x01000)


; функция dlsym связывает название функции с самой функцией и позволяет ее вызывать 
(define (dlsym  dll type name . prototype)
;  (print "dlsym: " name)
   ; todo: add arguments to the call of function and use as types
   ; должно быть так: если будет явное преобразование типа в аргументе функции, то пользовать его
   ; иначе использовать указанное в arguments; обязательно выводить предупреждение, если количество аргументов не
   ; совпадает (возможно еще во время компиляции)
   (let ((rtty (cons type prototype))
         (function (sys-prim 1031 dll (c-string name) #false))) ; todo: избавиться от (c-string)
      (lambda args
;        (print "pinvoke: " name)
         (sys-prim 1032 function args rtty))))
;; dlsym-c - аналог dlsym, то с правилом вызова __cdecl         
;;(define (dlsym-c type dll name . prototype)
;;; todo: отправлять тип функции третим параметром (sys-prim 1031) и в виртуальной машине
;;;   возвращать структуру с (byte-vector адрес-функции адрес-вызыватора-с-соответвующей-конвенцией) ? 
;;   (let ((function (cons '((bor type 64) . prototype) (sys-prim 1031 dll (c-string name) #false)))) ; todo: избавиться от (c-string)
;;;;;(let ((function (cons (bor type 64) (sys-prim 1031 dll (c-string name) #false)))) ; todo: переделать 64 во что-то поприятнее
;;      (lambda args ;  function       type          ;arguments
;;         (sys-prim 1032 (cdr function) (car function) args))))

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

;; OS detection
(define (null? x) (eq? x '()))
; see also: http://www.boost.org/doc/libs/1_55_0/libs/predef/doc/html/predef/reference/boost_os_operating_system_macros.html
(define *OS_AIX* #f)    ; http://en.wikipedia.org/wiki/AIX_operating_system
(define *OS_AMIGAOS* #f); http://en.wikipedia.org/wiki/AmigaOS
(define *OS_ANDROID* #f); http://en.wikipedia.org/wiki/Android_%28operating_system%29
(define *OS_BEOS* #f)   ; http://en.wikipedia.org/wiki/BeOS
(define *OS_BSD* #f)    ; http://en.wikipedia.org/wiki/Berkeley_Software_Distribution
(define *OS_CYGWIN* #f) ; http://en.wikipedia.org/wiki/Cygwin
(define *OS_HPUX* #f)   ; http://en.wikipedia.org/wiki/HP-UX
(define *OS_IRIX* #f)   ; http://en.wikipedia.org/wiki/Irix
(define *OS_LINUX* #f)  ; http://en.wikipedia.org/wiki/Linux
(define *OS_MACOS* #f)  ; http://en.wikipedia.org/wiki/Mac_OS
(define *OS_OS400* #f)  ; http://en.wikipedia.org/wiki/IBM_i
(define *OS_QNX*   #f)  ; http://en.wikipedia.org/wiki/QNX
(define *OS_SOLARIS* #f); http://en.wikipedia.org/wiki/Solaris_Operating_Environment
(define *OS_UNIX* #f)   ; http://en.wikipedia.org/wiki/Unix
(define *OS_OS_SVR4* #f); http://en.wikipedia.org/wiki/UNIX_System_V
(define *OS_VMS* #f)    ; http://en.wikipedia.org/wiki/Vms
(define *OS_WINDOWS* #f);     (not (null? (dlopen (c-string "kernel32") 0))))   ; http://en.wikipedia.org/wiki/Category:Microsoft_Windows
(define *OS_BSD_BSDI* #f); http://en.wikipedia.org/wiki/BSD/OS
(define *OS_BSD_DRAGONFLY* #f); http://en.wikipedia.org/wiki/DragonFly_BSD
(define *OS_BSD_FREE* #f); http://en.wikipedia.org/wiki/Freebsd
(define *OS_BSD_NET* #f); http://en.wikipedia.org/wiki/Netbsd
(define *OS_BSD_OPEN* #f); http://en.wikipedia.org/wiki/Openbsd

(define *OS*
  (if *OS_WINDOWS* 1
  (if *OS_LINUX*   2
  (if *OS_ANDROID* 3
  (if *OS_MACOS*   4)))))

))