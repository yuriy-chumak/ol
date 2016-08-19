;;; SQLite3 interface for Otus Lisp
;;; https://github.com/yuriy-chumak/OL
;;; http://www.sqlite.org

;;; Copyright (c) 2014, 2016 Yuriy Chumak
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

(define-library (lib sqlite)
   (export
    make-sqlite3 make-sqlite3-stmt

  ; constants
    SQLITE-OK SQLITE-ERROR SQLITE-BUSY SQLITE-LOCKED
    SQLITE-DONE SQLITE-ROW
    SQLITE-STATIC SQLITE-TRANSIENT
    SQLITE-INTEGER SQLITE-FLOAT SQLITE-BLOB SQLITE-NULL SQLITE-TEXT

  ; creation/destruction
    sqlite3-open
    sqlite3-close

  ; statement management
    sqlite3-prepare-v2
    sqlite3-step
    sqlite3-reset
    sqlite3-finalize
    sqlite3-last-insert-rowid
    sqlite3_changes

  ; ss
    sqlite3-bind-parameter-index
    sqlite3-bind-int
    sqlite3-bind-double
    sqlite3-bind-text

  ; result set
    sqlite3_column_type
    sqlite3_column_count
    sqlite3_column_name
    sqlite3_column_int
    sqlite3_column_bytes
    ;sqlite3_column_double
    sqlite3_column_text
    ;sqlite3_column_blob

  ; additional
    sqlite:exec
   )

   (import
      (otus lisp)
      (otus pinvoke))

(begin

(define uname (syscall 63 #f #f #f))

(define win32? (string-ci=? (ref uname 1) "Windows"))
(define linux? (string-ci=? (ref uname 1) "Linux"))

(define (new-void*) (raw type-void* '(0)))

(define % (dlopen (cond
   (win32? "sqlite3")
   (linux? "libsqlite3.so")
   (else (runtime-error "No sqlite3 library support" "Unknown platform")))))

(if (not %)
   (runtime-error "Can't load sqlite3 library." (cond
      (win32?
         "Download dll from http://www.sqlite.org/download.html")
      (linux?
         "Use, for example, sudo apt-get install sqlite3"))))

; служебные
(define (make-sqlite3)      (new-void*)) ;like void* (raw type-vector-raw '(0)))
(define (make-sqlite3-stmt) (new-void*)) ; or (list->byte-vector '(0 0 0 0)))

; todo: завести под это дело отдельный тип - что-то вроде type-int+-ref и т.д.
(define sqlite3*  type-void*)
(define sqlite3** type-void**)
(define sqlite3_stmt*  type-void*)
(define sqlite3_stmt** type-void**)
(define char** type-vector-raw)

(define sqlite3_value type-fix+)
(define sqlite3_int64 type-vector-raw)

(define SQLITE-OK 0)
(define SQLITE-ERROR 1)
(define SQLITE-BUSY 5)
(define SQLITE-LOCKED 6)

(define SQLITE-DONE 101)
(define SQLITE-ROW 100)

(define SQLITE-STATIC 0)
(define SQLITE-TRANSIENT -1)

(define SQLITE-INTEGER 1)
(define SQLITE-FLOAT 2)
(define SQLITE-BLOB 4)
(define SQLITE-NULL 5)
(define SQLITE-TEXT 3)

(define SQLITE-MISUSE 21)


; https://www.sqlite.org/c3ref/open.html
; ex: file:data.db?mode=ro&cache=private
(define sqlite3-open  (dlsym % (__cdecl type-fix+) "sqlite3_open"  type-string sqlite3**))
(define sqlite3-close (dlsym % (__cdecl type-fix+) "sqlite3_close" sqlite3*))

(define sqlite3-prepare-v2 (dlsym % (__cdecl type-fix+) "sqlite3_prepare_v2" sqlite3* type-string type-fix+ sqlite3_stmt** char**)) ; проблема с крайним параметром (char**) - надо этот результат сконвертировать снова в строку, новую
(define sqlite3-sql      (dlsym % (__cdecl type-string) "sqlite3_sql"        sqlite3_stmt*))
(define sqlite3-step       (dlsym % (__cdecl type-fix+) "sqlite3_step"       sqlite3_stmt*))
(define sqlite3-reset      (dlsym % (__cdecl type-fix+) "sqlite3_reset"      sqlite3_stmt*))
(define sqlite3-finalize   (dlsym % (__cdecl type-fix+) "sqlite3_finalize"   sqlite3_stmt*))

(define sqlite3-last-insert-rowid (dlsym % (__cdecl type-fix+) "sqlite3_last_insert_rowid" sqlite3*))
(define sqlite3_changes (dlsym % (__cdecl type-int+) "sqlite3_changes" sqlite3*))

; In the SQL statement text input to sqlite3_prepare_v2() and its variants, literals may be replaced by a parameter that matches one of following templates:
;    ? ?NNN :VVV @VVV $VVV
;
; In the templates above, NNN represents an integer literal, and VVV represents an alphanumeric identifier.
; The values of these parameters (also called "host parameter names" or "SQL parameters") can be set using the sqlite3_bind_*() routines defined here.
(define sqlite3-bind-parameter-index (dlsym % (__cdecl type-fix+) "sqlite3_bind_parameter_index" sqlite3_stmt* type-string))
(define sqlite3-bind-int    (dlsym % (__cdecl type-fix+) "sqlite3_bind_int"    sqlite3_stmt* type-int+ type-int+))
(define sqlite3-bind-double (dlsym % (__cdecl type-fix+) "sqlite3_bind_double" sqlite3_stmt* type-int+ type-double))
(define sqlite3-bind-text   (dlsym % (__cdecl type-fix+) "sqlite3_bind_text"   sqlite3_stmt* type-int+ type-string type-fix+ type-void*))

(define sqlite3_column_type  (dlsym % (__cdecl type-fix+)   "sqlite3_column_type" sqlite3_stmt* type-fix+))
(define sqlite3_column_count (dlsym % (__cdecl type-fix+)   "sqlite3_column_count" sqlite3_stmt*))
(define sqlite3_column_name  (dlsym % (__cdecl type-string) "sqlite3_column_name" sqlite3_stmt* type-fix+))
(define sqlite3_column_int   (dlsym % (__cdecl type-int+)   "sqlite3_column_int" sqlite3_stmt* type-fix+))
(define sqlite3_column_bytes (dlsym % (__cdecl type-int+)   "sqlite3_column_bytes" sqlite3_stmt* type-fix+))
;(define sqlite3_column_double(dlsym % (__cdecl type-float+) "sqlite3_column_double" sqlite3_stmt* type-fix+))
(define sqlite3_column_text  (dlsym % (__cdecl type-string) "sqlite3_column_text" sqlite3_stmt* type-fix+))
;(define sqlite3_column_blob  (dlsym % (__cdecl type-string) "sqlite3_column_blob" sqlite3_stmt* type-fix+))



(define (sqlite:exec database query . args)
   (let ((statement (make-sqlite3-stmt)))
      (if (less? 0 (sqlite3-prepare-v2 database (c-string query) -1 statement null))
         (runtime-error "error query preparation" query))
      (let loop ((n 1) (args args))
         (if (not (null? args))
            (let ((arg (car args)))
               (cond
                  ((integer? arg)
                     ;todo: if > max-int-value use sqlite3_bind_int64
                     (sqlite3-bind-int    statement n arg))
                  ((rational? arg)
                     (sqlite3-bind-double statement n arg))
                  ((string? arg)
                     (sqlite3-bind-text   statement n arg (size arg) #f))
                  (else
                     (runtime-error "Unsupported parameter type" arg)))
               (loop (+ n 1) (cdr args)))))
      (sqlite3-step statement)
      (let ((result (sqlite3-last-insert-rowid database)))
         (sqlite3-finalize statement)
         result)))

))