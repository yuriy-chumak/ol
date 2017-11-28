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

  ; constants/errors
    SQLITE_OK SQLITE_ERROR SQLITE_INTERNAL SQLITE_PERM SQLITE_ABORT SQLITE_BUSY
    SQLITE_LOCKED SQLITE_NOMEM SQLITE_READONLY SQLITE_INTERRUPT SQLITE_IOERR
    SQLITE_CORRUPT SQLITE_NOTFOUND SQLITE_FULL SQLITE_CANTOPEN SQLITE_PROTOCOL
    SQLITE_EMPTY SQLITE_SCHEMA SQLITE_TOOBIG SQLITE_CONSTRAINT SQLITE_MISMATCH
    SQLITE_MISUSE SQLITE_NOLFS SQLITE_AUTH SQLITE_FORMAT SQLITE_RANGE
    SQLITE_NOTADB SQLITE_NOTICE SQLITE_WARNING

    SQLITE_DONE SQLITE_ROW

    SQLITE-STATIC SQLITE-TRANSIENT
    SQLITE-INTEGER SQLITE-FLOAT SQLITE-BLOB SQLITE-NULL SQLITE-TEXT

  ; text encodings
    SQLITE_UTF8

  ; creation/destruction
    sqlite3_open
    sqlite3-close
    sqlite3_errmsg

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
    sqlite3_bind_null

  ; extensions
    sqlite3_create_function_v2

    sqlite3_value_int
    sqlite3_result_int
    ;sqlite3_result_text

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

    sqlite:query
    sqlite:value
   )

   (import
      (otus lisp)
      (otus ffi))

(begin

(define uname (syscall 63 #f #f #f))

(define win32? (string-ci=? (ref uname 1) "Windows"))
(define linux? (string-ci=? (ref uname 1) "Linux"))

(define (new-void*) (vm:new-raw-object fft-void* 1)) ;(vm:wordsize)))

(define % (dlopen (cond
   (win32? "sqlite3")
   (linux? "libsqlite3.so")
   (else (runtime-error "No sqlite3 library support" "Unknown platform")))))

(if (not %)
   (runtime-error "Can't load sqlite3 library." (cond
      (win32?
         "Download dll from http://www.sqlite.org/download.html")
      (linux?
         "Use, for example, sudo apt-get install libsqlite3-dev"))))

; all sqlite imports are __cdecl under all OS
; olvm correctly processes cdecl and stdcall both, so no more need to special
; declaration of it
;(define :dlsym dlsym)
;(define (dlsym dll type . args)
;   (apply :dlsym (cons dll (cons (__cdecl type) args))))



; служебные
(define (make-sqlite3)      (new-void*)) ;like void* (vm:new-raw-object type-vector-raw '(0)))
(define (make-sqlite3-stmt) (new-void*)) ; or (list->byte-vector '(0 0 0 0)))

; todo: завести под это дело отдельный тип - что-то вроде type-int+-ref и т.д.
(define sqlite3*  fft-void*)
(define sqlite3** fft-void**)
(define sqlite3_stmt*  fft-void*)
(define sqlite3_stmt** fft-void**)
(define sqlite3_value* fft-void*)
(define sqlite3_context* fft-void*)
(define char** type-vector-raw)

(define sqlite3_value type-fix+)
(define sqlite3_int64 type-vector-raw)

(define SQLITE_OK 0)
(define SQLITE_ERROR 1)
(define SQLITE_INTERNAL 2)
(define SQLITE_PERM 3)
(define SQLITE_ABORT 4)
(define SQLITE_BUSY 5)
(define SQLITE_LOCKED 6)
(define SQLITE_NOMEM 7)
(define SQLITE_READONLY 8)
(define SQLITE_INTERRUPT 9)
(define SQLITE_IOERR 10)
(define SQLITE_CORRUPT 11)
(define SQLITE_NOTFOUND 12)
(define SQLITE_FULL 13)
(define SQLITE_CANTOPEN 14)
(define SQLITE_PROTOCOL 15)
(define SQLITE_EMPTY 16)
(define SQLITE_SCHEMA 17)
(define SQLITE_TOOBIG 18)
(define SQLITE_CONSTRAINT 19)
(define SQLITE_MISMATCH 20)
(define SQLITE_MISUSE 21)
(define SQLITE_NOLFS 22)
(define SQLITE_AUTH 23)
(define SQLITE_FORMAT 24)
(define SQLITE_RANGE 25)
(define SQLITE_NOTADB 26)
(define SQLITE_NOTICE 27)
(define SQLITE_WARNING 28)

(define SQLITE_ROW 100)
(define SQLITE_DONE 101)

(define SQLITE-STATIC 0)
(define SQLITE-TRANSIENT -1)

(define SQLITE-INTEGER 1)
(define SQLITE-FLOAT 2)
(define SQLITE-BLOB 4)
(define SQLITE-NULL 5)
(define SQLITE-TEXT 3)

(define SQLITE-MISUSE 21)

(define SQLITE_UTF8 1)


; https://www.sqlite.org/c3ref/open.html
; ex: file:data.db?mode=ro&cache=private
(define sqlite3_open  (dlsym % type-fix+ "sqlite3_open"  type-string sqlite3**))
(define sqlite3-close (dlsym % type-fix+ "sqlite3_close" sqlite3*))
(define sqlite3_errmsg(dlsym % type-string "sqlite3_errmsg" sqlite3*))

(define sqlite3-prepare-v2 (dlsym % type-fix+ "sqlite3_prepare_v2" sqlite3* type-string type-fix+ sqlite3_stmt** char**)) ; проблема с крайним параметром (char**) - надо этот результат сконвертировать снова в строку, новую
(define sqlite3-sql      (dlsym % type-string "sqlite3_sql"        sqlite3_stmt*))
(define sqlite3-step       (dlsym % type-fix+ "sqlite3_step"       sqlite3_stmt*))
(define sqlite3-reset      (dlsym % type-fix+ "sqlite3_reset"      sqlite3_stmt*))
(define sqlite3-finalize   (dlsym % type-fix+ "sqlite3_finalize"   sqlite3_stmt*))

(define sqlite3-last-insert-rowid (dlsym % type-fix+ "sqlite3_last_insert_rowid" sqlite3*))
(define sqlite3_changes (dlsym % type-int+ "sqlite3_changes" sqlite3*))

; In the SQL statement text input to sqlite3_prepare_v2() and its variants, literals may be replaced by a parameter that matches one of following templates:
;    ? ?NNN :VVV @VVV $VVV
;
; In the templates above, NNN represents an integer literal, and VVV represents an alphanumeric identifier.
; The values of these parameters (also called "host parameter names" or "SQL parameters") can be set using the sqlite3_bind_*() routines defined here.
(define sqlite3-bind-parameter-index (dlsym % type-fix+ "sqlite3_bind_parameter_index" sqlite3_stmt* type-string))
(define sqlite3-bind-int    (dlsym % type-fix+ "sqlite3_bind_int"    sqlite3_stmt* type-int+ type-int+))
(define sqlite3-bind-double (dlsym % type-fix+ "sqlite3_bind_double" sqlite3_stmt* type-int+ fft-double))
(define sqlite3-bind-text   (dlsym % type-fix+ "sqlite3_bind_text"   sqlite3_stmt* type-int+ type-string type-fix+ fft-void*))
(define sqlite3_bind_null   (dlsym % type-fix+ "sqlite3_bind_null"   sqlite3_stmt* type-int+))

(define sqlite3_create_function_v2 (dlsym % type-fix+ "sqlite3_create_function_v2"   sqlite3* type-string type-int+ type-int+ fft-void* type-callable type-callable type-callable type-vptr))

(define sqlite3_value_int  (dlsym % type-int+ "sqlite3_value_int" sqlite3_value*))
(define sqlite3_result_int (dlsym % fft-void "sqlite3_result_int" sqlite3_context* type-int+))


(define sqlite3_column_type  (dlsym % type-fix+   "sqlite3_column_type" sqlite3_stmt* type-fix+))
(define sqlite3_column_count (dlsym % type-fix+   "sqlite3_column_count" sqlite3_stmt*))
(define sqlite3_column_name  (dlsym % type-string "sqlite3_column_name" sqlite3_stmt* type-fix+))
(define sqlite3_column_int   (dlsym % type-int+   "sqlite3_column_int" sqlite3_stmt* type-fix+))
(define sqlite3_column_bytes (dlsym % type-int+   "sqlite3_column_bytes" sqlite3_stmt* type-fix+))
;(define sqlite3_column_double(dlsym % fft-double "sqlite3_column_double" sqlite3_stmt* type-fix+))
(define sqlite3_column_text  (dlsym % type-string "sqlite3_column_text" sqlite3_stmt* type-fix+))
;(define sqlite3_column_blob  (dlsym % type-string "sqlite3_column_blob" sqlite3_stmt* type-fix+))


(define (starts-with string sub)
   (if (> (string-length sub) (string-length string))
      #false
      (string-eq? (substring string 0 (string-length sub)) sub)))


; DEPRECATED
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

; при инвалидном запросе должен бросать runtime исключение
(define (sqlite:query database query . args) ; select multiple values
   (let ((statement (make-sqlite3-stmt)))
      (if (less? 0 (sqlite3-prepare-v2 database (c-string query) -1 statement null))
         (runtime-error "error query preparation" query))
      ; apply arguments:
      (let loop ((n 1) (args args))
         (if (null? args) #true
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
      ; analyze results:
      (let ((code (sqlite3-step statement)))
         (case code
            (SQLITE_ROW ; SELECT
               statement)
            (SQLITE_DONE
               (sqlite3-finalize statement)
               #false) ; no query results present
            (else
               (sqlite3-finalize statement)
               (runtime-error "Can't execute SQL statement" code))))))

; возвращает только одно значение из запроса (если было запрошено одно, иначе целую строку)
; note: для циклической обработки строк используйте db:for-each
; INSERT -> sqlite3-last-insert-rowid()
; UPDATE -> sqlite3_changes()
; DELETE -> sqlite3_changes()
; SELECT ->
(define (sqlite:value database query . args) ; select only one value
   (print "SQLITE: " query ": (" (length args) ")> " args)
   (let ((statement (make-sqlite3-stmt)))
      (if (less? 0 (sqlite3-prepare-v2 database (c-string query) -1 statement null))
         (runtime-error "error query preparation" query))
      ; apply arguments
      (let loop ((n 1) (args args))
         (if (null? args) #true
            (let ((arg (car args)))
               (cond
                  ((integer? arg)
                     ;todo: if > max-int-value use sqlite3_bind_int64
                     (sqlite3-bind-int    statement n arg))
                  ((rational? arg)
                     (sqlite3-bind-double statement n arg))
                  ((string? arg)
                     (sqlite3-bind-text   statement n arg (size arg) #f))
                  ((eq? arg #false)
                     (sqlite3-bind-int    statement n 0))
                  (else
                     (runtime-error "Unsupported parameter type" arg)))
               (loop (+ n 1) (cdr args)))))
      (let ((code (sqlite3-step statement)))
      ; analyze results
      (print "db:value result: " code)
      (case code
         (SQLITE_DONE
            (print "SQLITE_DONE")
            (cond
               ((starts-with query "SELECT ")
                  (sqlite3-finalize statement)
                  #false)
               ((starts-with query "INSERT ")
                  (let ((id (sqlite3-last-insert-rowid database)))
                     (sqlite3-finalize statement)
                     (print "id: " id)
                     id))  ; return inserted row id (usually: the key)
               ((or (starts-with query "UPDATE ")
                    (starts-with query "DELETE "))
                  (let ((changes (sqlite3_changes database)))
                     (sqlite3-finalize statement)
                     (print "changes: " changes)
                     changes)) ; return count of changed/deleted lines
               (else
                  #true)))
         (SQLITE_ROW
            (print "SQLITE_ROW")
            (let ((n (sqlite3_column_count statement)))
            ;(print "n: " n)
            (if (less? 0 n)
               (let ((result
                        (let subloop ((i (- n 1)) (args '()))
                           ;(print "args: " args)
                           ;(print "sqlite3_column_type statement i: " (sqlite3_column_type statement i))
                           ;(print "i: " i)
                           ;(print "?: " (< i 0))
                           (if (< i 0) args
                              (subloop (- i 1) (cons
                                 (case (sqlite3_column_type statement i)
                                    (SQLITE-NULL    #false)
                                    (SQLITE-INTEGER (sqlite3_column_int statement i))
                                    ;(SQLITE-FLOAT   (sqlite3_column_double statement i))
                                    (SQLITE-TEXT    (sqlite3_column_text statement i))
                                    (else (runtime-error "Unsupported column type " i)))
                                 args))))))
                  (sqlite3-finalize statement)
                  (if (eq? n 1)
                     (car result)
                     result)))))
         ; INSERT
         (SQLITE_CONSTRAINT
            (print "SQLITE_CONSTRAINT")
            (sqlite3-finalize statement)
            #false)
         (else
            (print "Can't execute SQL statement with err: " code)
            (sqlite3-finalize statement)
            (runtime-error "Can't execute SQL statement" #t))))))

))
