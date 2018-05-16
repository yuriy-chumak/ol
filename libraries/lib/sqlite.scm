;;; SQLite3 interface for Otus Lisp
;;; https://github.com/yuriy-chumak/ol
;;; http://www.sqlite.org

;;; Copyright (c) 2014, 2016, 2017, 2018 Yuriy Chumak
;;; All rights reserved.
;;;
;;; --------------------------------------------------------------
;;; This program is free software;  you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; --------------------------------------------------------------
(define-library (lib sqlite)

(export
    make-sqlite3 make-sqlite3_stmt

  ; constants/errors
    SQLITE_OK SQLITE_ERROR

    SQLITE_INTERNAL SQLITE_PERM SQLITE_ABORT SQLITE_BUSY SQLITE_LOCKED
    SQLITE_NOMEM SQLITE_READONLY SQLITE_INTERRUPT SQLITE_IOERR SQLITE_CORRUPT
    SQLITE_NOTFOUND SQLITE_FULL SQLITE_CANTOPEN SQLITE_PROTOCOL SQLITE_EMPTY
    SQLITE_SCHEMA SQLITE_TOOBIG SQLITE_CONSTRAINT SQLITE_MISMATCH SQLITE_MISUSE
    SQLITE_NOLFS SQLITE_AUTH SQLITE_FORMAT SQLITE_RANGE SQLITE_NOTADB
    SQLITE_NOTICE SQLITE_WARNING

    SQLITE_DONE SQLITE_ROW

    SQLITE_STATIC SQLITE_TRANSIENT
    SQLITE_INTEGER SQLITE_FLOAT SQLITE_BLOB SQLITE_NULL SQLITE_TEXT

  ; text encodings
    SQLITE_UTF8

   ;sqlite3_version[]
    sqlite3_libversion ; const char* ()
    sqlite3_sourceid ; const char* ()
    sqlite3_libversion_number ; int ()

    sqlite3_threadsafe ; int ()

    sqlite3_last_insert_rowid ; sqlite3_int64 (sqlite3*)
    sqlite3_changes ; int (sqlite3*)
    sqlite3_total_changes ; int (sqlite3*)

  ; Formatted String Printing Functions
   ;sqlite3_mprintf
   ;sqlite3_vmprintf
   ;sqlite3_snprintf
   ;sqlite3_vsnprintf

  ; Memory Allocation Subsystem
   ;sqlite3_malloc
   ;sqlite3_malloc64
   ;sqlite3_realloc
   ;sqlite3_realloc64
   ;sqlite3_free
   ;sqlite3_msize

  ; Memory Allocator Statistics
   ;sqlite3_memory_used
   ;sqlite3_memory_highwater

  ; Pseudo-Random Number Generator
   ;sqlite3_randomness

  ; Compile-Time Authorization Callbacks
   ;sqlite3_set_authorizer

  ; Tracing And Profiling Functions
   ;sqlite3_trace
   ;sqlite3_profile
   ;sqlite3_progress_handler

  ; Creation / Destruction / Info
    sqlite3_open ; int (const char* filename, sqlite3**)
   ;sqlite3_open16 ; int (const char* filename, sqlite3**)
   ;sqlite3_open_v2 ; int (const char* filename, sqlite3**, int, const char*)
    sqlite3_close ; int (sqlite3*)
   ;sqlite3_close_v2 ; int (sqlite3*)
    sqlite3_db_filename ; const char* (sqlite3*, const char*)
    sqlite3_db_readonly ; int (sqlite3*, const char*)

  ; Error Codes And Messages
    sqlite3_errcode ; int (sqlite3*)
   ;sqlite3_extended_errcode
    sqlite3_errmsg ; const char* (sqlite3*)
   ;sqlite3_errmsg16
    sqlite3_errstr ; const char* (int)

  ; Run-time Limits
   ;sqlite3_limit

  ; Compiling An SQL Statement
   ;sqlite3_prepare ; int (sqlite3*, const char*, int, sqlite3_stmt**, const char**)
    sqlite3_prepare_v2 ; int (sqlite3*, const char*, int, sqlite3_stmt**, const char**)
   ;sqlite3_prepare16
   ;sqlite3_prepare16_v2
    sqlite3_sql ; const char* (sqlite3_stmt*)
    sqlite3_stmt_readonly ; int (sqlite3_stmt*)
   ;sqlite3_stmt_busy

  ; Binding Values To Prepared Statements
   ;sqlite3_bind_blob
   ;sqlite3_bind_blob64
    sqlite3_bind_double
    sqlite3_bind_int
    sqlite3_bind_int64
    sqlite3_bind_null
    sqlite3_bind_text
    sqlite3_bind_text16
   ;sqlite3_bind_text64
   ;sqlite3_bind_value
   ;sqlite3_bind_zeroblob
   ;sqlite3_bind_zeroblob64
    sqlite3_bind_parameter_count ; int (sqlite3_stmt*)
    sqlite3_bind_parameter_name ; const char* (sqlite3_stmt*, int)
    sqlite3_bind_parameter_index ; int (sqlite3_stmt*, const char*)
    sqlite3_clear_bindings ; int (sqlite3_stmt*)

  ; Columns
    sqlite3_column_count ; int (sqlite3_stmt*)
    sqlite3_column_name ; const char* (sqlite3_stmt*, int)
   ;sqlite3_column_name16

  ; Source Of Data In A Query Result
   ;sqlite3_column_database_name
   ;sqlite3_column_database_name16
   ;sqlite3_column_table_name
   ;sqlite3_column_table_name16
   ;sqlite3_column_origin_name
   ;sqlite3_column_origin_name16

  ; Declared Datatype Of A Query Result
   ;sqlite3_column_decltype
   ;sqlite3_column_decltype16

  ; Evaluate An SQL Statement
    sqlite3_step

  ; Number of columns in a result set
    sqlite3_data_count ; int (sqlite3_stmt*)

  ; Result Values From A Query
   ;sqlite3_column_blob
   ;sqlite3_column_bytes
   ;sqlite3_column_bytes16
    sqlite3_column_double
    sqlite3_column_int
    sqlite3_column_int64
    sqlite3_column_text
    sqlite3_column_text16
    sqlite3_column_type
    sqlite3_column_value

  ; Destroy A Prepared Statement Object
    sqlite3_finalize ; int (sqlite3_stmt*)
  ; Reset A Prepared Statement Object
    sqlite3_reset ; int (sqlite3_stmt*)

  ; Create Or Redefine SQL Functions
   ;sqlite3_create_function
   ;sqlite3_create_function16
    sqlite3_create_function_v2

  ; Obtaining SQL Values
   ;sqlite3_value_blob
   ;sqlite3_value_bytes
   ;sqlite3_value_bytes16
    sqlite3_value_double ; double (sqlite3_stmt*)
    sqlite3_value_int ; int (sqlite3_stmt*)
   ;sqlite3_value_int64
    sqlite3_value_text ; const unsigned char* (sqlite3_stmt*)
   ;sqlite3_value_text16
   ;sqlite3_value_text16le
   ;sqlite3_value_text16be
    sqlite3_value_type ; int (sqlite3_stmt*)
    sqlite3_value_numeric_type ; int (sqlite3_stmt*)

  ; Finding The Subtype Of SQL Values
   ;sqlite3_value_subtype

  ; Copy And Free SQL Values
   ;sqlite3_value_dup
   ;sqlite3_value_free

  ; Obtain Aggregate Function Context
   ;sqlite3_aggregate_context

  ; User Data For Functions
   ;sqlite3_user_data

  ; Setting The Result Of An SQL Function
   ;sqlite3_result_blob
   ;sqlite3_result_blob64
   ;sqlite3_result_double
   ;sqlite3_result_error
   ;sqlite3_result_error16
   ;sqlite3_result_error_toobig
   ;sqlite3_result_error_nomem
   ;sqlite3_result_error_code
    sqlite3_result_int
   ;sqlite3_result_int64
   ;sqlite3_result_null
   ;sqlite3_result_text
   ;sqlite3_result_text64
   ;sqlite3_result_text16
   ;sqlite3_result_text16le
   ;sqlite3_result_text16be
   ;sqlite3_result_value
   ;sqlite3_result_zeroblob
   ;sqlite3_result_zeroblob64

   ;sqlite3_result_subtype

  ; Define New Collating Sequences
   ;sqlite3_create_collation
   ;sqlite3_create_collation_v2
   ;sqlite3_create_collation16
   ;sqlite3_collation_needed
   ;sqlite3_collation_needed16

  ; ...
   ;sqlite3_key
   ;sqlite3_key_v2
   ;sqlite3_rekey
   ;sqlite3_rekey_v2
   ;sqlite3_activate_see

    sqlite:query
    sqlite:value

    sqlite:for-each
    sqlite:map)

; ============================================================================
(import
   (otus lisp)
   (otus ffi))

(begin

(define uname (uname))

(define win32? (string-ci=? (ref uname 1) "Windows"))
(define linux? (string-ci=? (ref uname 1) "Linux"))

(define (make-void*) (vm:new-raw-object fft-void* (vm:wordsize)))

(define sqlite (load-dynamic-library (cond
   (win32? "sqlite3")
   (linux? "libsqlite3.so")
   (else
      (runtime-error "sqlite3: unknown platform" uname)))))

(if (not sqlite)
   (runtime-error "Can't load sqlite3 library." (cond
      (win32?
         "Download dll from http://www.sqlite.org/download.html")
      (linux?
         "Use, for example, sudo apt-get install libsqlite3-dev"))))


; TEMP:
(define (runtime-error reason info)
   (print "\e[0;31m" info "\e[0;0m;"))

; --
; version:
(define sqlite3_libversion (sqlite type-string "sqlite3_libversion"))
(define sqlite3_sourceid (sqlite type-string "sqlite3_sourceid"))
(define sqlite3_libversion_number (sqlite fft-int "sqlite3_libversion_number"))

(define sqlite3_threadsafe (sqlite fft-int "sqlite3_threadsafe"))

; types
(define sqlite3*  fft-void*)
(define sqlite3** fft-void**)
(define sqlite3_stmt*  fft-void*)
(define sqlite3_stmt** fft-void**)
(define sqlite3_value* fft-void*)
(define sqlite3_context* fft-void*)
(define char** fft-void**) ;?

(define sqlite3_value fft-int)
(define sqlite3_int64 fft-int64)
(define sqlite3_uint64 fft-uint64)

; служебные
(define (make-sqlite3)      (make-void*))
(define (make-sqlite3_stmt) (make-void*))

; constants
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

(define SQLITE_STATIC 0)
(define SQLITE_TRANSIENT -1)

(define SQLITE_INTEGER 1)
(define SQLITE_FLOAT 2)
(define SQLITE_BLOB 4)
(define SQLITE_NULL 5)
(define SQLITE_TEXT 3)

(define SQLITE_MISUSE 21)

(define SQLITE_UTF8 1)

; --- functions --------------------------------------------------------------
(define sqlite3_last_insert_rowid (sqlite sqlite3_int64 "sqlite3_last_insert_rowid" sqlite3*))
(define sqlite3_changes (sqlite fft-int "sqlite3_changes" sqlite3*))
(define sqlite3_total_changes (sqlite fft-int "sqlite3_total_changes" sqlite3*))
(define sqlite3_db_filename (sqlite type-string "sqlite3_db_filename" sqlite3* type-string))
(define sqlite3_db_readonly (sqlite fft-int "sqlite3_db_readonly" sqlite3* type-string))

; https://www.sqlite.org/c3ref/open.html
; ex: file:data.db?mode=ro&cache=private
(define sqlite3_open (sqlite fft-int "sqlite3_open" type-string sqlite3**))
(define sqlite3_close (sqlite fft-int "sqlite3_close" sqlite3*))
(define sqlite3_errcode (sqlite fft-int "sqlite3_errcode" sqlite3*))
(define sqlite3_errmsg (sqlite type-string "sqlite3_errmsg" sqlite3*))
(define sqlite3_errstr (sqlite type-string "sqlite3_errstr" fft-int))


; sqlite3_prepare
; TODO: проблема с крайним параметром (char**) - надо этот результат сконвертировать снова в строку, новую
(define sqlite3_prepare_v2 (sqlite fft-int "sqlite3_prepare_v2" sqlite3* type-string fft-int sqlite3_stmt** char**))
; sqlite3_prepare16
; sqlite3_prepare16_v2

(define sqlite3_sql (sqlite type-string "sqlite3_sql" sqlite3_stmt*))
(define sqlite3_stmt_readonly (sqlite fft-int "sqlite3_stmt_readonly" sqlite3_stmt*))

(define sqlite3_step (sqlite fft-int "sqlite3_step" sqlite3_stmt*))
(define sqlite3_reset (sqlite fft-int "sqlite3_reset" sqlite3_stmt*))
(define sqlite3_finalize (sqlite fft-int "sqlite3_finalize" sqlite3_stmt*))

; In the SQL statement text input to sqlite3_prepare_v2() and its variants,
;  literals may be replaced by a parameter that matches one of following templates:
;    ? ?NNN :VVV @VVV $VVV
; The values of these parameters (also called "host parameter names" or "SQL parameters")
; can be set using the sqlite3_bind_*() routines defined here.
   ;sqlite3_bind_blob
   ;sqlite3_bind_blob64
(define sqlite3_bind_double (sqlite fft-int "sqlite3_bind_double" sqlite3_stmt* fft-int fft-double))
(define sqlite3_bind_int (sqlite fft-int "sqlite3_bind_int" sqlite3_stmt* fft-int fft-int))
(define sqlite3_bind_int64 (sqlite fft-int "sqlite3_bind_int64" sqlite3_stmt* fft-int sqlite3_int64))
(define sqlite3_bind_null (sqlite fft-int "sqlite3_bind_null" sqlite3_stmt* fft-int))
(define sqlite3_bind_text (sqlite fft-int "sqlite3_bind_text" sqlite3_stmt* fft-int type-string fft-int type-callable))
(define sqlite3_bind_text16 (sqlite fft-int "sqlite3_bind_text16" sqlite3_stmt* fft-int type-string-wide fft-int type-callable))
   ;sqlite3_bind_text64
   ;sqlite3_bind_value
   ;sqlite3_bind_zeroblob
   ;sqlite3_bind_zeroblob64
(define sqlite3_bind_parameter_count (sqlite fft-int "sqlite3_bind_parameter_count" sqlite3_stmt*))
(define sqlite3_bind_parameter_name (sqlite type-string "sqlite3_bind_parameter_name" sqlite3_stmt* fft-int))
(define sqlite3_bind_parameter_index (sqlite fft-int "sqlite3_bind_parameter_index" sqlite3_stmt* type-string))
(define sqlite3_clear_bindings (sqlite fft-int "sqlite3_clear_bindings" sqlite3_stmt*))

(define sqlite3_column_count (sqlite fft-int "sqlite3_column_count" sqlite3_stmt*))
(define sqlite3_column_name (sqlite type-string "sqlite3_column_name" sqlite3_stmt* fft-int))

(define sqlite3_data_count (sqlite fft-int "sqlite3_data_count" sqlite3_stmt*))

;(define sqlite3_column_blob ; (dlsym % type-string "sqlite3_column_blob" sqlite3_stmt* type-fix+))
;(define sqlite3_column_bytes ;(dlsym % type-int+   "sqlite3_column_bytes" sqlite3_stmt* type-fix+))
;(define sqlite3_column_bytes16 ; (dlsym % type-int+   "sqlite3_column_bytes" sqlite3_stmt* type-fix+))
(define sqlite3_column_double (sqlite fft-double "sqlite3_column_double" sqlite3_stmt* fft-int))
(define sqlite3_column_int (sqlite fft-int "sqlite3_column_int" sqlite3_stmt* fft-int))
(define sqlite3_column_int64 (sqlite sqlite3_int64 "sqlite3_column_int" sqlite3_stmt* fft-int))
(define sqlite3_column_text (sqlite type-string "sqlite3_column_text" sqlite3_stmt* fft-int))
(define sqlite3_column_text16 (sqlite type-string-wide "sqlite3_column_text16" sqlite3_stmt* fft-int))

(define sqlite3_column_type (sqlite fft-int "sqlite3_column_type" sqlite3_stmt* fft-int))
(define sqlite3_column_value (sqlite sqlite3_value* "sqlite3_column_value" sqlite3_stmt* fft-int))

(define sqlite3_value_double (sqlite fft-double "sqlite3_value_double" sqlite3_stmt*))
(define sqlite3_value_int (sqlite fft-int "sqlite3_value_int" sqlite3_stmt*))
(define sqlite3_value_text (sqlite type-string "sqlite3_value_text" sqlite3_stmt*))
(define sqlite3_value_type (sqlite fft-int "sqlite3_value_type" sqlite3_stmt*))
(define sqlite3_value_numeric_type (sqlite fft-int "sqlite3_value_numeric_type" sqlite3_stmt*))

(define sqlite3_create_function_v2 (sqlite fft-int "sqlite3_create_function_v2"   sqlite3* type-string fft-int fft-int fft-void* type-callable type-callable type-callable type-vptr))
;
;(define sqlite3_value_int  (dlsym % type-int+ "sqlite3_value_int" sqlite3_value*))
(define sqlite3_result_int (sqlite fft-void "sqlite3_result_int" sqlite3_context* fft-int))
;
;
;(define sqlite3_column_type  (dlsym % type-fix+   "sqlite3_column_type" sqlite3_stmt* type-fix+))


; internal fast function
(define (starts-with-ci? string sub)
   (let loop ((a (str-iter string))
              (b (str-iter sub)))
      (cond
         ((null? a)
            #false)
         ((null? b)
            #true)
         ((not (eq? (car a) (car b)))
            #false)
         (else
            (loop (force (cdr a)) (force (cdr b)))))))

; ============================================================================

; internal function:
(define (get-result-as-row statement)
   (let ((n (sqlite3_column_count statement)))
      ;(print "n: " n)
      (if (less? 0 n)
         (let subloop ((i (- n 1)) (args '()))
            ;(print "args: " args)
            ;(print "sqlite3_column_type statement i: " (sqlite3_column_type statement i))
            ;(print "i: " i)
            ;(print "?: " (< i 0))
            (if (< i 0) args
               (subloop (- i 1) (cons
                  (case (sqlite3_column_type statement i)
                     (SQLITE_NULL    #false)
                     (SQLITE_INTEGER (sqlite3_column_int statement i))
                     (SQLITE_FLOAT   (sqlite3_column_double statement i))
                                     ;old /potentially more compatible/ case:
                                     ;   (bytes->string (string->runes (sqlite3_column_text statement i)))
                     (SQLITE_TEXT    (sqlite3_column_text16 statement i))
                     (else (runtime-error "Unsupported column type " i)))
                  args)))))))

; при инвалидном запросе бросает runtime исключение
; select multiple values
(define (sqlite:query database query . args)
   (print "SQLITE: \e[0;35m" query "\e[0;0m: (" (length args) ")> " args)
   (let ((statement (make-sqlite3_stmt)))
      (unless (eq? 0 (sqlite3_prepare_v2 database (c-string query) -1 statement #f))
         (runtime-error "error query preparation:" (list
            query (sqlite3_errmsg database))))
      ; apply arguments:
      (let loop ((n 1) (args args))
         (unless (null? args)
            (let ((arg (car args)))
               (if arg
                  (cond
                     ((integer? arg)
                        (sqlite3_bind_int64  statement n arg))
                     ((rational? arg)
                        (sqlite3_bind_double statement n arg))
                     ((eq? (type arg) type-string)
                        (sqlite3_bind_text   statement n arg (size arg) #f))
                     ((eq? (type arg) type-string-wide)
                        (sqlite3_bind_text16 statement n arg (* (size arg) 2) #f))
                     ((null? arg)
                        (sqlite3_bind_null   statement n))
                     (else
                        (runtime-error "Unsupported parameter type" arg)))
                  (sqlite3_bind_null statement n))
               (loop (+ n 1) (cdr args)))))
      ; analyze results:
      (let ((code (sqlite3_step statement)))
         (case code
            (SQLITE_ROW ; SELECT
               statement)
            (SQLITE_DONE; ...
               (sqlite3_finalize statement)
               #true) ; no query results present, but it's ok
            (SQLITE_CONSTRAINT ; INSERT
               (print "SQLITE_CONSTRAINT")
               (sqlite3_finalize statement)
               #false) ; something wrong, got error
            (else
              ;(print "Can't execute SQL statement with err: " code)
               (sqlite3_finalize statement)
               (runtime-error "Can't execute SQL statement" (list
                  code (sqlite3_errmsg database))))))))

; executes the statement and returns
; last row id: in case of "insert"
; updated rows: in case of "update" ?
(define (sqlite:value database query . args)
   (let ((statement (apply sqlite:query (cons database (cons query args)))))
      (case statement
         (#f #false) ; error :(
         (#t         ; ok, but no data returned
            (cond
               ((starts-with-ci? query "SELECT ")
                  #false)
               ((starts-with-ci? query "INSERT ")
                  (let ((id (sqlite3_last_insert_rowid database)))
                        (print "id: " id) ; debug output
                     (if (less? 0 id) id))) ; return inserted row id (usually: the key) or #false
               ((or (starts-with-ci? query "UPDATE ")
                    (starts-with-ci? query "DELETE "))
                  (let ((changes (sqlite3_changes database)))
                        (print "changes: " changes) ; debug output
                     (if (less? 0 changes) changes)))
               (else
                  #true)))
         (else ; got a values!
            (let ((result (get-result-as-row statement)))
               (sqlite3_finalize statement)
               (if result 
                  (if (eq? (cdr result) #null)
                     (car result)
                     result)))))))

(define (sqlite:for-each statement f)
   (case statement
      (#f #false) ; error
      (#t #true)  ; no data available
      (else
         (let loop ()
            (let ((row (get-result-as-row statement)))
               (if row
                  (begin (apply f row)
                     (case (sqlite3_step statement)
                        (SQLITE_ROW
                           (loop))
                        (SQLITE_DONE
                           (sqlite3_finalize statement)
                           #true)
                        (else
                           (sqlite3_finalize statement)
                           (runtime-error "Can't execute SQL statement" #t))))))))))

(define (sqlite:map statement f)
   (case statement
      (#f #false) ; error
      (#t #null)  ; no data available
      (else
         (reverse
         (let loop ((t '()))
            (let ((row (get-result-as-row statement)))
               (if row
                  (let ((v (apply f row)))
                     (case (sqlite3_step statement)
                        (SQLITE_ROW
                           (loop (cons v t)))
                        (SQLITE_DONE
                           (sqlite3_finalize statement)
                           (cons v t))
                        (else
                           (sqlite3_finalize statement)
                           (runtime-error "Can't execute SQL statement" #t)))))))))))

))
