;;; SQLite3 interface for Otus Lisp
;;; https://github.com/yuriy-chumak/ol
;;; http://www.sqlite.org

;;; Copyright (c) 2014-2021 Yuriy Chumak
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
    sqlite3_context*

  ; Result Codes
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
  ; Extended Result Codes
    ; TBD.
  ; Flags For File Open Operations
    ; TBD.
  ; Device Characteristics
    ; TBD.
  ; File Locking Levels
    ; TBD.
  ; Synchronization Type Flags
    ; TBD.
  ; Standard File Control Opcodes
    ; TBD.
  ; Flags for the xAccess VFS method
    ; TBD.
  ; Flags for the xShmLock VFS method
    ; TBD.
  ; Maximum xShmLock index
    ; TBD.

  ; Text Encodings
    SQLITE_UTF8     ; IMP: R-37514-35566
    SQLITE_UTF16LE  ; IMP: R-03371-37637
    SQLITE_UTF16BE  ; IMP: R-51971-34154
    SQLITE_UTF16    ; Use native byte order
    SQLITE_ANY      ; * deprecated
    SQLITE_UTF16_ALIGNED ; sqlite3_create_collation onl

  ; Configuration Options
    SQLITE_CONFIG_SINGLETHREAD
    SQLITE_CONFIG_MULTITHREAD
    SQLITE_CONFIG_SERIALIZED
   ;SQLITE_CONFIG_MALLOC
   ;SQLITE_CONFIG_GETMALLOC
   ;SQLITE_CONFIG_SCRATCH
   ;SQLITE_CONFIG_PAGECACHE
   ;SQLITE_CONFIG_HEAP
   ;SQLITE_CONFIG_MEMSTATUS
   ;SQLITE_CONFIG_MUTEX
   ;SQLITE_CONFIG_GETMUTEX
   ;SQLITE_CONFIG_LOOKASIDE
   ;SQLITE_CONFIG_PCACHE
   ;SQLITE_CONFIG_GETPCACHE
   ;SQLITE_CONFIG_LOG
   ;SQLITE_CONFIG_URI
   ;SQLITE_CONFIG_PCACHE2
   ;SQLITE_CONFIG_GETPCACHE2
   ;SQLITE_CONFIG_COVERING_INDEX_SCAN
   ;SQLITE_CONFIG_SQLLOG
   ;SQLITE_CONFIG_MMAP_SIZE
   ;SQLITE_CONFIG_WIN32_HEAPSIZE
   ;SQLITE_CONFIG_PCACHE_HDRSZ
   ;SQLITE_CONFIG_PMASZ
   ;SQLITE_CONFIG_STMTJRNL_SPILL
   ;SQLITE_CONFIG_SMALL_MALLOC
   ;SQLITE_CONFIG_SORTERREF_SIZE
   ;SQLITE_CONFIG_MEMDB_MAXSIZE


  ; Run-Time Library Version Numbers
   ;sqlite3_version[]; is not accessible as external static char[], will enabled in feature
    sqlite3_libversion ; const char* ()
    sqlite3_sourceid ; const char* ()
    sqlite3_libversion_number ; int ()

  ; Test To See If The Library Is Threadsafe
    sqlite3_threadsafe ; int ()

  ; One-Step Query Execution Interface
   ; https://www.sqlite.org/c3ref/exec.html
    sqlite3_exec ; int ()

  ; Initialize The SQLite Library
   ;sqlite3_initialize
   ;sqlite3_shutdown
   ;sqlite3_os_init
   ;sqlite3_os_end

  ; Configuring The SQLite Library
    sqlite3_config

  ; Configure database connections
   ;sqlite3_db_config

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

  ; Closing A Database Connection
    sqlite3_close ; int (sqlite3*)
    sqlite3_close_v2 ; int (sqlite3*)

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
    sqlite3_bind_blob
    sqlite3_bind_blob64
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
    sqlite3_column_blob
    sqlite3_column_bytes
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
    sqlite3_result_text
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
   (scheme char)
   (otus ffi))

(cond-expand
   (Linux
      (begin
         (define sqlite (load-dynamic-library "libsqlite3.so"))))
   (Android
      (begin
         (define sqlite (load-dynamic-library "libsqlite.so"))))
   (Windows
      (begin
         (define sqlite (load-dynamic-library "sqlite3.dll"))))
   (else
      (begin
         (runtime-error "sqlite3: unknown platform" (uname)))))

(begin
   (define (make-void*) (vm:cast 0 type-vptr))

   (if (not sqlite)
      (runtime-error "Sqlite3 library not found. Please install one." #null))

   ; Types
   (define sqlite3*  fft-void*)
   (define sqlite3** fft-void**)
   (define sqlite3_stmt*  fft-void*)
   (define sqlite3_stmt** fft-void**)
   (define sqlite3_value* fft-void*)
   (define sqlite3_context* fft-void*)

   ; * internal types
   (define void* fft-void*)
   (define int fft-int)
   (define char* type-string)
   (define char** fft-void**) ;?

   (define sqlite3_int64 fft-int64)
   (define sqlite3_callback type-callable)

   (define sqlite3_value fft-int)
   (define sqlite3_int64 fft-int64)
   (define sqlite3_uint64 fft-uint64)

   ; Result Codes
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

   ; Extended Result Codes .. Maximum xShmLock index
   ; TBD.

   ; Text Encodings
   (define SQLITE_UTF8 1)
   (define SQLITE_UTF16LE 2)
   (define SQLITE_UTF16BE 3)
   (define SQLITE_UTF16 4)
   (define SQLITE_ANY 5)
   (define SQLITE_UTF16_ALIGNED 8)

   ; Configuration Options
   (define SQLITE_CONFIG_SINGLETHREAD   1) ; nil
   (define SQLITE_CONFIG_MULTITHREAD    2) ; nil
   (define SQLITE_CONFIG_SERIALIZED     3) ; nil
   (define SQLITE_CONFIG_MALLOC         4) ; sqlite3_mem_methods*
   (define SQLITE_CONFIG_GETMALLOC      5) ; sqlite3_mem_methods*
   (define SQLITE_CONFIG_SCRATCH        6) ; No longer used
   (define SQLITE_CONFIG_PAGECACHE      7) ; void*, int sz, int N
   (define SQLITE_CONFIG_HEAP           8) ; void*, int nByte, int min
   (define SQLITE_CONFIG_MEMSTATUS      9) ; boolean
   (define SQLITE_CONFIG_MUTEX         10) ; sqlite3_mutex_methods*
   (define SQLITE_CONFIG_GETMUTEX      11) ; sqlite3_mutex_methods*
   (define SQLITE_CONFIG_LOOKASIDE     13) ; int int
   (define SQLITE_CONFIG_PCACHE        14) ; no-op
   (define SQLITE_CONFIG_GETPCACHE     15) ; no-op
   (define SQLITE_CONFIG_LOG           16) ; xFunc, void*
   (define SQLITE_CONFIG_URI           17) ; int
   (define SQLITE_CONFIG_PCACHE2       18) ; sqlite3_pcache_methods2*
   (define SQLITE_CONFIG_GETPCACHE2    19) ; sqlite3_pcache_methods2*
   (define SQLITE_CONFIG_COVERING_INDEX_SCAN  20) ; int
   (define SQLITE_CONFIG_SQLLOG        21) ; xSqllog, void*
   (define SQLITE_CONFIG_MMAP_SIZE     22) ; sqlite3_int64, sqlite3_int64
   (define SQLITE_CONFIG_WIN32_HEAPSIZE       23) ; int nByte
   (define SQLITE_CONFIG_PCACHE_HDRSZ         24) ; int *psz
   (define SQLITE_CONFIG_PMASZ                25) ; unsigned int szPma
   (define SQLITE_CONFIG_STMTJRNL_SPILL       26) ; int nByte
   (define SQLITE_CONFIG_SMALL_MALLOC         27) ; boolean
   (define SQLITE_CONFIG_SORTERREF_SIZE       28) ; int nByte
   (define SQLITE_CONFIG_MEMDB_MAXSIZE        29) ; sqlite3_int64


   ; -----------------------------------------------

   ; Run-Time Library Version Numbers
   (define sqlite3_libversion (sqlite char* "sqlite3_libversion"))
   (define sqlite3_sourceid (sqlite char* "sqlite3_sourceid"))
   (define sqlite3_libversion_number (sqlite int "sqlite3_libversion_number"))

   ; Test To See If The Library Is Threadsafe
   (define sqlite3_threadsafe (sqlite int "sqlite3_threadsafe"))

   ; One-Step Query Execution Interface
   (define sqlite3_exec (sqlite int "sqlite3_exec" sqlite3* char* sqlite3_callback void* char**))

   ; * internal helpers
   (define (make-sqlite3)      (make-void*))
   (define (make-sqlite3_stmt) (make-void*))

   ; Initialize The SQLite Library
   ;sqlite3_initialize
   ;sqlite3_shutdown
   ;sqlite3_os_init
   ;sqlite3_os_end

   ; Configuring The SQLite Library
      (setq sqlite3_config_ (sqlite int "sqlite3_config" int))
   (define (sqlite3_config option . args)
      (case option
         (SQLITE_CONFIG_SINGLETHREAD
            (sqlite3_config_ option))
         (SQLITE_CONFIG_MULTITHREAD
            (sqlite3_config_ option))
         (SQLITE_CONFIG_SERIALIZED
            (sqlite3_config_ option))
         ;...
         (else
            (runtime-error "Unknown sqlite3_config option" option))))

   ; Configure database connections
   ;sqlite3_db_config

   ; --- functions --------------------------------------------------------------
   (define sqlite3_last_insert_rowid (sqlite sqlite3_int64 "sqlite3_last_insert_rowid" sqlite3*))
   (define sqlite3_changes (sqlite int "sqlite3_changes" sqlite3*))
   (define sqlite3_total_changes (sqlite int "sqlite3_total_changes" sqlite3*))
   (define sqlite3_db_filename (sqlite type-string "sqlite3_db_filename" sqlite3* type-string))
   (define sqlite3_db_readonly (sqlite int "sqlite3_db_readonly" sqlite3* type-string))

   ; https://www.sqlite.org/c3ref/open.html
   ; ex: file:data.db?mode=ro&cache=private
   (define sqlite3_open (sqlite int "sqlite3_open" type-string sqlite3**))

  ; Closing A Database Connection
   (define sqlite3_close (sqlite int "sqlite3_close" sqlite3*))
   (define sqlite3_close_v2 (sqlite int "sqlite3_close_v2" sqlite3*))

   (define sqlite3_errcode (sqlite int "sqlite3_errcode" sqlite3*))
   (define sqlite3_errmsg (sqlite char* "sqlite3_errmsg" sqlite3*))
   (define sqlite3_errstr (sqlite char* "sqlite3_errstr" int))


   ; sqlite3_prepare
   ; TODO: проблема с крайним параметром (char**) - надо этот результат сконвертировать снова в строку, новую
   (define sqlite3_prepare_v2 (sqlite int "sqlite3_prepare_v2" sqlite3* type-string int sqlite3_stmt** char**))
   ; sqlite3_prepare16
   ; sqlite3_prepare16_v2

   (define sqlite3_sql (sqlite type-string "sqlite3_sql" sqlite3_stmt*))
   (define sqlite3_stmt_readonly (sqlite int "sqlite3_stmt_readonly" sqlite3_stmt*))

   (define sqlite3_step (sqlite int "sqlite3_step" sqlite3_stmt*))
   (define sqlite3_reset (sqlite int "sqlite3_reset" sqlite3_stmt*))
   (define sqlite3_finalize (sqlite int "sqlite3_finalize" sqlite3_stmt*))

   ; In the SQL statement text input to sqlite3_prepare_v2() and its variants,
   ;  literals may be replaced by a parameter that matches one of following templates:
   ;    ? ?NNN :VVV @VVV $VVV
   ; The values of these parameters (also called "host parameter names" or "SQL parameters")
   ; can be set using the sqlite3_bind_*() routines defined here.
   (define sqlite3_bind_blob (sqlite int "sqlite3_bind_blob" sqlite3_stmt* int void* int fft-any))
   (define sqlite3_bind_blob64 (sqlite int "sqlite3_bind_blob64" sqlite3_stmt* int void* sqlite3_int64 fft-any))
   (define sqlite3_bind_double (sqlite int "sqlite3_bind_double" sqlite3_stmt* int fft-double))
   (define sqlite3_bind_int (sqlite int "sqlite3_bind_int" sqlite3_stmt* int int))
   (define sqlite3_bind_int64 (sqlite int "sqlite3_bind_int64" sqlite3_stmt* int sqlite3_int64))
   (define sqlite3_bind_null (sqlite int "sqlite3_bind_null" sqlite3_stmt* int))
   (define sqlite3_bind_text (sqlite int "sqlite3_bind_text" sqlite3_stmt* int type-string int fft-any))
   (define sqlite3_bind_text16 (sqlite int "sqlite3_bind_text16" sqlite3_stmt* int type-string-wide int type-callable))
      ;sqlite3_bind_text64
      ;sqlite3_bind_value
      ;sqlite3_bind_zeroblob
      ;sqlite3_bind_zeroblob64
   (define sqlite3_bind_parameter_count (sqlite int "sqlite3_bind_parameter_count" sqlite3_stmt*))
   (define sqlite3_bind_parameter_name (sqlite type-string "sqlite3_bind_parameter_name" sqlite3_stmt* int))
   (define sqlite3_bind_parameter_index (sqlite int "sqlite3_bind_parameter_index" sqlite3_stmt* type-string))
   (define sqlite3_clear_bindings (sqlite int "sqlite3_clear_bindings" sqlite3_stmt*))

   (define sqlite3_column_count (sqlite int "sqlite3_column_count" sqlite3_stmt*))
   (define sqlite3_column_name (sqlite type-string "sqlite3_column_name" sqlite3_stmt* int))

   (define sqlite3_data_count (sqlite int "sqlite3_data_count" sqlite3_stmt*))

   (define sqlite3_column_blob (sqlite void* "sqlite3_column_blob" sqlite3_stmt* int))
   (define sqlite3_column_bytes (sqlite int "sqlite3_column_bytes" sqlite3_stmt* int))
   ;(define sqlite3_column_bytes16 ; (dlsym % type-int+   "sqlite3_column_bytes" sqlite3_stmt* type-enum+))
   (define sqlite3_column_double (sqlite fft-double "sqlite3_column_double" sqlite3_stmt* int))
   (define sqlite3_column_int (sqlite int "sqlite3_column_int" sqlite3_stmt* int))
   (define sqlite3_column_int64 (sqlite sqlite3_int64 "sqlite3_column_int" sqlite3_stmt* int))
   (define sqlite3_column_text (sqlite type-string "sqlite3_column_text" sqlite3_stmt* int))
   (define sqlite3_column_text16 (sqlite type-string-wide "sqlite3_column_text16" sqlite3_stmt* int))

   (define sqlite3_column_type (sqlite int "sqlite3_column_type" sqlite3_stmt* int))
   (define sqlite3_column_value (sqlite sqlite3_value* "sqlite3_column_value" sqlite3_stmt* int))

   (define sqlite3_value_double (sqlite fft-double "sqlite3_value_double" sqlite3_stmt*))
   (define sqlite3_value_int (sqlite int "sqlite3_value_int" sqlite3_stmt*))
   (define sqlite3_value_text (sqlite type-string "sqlite3_value_text" sqlite3_stmt*))
   (define sqlite3_value_type (sqlite int "sqlite3_value_type" sqlite3_stmt*))
   (define sqlite3_value_numeric_type (sqlite int "sqlite3_value_numeric_type" sqlite3_stmt*))

   (define sqlite3_create_function_v2 (sqlite int "sqlite3_create_function_v2"   sqlite3* type-string int int fft-void* type-callable type-callable type-callable type-vptr))
   ;
   ;(define sqlite3_value_int  (dlsym % type-int+ "sqlite3_value_int" sqlite3_value*))
   (define sqlite3_result_int (sqlite fft-void "sqlite3_result_int" sqlite3_context* int))
   (define sqlite3_result_text (sqlite fft-void "sqlite3_result_text" sqlite3_context* type-string int fft-void)) ; we do not support destructors

)
; -------------------------------------------------------------------
; enable sqlite logging
(begin
   (define RED "\e[0;31m")
   (define GREEN "\e[0;32m")
   (define YELLOW "\e[0;33m")
   (define MAGENTA "\e[0;35m")
   (define END "\e[0;0m"))

(cond-expand
   (sqlite-log-debug
      (begin
         (define-syntax debug
            (syntax-rules ()
               ((debug . args)
                  (begin .args))))
;                  (print-to stderr "SQLITE: " . args))))
         (define-syntax error
            (syntax-rules (list)
               ((error reason . args)
                  (begin
                     (runtime-error (string-append RED reason) (list .args))))))))
   (else
      (begin
         (define-syntax debug
            (syntax-rules ()
               ((debug . args)
                  #false)))
         (define-syntax error
            (syntax-rules (list)
               ((error reason . args)
                  (begin
                     (runtime-error reason (list .args)))))))))

(begin
   ; * internal function
   (define (get-result-as-row statement)
      (let ((n (sqlite3_column_count statement)))
         (if (less? 0 n)
            (let subloop ((i (- n 1)) (args '()))
               (if (< i 0) args
                  (subloop (- i 1) (cons
                     (case (sqlite3_column_type statement i)
                        (SQLITE_NULL    #false) ; should be #false or #null?
                        (SQLITE_INTEGER (sqlite3_column_int statement i))
                        (SQLITE_FLOAT   (sqlite3_column_double statement i))
                        (SQLITE_TEXT    (sqlite3_column_text statement i))
                        (SQLITE_BLOB    (syscall 9 ; mmap
                           (sqlite3_column_blob statement i)
                           (sqlite3_column_bytes statement i) 0))
                        (else (error "Unsupported column type " i)))
                     args)))))))

   ; при инвалидном запросе бросает runtime исключение
   ; select multiple values
   (define (sqlite:query database query . args)
      (debug
         (print-to stderr MAGENTA query END)
         (display YELLOW stderr) (write args stderr) (display END)
         (print-to stderr ", length: " YELLOW (length args) END))

      (let ((statement (make-sqlite3_stmt)))
         (unless (eq? 0 (sqlite3_prepare_v2 database query -1 statement #f))
            (error "sqlite3 query preparation error:" (sqlite3_errmsg database)))
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
                        ((string? arg)
                           (sqlite3_bind_text   statement n arg (string-length arg) SQLITE_TRANSIENT))
                        ((bytevector? arg)
                           (sqlite3_bind_blob   statement n arg (size arg) SQLITE_TRANSIENT))
                        ((null? arg)
                           (sqlite3_bind_null   statement n))
                        (else
                           (error "Unsupported parameter type" arg)))
                     (sqlite3_bind_null statement n))
                  (loop (+ n 1) (cdr args)))))
         ; analyze results:
         (let ((code (sqlite3_step statement)))
            (if (eq? code SQLITE_ROW) ; query returned a dataset
               statement
            else
               (sqlite3_finalize statement)
               (case code
                  (SQLITE_DONE #true) ; request successful

                  ; constraint violation is not a critical
                  ; error, just return #false
                  (SQLITE_CONSTRAINT #false)

                  ; other errors should throw an error
                  (else
                     (error "SQL statement execution error:"
                        code (sqlite3_errmsg database))
                     #false))))))

   ; executes the statement and returns just one result
   ; if query was "update" or "insert" return a count of updated/inserted rows
   ; if you want to receive inserted key - use "RETURNING" syntax or
   ; (sqlite3_last_insert_rowid db) function.
   (define (sqlite:value database query . args)
      (let ((statement (apply sqlite:query (cons database (cons query args)))))
         (case statement
            (#f #false) ; error
            (#t         ; ok, but no data returned
               (let ((changes (sqlite3_changes database)))
                  (if (less? 0 changes) changes)))
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
                  (when row
                     (apply f row)
                     (case (sqlite3_step statement)
                        (SQLITE_ROW
                           (loop))
                        (SQLITE_DONE
                           (sqlite3_finalize statement)
                           #true)
                        (else
                           (sqlite3_finalize statement)
                           (error "Can't execute SQL statement")))))))))

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
                              (error "Can't execute SQL statement")))))))))))

))
