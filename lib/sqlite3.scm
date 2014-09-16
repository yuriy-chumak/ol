(define-library (lib sqlite3)
  (export
  ; types
    ;sqlite3* sqlite3_value sqlite3_stmt
    make-sqlite3 make-sqlite3_stmt
    
  ; constants
    SQLITE_OK SQLITE_ERROR SQLITE_BUSY SQLITE_LOCKED
    SQLITE_DONE SQLITE_ROW
    SQLITE_STATIC SQLITE_TRANSIENT
    SQLITE_INTEGER SQLITE_FLOAT SQLITE_BLOB SQLITE_NULL SQLITE_TEXT
  
  ; creation/destruction
    sqlite3_open  ; 
    sqlite3_close
    
  ; statement management
    sqlite3_prepare_v2
    sqlite3_step
    sqlite3_reset
    sqlite3_finalize
    
  ; result set
    sqlite3_column_count
    sqlite3_column_name
    sqlite3_column_int
    sqlite3_column_bytes
    ;sqlite3_column_double
    sqlite3_column_text
    ;sqlite3_column_blob
  )

  (import
      (owl defmac) (owl io)
      (owl list) (owl string)
      (owl math) (owl vector)
      (owl pinvoke))
  (begin

(define % (dlopen "sqlite3" 0))

; служебные 
(define (make-sqlite3)      (list->byte-vector '(0 0 0 0)))
(define (make-sqlite3_stmt) (list->byte-vector '(0 0 0 0)))

; todo: завести под это дело отдельный тип - что-то вроде type-int+-ref и т.д.
(define sqlite3*  type-int+)
(define sqlite3** type-vector-raw)
(define sqlite3_stmt*  type-int+)
(define sqlite3_stmt** type-vector-raw)
(define char** type-vector-raw) ; тут проблема!!!

(define sqlite3_value type-fix+)
(define sqlite3_stmt  type-int+)
(define sqlite3_int64 type-vector-raw)

(define SQLITE_OK 0)
(define SQLITE_ERROR 1)
(define SQLITE_BUSY 5)
(define SQLITE_LOCKED 6)

(define SQLITE_DONE 101)
(define SQLITE_ROW 100)

(define SQLITE_STATIC 0)
(define SQLITE_TRANSIENT -1)

(define SQLITE_INTEGER 1)
(define SQLITE_FLOAT 2)
(define SQLITE_BLOB 4)
(define SQLITE_NULL 5)
(define SQLITE_TEXT 3)

(define SQLITE_MISUSE 21)


(define sqlite3_open  (dlsym % type-fix+ "sqlite3_open"  type-string sqlite3**))
(define sqlite3_close (dlsym % type-fix+ "sqlite3_close" sqlite3*))

(define sqlite3_prepare_v2 (dlsym % type-fix+ "sqlite3_prepare_v2" sqlite3* type-string type-fix+ sqlite3_stmt** char**)) ; проблема с крайним параметром (char**) - надо этот результат сконвертировать снова в строку, новую
(define sqlite3_sql      (dlsym % type-string "sqlite3_sql"      sqlite3_stmt*))
(define sqlite3_step       (dlsym % type-fix+ "sqlite3_step"     sqlite3_stmt*))
(define sqlite3_reset      (dlsym % type-fix+ "sqlite3_reset"    sqlite3_stmt*))
(define sqlite3_finalize   (dlsym % type-fix+ "sqlite3_finalize" sqlite3_stmt*))

(define sqlite3_column_count (dlsym % type-fix+ "sqlite3_column_count" sqlite3_stmt*))
(define sqlite3_column_name  (dlsym % type-string "sqlite3_column_name" sqlite3_stmt* type-fix+))
(define sqlite3_column_int   (dlsym % type-int+ "sqlite3_column_int" sqlite3_stmt* type-fix+))
(define sqlite3_column_bytes (dlsym % type-int+ "sqlite3_column_bytes" sqlite3_stmt* type-fix+))
;sqlite3_column_double
(define sqlite3_column_text  (dlsym % type-string "sqlite3_column_text" sqlite3_stmt* type-fix+))
;(define sqlite3_column_blob  (dlsym % type-string "sqlite3_column_name" sqlite3_stmt* type-fix+))

))