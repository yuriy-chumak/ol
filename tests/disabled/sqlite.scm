#!/bin/ol
; http://www.scheme.com/tspl4/ - The Scheme Programming Language (Fourth Edition)
; http://community.schemewiki.org/?scheme-faq-standards#implementations
;!

(import (lib sqlite))

; вспомогательный макрос для собрать в кучку все bor
(define (OR . args) (fold bor 0 args))

; todo: move this test into separate script (tests/sqlite3.scm)
(define database (make-sqlite3))
(print "open: "     (sqlite3_open (c-string ":memory:") database))
(print "close: "    (sqlite3-close database))
(print "open: "     (sqlite3_open (c-string "x.sqlite") database))



(define (execute query . args)
   (let ((statement (make-sqlite3-stmt)))
      (if (less? 0 (sqlite3-prepare-v2 database (c-string query) -1 statement null))
         (runtime-error "error query preparation" query))
      (let loop ((n 1) (args args))
         (if (not (null? args))
            (let ((arg (car args)))
               (print "type-arg: " (type arg))
               (cond
                  ((integer? arg)
                     (print "integer")
                     ;todo: if > max-int-value use sqlite3_bind_int64
                     (sqlite3-bind-int    statement n arg))
                  ((rational? arg)
                     (sqlite3-bind-double statement n arg))
                  (else
                     (runtime-error "Unsupported parameter type" arg)))
               (loop (+ n 1) (cdr args)))))
      (let ((result (sqlite3-step statement)))
         (sqlite3-finalize statement)
         result)))

(define (select query)
   (let ((statement (make-sqlite3-stmt)))
      (if (less? 0 (sqlite3-prepare-v2 database (c-string query) -1 statement null))
         (print "error query [" query "] preparation"))
      (sqlite3-step statement)
      statement))

(define (sqlite:for-each query handler)
   (let ((statement (make-sqlite3-stmt)))
      (if (less? 0 (sqlite3-prepare-v2 database (c-string query) -1 statement null))
         (runtime-error "error query preparation" query))
      (let loop ((x null))
         (if (= (sqlite3-step statement) SQLITE_ROW)
            (loop (handler statement))))
      (sqlite3-finalize statement)))

; todo: add sqlite3-fold function:
(define (with-sql-query string processor)
   (define query (select string))
   (processor query)
   (sqlite3-finalize query))

(define statement (make-sqlite3-stmt))

;(execute "DROP TABLE IF EXIST test")
;(execute "CREATE TABLE test (id INTEGER)")
(sqlite:exec database "INSERT INTO test VALUES (1)")
(sqlite:exec database "INSERT INTO test VALUES (2)")
(sqlite:exec database "INSERT INTO test VALUES (7)")


(let ((db (make-sqlite3)))
   (sqlite3_open (c-string "db.sqlite") db)
   (sqlite:exec db "CREATE TABLE IF NOT EXISTS T (id INTEGER PRIMARY KEY, text STRING)")
   (sqlite:exec db "INSERT INTO T (text) VALUES (?)" "one")
   (sqlite:exec db "INSERT INTO T (text) VALUES (?)" "two")
   (sqlite:exec db "INSERT INTO T (text) VALUES (?)" "three")
   (sqlite:exec db "INSERT INTO T (text) VALUES (?)" "four")
   (sqlite3-close db))


(print "exec: " (sqlite:exec database "INSERT INTO test VALUES (?)" 11))

;(print "prepare: "  (sqlite3_prepare_v2 database (c-string "CREATE TABLE test ( id INTEGER )") -1 statement null))
;(print "step: " (sqlite3_step statement))

;(print "1: " (sqlite3_column_count statement))
;(print "name: " (sqlite3_column_name statement 1))
(define query (select "SELECT COUNT(id) FROM test"))
(print "count(id) = " (sqlite3-column-int query 0))
(sqlite3-finalize query)

(sqlite:for-each "SELECT * FROM test" (lambda (statement)
   (print "value: " (sqlite3-column-int statement 0))))

(print "finalize: " (sqlite3-finalize statement))
(print "close: "    (sqlite3-close database))

