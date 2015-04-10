; http://www.scheme.com/tspl4/ - The Scheme Programming Language (Fourth Edition)
; http://community.schemewiki.org/?scheme-faq-standards#implementations
;!

;  (define isCompiled (list->byte-vector '(0 0 0 0)))
;  (sys-prim 1033 isCompiled #false #false)
(import (lib sqlite))

; вспомогательный макрос для собрать в кучку все bor
(define OR (lambda list (fold bor 0 list)))

; todo: move this test into separate script (tests/sqlite3.scm)
(define database (make-sqlite3))
(print "open: "     (sqlite3-open (c-string ":memory:") database))
(print "close: "    (sqlite3-close database))
(print "open: "     (sqlite3-open (c-string ":memory:") database))



(define (execute query)
   (let ((statement (make-sqlite3-stmt)))
      (if (> 0 (sqlite3-prepare-v2 database (c-string query) -1 statement null))
         (print "error query [" query "] preparation"))
      (sqlite3-step statement)
      (sqlite3-finalize statement)))
(define (select query)
   (let ((statement (make-sqlite3-stmt)))
      (if (> 0 (sqlite3-prepare-v2 database (c-string query) -1 statement null))
         (print "error query [" query "] preparation"))
      (sqlite3-step statement)
      statement))
(define (select2 query handler)
   (let ((statement (make-sqlite3-stmt)))
      (if (> 0 (sqlite3-prepare-v2 database (c-string query) -1 statement null))
         (print "error query [" query "] preparation"))
      (let ((result
                (handler (if (= (sqlite3-step statement) SQLITE-ROW) statement null))))
         (sqlite3-finalize statement)
         result)))
(define (with-sql-query string processor)
   (define query (select string))
   (processor query)
   (sqlite3-finalize query))

(define statement (make-sqlite3-stmt))

(execute "DROP TABLE IF EXIST test")
(execute "CREATE TABLE test (id INTEGER)")
(execute "INSERT INTO test VALUES (1)")
(execute "INSERT INTO test VALUES (2)")
(execute "INSERT INTO test VALUES (7)")
;(print "prepare: "  (sqlite3_prepare_v2 database (c-string "CREATE TABLE test ( id INTEGER )") -1 statement null))
;(print "step: " (sqlite3_step statement))

;(print "1: " (sqlite3_column_count statement))
;(print "name: " (sqlite3_column_name statement 1))
(define query (select "SELECT COUNT(id) FROM test"))
(print "count(id) = " (sqlite3-column-int query 0))
(sqlite3-finalize query)

(print "count(id)2 = " (select2 "SELECT COUNT(id) FROM test"
   (lambda (statement)
      (if (null? statement)
         "no result"
         (sqlite3-column-int statement 0)))))

(print "finalize: " (sqlite3-finalize statement))
(print "close: "    (sqlite3-close database))

