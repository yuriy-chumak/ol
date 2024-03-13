#!/usr/bin/env ol
; http://www.scheme.com/tspl4/ - The Scheme Programming Language (Fourth Edition)
; http://community.schemewiki.org/?scheme-faq-standards#implementations
;!

(import (lib sqlite))

; вспомогательный макрос для собрать в кучку все bor
(define (OR . args) (fold bor 0 args))

; todo: move this test into separate script (tests/sqlite3.scm)
(define database (make-sqlite3))
(print "open: "  (sqlite3_open ":memory:" database))

; todo: add sqlite3-fold function:
;(define (with-sql-query string processor)
;   (define query (select string))
;   (processor query)
;   (sqlite3-finalize query))

(sqlite:query database "CREATE TABLE test (id INTEGER)")
(sqlite:query database "INSERT INTO test VALUES (1)")
(sqlite:query database "INSERT INTO test VALUES (2)")
(sqlite:query database "INSERT INTO test VALUES (7)")

(let ((db (make-sqlite3)))
   (sqlite3_open "db.sqlite" db)
   (sqlite:query db "CREATE TABLE IF NOT EXISTS T (id INTEGER PRIMARY KEY, text STRING)")
   (sqlite:query db "INSERT INTO T (text) VALUES (?)" "one")
   (sqlite:query db "INSERT INTO T (text) VALUES (?)" "two")
   (sqlite:query db "INSERT INTO T (text) VALUES (?)" "three")
   (sqlite:query db "INSERT INTO T (text) VALUES (?)" "four")
   (sqlite3_close db))

(print "query: " (sqlite:query database "INSERT INTO test VALUES (?)" 11))

(sqlite:for-each (sqlite:query database "SELECT * FROM test")
   (lambda (row)
      (print "values: " row)))

(print
(sqlite:map (sqlite:query database "SELECT * FROM test")
   (lambda (row)
      row)))

(print "close: "    (sqlite3_close database))

