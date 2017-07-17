#!/usr/bin/ol
(import (lib sqlite))

; вспомогательный макрос для собрать в кучку все bor
(define (OR . args) (fold bor 0 args))
(define sqlite3_context* type-vptr)

(define database (make-sqlite3))
(sqlite3_open (c-string ":memory:") database)

; create extension function
(define calculate (syscall 175 (cons
   (list sqlite3_context* type-int+ (list type-vptr))
   (lambda (context argc argv)
      (print "argc: " argc)
      (print "argv: " argv)

      (sqlite3_result_int context 777))
) #f #f))

(sqlite3_create_function_v2 database (c-string "compress") 1 SQLITE_UTF8 #f calculate #f #f #f)


; sample table
(sqlite:exec database "CREATE TABLE test (id INTEGER)")
(sqlite:exec database "INSERT INTO test VALUES (1)")

(print "for simple select: "
   (sqlite:value database "SELECT id FROM test"))

(print "for extension select: "
   (sqlite:value database "SELECT compress(id) FROM test"))

(sqlite3-close database)