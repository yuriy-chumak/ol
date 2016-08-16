#!/usr/bin/ol

(import (lib sqlite))

; для упрощения работы и т.д,  пусть у нас будет одна большая база даных с множеством таблиц
(define database (make-sqlite3)) ; make new database connection
(print "open: "  (sqlite3-open (c-string "database.sqlite") database))

(define (sqlite:query query . args)
   (let ((statement (make-sqlite3-stmt)))
      (if (less? 0 (sqlite3-prepare-v2 database (c-string query) -1 statement null))
         (runtime-error "error query preparation" query))
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
      (case (sqlite3-step statement)
         (SQLITE-ROW
            statement)
         (SQLITE-DONE
            (let ((result (sqlite3-last-insert-rowid database)))
               (sqlite3-finalize statement)
               result))
         (else
            (sqlite3-finalize statement)
            (runtime-error "Can't execute SQL statement" #t)))))

(define (sqlite:for-each statement f)
   (let loop ()
      (let ((n (sqlite3_column_count statement)))
      ;(print "n: " n)
      (if (less? 0 n) (begin
         (apply f
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
                     args)))))
         ;(print "--------------------")
         (case (sqlite3-step statement)
            (SQLITE-ROW
               (loop))
            (SQLITE-DONE
               (sqlite3-finalize statement))
            (else
               (sqlite3-finalize statement)
               (runtime-error "Can't execute SQL statement" #t))))))))
