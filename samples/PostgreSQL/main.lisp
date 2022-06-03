#!/usr/bin/env ol

(import (lib pq))

; Step 1 — Installing PostgreSQL
;  sudo apt install postgresql postgresql-contrib
;  sudo systemctl start postgresql.service
; Step 2 — Using PostgreSQL Roles and Databases
; Step 3 — Creating a New Role
;  sudo -u postgres createuser --interactive
; Step 4 — Creating a New Database
;  sudo -u postgres createdb template1

#|
   begin, by setting the parameters for a backend connection if the
   parameters are null, then the system will try to use reasonable
   defaults by looking up environment variables or, failing that,
   using hardwired constants
|#

(define pghost #f)
(define pgport #f)
(define pgoptions #f)
(define pgtty #f)
(define dbName "template1")

; make a connection to the database
(define conn (PQsetdb pghost pgport pgoptions pgtty dbName))

; check to see that the backend connection was successfully made
(when (eq? (PQstatus conn) CONNECTION_BAD)
   (runtime-error (string-append "Connection to database " dbName " failed.")
      (PQerrorMessage conn)))

; start a transaction block
(define res (PQexec conn "BEGIN"))
(unless (and res (<= (PQresultStatus conn) PGRES_COMMAND_OK))
   (PQclear res)
   (runtime-error "BEGIN command failed" '()))

; should PQclear PGresult whenever it is no longer needed to avoid memory leaks
(PQclear res)

; create table and fill table with sample data
(for-each (lambda (query)
      (define res (PQexec conn query))
      (when (or
               (not res)
               (> (PQresultStatus conn) PGRES_COMMAND_OK))
         (PQclear res)
         (runtime-error "BEGIN command failed" '()))
      (PQclear res))
   '(
      "DROP TABLE IF EXISTS test"
      "CREATE TABLE test (id INTEGER, msg TEXT)"
      "INSERT INTO test VALUES (1,'one')"
      "INSERT INTO test VALUES (2,'two')"
      "INSERT INTO test VALUES (7,'seven')"
   ))
(PQexec conn "COMMIT")

; Fetch rows from pg_database, the system catalog of databases
(PQexec conn "BEGIN")
(define res (PQexec conn "DECLARE myportal CURSOR FOR select id,msg from test where id > 1"))
(unless (eq? (PQresultStatus res) PGRES_COMMAND_OK)
   (define message (PQerrorMessage conn))
   (PQclear res)
   (runtime-error "DECLARE CURSOR failed:" message))
(PQclear res)

(define res (PQexec conn "FETCH ALL in myportal"))
(unless (eq? (PQresultStatus res) PGRES_TUPLES_OK)
   (define message (PQerrorMessage conn))
   (PQclear res)
   (runtime-error "FETCH ALL failed:" message))

; first, print out the attribute names
(define nFields (PQnfields res))
(for-each (lambda (n)
      (print "name[" n "]: " (PQfname res n)))
   (iota nFields))

(for-each (lambda (n)
      (for-each (lambda (j)
            (print "value[" n ", " j "]: " (PQgetvalue res n j)))
         (iota nFields)))
   (iota (PQntuples res)))
(PQclear res)

(define res (PQexec conn "CLOSE myportal"))
(PQclear res)

; end the transaction
(PQclear (PQexec conn "END"))

; close the connection to the database and cleanup
(PQfinish conn)
(print "ok")