#!/usr/bin/env ol

(import (lib pq))

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
(when (or
         (not res)
         (not (eq? (PQresultStatus conn) PGRES_COMMAND_OK)))
   (PQclear res)
   (runtime-error "BEGIN command failed" '()))

; should PQclear PGresult whenever it is no longer needed to avoid memory leaks
(PQclear res)
; ...
; TBD.
; ...

; commit the transaction
(PQclear (PQexec conn "COMMIT"))

; close the connection to the database and cleanup
(PQfinish conn)
